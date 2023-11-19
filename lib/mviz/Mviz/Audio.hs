{-# LANGUAGE OverloadedStrings #-}

module Mviz.Audio
  ( runAudioSystem
  , shutdown
  , ClientAudioMessage (..)
  , ServerAudioMessage (..)
  , HasServerChannel (..)
  , HasClientChannel (..)
  ) where

import           Control.Concurrent.STM  (TQueue, atomically, tryReadTQueue,
                                          writeTQueue)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.Reader    (MonadReader, ReaderT, ask, asks,
                                          runReaderT)
import           Data.Functor            ((<&>))
import           Data.IORef              (IORef, readIORef, writeIORef)
import qualified Data.Text               as T
import           GHC.IORef               (newIORef)
import qualified Mviz.Audio.Client       as Client
import           Mviz.Audio.Inputs       (InputMap, mkInputMap)
import           Mviz.Audio.Types        (ClientAudioMessage (..),
                                          HasAudioClient (..),
                                          HasClientChannel (..),
                                          HasInputPorts (..),
                                          HasServerChannel (..), InputPort (..),
                                          JackReturnType, MonadAudioServer (..),
                                          MonadJack (..),
                                          ServerAudioMessage (..))
import qualified Sound.JACK              as JACK

data AudioState = AudioState
  { audioSendChannel :: TQueue ClientAudioMessage
  , audioRecvChannel :: TQueue ServerAudioMessage
  , audioClient      :: JACK.Client
  , audioInputPorts  :: IORef [InputPort] -- These are the ports that connect to the device we want to listen on
  }

newtype AudioM e a = AudioM (ReaderT e IO a)
  deriving ( Applicative
           , Functor
           , Monad
           , MonadUnliftIO
           , MonadIO
           , MonadReader e
           )

instance HasAudioClient AudioState where
  getAudioClient :: AudioState -> JACK.Client
  getAudioClient = audioClient

instance HasServerChannel AudioState where
  getServerChannel :: AudioState -> TQueue ServerAudioMessage
  getServerChannel = audioRecvChannel

instance HasClientChannel AudioState where
  getClientChannel :: AudioState -> TQueue ClientAudioMessage
  getClientChannel = audioSendChannel

instance (HasAudioClient env, HasInputPorts env) => MonadJack (AudioM env) where
  jackAction :: JackReturnType a -> AudioM env a
  jackAction = Client.jackAction

  ports :: AudioM env [T.Text]
  ports = ask >>= Client.getPorts . getAudioClient

  inputs :: AudioM env InputMap
  inputs = ports <&> mkInputMap

  bufferSize :: AudioM env Word
  bufferSize = ask >>= Client.getBufferSize . getAudioClient

  sampleRate :: AudioM env Word
  sampleRate = ask >>= Client.getSampleRate . getAudioClient

  newPort :: T.Text -> T.Text -> AudioM env InputPort
  newPort name target = do
    client <- asks getAudioClient
    port <- jackAction $ JACK.newPort client (T.unpack name)
    pure $ InputPort { inputPortHandle = port
                     , inputPortTarget = target
                     }

  disposePort :: InputPort -> AudioM env ()
  disposePort InputPort{inputPortHandle = handle} = jackAction . flip JACK.disposePort handle =<< asks getAudioClient

  setPorts :: [InputPort] -> AudioM env ()
  setPorts p = ask >>= liftIO . flip setInputPorts p

  getPorts :: AudioM env [InputPort]
  getPorts = ask >>= liftIO . getInputPorts

  portName :: InputPort -> AudioM env T.Text
  portName InputPort{inputPortHandle = handle} = do
    liftIO $ JACK.portName handle <&> T.pack

  connectPorts :: [T.Text] -> AudioM env ()
  connectPorts targets = do
    p <- getPorts
    c <- asks getAudioClient
    let d = zip p targets
    mapM_ (\(src, tgt) -> do
      srcName <- portName src
      connect c (T.unpack srcName) (T.unpack tgt)) d
    where
      connect client src = jackAction . JACK.connect client src

instance (HasServerChannel env, HasClientChannel env) => MonadAudioServer (AudioM env) where
  serverRecvChannel :: (HasServerChannel env, HasClientChannel env) => AudioM env (TQueue ServerAudioMessage)
  serverRecvChannel = asks getServerChannel

  serverSendChannel :: (HasServerChannel env, HasClientChannel env) => AudioM env (TQueue ClientAudioMessage)
  serverSendChannel = asks getClientChannel

  serverRecvMessage :: (HasServerChannel env, HasClientChannel env) => AudioM env (Maybe ServerAudioMessage)
  serverRecvMessage = liftIO . atomically . tryReadTQueue =<< serverRecvChannel

  serverSendMessage :: (HasServerChannel env, HasClientChannel env) => ClientAudioMessage -> AudioM env ()
  serverSendMessage msg = serverSendChannel >>= \c -> liftIO . atomically $ writeTQueue c msg

instance HasInputPorts AudioState where
  getInputPorts :: AudioState -> IO [InputPort]
  getInputPorts = readIORef . audioInputPorts
  setInputPorts :: AudioState -> [InputPort] -> IO ()
  setInputPorts = writeIORef . audioInputPorts

data Progress = Stop | Continue

handleAudioMessage :: (MonadAudioServer m, MonadJack m) => ServerAudioMessage -> m Progress
handleAudioMessage Quit          = pure Stop
handleAudioMessage GetBufferSize = bufferSize >>= serverSendMessage . BufferSize >> pure Continue
handleAudioMessage GetSampleRate = sampleRate >>= serverSendMessage . SampleRate >> pure Continue
handleAudioMessage GetInputs = inputs >>= serverSendMessage . Inputs >> pure Continue
handleAudioMessage (SetInput _input@(inputName, channels)) = do
  -- let targetPortNames = map (\c -> T.concat [inputName, ":", c]) channels
  let targetPortNames = [T.concat [inputName, ":", c] | c <- channels]
  let newPortNames = [T.concat ["in_", T.pack $ show n] | n <- [0,1..(length targetPortNames - 1)]]
  let srcTargets = zip newPortNames targetPortNames
  mapM_ disposePort =<< getPorts -- Remove the old ports
  p <- mapM (uncurry newPort) srcTargets -- Create new ports
  setPorts p
  -- TODO: Register new ports
  -- TODO: Connect new ports
  pure Continue

audioLoop :: ( HasAudioClient e
             , HasServerChannel e
             , MonadReader e m
             , MonadAudioServer m
             , MonadJack m
             ) => m ()
audioLoop = do
--  state <- ask
  -- rc <- recvChannel
  msg <- maybe (pure Continue) handleAudioMessage =<< serverRecvMessage
  case msg of
    Stop     -> pure ()
    Continue -> audioLoop

clientName :: String
clientName = "mviz"

runAudioSystem
  :: TQueue ClientAudioMessage
  -> TQueue ServerAudioMessage
  -> IO ()
runAudioSystem sendChan recvChan = do
  inputPorts <- newIORef []
  Client.withClient clientName $ \c -> do
    let state = AudioState { audioSendChannel = sendChan
                           , audioRecvChannel = recvChan
                           , audioClient = c
                           , audioInputPorts = inputPorts
                           }
    runAudio state $ do
      sampleRate >>= serverSendMessage . SampleRate -- Send the configured sample rate to the client
      bufferSize >>= serverSendMessage . BufferSize -- Send the configured buffer size to the client
      ports >>= serverSendMessage . Ports
      inputs >>= serverSendMessage . Inputs
      -- TODO: Create some ports
      -- TODO: Set the process callback
      -- TODO: Set the shutdown callback
      Client.withActivation c audioLoop
  where runAudio environment (AudioM action) = runReaderT action environment

shutdown :: (MonadIO m) => TQueue ServerAudioMessage -> m ()
shutdown writeChan = liftIO $ atomically $ writeTQueue writeChan msg
 where
  msg = Quit

