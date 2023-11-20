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
import           Control.Exception       (catch, evaluate, handle)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.Reader    (MonadReader, ReaderT, ask, asks,
                                          runReaderT)
import           Data.Array.Base         (getElems)
import           Data.Functor            ((<&>))
import           Data.IORef
import qualified Data.Text               as T
import           Foreign                 (Ptr)
import qualified Foreign                 as Ptr
import           Foreign.C               (CFloat)
import           Foreign.C.Error         (Errno, eOK)
import qualified Mviz.Audio.Client       as Client
import           Mviz.Audio.Inputs       (InputMap, mkInputMap)
import           Mviz.Audio.Types        (AudioError, ClientAudioMessage (..),
                                          HasAudioClient (..),
                                          HasClientChannel (..),
                                          HasInputPorts (..),
                                          HasSamples (getSampleBufferRef),
                                          HasServerChannel (..), InputPort (..),
                                          JackReturnType, MonadAudioServer (..),
                                          MonadJack (..),
                                          ServerAudioMessage (..))
import qualified Sound.JACK              as JACK
import qualified Sound.JACK.Audio        as JACKA

data AudioState = AudioState
  { audioSendChannel  :: TQueue ClientAudioMessage
  , audioRecvChannel  :: TQueue ServerAudioMessage
  , audioClient       :: JACK.Client
  , audioInputPorts   :: IORef [InputPort] -- These are the ports that connect to the device we want to listen on
  , audioSampleBuffer :: IORef [[JACKA.Sample]]
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

instance (HasAudioClient env, HasInputPorts env, HasSamples env) => MonadJack (AudioM env) where
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
    port :: JACK.Port CFloat JACK.Input <- jackAction $ JACK.newPort client (T.unpack name)
    longName <- portName $ InputPort{inputPortName = "", inputPortHandle = port, inputPortTarget = ""}
    pure $ InputPort { inputPortName = longName
                     , inputPortHandle = port
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

  connectPorts :: AudioM env ()
  connectPorts = do
    p <- getPorts
    c <- asks getAudioClient
    mapM_ (\InputPort{inputPortName = srcName, inputPortTarget = targetName} -> do
      -- srcName <- portName src
      connect c targetName srcName) p
    where
      connect client src tgt = jackAction $ JACK.connect client (T.unpack src) (T.unpack tgt)

  disconnectPorts :: (HasAudioClient env, HasInputPorts env) => AudioM env ()
  disconnectPorts = pure ()

  isPortConnected :: (HasAudioClient env, HasInputPorts env) => InputPort -> AudioM env Bool
  isPortConnected _ = pure False

  setProcessCallback :: JACK.Process a -> Ptr a -> AudioM env ()
  setProcessCallback f arg = do
    client <- asks getAudioClient
    p <- liftIO $ JACK.makeProcess f
    jackAction $ JACK.setProcess client p arg

  sampleBufferRef :: AudioM env (IORef [[JACKA.Sample]])
  sampleBufferRef = asks getSampleBufferRef

  sampleBuffer :: AudioM env [[JACKA.Sample]]
  sampleBuffer = liftIO . readIORef =<< sampleBufferRef

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

instance HasSamples AudioState where
  getSampleBufferRef :: AudioState -> IORef [[JACKA.Sample]]
  getSampleBufferRef = audioSampleBuffer

data Progress = Stop | Continue

-- TODO: Should the result also be averaged?
-- TODO: Should realToFrac be replaced with something else?
mixChannelBuffers :: [[CFloat]] -> [Float]
mixChannelBuffers [] = []
mixChannelBuffers [cb] = map realToFrac cb
mixChannelBuffers (f:r) =
  zipWith (+) firstBuffer restBuffers
  where
    firstBuffer = mixChannelBuffers [f]
    restBuffers = mixChannelBuffers r

handleAudioMessage :: (MonadAudioServer m, MonadJack m) => ServerAudioMessage -> m Progress
handleAudioMessage Quit          = pure Stop
handleAudioMessage GetBufferSize = bufferSize >>= serverSendMessage . BufferSize >> pure Continue
handleAudioMessage GetSampleRate = sampleRate >>= serverSendMessage . SampleRate >> pure Continue
handleAudioMessage GetInputs = inputs >>= serverSendMessage . Inputs >> pure Continue
handleAudioMessage (SetInput _input@(inputName, channels)) = do
  mapM_ disposePort =<< getPorts -- Remove the old ports
  p <- mapM (uncurry newPort) srcTargets -- Create new ports
  sb <- sampleBufferRef
  setPorts p
  setProcessCallback (processCallback p sb) Ptr.nullPtr
  connectPorts
  pure Continue
  where
    targetPortNames = [T.concat [inputName, ":", c] | c <- channels]
    newPortNames = [T.concat ["in_", T.pack $ show n] | n <- [0,1..(length targetPortNames - 1)]]
    srcTargets = zip newPortNames targetPortNames
handleAudioMessage GetSamples = do
  sampleBuffer >>= (serverSendMessage . Samples) . mixChannelBuffers
  pure Continue

audioLoop :: ( HasAudioClient e
             , HasServerChannel e
             , MonadReader e m
             , MonadAudioServer m
             , MonadJack m
             ) => m ()
audioLoop = do
  msg <- maybe (pure Continue) handleAudioMessage =<< serverRecvMessage
  case msg of
    Stop     -> pure ()
    Continue -> audioLoop

clientName :: String
clientName = "mviz"

handleException :: AudioError -> IO ()
handleException ex = do
  putStrLn $ "Caught exception: " <> show ex

runAudioSystem
  :: TQueue ClientAudioMessage
  -> TQueue ServerAudioMessage
  -> IO ()
runAudioSystem sendChan recvChan = do
  inputPorts <- newIORef []
  sb <- newIORef []
  handle @AudioError handleException $ do
    Client.withClient clientName $ \c -> do
      let state = AudioState { audioSendChannel = sendChan
                            , audioRecvChannel = recvChan
                            , audioClient = c
                            , audioInputPorts = inputPorts
                            , audioSampleBuffer = sb
                            }
      runAudio state $ do
        sampleRate >>= serverSendMessage . SampleRate -- Send the configured sample rate to the client
        bufferSize >>= serverSendMessage . BufferSize -- Send the configured buffer size to the client
        ports >>= serverSendMessage . Ports
        inputs >>= serverSendMessage . Inputs
        -- TODO: Set the shutdown callback
        Client.withActivation c audioLoop
  where runAudio environment (AudioM action) = runReaderT action environment

shutdown :: (MonadIO m) => TQueue ServerAudioMessage -> m ()
shutdown writeChan = liftIO . atomically $ writeTQueue writeChan Quit

processCallback :: [InputPort] -> IORef [[JACKA.Sample]] -> JACK.NFrames -> Ptr Int -> IO Errno
processCallback p bufferRef nframes _arg =
    mapM (flip JACKA.getBufferArray nframes . inputPortHandle) p
    >>= mapM getElems
    >>= evaluate -- Make sure everything is evaluted
    >>= writeIORef bufferRef
    >> pure eOK
