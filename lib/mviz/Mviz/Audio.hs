{-# LANGUAGE OverloadedStrings #-}

module Mviz.Audio
  ( runAudioSystem
  , shutdown
  , mixChannelBuffers
  , ClientAudioMessage (..)
  , ServerAudioMessage (..)
  , HasServerChannel (..)
  , HasClientChannel (..)
  ) where

import           Control.Concurrent            (MVar, newEmptyMVar, putMVar,
                                                takeMVar, tryPutMVar)
import           Control.Concurrent.STM        (TQueue, atomically, writeTQueue)
import           Control.Concurrent.STM.TQueue (readTQueue)
import           Control.Monad                 (when)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.IO.Unlift       (MonadUnliftIO)
import           Control.Monad.Reader          (MonadReader, ReaderT, ask, asks,
                                                runReaderT)
import           Data.Array.Base               (getElems)
import           Data.Functor                  ((<&>))
import           Data.IORef                    (IORef, newIORef, readIORef,
                                                writeIORef)
import qualified Data.Text                     as T
import           Foreign                       (Ptr)
import qualified Foreign                       as Ptr
import           Foreign.C                     (CFloat)
import           Foreign.C.Error               (Errno, eOK)
import           GHC.Stack                     (HasCallStack)
import qualified Mviz.Audio.Client             as Client
import           Mviz.Audio.Inputs             (InputMap, mkInputMap)
import           Mviz.Audio.Types              (ClientAudioMessage (..),
                                                HasAudioClient (..),
                                                HasClientChannel (..),
                                                HasInputPorts (..),
                                                HasPortLock (..),
                                                HasSamples (getSampleBufferRef),
                                                HasServerChannel (..),
                                                InputPort (..), JackReturnType,
                                                MonadAudioServer (..),
                                                MonadJack (..),
                                                ServerAudioMessage (..))
import qualified Mviz.Utils.Audio              as AU
import qualified Sound.JACK                    as JACK
import qualified Sound.JACK.Audio              as JACKA
import           UnliftIO.Exception            (SomeException, bracket_,
                                                evaluate, try)

data AudioState = AudioState
  { audioSendChannel  :: !(TQueue ClientAudioMessage)
  , audioRecvChannel  :: !(TQueue ServerAudioMessage)
  , audioClient       :: !(JACK.Client)
  , audioInputPorts   :: !(IORef [InputPort]) -- These are the ports that connect to the device we want to listen on
  , audioSampleBuffer :: !(IORef [[JACKA.Sample]])
  , audioPortLock     :: !(MVar ())
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

instance HasPortLock AudioState where
  getPortLock :: AudioState -> MVar ()
  getPortLock = audioPortLock

instance (HasAudioClient env, HasInputPorts env, HasSamples env, HasPortLock env) => MonadJack (AudioM env) where
  jackAction :: HasCallStack => JackReturnType a -> AudioM env a
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
    longName <- portName $ InputPort{inputPortName = "", inputPortHandle = port, inputPortTarget = ""}
    pure $ InputPort { inputPortName = longName
                     , inputPortHandle = port
                     , inputPortTarget = target
                     }

  disposePort :: InputPort -> AudioM env ()
  disposePort InputPort{inputPortHandle = h} = jackAction . flip JACK.disposePort h =<< asks getAudioClient

  setPorts :: [InputPort] -> AudioM env ()
  setPorts p = ask >>= liftIO . flip setInputPorts p

  getPorts :: AudioM env [InputPort]
  getPorts = ask >>= liftIO . getInputPorts

  portName :: InputPort -> AudioM env T.Text
  portName InputPort{inputPortHandle = h} = do
    liftIO $ JACK.portName h <&> T.pack

  connectPorts :: [InputPort] -> AudioM env ()
  connectPorts p = do
    -- p <- getPorts
    c <- asks getAudioClient
    mapM_ (\InputPort{inputPortName = srcName, inputPortTarget = targetName} -> do
      -- srcName <- portName src
      connect c targetName srcName) p
    where
      connect client src tgt = jackAction $ JACK.connect client (T.unpack src) (T.unpack tgt)

  disconnectPorts :: AudioM env ()
  disconnectPorts = pure ()

  isPortConnected :: InputPort -> AudioM env Bool
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

  withPortLock :: AudioM env a -> AudioM env a
  withPortLock action = do
    lock <- asks getPortLock
    bracket_ (liftIO $ putMVar lock ())
             (liftIO $ takeMVar lock)
             action

instance (HasServerChannel env, HasClientChannel env) => MonadAudioServer (AudioM env) where
  serverRecvChannel :: AudioM env (TQueue ServerAudioMessage)
  serverRecvChannel = asks getServerChannel

  serverSendChannel :: AudioM env (TQueue ClientAudioMessage)
  serverSendChannel = asks getClientChannel

  serverRecvMessage :: AudioM env ServerAudioMessage
  serverRecvMessage = liftIO . atomically . readTQueue =<< serverRecvChannel

  serverSendMessage :: ClientAudioMessage -> AudioM env ()
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

handleAudioMessage :: (MonadAudioServer m, MonadJack m, MonadIO m) => ServerAudioMessage -> m Progress
handleAudioMessage Quit          = pure Stop
handleAudioMessage GetBufferSize = bufferSize >>= serverSendMessage . BufferSize >> pure Continue
handleAudioMessage GetSampleRate = sampleRate >>= serverSendMessage . SampleRate >> pure Continue
handleAudioMessage GetInputs = inputs >>= serverSendMessage . Inputs >> pure Continue
handleAudioMessage (SetInput _input@(inputName, channels)) = do
  withPortLock $ do
    mapM_ disposePort =<< getPorts -- Remove the old ports
    p <- mapM (uncurry newPort) srcTargets -- Create new ports
    -- sb <- sampleBufferRef
    connectPorts p
    setPorts p
    pure Continue
  where
    targetPortNames = [T.concat [inputName, ":", c] | c <- channels]
    newPortNames = [T.concat ["in_", T.pack $ show n] | n <- [0,1..(length targetPortNames - 1)]]
    srcTargets = zip newPortNames targetPortNames
handleAudioMessage GetSamples = do
  bufSize <- bufferSize
  buf <- sampleBuffer
  let fft = AU.fft bufSize $ mixChannelBuffers buf
  case fft of
    Left err      -> liftIO $ putStrLn $ "Buffer error: " <> err
    Right samples -> serverSendMessage $ Samples samples
  pure Continue

audioLoop :: ( HasAudioClient e
             , HasServerChannel e
             , MonadReader e m
             , MonadAudioServer m
             , MonadJack m
             , MonadUnliftIO m
             ) => m ()
audioLoop = do
  msg :: Either SomeException Progress <- try $ handleAudioMessage =<< serverRecvMessage
  case msg of
    Left e         -> handleException e
    Right Stop     -> pure ()
    Right Continue -> audioLoop

clientName :: String
clientName = "mviz"

handleException :: (MonadAudioServer m) => SomeException -> m ()
handleException ex = serverSendMessage $ AudioThreadError ex

runAudioSystem
  :: TQueue ClientAudioMessage
  -> TQueue ServerAudioMessage
  -> IO ()
runAudioSystem sendChan recvChan = do
  inputPorts <- newIORef []
  sb <- newIORef []
  portLock <- newEmptyMVar
  Client.withClient clientName $ \c -> do
    let state = AudioState { audioSendChannel = sendChan
                           , audioRecvChannel = recvChan
                           , audioClient = c
                           , audioInputPorts = inputPorts
                           , audioSampleBuffer = sb
                           , audioPortLock = portLock
                           }
    runAudio state $ do
      sampleRate >>= serverSendMessage . SampleRate -- Send the configured sample rate to the client
      bufferSize >>= serverSendMessage . BufferSize -- Send the configured buffer size to the client
      ports >>= serverSendMessage . Ports
      inputs >>= serverSendMessage . Inputs
      setProcessCallback (processCallback portLock inputPorts sb) Ptr.nullPtr
      -- TODO: Set the shutdown callback
      Client.withActivation c audioLoop
  where runAudio environment (AudioM action) = runReaderT action environment

shutdown :: (MonadIO m) => TQueue ServerAudioMessage -> m ()
shutdown writeChan = liftIO . atomically $ writeTQueue writeChan Quit

processCallback :: MVar () -> IORef [InputPort] -> IORef [[JACKA.Sample]] -> JACK.NFrames -> Ptr Int -> IO Errno
processCallback lock ipRef bufferRef nframes _arg = do
    lockAcquired <- tryPutMVar lock ()
    when lockAcquired $
      readIORef ipRef
      >>= mapM (flip JACKA.getBufferArray nframes . inputPortHandle)
      >>= mapM getElems
      >>= evaluate -- Make sure everything is evaluted
      >>= writeIORef bufferRef
      >> takeMVar lock
    >> pure eOK
