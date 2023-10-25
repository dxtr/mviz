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
import qualified Data.Text               as T
import qualified Mviz.Audio.Client       as Client
import           Mviz.Audio.Inputs       (InputMap, mkInputMap)
import           Mviz.Audio.Types        (ClientAudioMessage (..),
                                          HasAudioClient (..),
                                          HasClientChannel (..),
                                          HasServerChannel (..), JackReturnType,
                                          MonadAudioServer (..), MonadJack (..),
                                          ServerAudioMessage (..))
import qualified Sound.JACK              as JACK

data AudioState = AudioState
  { audioSendChannel :: TQueue ClientAudioMessage
  , audioRecvChannel :: TQueue ServerAudioMessage
  , audioClient      :: JACK.Client
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

instance (HasAudioClient env) => MonadJack (AudioM env) where
  jackAction :: HasAudioClient env => JackReturnType a -> AudioM env a
  jackAction = Client.jackAction

  ports :: HasAudioClient env => AudioM env [T.Text]
  ports = ask >>= Client.getPorts . getAudioClient

  inputs :: HasAudioClient env => AudioM env InputMap
  inputs = ports <&> mkInputMap

  bufferSize :: HasAudioClient env => AudioM env Word
  bufferSize = ask >>= Client.getBufferSize . getAudioClient

  sampleRate :: HasAudioClient env => AudioM env Word
  sampleRate = ask >>= Client.getSampleRate . getAudioClient

instance (HasServerChannel env, HasClientChannel env) => MonadAudioServer (AudioM env) where
  serverRecvChannel :: (HasServerChannel env, HasClientChannel env) => AudioM env (TQueue ServerAudioMessage)
  serverRecvChannel = asks getServerChannel

  serverSendChannel :: (HasServerChannel env, HasClientChannel env) => AudioM env (TQueue ClientAudioMessage)
  serverSendChannel = asks getClientChannel

  serverRecvMessage :: (HasServerChannel env, HasClientChannel env) => AudioM env (Maybe ServerAudioMessage)
  serverRecvMessage = liftIO . atomically . tryReadTQueue =<< serverRecvChannel

  serverSendMessage :: (HasServerChannel env, HasClientChannel env) => ClientAudioMessage -> AudioM env ()
  serverSendMessage msg = serverSendChannel >>= \c -> liftIO . atomically $ writeTQueue c msg

data Progress = Stop | Continue

handleAudioMessage :: (MonadAudioServer m, MonadJack m) => ServerAudioMessage -> m Progress
handleAudioMessage Quit          = pure Stop
handleAudioMessage GetBufferSize = bufferSize >>= serverSendMessage . BufferSize >> pure Continue
handleAudioMessage GetSampleRate = sampleRate >>= serverSendMessage . SampleRate >> pure Continue
handleAudioMessage GetInputs = inputs >>= serverSendMessage . Inputs >> pure Continue

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
--  :: (MonadIO m)
  :: TQueue ClientAudioMessage
  -> TQueue ServerAudioMessage
  -> IO ()
runAudioSystem sendChan recvChan = do
  Client.withClient clientName $ \c -> do
    let state = AudioState { audioSendChannel = sendChan
                           , audioRecvChannel = recvChan
                           , audioClient = c
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
