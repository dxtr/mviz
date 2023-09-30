module Mviz.Audio
  ( AudioReturn
  , runAudioSystem
  , shutdown
  , ClientAudioMessage (..)
  , ServerAudioMessage (..)
  ) where

import           Control.Concurrent.STM     (TQueue, atomically, tryReadTQueue,
                                             writeTQueue)
import           Control.Monad.Except       (MonadError, runExceptT)
import           Control.Monad.Logger       (LoggingT, MonadLogger)
import           Control.Monad.Reader       (MonadReader, ReaderT, ask,
                                             runReaderT)
import           Control.Monad.State.Strict (MonadIO, MonadState, StateT,
                                             evalStateT, get, liftIO)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import qualified Mviz.Audio.Client          as Client
import           Mviz.Audio.Types           (AudioError (..),
                                             HasAudioClient (..),
                                             JackReturnType, MonadJack (..))
import qualified Sound.JACK                 as JACK

type AudioReturn = Either AudioError ()

-- data AudioMessage
--   = Quit
--   deriving (Show)

-- Messages directed to the client
data ClientAudioMessage
  = Port String -- Inform the client about a port
  | SampleRate Int -- Inform the client about the sample rate
  | BufferSize Int -- Inform the client about the buffer size
  deriving (Show)

-- Messages directed to the server
data ServerAudioMessage
  = Quit -- Tell the server to quit
  | GetSampleRate
  | GetBufferSize
  deriving (Show)

data AudioState = AudioState
  { audioSendChannel :: TQueue ClientAudioMessage
  , audioRecvChannel :: TQueue ServerAudioMessage
  , audioClient      :: JACK.Client
  }

newtype AudioM e a = AudioM (ReaderT e IO a)
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadReader e
           )

instance HasAudioClient AudioState where
  getAudioClient = audioClient

instance (HasAudioClient env) => MonadJack (AudioM env) where
  jackAction = Client.jackAction
  activateClient = ask >>= Client.activateClient . getAudioClient
  deactivateClient = ask >>= Client.deactivateClient . getAudioClient
  ports = ask >>= Client.getPorts . getAudioClient
  bufferSize = ask >>= Client.getBufferSize . getAudioClient
  sampleRate = ask >>= Client.getSampleRate . getAudioClient
--  closeClient = ask >>= Client.closeClient . getAudioClient

audioLoop :: (MonadIO m, MonadReader AudioState m) => m ()
audioLoop = do
  state <- ask
  msg <- liftIO $ atomically $ tryReadTQueue $ audioRecvChannel state
  case msg of
    Just _ -> do
      liftIO $ putStrLn "Quitting audio system"
      return ()
    Nothing -> audioLoop

clientName :: String
clientName = "mviz"

runAudioSystem
  :: (MonadIO m)
  => TQueue ClientAudioMessage
  -> TQueue ServerAudioMessage
  -> m ()
runAudioSystem sendChan recvChan = do
  client <- Client.createClient clientName
  let state = AudioState { audioSendChannel = sendChan
                         , audioRecvChannel = recvChan
                         , audioClient = client
                         }
--  runAudio state audioLoop
  runReaderT audioLoop state

shutdown :: (MonadIO m) => TQueue ServerAudioMessage -> m ()
shutdown writeChan = liftIO $ atomically $ writeTQueue writeChan msg
 where
  msg = Quit
