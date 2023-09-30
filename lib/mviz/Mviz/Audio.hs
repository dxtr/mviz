module Mviz.Audio
  ( runAudioSystem
  , shutdown
  , ClientAudioMessage (..)
  , ServerAudioMessage (..)
  ) where

import           Control.Concurrent.STM (TQueue, atomically, tryReadTQueue,
                                         writeTQueue)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import qualified Mviz.Audio.Client      as Client
import           Mviz.Audio.Types       (ClientAudioMessage (..),
                                         HasAudioClient (..), MonadJack (..),
                                         ServerAudioMessage (..))
import qualified Sound.JACK             as JACK

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
