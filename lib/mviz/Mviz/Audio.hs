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
import qualified Data.Text               as T
import qualified Mviz.Audio.Client       as Client
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

  activateClient :: HasAudioClient env => AudioM env ()
  activateClient = ask >>= Client.activateClient . getAudioClient

  deactivateClient :: HasAudioClient env => AudioM env ()
  deactivateClient = ask >>= Client.deactivateClient . getAudioClient

  ports :: HasAudioClient env => AudioM env [T.Text]
  ports = ask >>= Client.getPorts . getAudioClient

  bufferSize :: HasAudioClient env => AudioM env Int
  bufferSize = ask >>= Client.getBufferSize . getAudioClient

  sampleRate :: HasAudioClient env => AudioM env Int
  sampleRate = ask >>= Client.getSampleRate . getAudioClient
--  closeClient = ask >>= Client.closeClient . getAudioClient

instance (HasServerChannel env, HasClientChannel env) => MonadAudioServer (AudioM env) where
  serverRecvChannel :: (HasServerChannel env, HasClientChannel env) => AudioM env (TQueue ServerAudioMessage)
  serverRecvChannel = asks getServerChannel

  serverSendChannel :: (HasServerChannel env, HasClientChannel env) => AudioM env (TQueue ClientAudioMessage)
  serverSendChannel = asks getClientChannel

  serverRecvMessage :: (HasServerChannel env, HasClientChannel env) => AudioM env (Maybe ServerAudioMessage)
  serverRecvMessage = liftIO . atomically . tryReadTQueue =<< serverRecvChannel

  serverSendMessage :: (HasServerChannel env, HasClientChannel env) => ClientAudioMessage -> AudioM env ()
  serverSendMessage msg = serverSendChannel >>= \c -> liftIO . atomically $ writeTQueue c msg

audioLoop :: ( HasAudioClient e
             , HasServerChannel e
             , MonadReader e m
             , MonadAudioServer m
             ) => m ()
audioLoop = do
--  state <- ask
  -- rc <- recvChannel
  msg <- serverRecvMessage
  case msg of
    Just _  -> return ()
    Nothing -> audioLoop

-- audioLoop :: ( HasAudioClient e
--              , HasRecvChannel e
--              , MonadReader e m
--              , MonadAudioServer m
--              ) => m ()
-- audioLoop = do


clientName :: String
clientName = "mviz"

-- runAudio :: AudioState -> AudioM AudioState a -> IO a
-- runAudio environment (AudioM action) = runReaderT action environment

runAudioSystem
--  :: (MonadIO m)
  :: TQueue ClientAudioMessage
  -> TQueue ServerAudioMessage
  -> IO ()
runAudioSystem sendChan recvChan = do
  client <- Client.createClient clientName
  let state = AudioState { audioSendChannel = sendChan
                         , audioRecvChannel = recvChan
                         , audioClient = client
                         }
--  runAudio state audioLoop
  _ <- runAudio state $ do
    -- TODO: Fetch all the ports and send on the channel
    ports >>= serverSendMessage . Ports
    audioLoop
  return ()
  where runAudio environment (AudioM action) = runReaderT action environment

shutdown :: (MonadIO m) => TQueue ServerAudioMessage -> m ()
shutdown writeChan = liftIO $ atomically $ writeTQueue writeChan msg
 where
  msg = Quit
