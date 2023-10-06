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
import qualified Mviz.Audio.Client       as Client
import           Mviz.Audio.Types        (ClientAudioMessage (..),
                                          HasAudioClient (..),
                                          HasClientChannel (..),
                                          HasServerChannel (..),
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
  getAudioClient = audioClient

instance HasServerChannel AudioState where
  getServerChannel = audioRecvChannel

instance HasClientChannel AudioState where
  getClientChannel = audioSendChannel

instance (HasAudioClient env) => MonadJack (AudioM env) where
  jackAction = Client.jackAction
  activateClient = ask >>= Client.activateClient . getAudioClient
  deactivateClient = ask >>= Client.deactivateClient . getAudioClient
  ports = ask >>= Client.getPorts . getAudioClient
  bufferSize = ask >>= Client.getBufferSize . getAudioClient
  sampleRate = ask >>= Client.getSampleRate . getAudioClient
--  closeClient = ask >>= Client.closeClient . getAudioClient

instance (HasServerChannel env, HasClientChannel env) => MonadAudioServer (AudioM env) where
  serverRecvChannel = asks getServerChannel
  serverSendChannel = asks getClientChannel
  serverRecvMessage = liftIO . atomically . tryReadTQueue =<< serverRecvChannel
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
