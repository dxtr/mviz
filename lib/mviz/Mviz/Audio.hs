module Mviz.Audio (
  AudioMessage (..),
  AudioReturn,
  runAudioSystem,
  shutdown,
) where

import           Control.Concurrent.STM     (TQueue, atomically, tryReadTQueue,
                                             writeTQueue)
import           Control.Monad.Except       (MonadError, runExceptT)
import           Control.Monad.State.Strict (MonadIO, MonadState, StateT,
                                             evalStateT, get, liftIO)
import           Control.Monad.Trans.Except (ExceptT (..))
import qualified Mviz.Audio.Client          as Client
import           Mviz.Audio.Types           (AudioError (..))
import qualified Sound.JACK                 as JACK

type AudioReturn = Either AudioError ()

data AudioMessage
  = Quit

data AudioState = AudioState
  { audioSendChannel :: TQueue AudioMessage
  , audioRecvChannel :: TQueue AudioMessage
  , audioClient      :: JACK.Client
  }

newtype AudioM a = AudioM (StateT AudioState (ExceptT AudioError IO) a)
  deriving (Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadError AudioError
           , MonadState AudioState
           )

runAudio :: AudioState -> AudioM a -> IO (Either AudioError a)
runAudio env (AudioM action) = runExceptT $ evalStateT action env

audioLoop :: AudioM ()
audioLoop = do
  state <- get
  msg <- liftIO $ atomically $ tryReadTQueue $ audioRecvChannel state
  case msg of
    Just _ -> do
      liftIO $ putStrLn "Quitting audio system"
      return ()
    Nothing -> audioLoop

runAudioSystem
  :: TQueue AudioMessage
  -> TQueue AudioMessage
  -> IO (Either AudioError ())
runAudioSystem sendChan recvChan = runExceptT $ do
  client <- ExceptT $ Client.createClient "mviz"
  let state = AudioState { audioSendChannel = sendChan
                         , audioRecvChannel = recvChan
                         , audioClient = client
                         }
  ExceptT $ runAudio state audioLoop

shutdown :: TQueue AudioMessage -> IO ()
shutdown writeChan = atomically $ writeTQueue writeChan msg
 where
  msg = Quit
