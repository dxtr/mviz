module Mviz.Audio (
  AudioMessage (..),
  AudioReturn,
  runAudioSystem,
  shutdown,
) where

import Control.Concurrent.STM (
  STM,
  TChan,
  atomically,
  readTChan,
  tryReadTChan,
  writeTChan,
 )

import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.State.Strict (
  MonadIO,
  MonadState,
  StateT,
  evalStateT,
  get,
  liftIO,
 )
import Mviz.Audio.Client qualified as Client
import Mviz.Audio.Types (AudioError (..))
import Sound.JACK qualified as JACK

type AudioReturn = Either AudioError ()

data AudioMessage
  = Quit

data AudioState = AudioState
  { audioSendChannel :: TChan AudioMessage
  , audioRecvChannel :: TChan AudioMessage
  , audioClient :: JACK.Client
  }

newtype AudioM a = AudioM (StateT AudioState (ExceptT AudioError IO) a)
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadError AudioError
    , MonadState AudioState
    )

runAudio :: AudioState -> AudioM a -> IO (Either AudioError a)
runAudio env (AudioM action) = runExceptT $ evalStateT action env

readChannel :: TChan AudioMessage -> STM (Maybe AudioMessage)
-- readChannel channel = tryReadTChan channel
readChannel channel = Just <$> readTChan channel

audioLoop :: AudioM ()
audioLoop = do
  state <- get
  msg <- liftIO $ atomically $ readChannel $ audioRecvChannel state
  case msg of
    Just _ -> do
      liftIO $ putStrLn "Quitting audio system"
      return ()
    Nothing -> audioLoop

runAudioSystem
  :: TChan AudioMessage
  -> TChan AudioMessage
  -> IO (Either AudioError ())
runAudioSystem sendChan recvChan = runExceptT $ do
  -- client <- Client.createClient "mviz"
  client <- ExceptT $ Client.createClient "mviz"
  let state =
        AudioState
        { audioSendChannel = sendChan
        , audioRecvChannel = recvChan
        , audioClient = client
        }
  ExceptT $ runAudio state audioLoop

shutdown :: TChan AudioMessage -> IO ()
shutdown writeChan = atomically $ writeTChan writeChan msg
 where
  msg = Quit
