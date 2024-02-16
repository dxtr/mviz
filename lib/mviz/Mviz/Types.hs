module Mviz.Types
  ( MvizError (..)
  , MvizEnvironment (..)
  , MvizFramerate (..)
  , MvizM (..)
  , MvizGL (..)
  , HasFramerate (..)
  , MonadFramerate (..)
  , MonadUI (..)
  , runMviz
  ) where

import           Control.Concurrent.Async (Async)
import           Control.Concurrent.STM   (TQueue, atomically, flushTQueue,
                                           tryReadTQueue, writeTQueue)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.IO.Unlift  (MonadUnliftIO)
import           Control.Monad.Logger     (Loc, LogLevel, LogSource, LogStr,
                                           MonadLogger (monadLoggerLog),
                                           ToLogStr (toLogStr))
import           Control.Monad.Reader     (MonadReader, ReaderT, ask, asks,
                                           runReaderT)
import qualified Data.ByteString.Char8    as C8
import           Data.Functor             ((<&>))
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import qualified Data.Vector              as V
import           Data.Word                (Word16, Word64)
import qualified ImGui
import           Mviz.Audio.Inputs        (InputMap)
import           Mviz.Audio.Types         (ClientAudioMessage,
                                           HasBufferSize (getBufferSizeRef),
                                           HasClientChannel (..),
                                           HasInputs (..),
                                           HasSampleRate (getSampleRate, getSampleRateRef),
                                           HasServerChannel (..),
                                           MonadAudio (..),
                                           MonadAudioClient (..),
                                           ServerAudioMessage)
import qualified Mviz.Graphics.Shader     as Shader
import           Mviz.Logger              (LogMessage, MonadLog (..))
import qualified Mviz.SDL
import qualified Mviz.SDL.Types
import qualified Mviz.Shader              as S
import           Mviz.Shader.Types
import           Mviz.UI                  (render)
import           Mviz.UI.LogWindow        (HasLogWindow (..),
                                           MonadLogWindow (..))
import           Mviz.UI.SettingsWindow   (HasSettingsWindow (getSettingsWindow, setSettingsWindow),
                                           MonadSettingsWindow (..))
import           Mviz.UI.Types            (HasUI (..), MonadUI (..), UIContext)
import           Mviz.UI.UIWindow         (LogWindow (..),
                                           SettingsWindow (settingsCheckedChannels, settingsSelectedInput))
import qualified Mviz.Utils.Ringbuffer    as RB
import           Mviz.Watch
import           Mviz.Window              (MonadDrawWindow (..),
                                           MonadHideWindow (..),
                                           MonadShowWindow (..),
                                           MonadWindow (..))
import           Mviz.Window.Types        (HasNativeWindow (..), HasWindow (..),
                                           Window)
import qualified System.INotify           as IN
import           System.INotify           (INotify, WatchDescriptor)
import           UnliftIO                 (IORef, atomicWriteIORef,
                                           modifyIORef', readIORef, writeIORef)
import qualified UnliftIO.Exception       as E

-- Types
data MvizError
  = IOError E.IOException
  | NoError

data MvizGL = MvizGL
  { mvizGLRenderer :: !T.Text
  , mvizGLVersion  :: !T.Text
  , mvizGLVendor   :: !T.Text
  }

data MvizFramerate = MvizFramerate
  { mvizFramerate        :: !Float -- The calculated framerate
  , mvizFramerateTime    :: !Float -- The average frame time
  , mvizFramerateSample  :: !Word64 -- The last time (In ticks) the framerate was sampled
  , mvizFramerateCounter :: !Word16 -- The amount of frames drawn since the last sample was taken
  }

data MvizEnvironment = MvizEnvironment
  { mvizWindow           :: !Window
  , mvizUIContext        :: !UIContext
  , mvizAudioThread      :: !(Async ())
  , mvizAudioSendChannel :: !(TQueue ServerAudioMessage)
  , mvizAudioRecvChannel :: !(TQueue ClientAudioMessage)
  , mvizAudioSampleRate  :: !(IORef Word)
  , mvizAudioBufferSize  :: !(IORef Word)
  , mvizAudioPorts       :: !(IORef [T.Text])
  , mvizAudioInputs      :: !(IORef InputMap)
  , mvizLog              :: !(RB.Ringbuffer LogMessage)
  , mvizShowUI           :: !(IORef Bool)
  , mvizFPS              :: !(IORef MvizFramerate)
  , mvizShaders          :: !(IORef (Map.Map T.Text Shader.ProgramObject))
  , mvizLogWindow        :: !LogWindow
  , mvizLogFunc          :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  , mvizSettingsWindow   :: !(IORef SettingsWindow)
  , mvizGL               :: !MvizGL
  , mvizInotify          :: !INotify
  , mvizWatches          :: !(IORef [WatchDescriptor])
  }

newtype MvizM e a = MvizM { unMvizM :: ReaderT e IO a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadUnliftIO
    , MonadIO
    , MonadReader e
    )

class HasFramerate a where
  getFramerate :: a -> IORef MvizFramerate

class HasLog a where
  getLog :: a -> RB.Ringbuffer LogMessage
  getLogFunc :: a -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())

class Monad m => MonadFramerate m where
  modifyFramerate :: MvizFramerate -> m ()

instance HasFramerate MvizEnvironment where
  getFramerate :: MvizEnvironment -> IORef MvizFramerate
  getFramerate = mvizFPS

instance HasLog MvizEnvironment where
  getLog :: MvizEnvironment -> RB.Ringbuffer LogMessage
  getLog = mvizLog

  getLogFunc :: MvizEnvironment -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  getLogFunc = mvizLogFunc

instance HasUI MvizEnvironment where
  getUIShown = readIORef . getUIShownRef
  getUIShownRef = mvizShowUI

instance HasLogWindow MvizEnvironment where
  getLogWindow :: MvizEnvironment -> LogWindow
  getLogWindow = mvizLogWindow

instance HasSettingsWindow MvizEnvironment where
  getSettingsWindow = readIORef . mvizSettingsWindow
  setSettingsWindow env window = writeIORef (mvizSettingsWindow env) window

instance HasWindow MvizEnvironment where
  getWindow :: MvizEnvironment -> Window
  getWindow = mvizWindow

instance HasNativeWindow MvizEnvironment where
  getNativeWindow :: MvizEnvironment -> Mviz.SDL.Types.SDLWindow
  getNativeWindow = getNativeWindow . getWindow

instance HasClientChannel MvizEnvironment where
  getClientChannel :: MvizEnvironment -> TQueue ClientAudioMessage
  getClientChannel = mvizAudioRecvChannel

instance HasServerChannel MvizEnvironment where
  getServerChannel :: MvizEnvironment -> TQueue ServerAudioMessage
  getServerChannel = mvizAudioSendChannel

instance HasSampleRate MvizEnvironment where
  getSampleRateRef :: MvizEnvironment -> IORef Word
  getSampleRateRef = mvizAudioSampleRate
  getSampleRate :: MvizEnvironment -> IO Word
  getSampleRate = liftIO . readIORef . getSampleRateRef

instance HasBufferSize MvizEnvironment where
  getBufferSizeRef :: MvizEnvironment -> IORef Word
  getBufferSizeRef = mvizAudioBufferSize

instance HasInputs MvizEnvironment where
  getPortsRef :: MvizEnvironment -> IORef [T.Text]
  getPortsRef = mvizAudioPorts
  getInputsRef :: MvizEnvironment -> IORef InputMap
  getInputsRef = mvizAudioInputs

instance (HasWindow env) => MonadWindow (MvizM env) where
  getWindowM :: MvizM env Window
  getWindowM = asks getWindow

instance (HasLogWindow env) => MonadLogWindow (MvizM env) where
  openLogWindow :: T.Text -> MvizM env () -> MvizM env Bool
  openLogWindow label f = ImGui.withCloseableWindow label [] f <&> fst

  isLogWindowOpen :: MvizM env Bool
  isLogWindowOpen =  liftIO . readIORef . logWindowOpen =<< asks getLogWindow

  setLogWindowOpen :: Bool -> MvizM env ()
  setLogWindowOpen = (asks getLogWindow >>=) . (liftIO .) . flip (atomicWriteIORef . logWindowOpen)

instance (HasSettingsWindow env) => MonadSettingsWindow (MvizM env) where
  openSettingsWindow label = ImGui.withCloseableWindow label []

  getSelectedInput :: MvizM env (Maybe T.Text)
  getSelectedInput = do
    sw <- asks getSettingsWindow
    sw2 <- liftIO sw
    return $ settingsSelectedInput sw2

  getSelectedChannels :: MvizM env [T.Text]
  getSelectedChannels = do
    sw <- liftIO =<< asks getSettingsWindow
    return $ settingsCheckedChannels sw

instance (HasNativeWindow env) => MonadShowWindow (MvizM env) where
  showWindow :: MvizM env ()
  showWindow = ask >>= Mviz.SDL.showWindow . getNativeWindow

instance (HasNativeWindow env) => MonadHideWindow (MvizM env) where
  hideWindow :: MvizM env ()
  hideWindow = ask >>= Mviz.SDL.hideWindow . getNativeWindow

instance (HasNativeWindow env) => MonadDrawWindow (MvizM env) where
  swapWindowBuffers :: MvizM env ()
  swapWindowBuffers = ask >>= Mviz.SDL.swapWindow . getNativeWindow

instance (HasFramerate env) => MonadFramerate (MvizM env) where
  modifyFramerate :: MvizFramerate -> MvizM env ()
  modifyFramerate newFramerate = ask >>= \env ->
    liftIO $ atomicWriteIORef (getFramerate env) newFramerate

instance (HasLog env) => MonadLog (MvizM env) where
  getLogVector :: MvizM env (V.Vector LogMessage)
  getLogVector = ask >>= liftIO . RB.toVector . getLog

instance (HasUI env, HasServerChannel env, HasLogWindow env, HasSettingsWindow env, HasLog env, HasInputs env, HasBufferSize env, HasSampleRate env) => MonadUI (MvizM env) where
  isUIShown :: MvizM env Bool
  isUIShown = ask >>= liftIO . getUIShown

  renderUI :: MvizM env Bool
  renderUI = Mviz.UI.render

  toggleUI :: MvizM env ()
  toggleUI = do
    env <- ask
    let suiRef = getUIShownRef env
    liftIO $ modifyIORef' suiRef not

instance (HasServerChannel env, HasClientChannel env) => MonadAudioClient (MvizM env) where
  clientRecvChannel :: MvizM env (TQueue ClientAudioMessage)
  clientRecvChannel = asks getClientChannel

  clientSendChannel :: MvizM env (TQueue ServerAudioMessage)
  clientSendChannel = asks getServerChannel

  clientRecvMessage :: MvizM env (Maybe ClientAudioMessage)
  clientRecvMessage = liftIO . atomically . tryReadTQueue =<< clientRecvChannel

  clientRecvMessages :: MvizM env [ClientAudioMessage]
  clientRecvMessages = liftIO . atomically . flushTQueue =<< clientRecvChannel

  clientSendMessage :: ServerAudioMessage -> MvizM env ()
  clientSendMessage msg = clientSendChannel >>= \c -> liftIO . atomically $ writeTQueue c msg

instance (HasInputs env, HasBufferSize env, HasSampleRate env) => MonadAudio (MvizM env) where
  audioPorts :: MvizM env [T.Text]
  audioPorts = ask >>= liftIO . readIORef . getPortsRef

  audioInputs :: MvizM env InputMap
  audioInputs = ask >>= liftIO . readIORef . getInputsRef

  audioBufferSize :: MvizM env Word
  audioBufferSize = ask >>= liftIO . readIORef . getBufferSizeRef

  audioSampleRate :: MvizM env Word
  audioSampleRate = ask >>= liftIO . getSampleRate

instance (HasLog env) => MonadLogger (MvizM env) where
  monadLoggerLog :: (ToLogStr msg) => Loc -> LogSource -> LogLevel -> msg -> MvizM env ()
  monadLoggerLog loc logSource logLevel msg = asks getLogFunc >>=
    \logFunc -> liftIO $ logFunc loc logSource logLevel (toLogStr msg)

instance MonadShader (MvizM MvizEnvironment) where
  listShaders = S.listShaders
  loadShader _shaderName = pure $ Shader undefined undefined -- TODO

instance HasINotify MvizEnvironment where
  getINotify = mvizInotify
  getWatches = liftIO . readIORef . getWatchesRef
  getWatchesRef = mvizWatches

instance (HasINotify env) => MonadWatch (MvizM env) where
  addWatch fp =
    let callback _a = pure () in -- TODO: Sort out a proper callback
      asks getINotify
      >>= \inotify -> liftIO $ IN.addWatch inotify [IN.Modify] (C8.pack fp) callback
  setWatches new = asks getWatchesRef >>= liftIO . flip writeIORef new
  removeWatch watch = do
    liftIO $ IN.removeWatch watch
    asks getWatches >>= liftIO >>= setWatches . filter (/= watch)
  removeAllWatches = do
    asks getWatches >>= liftIO >>= mapM_ (liftIO . IN.removeWatch)
    setWatches []

-- Functions
runMviz :: MvizEnvironment -> MvizM MvizEnvironment a -> IO a
runMviz environment action = runReaderT (unMvizM action) environment
