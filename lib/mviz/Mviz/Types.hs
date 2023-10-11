module Mviz.Types
  ( MvizError (..)
  , MvizEnvironment (..)
  , MvizFramerate (..)
  , MvizM (..)
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
import           Data.IORef               (IORef, atomicWriteIORef, readIORef)
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import qualified Data.Vector              as V
import           Data.Word                (Word16, Word64)
import qualified ImGui
import           Mviz.Audio.Types         (ClientAudioMessage,
                                           HasClientChannel (..),
                                           HasServerChannel (..),
                                           MonadAudioClient (..),
                                           ServerAudioMessage)
import qualified Mviz.Graphics.Shader     as Shader
import           Mviz.Logger              (LogMessage, MonadLog (..))
import qualified Mviz.SDL
import qualified Mviz.SDL.Types
import           Mviz.UI                  (render)
import           Mviz.UI.LogWindow        (HasLogWindow (..),
                                           MonadLogWindow (..))
import           Mviz.UI.SettingsWindow   (HasSettingsWindow (getSettingsWindow),
                                           MonadSettingsWindow (isSettingsWindowOpen, openSettingsWindow, setSettingsWindowOpen))
import           Mviz.UI.Types            (HasUI (..), MonadUI (..), UIContext)
import           Mviz.UI.UIWindow         (LogWindow (..),
                                           SettingsWindow (settingsWindowOpen))
import qualified Mviz.Utils.Ringbuffer    as RB
import           Mviz.Window              (MonadDrawWindow (..),
                                           MonadHideWindow (..),
                                           MonadShowWindow (..),
                                           MonadWindow (..))
import           Mviz.Window.Types        (HasNativeWindow (..), HasWindow (..),
                                           Window)
import qualified UnliftIO.Exception       as E

-- Types
data MvizError
  = IOError E.IOException
  | NoError

data MvizFramerate = MvizFramerate
  { mvizFramerate        :: Float -- The calculated framerate
  , mvizFramerateTime    :: Float -- The average frame time
  , mvizFramerateSample  :: Word64 -- The last time (In ticks) the framerate was sampled
  , mvizFramerateCounter :: Word16 -- The amount of frames drawn since the last sample was taken
  }

data MvizEnvironment = MvizEnvironment
  { mvizWindow           :: Window
  , mvizUIContext        :: UIContext
  , mvizAudioThread      :: Async ()
  , mvizAudioSendChannel :: TQueue ServerAudioMessage
  , mvizAudioRecvChannel :: TQueue ClientAudioMessage
  , mvizAudioSampleRate  :: IORef Int
  , mvizAudioBufferSize  :: IORef Int
  , mvizAudioPorts       :: IORef [T.Text]
  , mvizLog              :: RB.Ringbuffer LogMessage
  , mvizShowUI           :: IORef Bool
  , mvizFPS              :: IORef MvizFramerate
  , mvizShaders          :: IORef (Map.Map T.Text Shader.ProgramObject)
  , mvizLogWindow        :: LogWindow
  , mvizLogFunc          :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  , mvizSettingsWindow   :: SettingsWindow
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

-- Type classes and instances

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
  getUIShownRef :: MvizEnvironment -> IORef Bool
  getUIShownRef = mvizShowUI

instance HasLogWindow MvizEnvironment where
  getLogWindow :: MvizEnvironment -> LogWindow
  getLogWindow = mvizLogWindow

instance HasSettingsWindow MvizEnvironment where
  getSettingsWindow = mvizSettingsWindow

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

instance (HasWindow env) => MonadWindow (MvizM env) where
  getWindowM :: HasWindow env => MvizM env Window
  getWindowM = asks getWindow

instance (HasLogWindow env) => MonadLogWindow (MvizM env) where
  openLogWindow :: HasLogWindow env => T.Text -> MvizM env () -> MvizM env Bool
  openLogWindow label func = do
    ImGui.withCloseableWindow label [] func

  isLogWindowOpen :: HasLogWindow env => MvizM env Bool
  isLogWindowOpen = do
    wnd <- asks getLogWindow
    liftIO $ readIORef $ logWindowOpen wnd

  setLogWindowOpen :: HasLogWindow env => Bool -> MvizM env ()
  setLogWindowOpen newValue = do
    wnd <- asks getLogWindow
    liftIO $ atomicWriteIORef (logWindowOpen wnd) newValue

instance (HasSettingsWindow env) => MonadSettingsWindow (MvizM env) where
  openSettingsWindow :: HasSettingsWindow env => T.Text -> MvizM env () -> MvizM env Bool
  openSettingsWindow label func = do
    ImGui.withCloseableWindow label [] func

  isSettingsWindowOpen :: HasSettingsWindow env => MvizM env Bool
  isSettingsWindowOpen = do
    wnd <- asks getSettingsWindow
    liftIO $ readIORef $ settingsWindowOpen wnd

  setSettingsWindowOpen :: HasSettingsWindow env => Bool -> MvizM env ()
  setSettingsWindowOpen newValue = do
    wnd <- asks getSettingsWindow
    liftIO $ atomicWriteIORef (settingsWindowOpen wnd) newValue

instance (HasNativeWindow env) => MonadShowWindow (MvizM env) where
  showWindow :: HasNativeWindow env => MvizM env ()
  showWindow = ask >>= Mviz.SDL.showWindow . getNativeWindow

instance (HasNativeWindow env) => MonadHideWindow (MvizM env) where
  hideWindow :: HasNativeWindow env => MvizM env ()
  hideWindow = ask >>= Mviz.SDL.hideWindow . getNativeWindow

instance (HasNativeWindow env) => MonadDrawWindow (MvizM env) where
  swapWindowBuffers :: HasNativeWindow env => MvizM env ()
  swapWindowBuffers = ask >>= Mviz.SDL.swapWindow . getNativeWindow

instance (HasFramerate env) => MonadFramerate (MvizM env) where
  modifyFramerate :: HasFramerate env => MvizFramerate -> MvizM env ()
  modifyFramerate newFramerate = ask >>= \env ->
    liftIO $ atomicWriteIORef (getFramerate env) newFramerate

instance (HasLog env) => MonadLog (MvizM env) where
  getLogVector :: HasLog env => MvizM env (V.Vector LogMessage)
  getLogVector = ask >>= liftIO . RB.toVector . getLog

instance (HasUI env, HasLogWindow env, HasSettingsWindow env, HasLog env) => MonadUI (MvizM env) where
  isUIShown :: (HasUI env, HasLogWindow env, HasLog env) => MvizM env Bool
  isUIShown = ask >>= liftIO . readIORef . getUIShownRef

  renderUI :: (HasUI env, HasLogWindow env, HasSettingsWindow env, HasLog env) => MvizM env ()
  renderUI = Mviz.UI.render

instance (HasServerChannel env, HasClientChannel env) => MonadAudioClient (MvizM env) where
  clientRecvChannel :: (HasServerChannel env, HasClientChannel env) => MvizM env (TQueue ClientAudioMessage)
  clientRecvChannel = asks getClientChannel

  clientSendChannel :: (HasServerChannel env, HasClientChannel env) => MvizM env (TQueue ServerAudioMessage)
  clientSendChannel = asks getServerChannel

  clientRecvMessage :: (HasServerChannel env, HasClientChannel env) => MvizM env (Maybe ClientAudioMessage)
  clientRecvMessage = liftIO . atomically . tryReadTQueue =<< clientRecvChannel

  clientRecvMessages :: (HasServerChannel env, HasClientChannel env) => MvizM env [ClientAudioMessage]
  clientRecvMessages = liftIO . atomically . flushTQueue =<< clientRecvChannel

  clientSendMessage :: (HasServerChannel env, HasClientChannel env) => ServerAudioMessage -> MvizM env ()
  clientSendMessage msg = clientSendChannel >>= \c -> liftIO . atomically $ writeTQueue c msg

instance (HasLog env) => MonadLogger (MvizM env) where
  monadLoggerLog :: (HasLog env, ToLogStr msg) => Loc -> LogSource -> LogLevel -> msg -> MvizM env ()
  monadLoggerLog loc logSource logLevel msg = do
    logFunc <- asks getLogFunc
    liftIO $ logFunc loc logSource logLevel (toLogStr msg)

-- Functions
runMviz :: MvizEnvironment -> MvizM MvizEnvironment a -> IO a
runMviz environment action = runReaderT (unMvizM action) environment
