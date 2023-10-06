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
import           Control.Monad.Logger     (LoggingT, MonadLogger)
import           Control.Monad.Reader     (MonadReader, ReaderT, ask, asks,
                                           runReaderT)
import           Data.IORef               (IORef, atomicWriteIORef, readIORef)
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import qualified Data.Vector              as V
import           Data.Word                (Word16, Word64)
import qualified ImGui
import           Mviz.Audio.Types
import qualified Mviz.Graphics.Shader     as Shader
import           Mviz.Logger              (LogMessage, MonadLog (..),
                                           runRingbufferLoggingT)
import qualified Mviz.SDL
import qualified Mviz.SDL.Types
import           Mviz.UI
import           Mviz.UI.LogWindow        (HasLogWindow (..),
                                           MonadLogWindow (..))
import           Mviz.UI.Types
import           Mviz.UI.UIWindow         (LogWindow (..))
import qualified Mviz.Utils.Ringbuffer    as RB
import           Mviz.Window
import           Mviz.Window.Types
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
  , mvizLog              :: RB.Ringbuffer LogMessage
  , mvizShowUI           :: IORef Bool
  , mvizFPS              :: IORef MvizFramerate
  , mvizShaders          :: IORef (Map.Map T.Text Shader.ProgramObject)
  , mvizLogWindow        :: LogWindow
  }

newtype MvizM e a = MvizM (ReaderT e (LoggingT IO) a)
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadUnliftIO
    , MonadIO
    , MonadReader e
    , MonadLogger
    )

-- Type classes and instances

class HasFramerate a where
  getFramerate :: a -> IORef MvizFramerate

class HasLog a where
  getLog :: a -> RB.Ringbuffer LogMessage

class Monad m => MonadFramerate m where
  modifyFramerate :: MvizFramerate -> m ()

instance HasFramerate MvizEnvironment where
  getFramerate :: MvizEnvironment -> IORef MvizFramerate
  getFramerate = mvizFPS

instance HasLog MvizEnvironment where
  getLog :: MvizEnvironment -> RB.Ringbuffer LogMessage
  getLog = mvizLog

instance HasUI MvizEnvironment where
  getUIShownRef :: MvizEnvironment -> IORef Bool
  getUIShownRef = mvizShowUI

instance HasLogWindow MvizEnvironment where
  getLogWindow :: MvizEnvironment -> LogWindow
  getLogWindow = mvizLogWindow

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
    env <- ask
    let wnd = getLogWindow env
    liftIO $ readIORef $ logWindowOpen wnd

  setLogWindowOpen :: HasLogWindow env => Bool -> MvizM env ()
  setLogWindowOpen newValue = do
    env <- ask
    let wnd = getLogWindow env
    liftIO $ atomicWriteIORef (logWindowOpen wnd) newValue

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

instance (HasUI env, HasLogWindow env, HasLog env) => MonadUI (MvizM env) where
  isUIShown :: (HasUI env, HasLogWindow env, HasLog env) => MvizM env Bool
  isUIShown = ask >>= liftIO . readIORef . getUIShownRef
  renderUI :: (HasUI env, HasLogWindow env, HasLog env) => MvizM env ()
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

-- Functions
runMviz
  :: MvizEnvironment
  -> MvizM MvizEnvironment a
  -> IO a
runMviz environment (MvizM action) = runLogger environment
 where
  runReader = runReaderT action environment
  runLogger env = runRingbufferLoggingT (mvizLog env) runReader
