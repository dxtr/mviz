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

import           Control.Concurrent.Async  (Async)
import           Control.Concurrent.STM    (TQueue, atomically, tryReadTQueue,
                                            writeTQueue)
import           Control.Monad             (liftM)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.IO.Unlift   (MonadUnliftIO)
import           Control.Monad.Logger      (LoggingT, MonadLogger)
import           Control.Monad.Reader      (MonadReader, ReaderT, ask,
                                            runReaderT)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Data.IORef                (IORef, atomicWriteIORef, readIORef)
import qualified Data.Map.Strict           as Map
import qualified Data.Text                 as T
import           Data.Word                 (Word16, Word64)
import qualified ImGui
import           Mviz.Audio                (ClientAudioMessage (..),
                                            HasClientChannel (..),
                                            HasServerChannel (..),
                                            ServerAudioMessage (..))
import           Mviz.Audio.Types
import qualified Mviz.Graphics.Shader      as Shader
import           Mviz.Logger               (LogMessage, MonadLog (..),
                                            runRingbufferLoggingT)
import qualified Mviz.SDL
import           Mviz.UI
import           Mviz.UI.LogWindow         (HasLogWindow (..),
                                            MonadLogWindow (..))
import           Mviz.UI.Types
import           Mviz.UI.UIWindow          (LogWindow (..))
import qualified Mviz.Utils.Ringbuffer     as RB
import           Mviz.Window
import           Mviz.Window.Types
import qualified UnliftIO.Exception        as E

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
  getFramerate = mvizFPS

instance HasLog MvizEnvironment where
  getLog = mvizLog

instance HasUI MvizEnvironment where
  getUIShownRef = mvizShowUI

instance HasLogWindow MvizEnvironment where
  getLogWindow = mvizLogWindow

instance HasWindow MvizEnvironment where
  getWindow = mvizWindow

instance HasNativeWindow MvizEnvironment where
  getNativeWindow = getNativeWindow . getWindow

instance HasClientChannel MvizEnvironment where
  getClientChannel = mvizAudioRecvChannel

instance HasServerChannel MvizEnvironment where
  getServerChannel = mvizAudioSendChannel

instance (HasWindow env) => MonadWindow (MvizM env) where
  getWindowM = do
    env <- ask
    return $ getWindow env

instance (HasLogWindow env) => MonadLogWindow (MvizM env) where
  openLogWindow label func = do
    ImGui.withCloseableWindow label [] func
  isLogWindowOpen = do
    env <- ask
    let wnd = getLogWindow env
    liftIO $ readIORef $ logWindowOpen wnd
  setLogWindowOpen newValue = do
    env <- ask
    let wnd = getLogWindow env
    liftIO $ atomicWriteIORef (logWindowOpen wnd) newValue

instance (HasNativeWindow env) => MonadShowWindow (MvizM env) where
  showWindow = ask >>= Mviz.SDL.showWindow . getNativeWindow

instance (HasNativeWindow env) => MonadHideWindow (MvizM env) where
  hideWindow = ask >>= Mviz.SDL.hideWindow . getNativeWindow

instance (HasNativeWindow env) => MonadDrawWindow (MvizM env) where
  swapWindowBuffers = ask >>= Mviz.SDL.swapWindow . getNativeWindow

instance (HasFramerate env) => MonadFramerate (MvizM env) where
  modifyFramerate newFramerate = ask >>= \env ->
    liftIO $ atomicWriteIORef (getFramerate env) newFramerate

instance (HasLog env) => MonadLog (MvizM env) where
  getLogVector = ask >>= liftIO . RB.toVector . getLog

instance (HasUI env, HasLogWindow env, HasLog env) => MonadUI (MvizM env) where
  isUIShown = ask >>= liftIO . readIORef . getUIShownRef
  renderUI = Mviz.UI.render

instance (HasServerChannel env, HasClientChannel env) => MonadAudioClient (MvizM env) where
  clientRecvChannel = ask >>= return . getClientChannel
  clientSendChannel = ask >>= return . getServerChannel
  clientRecvMessage = liftIO . atomically . tryReadTQueue =<< clientRecvChannel
  clientSendMessage msg = clientSendChannel >>= \c -> liftIO . atomically $ writeTQueue c msg

instance (MonadAudioClient m) => MonadAudioClient (MaybeT m) where
  clientRecvChannel = lift clientRecvChannel
  clientSendChannel = lift clientSendChannel
  clientRecvMessage = lift clientRecvMessage
  clientSendMessage = lift . clientSendMessage

-- Functions
runMviz
  :: MvizEnvironment
  -> MvizM MvizEnvironment a
  -> IO a
runMviz environment (MvizM action) = runLogger environment
 where
  runReader = runReaderT action environment
  runLogger env = runRingbufferLoggingT (mvizLog env) $ runReader
