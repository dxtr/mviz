module Mviz.Window
  ( MonadShowWindow (..)
  , MonadHideWindow (..)
  , MonadGetWindowSize (..)
  , MonadDrawWindow (..)
  , MonadSetWindowMode (..)
  , MonadWindow (..)
  , createWindow
  , withWindow
  , destroyWindow
  ) where

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.IO.Unlift
import qualified Data.Text               as T
import qualified Mviz.SDL                (createGlContext, createWindow,
                                          destroyWindow, getDrawableSize,
                                          getScalingFactor, getWindowSize,
                                          hideWindow, setWindowMode, showWindow,
                                          swapWindow)
import           Mviz.Window.Types       (HasNativeWindow (..), Size,
                                          Window (..), WindowMode)
import           UnliftIO.Exception

class Monad m => MonadWindow m where
  getWindowM :: m Window

class Monad m => MonadShowWindow m where
  showWindow :: m ()

class Monad m => MonadHideWindow m where
  hideWindow :: m ()

class Monad m => MonadSetWindowMode m a where
  setWindowMode :: a -> WindowMode -> m ()

class Monad m => MonadGetWindowSize m a where
  getWindowSize :: a -> m (Size)
  getDrawableSize :: a -> m (Size)
  getScalingFactor :: a -> m (Float, Float)

class Monad m => MonadDrawWindow m where
  swapWindowBuffers :: m ()

--
-- Instances
--
instance (MonadIO m, HasNativeWindow a) => MonadSetWindowMode m a where
  setWindowMode :: a -> WindowMode -> m ()
  setWindowMode win wm = do
    let wnd = getNativeWindow win
    liftIO $ Mviz.SDL.setWindowMode wnd wm

instance (MonadIO m, HasNativeWindow a) => MonadGetWindowSize m a where
  getWindowSize = liftIO . Mviz.SDL.getWindowSize . getNativeWindow
  getDrawableSize = liftIO . Mviz.SDL.getDrawableSize . getNativeWindow
  getScalingFactor = liftIO . Mviz.SDL.getScalingFactor . getNativeWindow


-- showTheWindow :: (MonadShowWindow m a) => a -> m ()
-- showTheWindow wnd = showWindow wnd

-- Creates a window and a gl context
createWindow :: (MonadUnliftIO m) => T.Text -> Bool -> m Window
createWindow title vsync = do
  wnd <- Mviz.SDL.createWindow title
  glContext <- Mviz.SDL.createGlContext wnd vsync
  return Window { windowHandle = wnd, windowGlContext = glContext }

destroyWindow :: (MonadUnliftIO m, HasNativeWindow a) => a -> m ()
destroyWindow win = do
  Mviz.SDL.destroyWindow $ getNativeWindow win

withWindow :: (MonadUnliftIO m) => T.Text -> Bool -> (Window -> m c) -> m c
withWindow title vsync body =
  bracket
    (createWindow title vsync)
    destroyWindow
    (\window -> body window)
