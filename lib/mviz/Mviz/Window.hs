module Mviz.Window where

import           Mviz.SDL          (getDrawableSize, getScalingFactor,
                                    getWindowSize)
import qualified Mviz.SDL          (Window, hideWindow, setWindowMode,
                                    showWindow, swapWindow)
import           Mviz.Window.Types (Size, WindowMode)

--
-- Typeclasses
--
class ShowWindow a where
  showWindow :: a -> IO ()

class HideWindow a where
  hideWindow :: a -> IO ()

class SetWindowMode a where
  setWindowMode :: a -> WindowMode -> IO ()

class GetWindowSize a where
  getWindowSize :: a -> IO (Size)
  getDrawableSize :: a -> IO (Size)
  getScalingFactor :: a -> IO (Float, Float)

class DrawWindow a where
  swapWindowBuffers :: a -> IO ()

--
-- Instances
--
instance ShowWindow Mviz.SDL.Window where
  showWindow = Mviz.SDL.showWindow

instance HideWindow Mviz.SDL.Window where
  hideWindow = Mviz.SDL.hideWindow

instance SetWindowMode Mviz.SDL.Window where
  setWindowMode = Mviz.SDL.setWindowMode

instance GetWindowSize Mviz.SDL.Window where
  getWindowSize = Mviz.SDL.getWindowSize
  getDrawableSize = Mviz.SDL.getDrawableSize
  getScalingFactor = Mviz.SDL.getScalingFactor

instance DrawWindow Mviz.SDL.Window where
  swapWindowBuffers = Mviz.SDL.swapWindow

showTheWindow :: (ShowWindow a) => a -> IO ()
showTheWindow wnd = showWindow wnd
