module Mviz.Window where

import Mviz.SDL (getDrawableSize, getScalingFactor, getWindowSize)
import Mviz.SDL qualified (Window, hideWindow, setWindowMode, showWindow)
import Mviz.Window.Types (Size, WindowMode)

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

-- withWindow :: T.Text -> Bool -> (Window -> GLContext -> IO c) -> IO c
-- withWindow title vsync body =
--     bracket
--         (createWindow title False)
--         destroyWindow
--         ( \wnd ->
--             bracket
--                 (createGlContext wnd vsync)
--                 deleteGlContext
--                 (body wnd)
--         )

showTheWindow :: (ShowWindow a) => a -> IO ()
showTheWindow wnd = showWindow wnd
