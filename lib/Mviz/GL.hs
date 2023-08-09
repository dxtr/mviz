module Mviz.GL where
import qualified Mviz.SDL as SDL

class GLMakeCurrent a where
  glMakeCurrent :: a -> IO ()

class GLSwapBuffers a where
  glSwapBuffers :: a -> IO ()

instance GLMakeCurrent SDL.Window where
  glMakeCurrent = SDL.makeContextCurrent

instance GLSwapBuffers SDL.Window where
  glSwapBuffers = SDL.swapWindow

