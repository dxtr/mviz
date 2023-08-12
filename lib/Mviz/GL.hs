module Mviz.GL (vendor,
                renderer,
                version,
                GLMakeCurrent,
                glMakeCurrent) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Mviz.SDL                  as SDL

vendor :: IO String
vendor = GL.get GL.vendor

renderer :: IO String
renderer = GL.get GL.renderer

version :: IO String
version = GL.get GL.glVersion

class GLMakeCurrent a where
  glMakeCurrent :: a -> IO ()

class GLSwapBuffers a where
  glSwapBuffers :: a -> IO ()

instance GLMakeCurrent SDL.Window where
  glMakeCurrent = SDL.makeContextCurrent

instance GLSwapBuffers SDL.Window where
  glSwapBuffers = SDL.swapWindow

