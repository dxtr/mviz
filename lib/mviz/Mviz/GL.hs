{-# LANGUAGE AllowAmbiguousTypes #-}

module Mviz.GL
  ( HasGLContext (..)
  , GLMakeCurrent (..)
  , GLSwapBuffers (..)
  , vendor
  , renderer
  , version
  ) where

import           Control.Monad.IO.Class    (MonadIO)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Mviz.SDL.Types            as Mviz.SDL (GLContext)
-- import           Mviz.Window               (IsNativeWindow)
-- import           Mviz.GL.Types
import           Mviz.Window.Types         (HasNativeWindow (..),
                                            Window (windowGlContext))
import qualified SDL

vendor :: IO String
vendor = GL.get GL.vendor

renderer :: IO String
renderer = GL.get GL.renderer

version :: IO String
version = GL.get GL.glVersion

class HasGLContext a where
  getGLContext :: a -> Mviz.SDL.GLContext

class Monad m => GLMakeCurrent m a where
  glMakeCurrent :: a -> m ()

class Monad m => GLSwapBuffers m a where
  glSwapBuffers :: a -> m ()

instance HasGLContext Mviz.Window.Types.Window where
  getGLContext = windowGlContext

instance (MonadIO m, HasNativeWindow a, HasGLContext a) => GLMakeCurrent m a where
  glMakeCurrent win = do
    let nativeWindow = getNativeWindow win
    let glContext = getGLContext win
    SDL.glMakeCurrent nativeWindow glContext

instance MonadIO m => GLSwapBuffers m SDL.Window where
  glSwapBuffers = SDL.glSwapWindow

