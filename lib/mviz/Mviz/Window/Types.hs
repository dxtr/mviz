module Mviz.Window.Types
  ( WindowMode (..)
  , Size (..)
  , HasWindow (..)
  , Window (..)
  , HasNativeWindow (..)
  ) where

import qualified Mviz.SDL.Types as Mviz.SDL

data Window = Window
  { windowHandle    :: Mviz.SDL.SDLWindow
  , windowGlContext :: Mviz.SDL.GLContext
  }

data WindowMode = Fullscreen | FullscreenDesktop | Windowed

data Size = Size
  { sizeWidth  :: Int
  , sizeHeight :: Int
  }
  deriving (Show)

class HasWindow a where
  getWindow :: a -> Window

class HasNativeWindow a where
  getNativeWindow :: a -> Mviz.SDL.SDLWindow

instance HasNativeWindow Window where
  getNativeWindow = windowHandle
