module Mviz.SDL (
  SDLError (..),
  initialize,
  createWindow,
  destroyWindow,
  swapWindow,
  showWindow,
  hideWindow,
  setWindowMode,
  getWindowSize,
  getDrawableSize,
  getScalingFactor,
  getError,
  SDL.quit,
  SDL.ticks,
  createGlContext
) where

import           Control.Monad.IO.Unlift
import qualified Data.Text               as T
import           Foreign.C.String        (peekCString)
-- import qualified Graphics.Rendering.OpenGL as SDLGL
-- import qualified Mviz.GL.Types             as GL (GLContext)
import           Mviz.SDL.Types
import           Mviz.Window.Types       (Size (..), WindowMode (..))
import qualified SDL
import qualified SDL.Raw.Error
import           UnliftIO.Exception      (Exception, throwIO)

data SDLErrorKind
  = CallFailed
      { sdlErrorCaller   :: !T.Text
      , sdlErrorFunction :: !T.Text
      , sdlErrorMessage  :: !T.Text
      }
  | UnexpectedArgument
      { sdlErrorCaller   :: !T.Text
      , sdlErrorFunction :: !T.Text
      , sdlErrorValue    :: !String
      }
  | UnknownHintValue
      { sdlErrorHint  :: !String
      , sdlErrorValue :: !String
      }
  deriving (Show)

data SDLError
  = CreateWindow SDLErrorKind
  | SDLMessage T.Text
  deriving Show

instance Exception SDLError

initFlags :: [SDL.InitFlag]
initFlags = [SDL.InitVideo, SDL.InitEvents]

initialize :: (MonadIO m) => m ()
initialize = SDL.initialize initFlags

windowFlags :: SDL.WindowConfig
windowFlags =
  SDL.defaultWindow
    { SDL.windowHighDPI = True
    , SDL.windowGraphicsContext =
        SDL.OpenGLContext $ SDL.defaultOpenGL{SDL.glProfile = SDL.Core SDL.Normal 4 6}
    , SDL.windowPosition = SDL.Wherever
    , SDL.windowResizable = True
    , SDL.windowInitialSize = SDL.V2 1024 768
    }

createWindow :: (MonadUnliftIO m) => T.Text -> m SDLWindow
createWindow title = do
  wnd <- SDL.createWindow title windowFlags
  err <- liftIO getError
  case err of
    Nothing -> do
--      glCtx <- liftIO $ createGlContext wnd vsync
--      SDL.glMakeCurrent wnd glCtx
      return wnd
    Just e  -> throwIO $ SDLMessage e

destroyWindow :: (MonadIO m) => SDLWindow -> m ()
destroyWindow = SDL.destroyWindow

createGlContext :: (MonadIO m) => SDLWindow -> Bool -> m SDL.GLContext
createGlContext window False = createGlContext_ window SDL.ImmediateUpdates
createGlContext window True  = createGlContext_ window SDL.SynchronizedUpdates

createGlContext_ :: (MonadIO m) => SDLWindow -> SDL.SwapInterval -> m SDL.GLContext
createGlContext_ window swapInterval = do
  context <- SDL.glCreateContext window
  SDL.swapInterval SDL.$= swapInterval
  return context

showWindow :: (MonadIO m) => SDLWindow -> m ()
showWindow = SDL.showWindow

hideWindow :: (MonadIO m) => SDLWindow -> m ()
hideWindow = SDL.hideWindow

setWindowMode :: (MonadIO m) => SDLWindow -> WindowMode -> m ()
setWindowMode window Fullscreen = SDL.setWindowMode window SDL.Fullscreen
setWindowMode window FullscreenDesktop = SDL.setWindowMode window SDL.FullscreenDesktop
setWindowMode window Windowed = SDL.setWindowMode window SDL.Windowed

swapWindow :: (MonadIO m) => SDLWindow -> m ()
swapWindow = SDL.glSwapWindow

getWindowSize :: (MonadIO m) => SDLWindow -> m Size
getWindowSize window = do
  SDL.V2 width height <- SDL.get $ SDL.windowSize window
  return $ Size{sizeWidth = fromIntegral width, sizeHeight = fromIntegral height}

getDrawableSize :: (MonadIO m) => SDLWindow -> m Size
getDrawableSize window = do
  SDL.V2 width height <- SDL.glGetDrawableSize window
  return $ Size{sizeWidth = fromIntegral width, sizeHeight = fromIntegral height}

getScalingFactor :: (MonadIO m) => SDLWindow -> m (Float, Float)
getScalingFactor window = do
  Size{sizeWidth = windowWidth, sizeHeight = windowHeight} <- getWindowSize window
  Size{sizeWidth = drawableWidth, sizeHeight = drawableHeight} <-
    getDrawableSize window
  return
    ( fromIntegral drawableWidth/ fromIntegral windowWidth
    , fromIntegral drawableHeight / fromIntegral windowHeight
    )

-- makeContextCurrent :: Window -> IO ()
-- makeContextCurrent Window{windowHandle = window, windowGlContext = glCtx} =
--   SDL.glMakeCurrent window glCtx

-- sdlWindow :: Window -> Mviz.SDL.Types.SDLWindow
-- sdlWindow Window{windowSdlHandle = window} = window

-- glContext :: Window -> GLContext
-- glContext Window{windowGlContext = glCtx} = glCtx

getError :: IO (Maybe T.Text)
getError = do
  errMsg <- SDL.Raw.Error.getError
  errMsgStr <- peekCString errMsg
  return $ case length errMsgStr of
    0 -> Nothing
    _ -> Just $ T.pack errMsgStr
