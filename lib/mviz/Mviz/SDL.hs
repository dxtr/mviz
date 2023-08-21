module Mviz.SDL (
  Window,
  SDLWindow,
  GLContext,
  Event,
  SDLError (..),
  initialize,
  quit,
  createWindow,
  destroyWindow,
  withWindow,
  swapWindow,
  showWindow,
  hideWindow,
  setWindowMode,
  getWindowSize,
  getDrawableSize,
  getScalingFactor,
  makeContextCurrent,
  sdlWindow,
  glContext,
  getError
) where

import           Control.Exception         (bracket)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Foreign.C.String          (peekCString)
import qualified Graphics.Rendering.OpenGL as SDL
import           Mviz.Window.Types         (Size (..), WindowMode (..))
import qualified SDL
import qualified SDL.Raw.Error

type SDLWindow = SDL.Window

type GLContext = SDL.GLContext

type Event = SDL.Event

data Window = Window
  { windowSdlHandle :: SDL.Window
  , windowGlContext :: GLContext
  }

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

data SDLError = CreateWindow SDLErrorKind

initFlags :: [SDL.InitFlag]
initFlags = [SDL.InitVideo, SDL.InitEvents]

initialize :: IO ()
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

createWindow :: T.Text -> Bool -> IO Window
createWindow title vsync = do
  putStrLn "Creating window..."
  wnd <- SDL.createWindow title windowFlags
  err <- getError
  case err of
    Nothing -> putStrLn "Window was created"
    Just e  -> TIO.putStrLn e
  putStrLn "Created window!"
  glCtx <- createGlContext wnd vsync
  SDL.glMakeCurrent wnd glCtx
  return $ Window{windowSdlHandle = wnd, windowGlContext = glCtx}

destroyWindow :: Window -> IO ()
destroyWindow Window{windowSdlHandle = wndHandle, windowGlContext = glCtx} = do
  SDL.glDeleteContext glCtx
  SDL.destroyWindow wndHandle

createGlContext :: SDL.Window -> Bool -> IO (SDL.GLContext)
createGlContext window False = createGlContext_ window SDL.ImmediateUpdates
createGlContext window True  = createGlContext_ window SDL.SynchronizedUpdates

createGlContext_ :: SDL.Window -> SDL.SwapInterval -> IO (SDL.GLContext)
createGlContext_ window swapInterval = do
  context <- SDL.glCreateContext window
  SDL.swapInterval SDL.$= swapInterval
  return context

withWindow :: T.Text -> Bool -> (Window -> IO c) -> IO c
withWindow title vsync body =
  bracket
    (createWindow title vsync)
    destroyWindow
    body

showWindow :: Window -> IO ()
showWindow Window{windowSdlHandle = window} = SDL.showWindow window

hideWindow :: Window -> IO ()
hideWindow Window{windowSdlHandle = window} = SDL.hideWindow window

setWindowMode :: Window -> WindowMode -> IO ()
setWindowMode Window{windowSdlHandle = window} Fullscreen = SDL.setWindowMode window SDL.Fullscreen
setWindowMode Window{windowSdlHandle = window} FullscreenDesktop = SDL.setWindowMode window SDL.FullscreenDesktop
setWindowMode Window{windowSdlHandle = window} Windowed = SDL.setWindowMode window SDL.Windowed

swapWindow :: Window -> IO ()
swapWindow Window{windowSdlHandle = window} = SDL.glSwapWindow window

quit :: IO ()
quit = SDL.quit

getWindowSize :: Window -> IO (Size)
getWindowSize Window{windowSdlHandle = window} = do
  SDL.V2 width height <- SDL.get $ SDL.windowSize window
  return $ Size{sizeWidth = fromIntegral width, sizeHeight = fromIntegral height}

getDrawableSize :: Window -> IO (Size)
getDrawableSize Window{windowSdlHandle = window} = do
  SDL.V2 width height <- SDL.glGetDrawableSize window
  return $ Size{sizeWidth = fromIntegral width, sizeHeight = fromIntegral height}

getScalingFactor :: Window -> IO (Float, Float)
getScalingFactor window = do
  Size{sizeWidth = windowWidth, sizeHeight = windowHeight} <- getWindowSize window
  Size{sizeWidth = drawableWidth, sizeHeight = drawableHeight} <-
    getDrawableSize window
  return $
    ( (fromIntegral drawableWidth) / (fromIntegral windowWidth)
    , (fromIntegral drawableHeight) / (fromIntegral windowHeight)
    )

makeContextCurrent :: Window -> IO ()
makeContextCurrent Window{windowSdlHandle = window, windowGlContext = glCtx} =
  SDL.glMakeCurrent window glCtx

sdlWindow :: Window -> SDLWindow
sdlWindow Window{windowSdlHandle = window} = window

glContext :: Window -> GLContext
glContext Window{windowGlContext = glCtx} = glCtx

getError :: IO (Maybe T.Text)
getError = do
  errMsg <- SDL.Raw.Error.getError
  errMsgStr <- peekCString errMsg
  return $ case length errMsgStr of
    0 -> Nothing
    _ -> Just $ T.pack errMsgStr