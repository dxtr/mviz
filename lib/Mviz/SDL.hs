module Mviz.SDL (
    Window,
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
    eventLoop
) where

import Control.Exception (bracket)
import Data.Text qualified as T
import Mviz.Window.Types (Size (..), WindowMode (..))
import SDL qualified
import Control.Monad (unless)
import qualified Graphics.Rendering.OpenGL as OpenGL
import Graphics.Rendering.OpenGL (Color4(Color4))

data Window = Window
    { windowSdlHandle :: SDL.Window
    , windowGlContext :: SDL.GLContext
    }

data SDLErrorKind
    = CallFailed
        { sdlErrorCaller :: !T.Text
        , sdlErrorFunction :: !T.Text
        , sdlErrorMessage :: !T.Text
        }
    | UnexpectedArgument
        { sdlErrorCaller :: !T.Text
        , sdlErrorFunction :: !T.Text
        , sdlErrorValue :: !String
        }
    | UnknownHintValue
        { sdlErrorHint :: !String
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
        , SDL.windowGraphicsContext = SDL.OpenGLContext $ SDL.defaultOpenGL{SDL.glProfile = SDL.Core SDL.Normal 4 6}
        , SDL.windowPosition = SDL.Wherever
        , SDL.windowResizable = True
        , SDL.windowInitialSize = SDL.V2 1024 768
        }

createWindow :: T.Text -> Bool -> IO Window
createWindow title vsync =
    SDL.createWindow title windowFlags
        >>= \wnd ->
            createGlContext wnd vsync
                >>= \glContext -> SDL.glMakeCurrent wnd glContext >> pure $ Window{windowSdlHandle = wnd, windowGlContext = glContext}

destroyWindow :: Window -> IO ()
destroyWindow Window{windowSdlHandle = wndHandle, windowGlContext = glContext} = do
    SDL.glDeleteContext glContext
    SDL.destroyWindow wndHandle

createGlContext :: SDL.Window -> Bool -> IO (SDL.GLContext)
createGlContext window False = createGlContext_ window SDL.ImmediateUpdates
createGlContext window True = createGlContext_ window SDL.SynchronizedUpdates

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

swapWindow :: SDL.Window -> IO ()
swapWindow = SDL.glSwapWindow

quit :: IO ()
quit = SDL.quit

-- getWindowSize :: SDL.Window -> IO (CInt, CInt)
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
    Size{sizeWidth = drawableWidth, sizeHeight = drawableHeight} <- getDrawableSize window
    return $
        ( (fromIntegral drawableWidth) / (fromIntegral windowWidth)
        , (fromIntegral drawableHeight) / (fromIntegral windowHeight)
        )


eventLoop :: Window -> IO ()
eventLoop wnd@Window{windowSdlHandle = window, windowGlContext = context} = do
    events <- collectEvents
    let doQuit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
    OpenGL.clearColor OpenGL.$= (Color4 0 0 0 1) 
    OpenGL.clear [OpenGL.ColorBuffer]
    unless doQuit (eventLoop wnd)
  where
    collectEvents :: IO [SDL.Event]
    collectEvents = do
        e <- SDL.pollEvent
        case e of
            Nothing -> return []
            Just e' -> (e' :) <$> collectEvents
