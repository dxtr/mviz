module Mviz.UI (
  collectEvents,
  createUIContext,
  destroyUIContext,
  endFrame,
  getCurrentContext,
  initialize,
  newFrame,
  pollEvent,
  render,
  showDemoWindow,
  showMetricsWindow,
  showUserGuide,
  shutdown,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import DearImGui qualified as ImGUI
import DearImGui.OpenGL3 qualified as ImGUI.GL
import DearImGui.SDL qualified as ImGUI.SDL
import DearImGui.SDL.OpenGL qualified as ImGUI.SDL.OpenGL
import Mviz.GL (GLMakeCurrent, glMakeCurrent)
import Mviz.SDL (Window, glContext, sdlWindow)
import Mviz.Types (MvizM)
import Mviz.UI.Types
import Mviz.Utils ((<&&>))
import Mviz.Window.Events (Event (IgnoredEvent, Quit, WindowResized))
import SDL qualified (
  EventPayload (QuitEvent, WindowResizedEvent),
  V2 (..),
  WindowResizedEventData (WindowResizedEventData, windowResizedEventSize),
  eventPayload,
 )

initialize :: Window -> IO Bool
initialize window = do
  ImGUI.checkVersion
  ImGUI.styleColorsDark
  ImGUI.SDL.OpenGL.sdl2InitForOpenGL wnd ctx <&&> ImGUI.GL.openGL3Init
 where
  wnd = sdlWindow window
  ctx = glContext window

shutdown :: IO ()
shutdown = do
  ImGUI.GL.openGL3Shutdown

createUIContext :: (GLMakeCurrent c) => c -> IO UIContext
createUIContext glCtx = do
  _ <- glMakeCurrent glCtx
  ImGUI.createContext

destroyUIContext :: UIContext -> IO ()
destroyUIContext = ImGUI.destroyContext

getCurrentContext :: IO UIContext
getCurrentContext = ImGUI.getCurrentContext

showDemoWindow :: IO ()
showDemoWindow = ImGUI.showDemoWindow

showMetricsWindow :: IO ()
showMetricsWindow = ImGUI.showMetricsWindow

showUserGuide :: IO ()
showUserGuide = ImGUI.showUserGuide

newFrame :: IO ()
newFrame = do
  ImGUI.GL.openGL3NewFrame
  ImGUI.SDL.sdl2NewFrame
  ImGUI.newFrame

endFrame :: IO ()
endFrame = ImGUI.endFrame

render :: MvizM ()
render = do
  liftIO $ do
    newFrame
    showDemoWindow
  ImGUI.render
  ImGUI.GL.openGL3RenderDrawData =<< ImGUI.getDrawData

pollEvent :: IO (Maybe Mviz.Window.Events.Event)
pollEvent = runMaybeT $ do
  event <- MaybeT $ ImGUI.SDL.pollEventWithImGui
  return $ case SDL.eventPayload event of
    SDL.QuitEvent -> Mviz.Window.Events.Quit
    SDL.WindowResizedEvent
      SDL.WindowResizedEventData{SDL.windowResizedEventSize = SDL.V2 newX newY} -> Mviz.Window.Events.WindowResized (fromIntegral newX) (fromIntegral newY)
    ignoredEvent -> IgnoredEvent ignoredEvent

collectEvents :: IO [Mviz.Window.Events.Event]
collectEvents = pollEvent >>= f
 where
  f Nothing = return []
  f (Just e) = (e :) <$> collectEvents
