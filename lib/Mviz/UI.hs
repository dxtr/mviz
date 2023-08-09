module Mviz.UI (
  UIContext,
  initialize,
  shutdown,
  createUIContext,
  destroyUIContext,
  showDemoWindow,
  showMetricsWindow,
  showUserGuide,
  newFrame,
  endFrame,
  render
) where

import DearImGui qualified as ImGUI
import DearImGui.OpenGL3 qualified as ImGUI.GL
--import DearImGui.SDL qualified as ImGUI.SDL
import Mviz.GL (GLMakeCurrent, glMakeCurrent)

type UIContext = ImGUI.Context

initialize :: IO Bool
initialize = do
  ImGUI.GL.openGL3Init

shutdown :: IO ()
shutdown = do
  ImGUI.GL.openGL3Shutdown

createUIContext :: (GLMakeCurrent c) => c -> IO UIContext
createUIContext glContext = do
  _ <- glMakeCurrent glContext
  ImGUI.createContext

destroyUIContext :: UIContext -> IO ()
destroyUIContext = ImGUI.destroyContext

showDemoWindow :: IO ()
showDemoWindow = ImGUI.showDemoWindow

showMetricsWindow :: IO ()
showMetricsWindow = ImGUI.showMetricsWindow

showUserGuide :: IO ()
showUserGuide = ImGUI.showUserGuide

newFrame :: IO ()
newFrame = ImGUI.newFrame

endFrame :: IO ()
endFrame = ImGUI.endFrame

render :: IO ()
render = ImGUI.render

