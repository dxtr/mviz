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
  version
) where

import           Control.Monad              (when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (get, put)
import           Control.Monad.Trans.Maybe  (MaybeT (..), runMaybeT)
import qualified Data.Text                  as T
import qualified ImGui
import           Mviz.GL                    (GLMakeCurrent, glMakeCurrent)
import           Mviz.SDL                   (Window, glContext, sdlWindow)
import           Mviz.Types                 (MvizM, MvizState (..))
import           Mviz.UI.LogWindow          (renderLogWindow)
import           Mviz.UI.Types
import           Mviz.UI.UIWindow           (LogWindow (..))
import           Mviz.Utils                 ((<&&>))
import           Mviz.Window.Events         (Event (IgnoredEvent, Quit, WindowResized))
import qualified SDL                        (EventPayload (KeyboardEvent, QuitEvent, WindowResizedEvent),
                                             KeyboardEventData (..), V2 (..),
                                             WindowResizedEventData (..),
                                             eventPayload)

version :: IO T.Text
version = ImGui.getVersion

initialize :: Window -> IO (Either () ())
initialize window = do
  ImGUI.checkVersion
  ImGUI.styleColorsDark
  success <- ImGUI.SDL.OpenGL.sdl2InitForOpenGL wnd ctx <&&> ImGUI.GL.openGL3Init
  -- TODO: Actually get a proper error message here
  return $ case success of
    True  -> Right ()
    False -> Left ()
 where
  wnd = sdlWindow window
  ctx = glContext window

shutdown :: IO ()
shutdown = do
  ImGUI.GL.openGL3Shutdown

createUIContext :: (GLMakeCurrent c) => c -> IO UIContext
createUIContext glCtx = do
  _ <- glMakeCurrent glCtx
  ImGui.createContext

destroyUIContext :: UIContext -> IO ()
destroyUIContext = ImGui.destroyContext

getCurrentContext :: IO UIContext
getCurrentContext = ImGui.getCurrentContext

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
  ImGui.newFrame

endFrame :: IO ()
endFrame = ImGui.endFrame

render :: MvizM ()
render = do
  state <- get
  let logWindow = mvizLogWindow state
  liftIO $ do
    newFrame
    showDemoWindow

  (when $ logWindowShow logWindow) renderLogWindow

  ImGui.render
  ImGui.GL.openGL3RenderDrawData =<< ImGui.getDrawData

translateEvent :: SDL.EventPayload -> Mviz.Window.Events.Event
translateEvent SDL.QuitEvent = Mviz.Window.Events.Quit
translateEvent (SDL.WindowResizedEvent SDL.WindowResizedEventData{SDL.windowResizedEventSize = SDL.V2 newX newY}) =
  Mviz.Window.Events.WindowResized (fromIntegral newX) (fromIntegral newY)
-- translateEvent (SDL.KeyboardEvent SDL.KeyboardEventData{ SDL.keyboardEventKeyMotion = SDL.Pressed
--                                                        , SDL.keyboardEventRepeat = repeat
--                                                        , SDL.keyboardEventKeysym = keysym
--                                                        }) = IgnoredEvent
-- translateEvent (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeyMotion = SDL.Released
--                                                         , SDL.keyboardEventRepeat = repeat
--                                                         , SDL.keyboardEventKeysym = keysym
--                                                         }) = Mviz.Window.Events.Quit
translateEvent ignoredEvent = IgnoredEvent ignoredEvent

pollEvent :: IO (Maybe Mviz.Window.Events.Event)
pollEvent = runMaybeT $ (MaybeT $ ImGUI.SDL.pollEventWithImGui) >>= return . translateEvent . SDL.eventPayload

collectEvents :: IO [Mviz.Window.Events.Event]
collectEvents = pollEvent >>= f
 where
  f Nothing  = return []
  f (Just e) = (e :) <$> collectEvents
