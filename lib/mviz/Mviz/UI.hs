{-# LANGUAGE AllowAmbiguousTypes #-}

module Mviz.UI
  ( collectEvents
  , createUIContext
  , destroyUIContext
  , endFrame
  , getCurrentContext
  , initialize
  , newFrame
  , pollEvent
  , render
  , showDemoWindow
  , showMetricsWindow
  , showUserGuide
  , shutdown
  , version
  ) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (MonadReader)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.Text                 as T
import qualified ImGui
import qualified ImGui.GL
import qualified ImGui.SDL
import           Mviz.GL                   (GLMakeCurrent (..),
                                            HasGLContext (..))
import           Mviz.Logger               (MonadLog (..))
-- import           Mviz.SDL                  (glContext, sdlWindow)
-- import           Mviz.Types                (MvizEnvironment (..))
import           Data.Functor              ((<&>))
import           Mviz.Audio.Types          (MonadAudio (..))
import           Mviz.UI.LogWindow         (MonadLogWindow (..),
                                            renderLogWindow)
import           Mviz.UI.SettingsWindow    (MonadSettingsWindow,
                                            renderSettingsWindow)
import           Mviz.UI.Types             (HasUI, MonadUI, UIContext)
import           Mviz.Utils                ((<&&>))
import           Mviz.Window.Events        (Event (IgnoredEvent, Quit, WindowResized))
import           Mviz.Window.Types         (HasNativeWindow (..))
import qualified SDL                       (EventPayload (QuitEvent, WindowResizedEvent),
                                            V2 (..),
                                            WindowResizedEventData (..),
                                            eventPayload)

version :: IO T.Text
version = ImGui.getVersion

initialize :: (MonadIO m, HasGLContext a, HasNativeWindow a) => a -> m (Either () ())
initialize window = do
  ImGui.checkVersion
  ImGui.styleColorsDark
  success <- ImGui.SDL.sdlInit wnd ctx <&&> ImGui.GL.glInit
  -- TODO: Actually get a proper error message here
  return (if success then Right () else Left ())
 where
  wnd = getNativeWindow window
  ctx = getGLContext window

shutdown :: IO ()
shutdown = do
  ImGui.GL.glShutdown

createUIContext :: (MonadIO m, HasNativeWindow a, HasGLContext a) => a -> m ImGui.Context
createUIContext glCtx = do
  _ <- glMakeCurrent glCtx
  ImGui.createContext

destroyUIContext :: (MonadIO m) => UIContext -> m ()
destroyUIContext = ImGui.destroyContext

getCurrentContext :: (MonadIO m) => m UIContext
getCurrentContext = ImGui.getCurrentContext

showDemoWindow :: (MonadIO m) => m Bool
showDemoWindow = ImGui.showDemoWindow

showMetricsWindow :: (MonadIO m) => m Bool
showMetricsWindow = ImGui.showMetricsWindow

showUserGuide :: (MonadIO m) => m ()
showUserGuide = ImGui.showUserGuide

newFrame :: (MonadIO m) => m ()
newFrame = do
  ImGui.GL.glNewFrame
  ImGui.SDL.sdlNewFrame
  ImGui.newFrame

endFrame :: (MonadIO m) => m ()
endFrame = ImGui.endFrame

render :: ( HasUI e
          , MonadReader e m
          , MonadLogWindow m
          , MonadSettingsWindow m
          , MonadLog m
          , MonadUI m
          , MonadAudio m
          ) => m ()
render = do
  _ <- liftIO $ do
    newFrame
    ImGui.showDemoWindow

  sampleRate <- audioSampleRate
  bufferSize <- audioBufferSize
  ports <- audioPorts

  renderLogWindow
  renderSettingsWindow sampleRate bufferSize ports

  ImGui.render
  ImGui.GL.glRenderDrawData =<< ImGui.getDrawData

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
pollEvent = runMaybeT (MaybeT ImGui.SDL.sdlPollEvent <&> (translateEvent . SDL.eventPayload))

collectEvents :: IO [Mviz.Window.Events.Event]
collectEvents = pollEvent >>= f
 where
  f Nothing  = return []
  f (Just e) = (e :) <$> collectEvents
