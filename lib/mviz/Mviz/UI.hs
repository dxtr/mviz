--{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}

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
  , handleEvents
  ) where

import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Logger       (MonadLogger)
import           Control.Monad.Reader       (MonadReader, ask, asks)
import           Control.Monad.State.Strict (runStateT)
import           Control.Monad.Trans.Maybe  (MaybeT (..), runMaybeT)
import           Data.Functor               ((<&>))
import qualified Data.Text                  as T
import qualified ImGui
import qualified ImGui.GL
import qualified ImGui.SDL
import           Mviz.Audio.Types           (MonadAudio (..),
                                             MonadAudioClient (..),
                                             ServerAudioMessage (..))
import           Mviz.GL                    (GLMakeCurrent (..),
                                             HasGLContext (..))
import           Mviz.Logger                (MonadLog (..))
import           Mviz.UI.LogWindow          (MonadLogWindow (..),
                                             renderLogWindow)
import           Mviz.UI.SettingsWindow     (HasSettingsWindow,
                                             MonadSettingsWindow,
                                             getSettingsWindow,
                                             renderSettingsWindow,
                                             setSettingsWindow)
import           Mviz.UI.Types              (MonadUI, UIContext)
import           Mviz.UI.UIWindow           (SettingsWindow (..))
import           Mviz.Utils                 ((<&&>))
import           Mviz.Window.Events         (Event (..))
import           Mviz.Window.Types          (HasNativeWindow (..))
import qualified SDL                        (EventPayload (KeyboardEvent, QuitEvent),
                                             InputMotion (..),
                                             KeyboardEventData (..),
                                             Keysym (..), eventPayload,
                                             pattern KeycodeEscape,
                                             pattern KeycodeF1,
                                             pattern KeycodeF11)

version :: IO T.Text
version = ImGui.getVersion

initialize :: (MonadIO m, HasGLContext a, HasNativeWindow a) => a -> m (Either () ())
initialize window = do
  ImGui.checkVersion
  ImGui.styleColorsDark
  success <- ImGui.SDL.sdlInit wnd ctx <&&> ImGui.GL.glInit
  -- TODO: Actually get a proper error message here
  pure (if success then Right () else Left ())
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

render :: ( MonadReader e m
          , HasSettingsWindow e
          , MonadLogWindow m
          , MonadSettingsWindow m
          , MonadLog m
          , MonadUI m
          , MonadAudio m
          ) => m Bool
render = do
  _ <- liftIO $ do
    newFrame
    ImGui.showDemoWindow

  env <- ask
  sampleRate <- audioSampleRate
  bufferSize <- audioBufferSize
  inputs <- audioInputs

  renderLogWindow
  (settingsChanged, newSettings) <- (asks getSettingsWindow >>= liftIO) >>=
    runStateT (renderSettingsWindow sampleRate bufferSize inputs [])
  liftIO $ setSettingsWindow env newSettings
  ImGui.render
  ImGui.GL.glRenderDrawData =<< ImGui.getDrawData
  pure settingsChanged

translateEvent :: SDL.EventPayload -> Mviz.Window.Events.Event
translateEvent SDL.QuitEvent = Mviz.Window.Events.Quit
translateEvent (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeyMotion = SDL.Released
                                                        , SDL.keyboardEventRepeat = False
                                                        , SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymKeycode = SDL.KeycodeEscape }
                                                        }) = Mviz.Window.Events.Quit
translateEvent (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeyMotion = SDL.Released
                                                        , SDL.keyboardEventRepeat = False
                                                        , SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymKeycode = SDL.KeycodeF11 }
                                                        }) = Mviz.Window.Events.ToggleFullscreen
translateEvent (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeyMotion = SDL.Released
                                                        , SDL.keyboardEventRepeat = False
                                                        , SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymKeycode = SDL.KeycodeF1 }
                                                        }) = Mviz.Window.Events.ToggleUI
translateEvent ignoredEvent = IgnoredEvent ignoredEvent

pollEvent :: IO (Maybe Mviz.Window.Events.Event)
pollEvent = runMaybeT (MaybeT ImGui.SDL.sdlPollEvent <&> (translateEvent . SDL.eventPayload))

collectEvents :: IO [Mviz.Window.Events.Event]
collectEvents = pollEvent >>= f
 where
  f Nothing  = pure []
  f (Just e) = (e :) <$> collectEvents

handleEvents :: (Mviz.Window.Events.Event -> IO ()) -> IO ()
handleEvents f = do
  evt <- pollEvent
  case evt of
    Nothing -> pure ()
    Just e  -> f e >> handleEvents f
