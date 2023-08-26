{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Mviz where

import           Control.Concurrent.Async   (asyncBound, wait)
import           Control.Concurrent.STM     (newTQueueIO)
import           Control.Exception          (bracket)
import           Control.Monad              (unless, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader       (ask)
import           Control.Monad.State.Strict (get, put)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import qualified Data.Text                  as T
import           Data.Word                  (Word16)
import qualified Graphics.Rendering.OpenGL  as OpenGL
import           ImGui                      (checkVersion)
import qualified Mviz.Audio
import qualified Mviz.GL                    (vendor, version)
import qualified Mviz.SDL
import           Mviz.Types                 (MvizEnvironment (..),
                                             MvizFramerate (..), MvizM (..),
                                             MvizState (..), runMviz)
import qualified Mviz.UI
import           Mviz.Utils                 (if')
import           Mviz.Window                (showWindow, swapWindowBuffers)
import qualified Mviz.Window.Events

calculateFramerate :: MvizM ()
calculateFramerate = do
  ticks <- fromIntegral <$> Mviz.SDL.ticks
  state <- get
  let fps@MvizFramerate{mvizFramerateCounter = frames} = mvizFPS state
  let tickDifference :: Word16 = fromIntegral $ ticks - mvizFramerateSample fps
  if' (tickDifference >= 1000)
    (do let newFrameTime = (fromIntegral tickDifference) / (fromIntegral frames)
        let newFramerate = 1000.0 / newFrameTime
        logDebugN $ "Framerate: " <> (T.pack $ show newFramerate)
        put state { mvizFPS = fps { mvizFramerate = newFramerate
                                  , mvizFramerateTime = newFrameTime
                                  , mvizFramerateSample = ticks
                                  , mvizFramerateCounter = 0 }})
    (put state { mvizFPS = fps { mvizFramerateCounter = frames + 1 } })

mainLoop :: MvizM ()
mainLoop = do
  calculateFramerate

  events <- liftIO $ Mviz.UI.collectEvents
  let doQuit = elem Mviz.Window.Events.Quit events

  OpenGL.clearColor OpenGL.$= (OpenGL.Color4 0 0 0 1)
  liftIO $ OpenGL.clear [OpenGL.ColorBuffer]

  state <- get
  environment <- ask
  -- Render the UI
  let showUI = mvizShowUI state
  when showUI $ Mviz.UI.render

  let wnd = mvizWindow environment
  liftIO $ swapWindowBuffers wnd
  unless doQuit mainLoop
 where

run :: MvizM ()
run = do
  environment <- ask
  imguiVersion <- liftIO $ Mviz.UI.version
  glVendor <- liftIO $ T.pack <$> Mviz.GL.vendor
  glVersion <- liftIO $ T.pack <$> Mviz.GL.version
  $(logInfo) $ "OpenGL vendor: " <> glVendor <> ", version: " <> glVersion
  $(logInfo) $ "Dear ImGUI version: " <> imguiVersion

  liftIO $ showWindow $ mvizWindow environment
  mainLoop

startup :: IO (MvizEnvironment)
startup = do
  ImGui.checkVersion
  Mviz.SDL.initialize
  wnd <- Mviz.SDL.createWindow "mviz" True
  err <- Mviz.SDL.getError
  putStrLn $ show err
  uiContext <- Mviz.UI.createUIContext wnd
  -- TODO: Deal with errors here
  _res <- runExceptT $ ExceptT $ Mviz.UI.initialize wnd
  audioSendChannel <- newTQueueIO
  audioRecvChannel <- newTQueueIO
  audioThread <- asyncBound $ Mviz.Audio.runAudioSystem audioRecvChannel audioSendChannel
  return $ MvizEnvironment { mvizWindow = wnd
                           , mvizUIContext = uiContext
                           , mvizAudioThread = audioThread
                           , mvizAudioSendChannel = audioSendChannel
                           , mvizAudioRecvChannel = audioRecvChannel
                           }

cleanup :: MvizEnvironment -> IO ()
cleanup
  MvizEnvironment { mvizWindow = wnd
                  , mvizUIContext = uiContext
                  , mvizAudioThread = audioThread
                  , mvizAudioSendChannel = audioSendChannel
                  } =
  Mviz.Audio.shutdown audioSendChannel
  >> Mviz.UI.shutdown
  >> Mviz.UI.destroyUIContext uiContext
  >> Mviz.SDL.destroyWindow wnd
  >> Mviz.SDL.quit
  >> wait audioThread
  >> return ()

main :: IO ()
main = bracket startup cleanup $ \env -> do
  _ <- runMviz env run
  return ()
