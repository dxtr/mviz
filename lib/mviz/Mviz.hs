{-# LANGUAGE OverloadedStrings #-}

module Mviz where

import           Control.Concurrent.Async   (asyncBound, wait)
import           Control.Concurrent.STM     (newTChanIO)
import           Control.Exception          (bracket)
import           Control.Monad              (unless, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (ask)
import           Control.Monad.State.Strict (get)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import qualified Data.Text.IO               as TIO (putStrLn)
import qualified Graphics.Rendering.OpenGL  as OpenGL
import           ImGui                      (checkVersion)
import qualified Mviz.Audio
import qualified Mviz.GL                    (vendor, version)
import qualified Mviz.SDL
import           Mviz.Types                 (MvizEnvironment (..), MvizM (..),
                                             MvizState (..), runMviz)
import qualified Mviz.UI
import           Mviz.Window                (showWindow, swapWindowBuffers)
import qualified Mviz.Window.Events
import           System.Exit                (exitFailure)

mainLoop :: MvizM ()
mainLoop = do
  events <- liftIO $ Mviz.UI.collectEvents
  let doQuit = elem Mviz.Window.Events.Quit events

  OpenGL.clearColor OpenGL.$= (OpenGL.Color4 0 0 0 1)
  liftIO $ OpenGL.clear [OpenGL.ColorBuffer]

  environment <- ask
  state <- get
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
  liftIO $ do
    imguiVersion <- Mviz.UI.version
    glVendor <- Mviz.GL.vendor
    glVersion <- Mviz.GL.version
    putStrLn $ "OpenGL vendor: " <> glVendor <> ", version: " <> glVersion
    TIO.putStrLn $ "Dear ImGUI version: " <> imguiVersion
    showWindow $ mvizWindow environment
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
  audioSendChannel <- newTChanIO
  audioRecvChannel <- newTChanIO
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
