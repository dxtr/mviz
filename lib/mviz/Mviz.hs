{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Mviz where

import           Control.Concurrent.Async   (asyncBound, wait)
import           Control.Concurrent.STM     (newTQueueIO)
import           Control.Exception          (bracket)
import           Control.Monad              (unless, when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader       (MonadReader, ask)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Data.IORef                 (newIORef, readIORef)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Word                  (Word16)
import qualified Graphics.Rendering.OpenGL  as OpenGL
import           ImGui                      (checkVersion)
import qualified Mviz.Audio
import qualified Mviz.GL                    (vendor, version)
import           Mviz.Logger                (MonadLog (..))
import qualified Mviz.SDL
import           Mviz.Types                 (HasFramerate (..),
                                             MonadFramerate (..),
                                             MvizEnvironment (..),
                                             MvizFramerate (..), MvizM (..),
                                             getFramerate, runMviz)
import qualified Mviz.UI
import           Mviz.UI.LogWindow          (MonadLogWindow)
import           Mviz.UI.Types
import           Mviz.UI.UIWindow           (makeLogWindow)
import qualified Mviz.Utils.Ringbuffer      as RB
import           Mviz.Window                (MonadDrawWindow (..), createWindow,
                                             destroyWindow, showWindow,
                                             swapWindowBuffers)
import qualified Mviz.Window.Events
import           Mviz.Window.Types          (HasWindow (..))

calculateFramerate :: (MonadReader e m, MonadFramerate m, MonadLogger m, MonadIO m, HasFramerate e) => m ()
calculateFramerate = do
  ticks <- fromIntegral <$> Mviz.SDL.ticks
  state <- ask
  fps@MvizFramerate { mvizFramerateCounter = frames } <- liftIO . readIORef $ getFramerate state
  let tickDifference :: Word16 = fromIntegral $ ticks - mvizFramerateSample fps
  if (tickDifference >= 1000)
    then (do let newFrameTime = (fromIntegral tickDifference) / (fromIntegral frames)
             let newFramerate = 1000.0 / newFrameTime
             logDebugN $ "Framerate: " <> (T.pack $ show newFramerate)
             modifyFramerate $ fps { mvizFramerate = newFramerate
                                   , mvizFramerateTime = newFrameTime
                                   , mvizFramerateSample = ticks
                                   , mvizFramerateCounter = 0
                                   })
    else modifyFramerate $ fps { mvizFramerateCounter = frames + 1 }

mainLoop :: (MonadReader e m,
             MonadFramerate m,
             MonadLogger m,
             MonadIO m,
             MonadLogWindow m,
             MonadLog m,
             MonadUI m,
             MonadDrawWindow m,
             HasWindow e,
             HasUI e,
             HasFramerate e,
             HasWindow e) => m ()
mainLoop = do
  calculateFramerate

  events <- liftIO $ Mviz.UI.collectEvents
  let doQuit = elem Mviz.Window.Events.Quit events

  OpenGL.clearColor OpenGL.$= (OpenGL.Color4 0 0 0 1)
  liftIO $ OpenGL.clear [OpenGL.ColorBuffer]

  -- environment <- ask
  -- Render the UI
  -- showUI <- liftIO $ readIORef $ mvizShowUI environment
  showUI <- isUIShown
  when showUI $ Mviz.UI.render

--  let wnd = getWindow
  swapWindowBuffers
  unless doQuit mainLoop
 where

run :: MvizM MvizEnvironment ()
run = do
  imguiVersion <- liftIO $ Mviz.UI.version
  glVendor <- liftIO $ T.pack <$> Mviz.GL.vendor
  glVersion <- liftIO $ T.pack <$> Mviz.GL.version
  $(logInfo) $ "OpenGL vendor: " <> glVendor <> ", version: " <> glVersion
  $(logInfo) $ "Dear ImGUI version: " <> imguiVersion

  showWindow
  mainLoop

startup :: IO (MvizEnvironment)
startup = do
  ImGui.checkVersion
  Mviz.SDL.initialize
  wnd <- createWindow "mviz" True
  err <- Mviz.SDL.getError
  putStrLn $ show err
  uiContext <- Mviz.UI.createUIContext wnd
  -- TODO: Deal with errors here
  _res <- runExceptT $ ExceptT $ Mviz.UI.initialize wnd
  audioSendChannel <- newTQueueIO
  audioRecvChannel <- newTQueueIO
  audioThread <- asyncBound $ Mviz.Audio.runAudioSystem audioRecvChannel audioSendChannel
  logBuffer <- RB.empty 100
  logWindow <- makeLogWindow True
  showUI <- newIORef True
  fps <- newIORef $ MvizFramerate { mvizFramerate = 0.0
                                  , mvizFramerateTime = 0.0
                                  , mvizFramerateSample = 0
                                  , mvizFramerateCounter = 0
                                  }
  shaders <- newIORef M.empty
  return $ MvizEnvironment { mvizWindow = wnd
                           , mvizUIContext = uiContext
                           , mvizAudioThread = audioThread
                           , mvizAudioSendChannel = audioSendChannel
                           , mvizAudioRecvChannel = audioRecvChannel
                           , mvizLog = logBuffer
                           , mvizLogWindow = logWindow
                           , mvizShowUI = showUI
                           , mvizFPS = fps
                           , mvizShaders = shaders
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
  >> destroyWindow wnd
  >> Mviz.SDL.quit
  >> wait audioThread
  >> return ()

main :: IO ()
main = bracket startup cleanup $ \env -> do
  _ <- runMviz env run
  return ()
