{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Mviz where

import           Control.Concurrent.Async   (async, wait)
import           Control.Concurrent.STM     (newTQueueIO)
import           Control.Exception          (bracket)
import           Control.Monad              (unless, when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Logger       (MonadLogger, logDebugN, logInfo)
import           Control.Monad.Reader       (MonadReader, MonadTrans (lift),
                                             ask)
import           Control.Monad.Reader.Class (asks)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Maybe  (MaybeT (MaybeT), runMaybeT)
import           Data.IORef                 (newIORef, readIORef, writeIORef)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Word                  (Word16)
import qualified Graphics.Rendering.OpenGL  as OpenGL
import           ImGui                      (checkVersion)
import qualified Mviz.Audio
import           Mviz.Audio.Inputs          (mkInputMap)
import           Mviz.Audio.Types           (ClientAudioMessage (..),
                                             HasBufferSize (getBufferSizeRef),
                                             HasInputs (..),
                                             HasSampleRate (getSampleRateRef),
                                             MonadAudio, MonadAudioClient (..),
                                             ServerAudioMessage (GetSamples, SetInput))
import qualified Mviz.GL                    (vendor, version)
import           Mviz.Logger                (MonadLog (..), ringBufferOutput)
import qualified Mviz.SDL
import           Mviz.Types                 (HasFramerate (..),
                                             MonadFramerate (..),
                                             MvizEnvironment (..),
                                             MvizFramerate (..), MvizM (..),
                                             getFramerate, runMviz)
import qualified Mviz.UI
import           Mviz.UI.LogWindow          (MonadLogWindow)
import           Mviz.UI.SettingsWindow     (MonadSettingsWindow (getSelectedChannels, getSelectedInput))
import           Mviz.UI.Types              (HasUI, MonadUI (..))
import           Mviz.UI.UIWindow           (makeLogWindow, makeSettingsWindow)
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
  if tickDifference >= 1000
    then (do let newFrameTime = fromIntegral tickDifference / fromIntegral frames
             let newFramerate = 1000.0 / newFrameTime
             logDebugN $ "Framerate: " <> T.pack (show newFramerate)
             modifyFramerate $ fps { mvizFramerate = newFramerate
                                   , mvizFramerateTime = newFrameTime
                                   , mvizFramerateSample = ticks
                                   , mvizFramerateCounter = 0
                                   })
    else modifyFramerate $ fps { mvizFramerateCounter = frames + 1 }

handleAudioMessage ::
  ( HasInputs r
  , HasSampleRate r
  , HasBufferSize r
  , MonadIO m
  , MonadReader r m
  , MonadLogger m
  ) => ClientAudioMessage -> m ()
handleAudioMessage (Inputs inputs) = asks getInputsRef >>= \inputsRef ->
  liftIO $ writeIORef inputsRef inputs
handleAudioMessage (Ports ports) = asks getPortsRef >>= \portsRef ->
  liftIO $ writeIORef portsRef ports
handleAudioMessage (SampleRate sr) = asks getSampleRateRef >>= \srRef ->
  liftIO $ writeIORef srRef sr
handleAudioMessage (BufferSize bs) = asks getBufferSizeRef >>= \bsRef ->
  liftIO $ writeIORef bsRef bs
handleAudioMessage (Samples samples) = do
  logDebugN $ "Got samples: " <> T.pack (show samples)

mainLoop :: (MonadReader e m,
             MonadFramerate m,
             MonadLogger m,
             MonadIO m,
             MonadLogWindow m,
             MonadSettingsWindow m,
             MonadLog m,
             MonadUI m,
             MonadDrawWindow m,
             MonadAudioClient m,
             MonadAudio m,
             HasWindow e,
             HasUI e,
             HasFramerate e,
             HasSampleRate e,
             HasBufferSize e,
             HasInputs e,
             HasWindow e) => m ()
mainLoop = do
  calculateFramerate
  _ <- clientSendMessage GetSamples

  -- Read messages from the audio server
  -- TODO: Read all messages in one go?
  _ <- runMaybeT $ do
    audioMessage <- MaybeT clientRecvMessage
    logDebugN $ "Audio message: " <> T.pack (show audioMessage)
    lift $ handleAudioMessage audioMessage

  events <- liftIO Mviz.UI.collectEvents
  let doQuit = Mviz.Window.Events.Quit `elem` events

  OpenGL.clearColor OpenGL.$= OpenGL.Color4 0 0 0 1
  liftIO $ OpenGL.clear [OpenGL.ColorBuffer]

  showUI <- isUIShown
  when showUI $ do
    changes <- renderUI
    when changes $ do
      channels <- getSelectedChannels
      _ <- runMaybeT $ do
        input <- MaybeT getSelectedInput
        clientSendMessage $ SetInput (input, channels)
      return ()

  swapWindowBuffers
  unless doQuit mainLoop

run :: MvizM MvizEnvironment ()
run = do
  imguiVersion <- liftIO Mviz.UI.version
  glVendor <- liftIO $ T.pack <$> Mviz.GL.vendor
  glVersion <- liftIO $ T.pack <$> Mviz.GL.version
  $(logInfo) $ "OpenGL vendor: " <> glVendor <> ", version: " <> glVersion
  $(logInfo) $ "Dear ImGUI version: " <> imguiVersion

  showWindow
  mainLoop

startup :: IO MvizEnvironment
startup = do
  ImGui.checkVersion
  Mviz.SDL.initialize
  wnd <- createWindow "mviz" True
  err <- Mviz.SDL.getError
  print err
  uiContext <- Mviz.UI.createUIContext wnd
  -- TODO: Deal with errors here
  _res <- runExceptT $ ExceptT $ Mviz.UI.initialize wnd
  audioSendChannel <- newTQueueIO
  audioRecvChannel <- newTQueueIO
  audioThread <- async $ Mviz.Audio.runAudioSystem audioRecvChannel audioSendChannel
  logBuffer <- RB.empty 100
  logWindow <- makeLogWindow True
  showUI <- newIORef True
  sampleRate <- newIORef 0
  bufferSize <- newIORef 0
  audioPorts <- newIORef []
  settingsWindow <- makeSettingsWindow True
  fps <- newIORef $ MvizFramerate { mvizFramerate = 0.0
                                  , mvizFramerateTime = 0.0
                                  , mvizFramerateSample = 0
                                  , mvizFramerateCounter = 0
                                  }
  shaders <- newIORef M.empty
  inputMap <- newIORef $ mkInputMap []
  pure $ MvizEnvironment { mvizWindow = wnd
                           , mvizUIContext = uiContext
                           , mvizAudioThread = audioThread
                           , mvizAudioSendChannel = audioSendChannel
                           , mvizAudioRecvChannel = audioRecvChannel
                           , mvizLog = logBuffer
                           , mvizLogFunc = ringBufferOutput logBuffer
                           , mvizLogWindow = logWindow
                           , mvizShowUI = showUI
                           , mvizFPS = fps
                           , mvizShaders = shaders
                           , mvizSettingsWindow = settingsWindow
                           , mvizAudioSampleRate = sampleRate
                           , mvizAudioBufferSize = bufferSize
                           , mvizAudioPorts = audioPorts
                           , mvizAudioInputs = inputMap
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
  >> pure ()

main :: IO ()
main = bracket startup cleanup $ \env -> do
  _ <- runMviz env run
  pure ()
