{-# LANGUAGE OverloadedStrings #-}

module Mviz where

import           Control.Concurrent.Async   (async, wait)
import           Control.Concurrent.STM     (newTQueueIO)
import           Control.Monad              (unless, when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Logger       (MonadLogger, logErrorN)
import           Control.Monad.Reader       (MonadReader, MonadTrans (lift),
                                             ask)
import           Control.Monad.Reader.Class (asks)
import           Control.Monad.Trans.Maybe  (MaybeT (MaybeT), runMaybeT)
import           Data.Either                (isLeft)
import           Data.Foldable              (for_)
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
import           Mviz.Config                (dumpConfig, fetchConfig)
import           Mviz.Config.Types          (Config (Config, configInputs, configShowUI))
import qualified Mviz.GL                    (renderer, version)
import           Mviz.Logger                (MonadLog (..), ringBufferOutput)
import qualified Mviz.SDL
import           Mviz.Shader                (shaderDirectory)
import qualified Mviz.Shader                as Shader (listShaders)
import           Mviz.Shader.Types          (listShaders)
import           Mviz.Types                 (HasFramerate (..),
                                             MonadFramerate (..),
                                             MvizEnvironment (..),
                                             MvizFramerate (..),
                                             MvizGL (MvizGL), MvizM (..),
                                             getFramerate, runMviz)
import qualified Mviz.UI
import           Mviz.UI.LogWindow          (MonadLogWindow)
import           Mviz.UI.SettingsWindow     (MonadSettingsWindow (getSelectedChannels, getSelectedInput),
                                             selectedPorts)
import           Mviz.UI.Types              (HasUI, MonadUI (..))
import           Mviz.UI.UIWindow           (makeLogWindow, makeSettingsWindow)
import           Mviz.Utils.Filesystem      (ensureDirectory)
import qualified Mviz.Utils.Ringbuffer      as RB
import           Mviz.Window                (MonadDrawWindow (..), createWindow,
                                             destroyWindow, showWindow,
                                             swapWindowBuffers)
import qualified Mviz.Window.Events
import           Mviz.Window.Types          (HasWindow (..))
import           System.INotify
import           UnliftIO.Exception         (bracket)

calculateFramerate :: (MonadReader e m, MonadFramerate m, MonadIO m, HasFramerate e) => m ()
calculateFramerate = do
  ticks <- fromIntegral <$> Mviz.SDL.ticks
  state <- ask
  fps@MvizFramerate { mvizFramerateCounter = frames } <- liftIO . readIORef $ getFramerate state
  let tickDifference :: Word16 = fromIntegral $ ticks - mvizFramerateSample fps
  if tickDifference >= 1000
    then (do let newFrameTime = fromIntegral tickDifference / fromIntegral frames
             let newFramerate = 1000.0 / newFrameTime
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
handleAudioMessage (Samples _samples) = do
  -- TODO: Send the samples to the shader
  pure ()
handleAudioMessage (AudioThreadError ex) = do
  logErrorN $ "Audio thread error: " <> T.pack (show ex)

shouldHandleEvent :: Mviz.Window.Events.Event -> Bool
shouldHandleEvent (Mviz.Window.Events.IgnoredEvent _) = False
shouldHandleEvent _                                   = True

handleEvent :: (MonadLogger m, MonadUI m) => Mviz.Window.Events.Event -> m ()
handleEvent (Mviz.Window.Events.IgnoredEvent _) = pure ()
handleEvent Mviz.Window.Events.ToggleUI         = toggleUI
handleEvent Mviz.Window.Events.ToggleFullscreen = pure () -- TODO
handleEvent Mviz.Window.Events.Quit             = pure () -- TODO

mainLoop :: (MonadReader e m,
             MonadFramerate m,
             MonadLogger m,
             MonadLogWindow m,
             MonadSettingsWindow m,
             MonadLog m,
             MonadUI m,
             MonadDrawWindow m,
             MonadAudioClient m,
             MonadAudio m,
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
    lift $ handleAudioMessage audioMessage

  events <- liftIO Mviz.UI.collectEvents
  mapM_ handleEvent events

  let doQuit = Mviz.Window.Events.Quit `elem` events -- This should probably be handled in handleEvent

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
  showWindow
  channels <- getSelectedChannels
  _ <- runMaybeT $ do
    input <- MaybeT getSelectedInput
    clientSendMessage $ SetInput (input, channels)
  mainLoop

startup :: IO MvizEnvironment
startup = do
  ImGui.checkVersion
  Mviz.SDL.initialize
  ensureDirectory =<< shaderDirectory
  config <- either error id <$> fetchConfig
--  shaders <- Shader.listShaders
  wnd <- createWindow "mviz" True
  err <- Mviz.SDL.getError
  for_ err (error . T.unpack)
  uiContext <- Mviz.UI.createUIContext wnd
  -- TODO: Deal with errors here
  res <- Mviz.UI.initialize wnd
  when (isLeft res) $ error "Could not initialize UI"
  audioSendChannel <- newTQueueIO
  audioRecvChannel <- newTQueueIO
  audioThread <- async $ Mviz.Audio.runAudioSystem audioRecvChannel audioSendChannel
  logBuffer <- RB.empty 100
  logWindow <- makeLogWindow True
  showUI <- newIORef $ configShowUI config
  sampleRate <- newIORef 0
  bufferSize <- newIORef 0
  audioPorts <- newIORef []
  watches <- newIORef []
  let inputs = configInputs config
  settingsWindow <- makeSettingsWindow True inputs
  inotify <- initINotify
  fps <- newIORef $ MvizFramerate { mvizFramerate = 0.0
                                  , mvizFramerateTime = 0.0
                                  , mvizFramerateSample = 0
                                  , mvizFramerateCounter = 0
                                  }
  shaders <- newIORef M.empty
  inputMap <- newIORef $ mkInputMap inputs
  imguiVersion <- Mviz.UI.version

  gl <- MvizGL
        <$> Mviz.GL.renderer
        <*> Mviz.GL.version
        <*> Mviz.GL.version

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
                         , mvizGL = gl
                         , mvizInotify = inotify
                         , mvizWatches = watches
                         }

cleanup :: MvizEnvironment -> IO ()
cleanup
  MvizEnvironment { mvizWindow = wnd
                  , mvizUIContext = uiContext
                  , mvizAudioThread = audioThread
                  , mvizAudioSendChannel = audioSendChannel
                  , mvizShowUI = showUI
                  , mvizSettingsWindow = settingsWindow
                  , mvizInotify = inotify
                  } =
  Mviz.Audio.shutdown audioSendChannel
  >> Mviz.UI.shutdown
  >> Mviz.UI.destroyUIContext uiContext
  >> destroyWindow wnd
  >> Mviz.SDL.quit
  >> killINotify inotify
  >> do
    sui <- readIORef showUI
    ports <- selectedPorts settingsWindow
    dumpConfig $ Config { configShowUI = sui
                        , configInputs = ports
                        }
  >> wait audioThread
  >> pure ()

main :: IO ()
main = bracket startup cleanup $ \env -> do
  _ <- runMviz env run
  pure ()
