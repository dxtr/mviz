{-# LANGUAGE OverloadedStrings #-}

module Mviz where

import Control.Concurrent (newEmptyMVar)
import Control.Concurrent.Async (asyncBound, wait)
import Control.Concurrent.STM (newTChanIO)
import Control.Exception (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (get)
import Data.Map.Strict qualified as Map
import Graphics.Rendering.OpenGL qualified as OpenGL
import Mviz.Audio qualified
import Mviz.SDL qualified
import Mviz.Types (MvizM (..), MvizState (..), runMviz)
import Mviz.UI qualified
import Mviz.Window (showWindow, swapWindowBuffers)
import SDL qualified

mainLoop :: MvizM ()
mainLoop = do
  state <- get
  events <- liftIO $ Mviz.UI.collectEvents
  let doQuit = elem SDL.QuitEvent $ map SDL.eventPayload events
  OpenGL.clearColor OpenGL.$= (OpenGL.Color4 0 0 0 1)

  -- Render the UI
  liftIO $ Mviz.UI.newFrame
  liftIO $ Mviz.UI.showDemoWindow
  liftIO $ OpenGL.clear [OpenGL.ColorBuffer]
  liftIO $ Mviz.UI.render

  let wnd = mvizWindow state
  liftIO $ swapWindowBuffers wnd
  unless doQuit mainLoop
 where

run :: MvizM ()
run = do
  state <- get
  liftIO $ putStrLn "Running with window!"
  liftIO $ showWindow $ mvizWindow state
  mainLoop

main :: IO ()
main = do
  -- Create window
  bracket
    -- Initialize everything
    ( do
        Mviz.SDL.initialize
        wnd <- Mviz.SDL.createWindow "mviz" True
        uiContext <- Mviz.UI.createUIContext wnd
        _ <- Mviz.UI.initialize wnd
        audioSendChannel <- newTChanIO
        audioRecvChannel <- newTChanIO
        audioRet <- newEmptyMVar
        audioThread <-
          asyncBound $ Mviz.Audio.runAudioSystem audioRecvChannel audioSendChannel
        return $
          MvizState
            { mvizWindow = wnd
            , mvizUIContext = uiContext
            , mvizShaders = Map.empty
            , mvizAudioThread = audioThread
            , mvizAudioSendChannel = audioSendChannel
            , mvizAudioRecvChannel = audioRecvChannel
            , mvizAudioReturn = audioRet
            }
            -- return $ (wnd, uiCtx)
    )
    -- Release resources
    ( \_state@MvizState
        { mvizWindow = wnd
        , mvizUIContext = uiContext
        , mvizAudioThread = audioThread
        , mvizAudioSendChannel = audioSendChannel
        } ->
          Mviz.Audio.shutdown audioSendChannel
            >> Mviz.UI.shutdown
            >> Mviz.UI.destroyUIContext uiContext
            >> Mviz.SDL.destroyWindow wnd
            >> Mviz.SDL.quit
            >> wait audioThread
            >> return ()
    )
    -- Run
    ( \state -> do
        _ <- runMviz state run
        return ()
    )
