{-# LANGUAGE OverloadedStrings #-}

module Mviz where

import Control.Exception (bracket)
import Control.Monad (unless)
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState, get, runStateT)
import Data.Map.Strict qualified as Map
import Graphics.Rendering.OpenGL qualified as OpenGL
import Mviz.SDL qualified
import Mviz.Types (MvizError (..), MvizM (..), MvizState (..), runMviz)
import Mviz.UI qualified
import Mviz.Window (DrawWindow, ShowWindow, showWindow, swapWindowBuffers)
import SDL qualified

eventLoop :: (DrawWindow w) => w -> IO ()
eventLoop wnd = do
  events <- collectEvents
  let doQuit = elem SDL.QuitEvent $ map SDL.eventPayload events
  OpenGL.clearColor OpenGL.$= (OpenGL.Color4 0 0 0 1)
  OpenGL.clear [OpenGL.ColorBuffer]

  swapWindowBuffers wnd
  unless doQuit (eventLoop wnd)
 where
  collectEvents :: IO [SDL.Event]
  collectEvents = do
    e <- SDL.pollEvent
    case e of
      Nothing -> return []
      Just e' -> (e' :) <$> collectEvents

run2 :: MvizM ()
run2 = do
  state <- get
  liftIO $ putStrLn "Running with window!"
  liftIO $ showWindow $ mvizWindow state
  liftIO $ eventLoop $ mvizWindow state

run :: MvizM ()
run = do
  state <- get
  liftIO $ putStrLn "Running with window!"
  liftIO $ showWindow $ mvizWindow state
  liftIO $ eventLoop $ mvizWindow state

main :: IO ()
main = do
  -- Create window
  bracket
    -- Initialize everything
    ( do
        Mviz.SDL.initialize
        wnd <- Mviz.SDL.createWindow "mviz" True
        uiCtx <- Mviz.UI.createUIContext wnd
        _ <- Mviz.UI.initialize wnd
        return $ (wnd, uiCtx)
    )
    -- Release resources
    ( \(wnd, uiContext) ->
        Mviz.UI.shutdown
          >> Mviz.UI.destroyUIContext uiContext
          >> Mviz.SDL.destroyWindow wnd
          >> Mviz.SDL.quit
    )
    -- Run
    ( \(wnd, uiContext) -> do
        _ <- runMviz (MvizState{mvizWindow = wnd, mvizUiContext = uiContext, mvizShaders = Map.empty}) run
        return ()
    )
