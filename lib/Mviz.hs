{-# LANGUAGE OverloadedStrings #-}

module Mviz where

import Control.Exception (bracket)
import Mviz.SDL qualified
import Mviz.SDL qualified as SDL
import Mviz.Window (ShowWindow)

run :: (ShowWindow w) => w -> IO ()
run wnd = do
  putStrLn "Running with window!"
  SDL.eventLoop wnd

main :: IO ()
main = do
  -- Create window
  bracket
    ( do
        Mviz.SDL.initialize
        Mviz.SDL.createWindow "mviz" True
    )
    ( \wnd ->
        Mviz.SDL.destroyWindow wnd
          >> Mviz.SDL.quit
    )
    run
