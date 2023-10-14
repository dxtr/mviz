module Mviz.UI.ConsoleWindow
  ( ConsoleWindow (..)
  , renderConsoleWindow
  ) where

-- Example: https://github.com/ocornut/imgui/blob/master/imgui_demo.cpp#L6665

import qualified Data.Text   as T
import qualified DearImGui   as ImGUI
import           Mviz.Logger (LogMessage)

data ConsoleWindow = ConsoleWindow
  { consoleWindowBuffer         :: [LogMessage]
  , consoleWindowInputBuffer    :: T.Text
  , consoleWindowAutoScroll     :: Bool
  , consoleWindowScrollToBottom :: Bool
  }

renderConsoleWindow :: IO ()
renderConsoleWindow = pure ()
