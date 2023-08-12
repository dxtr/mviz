module Mviz.UI.LogWindow (LogWindow, renderLogWindow) where

-- Example: https://github.com/ocornut/imgui/blob/master/imgui_demo.cpp#L7030
-- More useful stuff: https://github.com/ocornut/imgui/blob/master/imgui_demo.cpp#L1305

import qualified Data.Text   as T
import qualified DearImGui   as ImGUI
import           Mviz.Logger (LogMessage)

data LogWindow = LogWindow
  { logWindowBuffer         :: [LogMessage]
  , logWindowInputBuffer    :: T.Text
  , logWindowAutoScroll     :: Bool
  , logWindowScrollToBottom :: Bool
  }

addLog :: LogWindow -> [LogMessage] -> LogWindow
addLog logWindow logMessages = logWindow{logWindowBuffer = logMessages}


renderLogWindow :: LogWindow -> IO ()
renderLogWindow logWindow = do

