module Mviz.UI.LogWindow (LogWindow, renderLogWindow) where

import DearImGui qualified as ImGUI
import Mviz.Logger (LogMessage)

data LogWindow = LogWindow
  { logWindowBuffer :: [LogMessage]
  , logWindowAutoScroll :: Bool
  }

renderLogWindow :: IO ()
renderLogWindow = return ()
