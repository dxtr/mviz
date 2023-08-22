{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Mviz.UI.LogWindow (LogWindow, renderLogWindow) where

-- Example: https://github.com/ocornut/imgui/blob/master/imgui_demo.cpp#L7030
-- More useful stuff: https://github.com/ocornut/imgui/blob/master/imgui_demo.cpp#L1305

import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Control.Monad.State.Strict as State
import           Control.Monad.Trans.Maybe
import           Data.StateVar              (get)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Foreign.Ptr                (Ptr)
import qualified ImGui
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Mviz.Logger                (LogMessage (..), logMessage)
import           Mviz.Types                 (MvizM (..), mvizLog, mvizLogWindow)
import           Mviz.UI.UIWindow           (LogWindow (..))
import           Mviz.Utils.Ringbuffer      (toVector)

windowId :: T.Text
windowId = "logwindow"

windowTitle :: T.Text
windowTitle = "Log##" <> windowId

-- Todo: Clear the log buffer
clearButtonClicked :: MvizM ()
clearButtonClicked = return ()

renderLogLine :: LogMessage -> IO ()
renderLogLine msg@(LogMessage _ loc _ _ _) = do
  _ <- ImGui.selectable (logMessage msg) False []
  renderTooltip loc

renderTooltip :: Loc -> IO ()
renderTooltip (Loc locFilename _ locModule (sLine,_) _) = do
  tooltip <- ImGui.beginItemTooltip
  when tooltip $ do
    ImGui.textUnformatted $ "File: " <> (T.pack locFilename) <> ":" <> (T.pack $ show sLine)
    ImGui.textUnformatted $ "Module: " <> (T.pack locModule)
    ImGui.endTooltip

renderListBox :: V.Vector LogMessage -> IO ()
renderListBox logMessages = do
--  listBoxSize <- ImGui.defaultSize
  ImGui.withListBox listBoxTitle listBoxSize $ do
    _ <- mapM_ renderLogLine logMessages
    -- _ <- ImGui.selectable "Test" False []
    return ()
  where
    listBoxTitle = "##log"
    listBoxSize = ImGui.ImVec2 (-1.0) (-1.0)

renderLogWindow :: MvizM ()
renderLogWindow = do
  state <- State.get
  let logWindow = mvizLogWindow state
  let isOpen = logWindowOpen logWindow
  let logBuffer = mvizLog state
  logBufferVec <- toVector logBuffer
  when isOpen $ do
    closed <- liftIO $ ImGui.withCloseableWindow windowTitle [] $ do
      _ <- ImGui.button clearButtonTitle
      renderListBox logBufferVec
    State.put state{mvizLogWindow = logWindow{logWindowOpen = closed}}
  where clearButtonTitle = "Clear" :: T.Text
