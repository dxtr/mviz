{-# LANGUAGE OverloadedStrings #-}

module Mviz.UI.LogWindow (LogWindow, renderLogWindow) where

-- Example: https://github.com/ocornut/imgui/blob/master/imgui_demo.cpp#L7030
-- More useful stuff: https://github.com/ocornut/imgui/blob/master/imgui_demo.cpp#L1305

import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Control.Monad.State.Strict as State
import           Control.Monad.Trans.Maybe
import           Data.StateVar              (get)
import qualified Data.Text                  as T
import           Foreign.Ptr                (Ptr)
import qualified ImGui
import           Mviz.Logger                (LogMessage)
import           Mviz.Types                 (MvizM (..), mvizLogWindow)
import           Mviz.UI.UIWindow           (LogWindow (..))

windowId :: T.Text
windowId = "logwindow"

windowTitle :: T.Text
windowTitle = "Log##" <> windowId

-- Todo: Clear the log buffer
clearButtonClicked :: MvizM ()
clearButtonClicked = return ()

-- TODO: This should take the log messages as input
-- withListBox :: MonadUnliftIO m => Text -> ImVec2 -> (Bool -> m a) -> m a
-- selectable :: MonadIO m => Text -> m Bool
renderListBox :: IO ()
renderListBox = do
  ImGui.withListBox listBoxTitle listBoxSize $ do
    _ <- ImGui.selectable "Test" False [] (ImGui.ImVec2 0.0 0.0)
    return ()
  where
    listBoxTitle = "Log"
    listBoxSize = ImGui.ImVec2 1.0 1.0

renderLogWindow :: MvizM ()
renderLogWindow = do
  state <- State.get
  let logWindow = mvizLogWindow state
  let isOpen = logWindowOpen logWindow
  when isOpen $ do
    closed <- liftIO $ ImGui.withCloseableWindow windowTitle [] $ do
      _ <- ImGui.button clearButtonTitle
      renderListBox
    State.put state{mvizLogWindow = logWindow{logWindowOpen = closed}}
  where clearButtonTitle = "Clear" :: T.Text
