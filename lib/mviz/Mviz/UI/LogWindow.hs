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
import qualified ImGui
import           Mviz.Logger                (LogMessage)
import           Mviz.Types                 (MvizM)
import           Mviz.UI.UIWindow           (LogWindow)

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
    _ <- ImGui.selectableDefault "Test" False
    return ()
  where
    listBoxTitle = "Log"
    listBoxSize = ImGui.Vec2 1.0 1.0

renderLogWindow :: MvizM ()
renderLogWindow = do
  state <- State.get
  liftIO $ ImGui.withDefaultWindow windowTitle $ do
    _ <- ImGui.defaultButton clearButtonTitle
    _ <- renderListBox
    return ()
  State.put state
  -- liftIO $ do
  --   begin <- ImGUI.begin windowTitle
  --   when begin $ do
  --     -- TODO: Handle the clear button
  --     return ()


  --   ImGUI.end
  -- put startTextInput
--    State.put state
  where clearButtonTitle = "Clear" :: T.Text
