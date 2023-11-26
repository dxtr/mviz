{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Mviz.UI.LogWindow
  ( LogWindow
  , HasLogWindow (..)
  , MonadLogWindow (..)
  , renderLogWindow
  ) where

-- Example: https://github.com/ocornut/imgui/blob/master/imgui_demo.cpp#L7030
-- Example: https://github.com/ocornut/imgui/blob/master/imgui.cpp#L14630
-- More useful stuff: https://github.com/ocornut/imgui/blob/master/imgui_demo.cpp#L1305

import           Control.Monad        (when)
import           Control.Monad.Reader
import qualified Data.Text            as T
import qualified Data.Vector          as V
import qualified ImGui
import           Language.Haskell.TH
import           Mviz.Logger          (LogMessage (..), MonadLog (..),
                                       logMessage)
import           Mviz.UI.Types        (MonadUI)
import           Mviz.UI.UIWindow     (LogWindow (..))

class HasLogWindow a where
  getLogWindow :: a -> LogWindow

class Monad m => MonadLogWindow m where
  openLogWindow :: T.Text -> m () -> m Bool
  isLogWindowOpen :: m Bool
  setLogWindowOpen :: Bool -> m ()

windowId :: T.Text
windowId = "logwindow"

windowTitle :: T.Text
windowTitle = "Log##" <> windowId

-- Todo: Clear the log buffer
-- clearButtonClicked :: MvizM ()
-- clearButtonClicked = return ()

renderLogLine :: LogMessage -> IO ()
renderLogLine msg@(LogMessage _ loc _ _ _) = do
  _ <- ImGui.selectable (logMessage msg) False []
  renderTooltip loc

renderTooltip :: Loc -> IO ()
renderTooltip (Loc locFilename _ locModule (sLine,_) _) = do
  tooltip <- ImGui.beginItemTooltip
  when tooltip $ do
    ImGui.textUnformatted $ "File: " <> T.pack locFilename <> ":" <> T.pack (show sLine)
    ImGui.textUnformatted $ "Module: " <> T.pack locModule
    ImGui.endTooltip

-- renderLine :: LogMessage -> IO ()
-- renderLine msg = do
--   ImGui.textUnformatted $ logMessage msg

renderListBox :: V.Vector LogMessage -> IO ()
renderListBox logMessages = do
--  listBoxSize <- ImGui.defaultSize
  _ <- ImGui.withListBox listBoxTitle listBoxSize $ mapM_ renderLogLine logMessages
  pure ()
  where
    listBoxTitle = "##log"
    listBoxSize = ImGui.ImVec2 (-1.0) (-1.0)

-- renderLog = do
--   itemData <- lastItemData


-- renderChild :: V.Vector LogMessage -> IO ()
-- renderChild logMessages = do
--   ImGui.withChild "##log" size True [ImGui.WindowFlagAlwaysVerticalScrollbar, ImGui.WindowFlagAlwaysHorizontalScrollbar] $ do
--     ImGui.ListClipper.withClipper messageCount itemHeight $ \clipper ->
--       whileM (ImGui.ListClipper.step clipper) $ do
        -- TODO: Print out the line
        -- TODO: Create an index of lines (See DebugLogIndex in the example)
--        itemData <- lastItemData


--      putStrLn "foo" -- TODO: Add ImGuiLastItemData (https://github.com/ocornut/imgui/blob/f617fe7890f78bc300e7a068a1c2acc9e9ce1e97/imgui_internal.h#L1193C8-L1202)

  -- where size = ImGui.ImVec2 0.0 0.0
  --       messageCount = V.length logMessages
  --       itemHeight = 0.0

renderLogWindow :: (MonadUI m, MonadLogWindow m, MonadLog m) => m ()
renderLogWindow = do
  isOpen <- isLogWindowOpen
  when isOpen $ do
    logBuffer <- getLogVector
    closed <- openLogWindow windowTitle $ do
      _ <- ImGui.button clearButtonTitle
      liftIO $ renderListBox logBuffer
    setLogWindowOpen closed
  where clearButtonTitle = "Clear" :: T.Text
