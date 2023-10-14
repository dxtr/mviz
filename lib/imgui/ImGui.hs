module ImGui ( Context
             , ImVec2(..)
             , ImVec3 (..)
             , WindowFlag (..)
             , fltMin
             , defaultSize
             , getVersion
             , Raw.checkVersion
             , Raw.getDrawData
             , Raw.begin
             , Raw.beginChild
             , Raw.end
             , withWindow
             , withCloseableWindow
             , withChild
             , Raw.button
             , Raw.selectable
             , beginListBox
             , endListBox
             , withListBox
             , Raw.beginGroup
             , Raw.endGroup
             , Raw.newFrame
             , Raw.endFrame
             , Raw.createContext
             , Raw.destroyContext
             , Raw.getCurrentContext
             , Raw.render
             , Raw.showMetricsWindow
             , Raw.showUserGuide
             , Raw.showDemoWindow
             , Raw.styleColorsDark
             , textUnformatted
             , Raw.beginTooltip
             , Raw.beginItemTooltip
             , Raw.endTooltip
             , Raw.isItemHovered
             , treeNode
             , Raw.treePop
             ) where

import           Control.Exception       (bracket)
import           Control.Monad           (when)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import qualified Data.Text               as T
import qualified Data.Text.Foreign       as TF
import           Foreign.C.String        (peekCString)
import           ImGui.Enums
import qualified ImGui.Raw               as Raw
import           ImGui.Structs

type Context = Raw.Context

getVersion :: MonadIO m => m T.Text
getVersion = liftIO $ do
  ver <- Raw.getVersion
  verStr <- peekCString ver
  return $ T.pack verStr

fltMin :: MonadIO m => m Float
fltMin = realToFrac <$> Raw.fltMin

defaultSize :: (MonadIO m) => m ImVec2
defaultSize = ImVec2 <$> fltMin <*> fltMin

withWindow :: MonadUnliftIO m => T.Text -> [WindowFlag] -> m () -> m ()
withWindow label flags func =
  withRunInIO $ \runInIO ->
                  bracket
                  (Raw.begin label flags)
                  (const Raw.end)
                  (`when` runInIO func)

withCloseableWindow :: MonadUnliftIO m => T.Text -> [WindowFlag] -> m () -> m Bool
withCloseableWindow label flags func =
  withRunInIO $ \runInIO ->
                  bracket
                  (Raw.beginCloseable label flags)
                  (const Raw.end)
                  (\(notCollapsed, pOpen) -> do
                      when notCollapsed $ runInIO func
                      return pOpen)

withChild :: MonadUnliftIO m => T.Text -> ImVec2 -> Bool -> [WindowFlag] -> m () -> m ()
withChild label size border flags func =
  withRunInIO $ \runInIO ->
                  bracket
                  (Raw.beginChild label size border flags)
                  (`when` Raw.endChild)
                  (`when` runInIO func)

-- Listbox
beginListBox :: MonadIO m => T.Text -> ImVec2 -> m Bool
beginListBox label size = liftIO $
  TF.withCString label $ \labelPtr ->
                           Raw.beginListBox labelPtr size

endListBox :: MonadIO m => m ()
endListBox = Raw.endListBox

withListBox :: MonadUnliftIO m => T.Text -> ImVec2 -> m () -> m ()
withListBox label size func =
  withRunInIO $ \runInIO ->
                  bracket
                  (beginListBox label size)
                  (`when` endListBox)
                  (`when` runInIO func)

-- Text
textUnformatted :: MonadIO m => T.Text -> m ()
textUnformatted text = liftIO $ TF.withCString text Raw.textUnformatted

-- Trees
treeNode :: MonadIO m => T.Text -> m Bool
treeNode label = liftIO $ TF.withCString label Raw.treeNode

