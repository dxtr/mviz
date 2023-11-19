{-# LANGUAGE BlockArguments #-}
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
             , collapsingHeader
             , withCollapsingHeader
             , checkbox
             , Raw.sameLine
             , withGroup
             , Raw.contentRegionAvail
             , Raw.calcTextSize
             , Raw.itemSpacing
             , Raw.itemInnerSpacing
             ) where

import           Control.Monad           (when)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Data.Bool               (bool)
import           Data.Functor            ((<&>))
import qualified Data.Text               as T
import qualified Data.Text.Foreign       as TF
import           Foreign                 (peek, with)
import           Foreign.C.String        (peekCString)
import           ImGui.Enums             (TreeNodeFlag, WindowFlag (..))
import qualified ImGui.Raw               as Raw
import           ImGui.Raw.Structs       (ImVec2 (..), ImVec3 (..))
import           UnliftIO                (bracket, bracket_)

type Context = Raw.Context

getVersion :: MonadIO m => m T.Text
getVersion = liftIO $ (Raw.getVersion >>= peekCString) <&> T.pack

fltMin :: MonadIO m => m Float
fltMin = realToFrac <$> Raw.fltMin

defaultSize :: (MonadIO m) => m ImVec2
defaultSize = ImVec2 <$> fltMin <*> fltMin

withWindow :: MonadUnliftIO m => T.Text -> [WindowFlag] -> m () -> m ()
withWindow label flags func =
  bracket (Raw.begin label flags)
          (const Raw.end)
          (`when` func)

withCloseableWindow :: MonadUnliftIO m => T.Text -> [WindowFlag] -> m a -> m (Bool, Maybe a)
withCloseableWindow label flags func =
  bracket (Raw.beginCloseable label flags)
          (const Raw.end)
          \(notCollapsed, pOpen) -> (pOpen, ) . flip (bool Nothing) notCollapsed . Just <$> func

withChild :: MonadUnliftIO m => T.Text -> ImVec2 -> Bool -> [WindowFlag] -> m () -> m ()
withChild label size border flags func =
  bracket (Raw.beginChild label size border flags)
          (`when` Raw.endChild)
          (`when` func)

-- Listbox
beginListBox :: MonadIO m => T.Text -> ImVec2 -> m Bool
beginListBox label size = liftIO $
  TF.withCString label $ \labelPtr ->
                           Raw.beginListBox labelPtr size

endListBox :: MonadIO m => m ()
endListBox = Raw.endListBox

withListBox :: MonadUnliftIO m => T.Text -> ImVec2 -> m a -> m (Maybe a)
withListBox label size func =
  bracket (beginListBox label size)
          (`when` endListBox)
          runFunc
  where runFunc False = pure Nothing
        runFunc True  = Just <$> func

-- Text
textUnformatted :: MonadIO m => T.Text -> m ()
textUnformatted text = liftIO $ TF.withCString text Raw.textUnformatted

-- Trees
treeNode :: MonadIO m => T.Text -> m Bool
treeNode label = liftIO $ TF.withCString label Raw.treeNode

-- Collapsing header
collapsingHeader :: MonadIO m => T.Text -> [TreeNodeFlag] -> m Bool
collapsingHeader label flags = liftIO $ TF.withCString label (`Raw.collapsingHeader` flags)

withCollapsingHeader :: MonadUnliftIO m => T.Text -> [TreeNodeFlag] -> (Bool -> m a) -> m a
withCollapsingHeader label flags =
    bracket (collapsingHeader label flags)
            (`when` pure ())

-- Checkbox
checkbox :: (MonadIO m) => T.Text -> Bool -> m (Bool, Bool)
checkbox label selected = liftIO $
  with (bool 0 1 selected) \boolPtr -> do
    changed <- TF.withCString label (`Raw.checkbox` boolPtr)
    peek boolPtr >>= \newValue -> return (changed, newValue == 1)

-- Groups
withGroup :: MonadUnliftIO m => m a -> m a
withGroup = bracket_ Raw.beginGroup Raw.endGroup
