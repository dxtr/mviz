module ImGui ( Context
             , ImVec2(..)
             , ImVec3 (..)
             , getVersion
             , checkVersion
             , getDrawData
             , begin
             , end
             , withWindow
             , withCloseableWindow
             , button
             , selectable
             , beginListBox
             , endListBox
             , withListBox
             , beginGroup
             , endGroup
             , newFrame
             , endFrame
             , createContext
             , destroyContext
             , getCurrentContext
             , render
             , showMetricsWindow
             , showUserGuide
             , showDemoWindow
             , styleColorsDark
             ) where

import           Control.Exception       (bracket, bracket_)
import           Control.Monad           (unless, when)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import qualified Data.Text               as T
import qualified Data.Text.Foreign       as TF
import           Foreign                 (Ptr, fromBool, with)
import           Foreign.C.String        (peekCString)
import           ImGui.Enums
import qualified ImGui.Raw               as Raw
import           ImGui.Structs
import           ImGui.Types

type Context = Raw.Context

getVersion :: MonadIO m => m T.Text
getVersion = liftIO $ do
  ver <- Raw.getVersion
  verStr <- peekCString ver
  return $ T.pack verStr

checkVersion :: MonadIO m => m ()
checkVersion = Raw.checkVersion

getDrawData :: MonadIO m => m DrawData
getDrawData = Raw.getDrawData

render :: MonadIO m => m ()
render = Raw.render

showUserGuide :: MonadIO m => m ()
showUserGuide = Raw.showUserGuide

showDemoWindow :: MonadIO m => m Bool
showDemoWindow = Raw.showDemoWindow

showMetricsWindow :: MonadIO m => m Bool
showMetricsWindow = Raw.showMetricsWindow

-- Style
styleColorsDark :: MonadIO m => m ()
styleColorsDark = Raw.styleColorsDark

-- Context
createContext :: MonadIO m => m Raw.Context
createContext = Raw.createContext

destroyContext :: MonadIO m => Raw.Context -> m ()
destroyContext context = liftIO $ Raw.destroyContext context

getCurrentContext :: MonadIO m => m Raw.Context
getCurrentContext = Raw.getCurrentContext

-- Frames
newFrame :: MonadIO m => m ()
newFrame = Raw.newFrame

endFrame :: MonadIO m => m ()
endFrame = Raw.newFrame

-- Windows
begin :: MonadIO m => T.Text -> [WindowFlag] -> m Bool
begin label flags = liftIO $
  TF.withCString label $ \labelPtr ->
    Raw.begin labelPtr flags

beginCloseable :: MonadIO m => T.Text -> [WindowFlag] -> m (Bool, Bool)
beginCloseable label flags = liftIO $
  TF.withCString label $ \labelPtr ->
    Raw.beginCloseable labelPtr flags

end :: MonadIO m => m ()
end = Raw.end

withWindow :: MonadUnliftIO m => T.Text -> [WindowFlag] -> m () -> m ()
withWindow label flags func =
  withRunInIO $ \runInIO ->
                  bracket
                  (begin label flags)
                  (const end)
                  (`when` runInIO func)

withCloseableWindow :: MonadUnliftIO m => T.Text -> [WindowFlag] -> m () -> m Bool
withCloseableWindow label flags func =
  withRunInIO $ \runInIO ->
                  bracket
                  (beginCloseable label flags)
                  (const end)
                  (\(notCollapsed, pOpen) -> do
                      when notCollapsed $ runInIO func
                      return pOpen)

-- Misc
beginGroup :: MonadIO m => m ()
beginGroup = Raw.beginGroup

endGroup :: MonadIO m => m ()
endGroup = Raw.endGroup

-- Buttons
button :: MonadIO m => T.Text -> m Bool
button label = liftIO $
  TF.withCString label $ \labelPtr ->
      Raw.button labelPtr

-- Selectable
selectable :: MonadIO m => T.Text -> Bool -> [SelectableFlag] -> ImVec2 -> m Bool
selectable label selected flags size = liftIO $
  TF.withCString label $ \labelPtr ->
    with size $ \sizePtr ->
      Raw.selectable labelPtr (fromBool selected) flags sizePtr

-- Listbox
beginListBox :: MonadIO m => T.Text -> ImVec2 -> m Bool
beginListBox label size = liftIO $
  TF.withCString label $ \labelPtr ->
    with size $ \sizePtr ->
      Raw.beginListBox labelPtr sizePtr

endListBox :: MonadIO m => m ()
endListBox = Raw.endListBox

withListBox :: MonadUnliftIO m => T.Text -> ImVec2 -> m () -> m ()
withListBox label size func =
  withRunInIO $ \runInIO ->
                  bracket
                  (beginListBox label size)
                  (`when` endListBox)
                  (`when` runInIO func)
