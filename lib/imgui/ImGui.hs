module ImGui ( Raw.Context
             , module ImGui.Types
             , getVersion
             , getDrawData
             , begin
             , beginDefault
             , end
             , withWindow
             , withDefaultWindow
             , button
             , defaultButton
             , selectable
             , selectableDefault
             , beginListBox
             , endListBox
             , withListBox
             , beginGroup
             , endGroup
             , newFrame
             , endFrame
             , createContext
             , createContextDefault
             , destroyContext
             ) where

import           Control.Exception       (bracket)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import qualified Data.Text               as T
import           Foreign.Ptr             (nullPtr)
import qualified ImGui.Raw               as Raw
import           ImGui.Types

getVersion :: MonadIO m => m T.Text
getVersion = liftIO $ T.pack <$> Raw.getVersion

getDrawData :: MonadIO m => m DrawData
getDrawData = liftIO $ Raw.getDrawData

-- Context
createContext :: MonadIO m => m Context
createContext = liftIO $ Raw.createContext

createContextDefault :: MonadIO m => m Context
createContextDefault = liftIO $ Raw.createContext nulLptr

destroyContext :: MonadIO m => Context -> m ()
destroyContext context = liftIO $ Raw.destroyContext context

-- Frames
newFrame :: MonadIO m => m ()
newFrame = liftIO $ Raw.newFrame

endFrame :: MonadIO m => m ()
endFrame = liftIO $ Raw.newFrame

-- Windows
begin :: MonadIO m => T.Text -> [WindowFlag] -> m Bool
begin name flags = liftIO $ Raw.begin name flags

beginDefault :: MonadIO m => T.Text -> m Bool
beginDefault name = begin name flags
  where flags = []

end :: MonadIO m => m ()
end = liftIO $ Raw.end

withWindow :: MonadUnliftIO m => T.Text -> [WindowFlag] -> (Bool -> m a) -> m a
withWindow label flags func =
  withRunInIO $ \runInIO ->
                  bracket (begin label flags) (\_ -> end) (\b -> runInIO (func b))

withDefaultWindow :: MonadUnliftIO m => T.Text -> (Bool -> m a) -> m a
withDefaultWindow label func = withWindow label flags func
  where flags = []

-- Misc
beginGroup :: MonadIO m => m ()
beginGroup = liftIO $ Raw.beginGroup

endGroup :: MonadIO m => m ()
endGroup = liftIO $ Raw.endGroup

-- Buttons
button :: MonadIO m => T.Text -> Vec2 -> m Bool
button label size = liftIO $ Raw.button label size

defaultButton :: MonadIO m => T.Text -> m Bool
defaultButton label = button label size
  where size = Vec2 0.0 0.0

-- Selectable
selectable :: MonadIO m => T.Text -> Bool -> [SelectableFlag] -> Vec2 -> m Bool
selectable label selected flags size = liftIO $ Raw.selectable label selected flags size

selectableDefault :: MonadIO m => T.Text -> Bool -> m Bool
selectableDefault label selected = selectable label selected flags size
  where flags = []
        size = Vec2 0.0 0.0

-- Listbox
beginListBox :: MonadIO m => T.Text -> Vec2 -> m Bool
beginListBox label size = liftIO $ Raw.beginListBox label size

endListBox :: MonadIO m => m ()
endListBox = liftIO $ Raw.endListBox

withListBox :: MonadUnliftIO m => T.Text -> Vec2 -> (Bool -> m a) -> m a
withListBox label size func =
  withRunInIO $ \runInIO ->
                  bracket (beginListBox label size) (\_ -> endListBox) (\b -> runInIO (func b))
