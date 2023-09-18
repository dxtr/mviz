module Mviz.UI.Types
  ( UIContext
  , HasUIContext (..)
  , HasUI (..)
  , MonadUI (..)
  ) where

-- import DearImGui qualified as ImGUI
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef             (IORef)
import qualified Data.Text              as T
import qualified ImGui

type UIContext = ImGui.Context

class HasUIContext a where
  getUIContext :: a -> UIContext

class HasUI a where
  getUIShownRef :: a -> IORef Bool

class MonadIO m => MonadUI m where
  isUIShown :: m Bool
  renderUI :: m ()
