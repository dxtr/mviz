module ImGui.GL
  ( glInit
  , glShutdown
  , glNewFrame
  , glRenderDrawData
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text              as T
import qualified ImGui.Raw.GL           as Raw
import           ImGui.Types

glInit :: MonadIO m => T.Text -> m Bool
glInit glVersion = liftIO $ Raw.glInit glVersion

glShutdown :: MonadIO m => m ()
glShutdown = liftIO $ Raw.glShutdown

glNewFrame :: MonadIO m => m ()
glNewFrame = liftIO $ Raw.glNewFrame

glRenderDrawData :: MonadIO m => DrawData -> m ()
glRenderDrawData drawData = liftIO $ Raw.glRenderDrawData drawData
