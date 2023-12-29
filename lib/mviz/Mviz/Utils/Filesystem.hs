module Mviz.Utils.Filesystem
  ( ensureDirectory
  ) where

import           UnliftIO           (MonadIO)
import           UnliftIO.Directory (createDirectoryIfMissing)

ensureDirectory :: (MonadIO m) => FilePath -> m ()
ensureDirectory = createDirectoryIfMissing True
