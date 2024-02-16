module Mviz.Watch
  ( HasINotify (..)
  , MonadWatch (..)
  ) where

import qualified System.INotify as IN
import           UnliftIO.IORef (IORef)

class HasINotify m where
  getINotify :: m -> IN.INotify
  getWatches :: m -> IO [IN.WatchDescriptor]
  getWatchesRef :: m -> IORef [IN.WatchDescriptor]

class Monad m => MonadWatch m where
  addWatch :: FilePath -> m IN.WatchDescriptor
  setWatches :: [IN.WatchDescriptor] -> m ()
  removeWatch :: IN.WatchDescriptor -> m ()
  removeAllWatches :: m ()
