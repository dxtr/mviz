{-# LANGUAGE OverloadedStrings #-}

module Mviz.UI.UIWindow (LogWindow(..)
                        , makeLogWindow) where

import           Data.IORef            (IORef, newIORef)
import qualified Data.Text             as T
import           Foreign.Marshal.Alloc (calloc)

data LogWindow = LogWindow
  { logWindowShow         :: Bool
  , logWindowInputBuffer  :: T.Text
  , logWindowAutoScroll   :: Bool
  , logWindowSelectedLine :: IORef Int
  , logWindowOpen         :: Bool
--  , logWindowScrollToBottom :: Bool
  }

makeLogWindow :: Bool -> IO LogWindow
makeLogWindow showWindow = do
  selectedLine <- newIORef (-1)
  return $ LogWindow{ logWindowShow = showWindow
                    , logWindowInputBuffer = ""
                    , logWindowAutoScroll = True
                    , logWindowSelectedLine = selectedLine
                    , logWindowOpen = True
                    }
