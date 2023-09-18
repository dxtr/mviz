{-# LANGUAGE OverloadedStrings #-}

module Mviz.UI.UIWindow (LogWindow(..)
                        , makeLogWindow) where

import           Data.IORef            (IORef, newIORef)
import qualified Data.Text             as T
import           Foreign.Marshal.Alloc (calloc)

data LogWindow = LogWindow
  { logWindowInputBuffer  :: T.Text
  , logWindowAutoScroll   :: Bool
  , logWindowSelectedLine :: IORef Int
  , logWindowOpen         :: IORef Bool
--  , logWindowScrollToBottom :: Bool
  }

makeLogWindow :: Bool -> IO LogWindow
makeLogWindow showWindow = do
  selectedLine <- newIORef (-1)
  windowOpen <- newIORef showWindow
  return $ LogWindow{ logWindowInputBuffer = ""
                    , logWindowAutoScroll = True
                    , logWindowSelectedLine = selectedLine
                    , logWindowOpen = windowOpen
                    }
