{-# LANGUAGE OverloadedStrings #-}

module Mviz.UI.UIWindow
  ( LogWindow(..)
  , makeLogWindow
  , AudioWindow(..)
  ) where

import           Data.IORef (IORef, newIORef)
import qualified Data.Text  as T

data LogWindow = LogWindow
  { logWindowInputBuffer  :: T.Text
  , logWindowAutoScroll   :: Bool
  , logWindowSelectedLine :: IORef Int
  , logWindowOpen         :: IORef Bool
--  , logWindowScrollToBottom :: Bool
  }

data AudioWindow = AudioWindow
  { audioWindowPorts      :: [T.Text]
  , audioWindowBufferSize :: Int
  , audioWindowSampleRate :: Int
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
