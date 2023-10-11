{-# LANGUAGE OverloadedStrings #-}

module Mviz.UI.UIWindow
  ( LogWindow(..)
  , SettingsWindow(..)
  , makeLogWindow
  , makeSettingsWindow
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

data SettingsWindow = SettingsWindow
  { settingsWindowOpen       :: IORef Bool }

makeLogWindow :: Bool -> IO LogWindow
makeLogWindow showWindow = do
  selectedLine <- newIORef (-1)
  windowOpen <- newIORef showWindow
  return $ LogWindow{ logWindowInputBuffer = ""
                    , logWindowAutoScroll = True
                    , logWindowSelectedLine = selectedLine
                    , logWindowOpen = windowOpen
                    }

makeSettingsWindow :: Bool -> IO SettingsWindow
makeSettingsWindow showWindow = do
  windowOpen <- newIORef showWindow
  return $ SettingsWindow{ settingsWindowOpen = windowOpen }
