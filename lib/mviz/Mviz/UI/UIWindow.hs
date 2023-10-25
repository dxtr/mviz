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
  { logWindowInputBuffer  :: !T.Text
  , logWindowAutoScroll   :: !Bool
  , logWindowSelectedLine :: IORef Int
  , logWindowOpen         :: IORef Bool
--  , logWindowScrollToBottom :: Bool
  }

data SettingsWindow = SettingsWindow
  { settingsWindowOpen      :: IORef Bool
  , settingsSelectedInput   :: IORef (Maybe T.Text)
  , settingsCheckedChannels :: IORef [T.Text]
  }

makeLogWindow :: Bool -> IO LogWindow
makeLogWindow showWindow = do
  selectedLine <- newIORef (-1)
  windowOpen <- newIORef showWindow
  pure $ LogWindow { logWindowInputBuffer = ""
                   , logWindowAutoScroll = True
                   , logWindowSelectedLine = selectedLine
                   , logWindowOpen = windowOpen
                   }

-- TODO: Should be able to initialize this from
-- a configuration file for example.
makeSettingsWindow :: Bool -> IO SettingsWindow
makeSettingsWindow showWindow = do
  windowOpen <- newIORef showWindow
  selectedInput <- newIORef Nothing
  checkedChannels <- newIORef []
  pure $ SettingsWindow { settingsWindowOpen = windowOpen
                        , settingsSelectedInput = selectedInput
                        , settingsCheckedChannels = checkedChannels
                        }
