{-# LANGUAGE OverloadedStrings #-}

module Mviz.UI.UIWindow
  ( LogWindow(..)
  , SettingsWindow(..)
  , makeLogWindow
  , makeSettingsWindow
  ) where

import           Data.IORef        (IORef, newIORef)
import qualified Data.Text         as T
import           Mviz.Utils.Inputs (splitInputs)

data LogWindow = LogWindow
  { logWindowInputBuffer  :: !T.Text
  , logWindowAutoScroll   :: !Bool
  , logWindowSelectedLine :: !(IORef Int)
  , logWindowOpen         :: !(IORef Bool)
--  , logWindowScrollToBottom :: Bool
  }

data SettingsWindow = SettingsWindow
  { settingsWindowOpen      :: !Bool
  , settingsSelectedInput   :: !(Maybe T.Text)
  , settingsCheckedChannels :: ![T.Text]
  , settingsChanged         :: !Bool
  , settingsShader          :: !(Maybe T.Text)
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

makeSettingsWindow :: Bool -> [T.Text] -> IO (IORef SettingsWindow)
makeSettingsWindow showWindow selectedInputs = do
  let selectedInput = fst <$> inputs
      checkedChannels = maybe [] snd inputs
  newIORef $ SettingsWindow { settingsWindowOpen = showWindow
                            , settingsSelectedInput = selectedInput
                            , settingsCheckedChannels = checkedChannels
                            , settingsChanged = False
                            , settingsShader = Nothing
                            }
  where inputs = splitInputs selectedInputs
