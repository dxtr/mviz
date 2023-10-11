{-# LANGUAGE OverloadedStrings #-}

module Mviz.UI.SettingsWindow
  ( HasSettingsWindow (..)
  , MonadSettingsWindow (..)
  , renderSettingsWindow
  ) where

import           Control.Monad    (when)
import qualified Data.Text        as T
import           Mviz.Audio.Types (MonadJack (bufferSize, ports, sampleRate))
import           Mviz.UI.Types    (MonadUI)
import           Mviz.UI.UIWindow (SettingsWindow)

class HasSettingsWindow a where
    getSettingsWindow :: a -> SettingsWindow

class Monad m => MonadSettingsWindow m where
    openSettingsWindow :: T.Text -> m () -> m Bool
    isSettingsWindowOpen :: m Bool
    setSettingsWindowOpen :: Bool -> m ()

windowId :: T.Text
windowId = "settingswindow"

windowTitle :: T.Text
windowTitle = "Settings##" <> windowId

renderSettingsWindow :: (MonadUI m, MonadSettingsWindow m, MonadJack m) => m ()
renderSettingsWindow = do
    isOpen <- isSettingsWindowOpen
    when isOpen $ do
        sr <- sampleRate
        bs <- bufferSize
        p <- ports
        closed <- openSettingsWindow windowTitle $ do
            return ()
        setSettingsWindowOpen closed
