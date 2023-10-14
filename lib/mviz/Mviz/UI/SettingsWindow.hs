{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}

module Mviz.UI.SettingsWindow
  ( HasSettingsWindow (..)
  , MonadSettingsWindow (..)
  , renderSettingsWindow
  ) where

import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import           ImGui                  (beginGroup, beginListBox, defaultSize,
                                         endGroup, endListBox, selectable,
                                         textUnformatted)
import           Mviz.UI.Types          (MonadUI)
import           Mviz.UI.UIWindow       (SettingsWindow)

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

renderStaticText :: Int -> Int -> IO ()
renderStaticText sampleRate bufferSize = do
    beginGroup
    _ <- textUnformatted $ "Sample rate: " <> T.pack (show sampleRate)
    _ <- textUnformatted $ "Buffer size: " <> T.pack (show bufferSize)
    endGroup

renderSettingsWindow :: ( MonadUI m
                        , MonadSettingsWindow m
                        ) => Int -> Int -> [T.Text] -> m ()
renderSettingsWindow sampleRate bufferSize ports = do
    isOpen <- isSettingsWindowOpen
    when isOpen $ do
        closed <- openSettingsWindow windowTitle $ do
            -- TODO: Return the selected ports
            _ <- liftIO $ renderStaticText sampleRate bufferSize
                >> (beginListBox "Ports" =<< defaultSize)
                >> mapM (\p -> selectable p False []) ports
                >> endListBox
            return ()
        setSettingsWindowOpen closed
