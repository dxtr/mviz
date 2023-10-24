{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}

module Mviz.UI.SettingsWindow
  ( HasSettingsWindow (..)
  , MonadSettingsWindow (..)
  , renderSettingsWindow
  ) where

import           Control.Monad          (void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text              as T
import           ImGui                  (ImVec2 (ImVec2), beginGroup, endGroup,
                                         sameLine, selectable, textUnformatted,
                                         withCollapsingHeader, withGroup,
                                         withListBox)
import           Mviz.UI.Types          (MonadUI)
import           Mviz.UI.UIWindow       (SettingsWindow)
import           UnliftIO               (MonadUnliftIO)

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

renderStaticText :: MonadIO m => Word -> Word -> m ()
renderStaticText sampleRate bufferSize = do
    beginGroup
    _ <- textUnformatted $ "Sample rate: " <> T.pack (show sampleRate) <> " Hz"
    _ <- textUnformatted $ "Buffer size: " <> T.pack (show bufferSize)
    endGroup

renderPortBox :: MonadUnliftIO m => [T.Text] -> m ()
renderPortBox ports = do
    withCollapsingHeader "Ports" [] $ do
        let size = ImVec2 0.0 0.0
        withGroup $ do
            withListBox "Inputs" size $ do
                _ <- mapM (\p -> selectable p False []) ports
                pure ()
        sameLine
        withGroup $ do
            withListBox "Channels" size $ do
                _ <- selectable "Foo" False []
                pure ()
        pure ()

renderShaderList :: MonadUnliftIO m => m ()
renderShaderList = do
    beginGroup
    withCollapsingHeader "Shaders" [] $ do
        void (selectable "Shader 1" False [])
    endGroup

renderSettingsWindow :: ( MonadUI m
                        , MonadSettingsWindow m
                        ) => Word -> Word -> [T.Text] -> m ()
renderSettingsWindow sampleRate bufferSize ports = do
    isOpen <- isSettingsWindowOpen
    when isOpen $ do
        closed <- openSettingsWindow windowTitle $ do
            -- TODO: Do something with the selected port and shader
            _ <- liftIO $ renderStaticText sampleRate bufferSize
                >> renderPortBox ports
                >> renderShaderList
            pure ()
        setSettingsWindowOpen closed
