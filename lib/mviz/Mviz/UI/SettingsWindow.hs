{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}

module Mviz.UI.SettingsWindow
  ( HasSettingsWindow (..)
  , MonadSettingsWindow (..)
  , renderSettingsWindow
  ) where

import           Control.Applicative       ((<|>))
import           Control.Monad             (unless, void, when)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Logger      (MonadLogger, logDebugN)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT),
                                            hoistMaybe)
import           Data.List                 (sort, uncons)
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import           ImGui                     (ImVec2 (ImVec2), beginGroup,
                                            calcTextSize, checkbox,
                                            contentRegionAvail, endGroup,
                                            itemSpacing, sameLine, selectable,
                                            textUnformatted,
                                            withCollapsingHeader, withListBox)
import           Mviz.Audio.Inputs         (InputMap, getChannels, getInputs)
import           Mviz.UI.Types             (MonadUI)
import           Mviz.UI.UIWindow          (SettingsWindow)
import           UnliftIO                  (MonadUnliftIO)

class HasSettingsWindow a where
    getSettingsWindow :: a -> SettingsWindow

class Monad m => MonadSettingsWindow m where
    openSettingsWindow :: T.Text -> m () -> m Bool
    isSettingsWindowOpen :: m Bool
    setSettingsWindowOpen :: Bool -> m ()
    setSelectedInput :: Maybe T.Text -> m ()
    getSelectedInput :: m (Maybe T.Text)
    setSelectedChannels :: [T.Text] -> m ()
    getSelectedChannels :: m [T.Text]

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

renderPortBox :: (MonadUnliftIO m, MonadIO m) => InputMap -> Maybe T.Text -> [T.Text] -> m (Maybe (T.Text, [T.Text]))
renderPortBox inputs selectedInput selectedChannels = withCollapsingHeader "Ports" [] $ \case
    False -> pure Nothing
    True -> do
        ImVec2 regionSizeX _ <- contentRegionAvail
        ImVec2 spacingX _ <- itemSpacing
        let sizeX = regionSizeX * 0.5
        let calcSize = flip subtract (sizeX - spacingX)
        let sortedInputs = sort $ getInputs inputs
        ImVec2 inputsLabelSize _ <- calcTextSize "Inputs" True
        ImVec2 channelsLabelSize _ <- calcTextSize "Channels" True

        runMaybeT $ do
            newSelectedInput <- MaybeT . liftIO $ withListBox "Inputs" (ImVec2 (calcSize inputsLabelSize) 0.0) $ do
                items <- uncons . filter snd <$> mapM (\p -> (p, ) <$> selectable p (Just p == selectedInput) []) sortedInputs
                case items of
                    Nothing     -> pure selectedInput
                    Just (i, _) -> pure $ Just $ fst i
            sameLine
            let channels = maybe [] (sort . getChannels inputs) newSelectedInput
            checkedChannels <- MaybeT . liftIO $ withListBox "Channels" (ImVec2 (calcSize channelsLabelSize) 0.0) $ do
                chans <- mapM (\c -> (c, ) <$> checkbox c (c `elem` selectedChannels)) channels
                pure . map fst $ filter (\(_, (_, c)) -> c) chans
            sInput <- hoistMaybe newSelectedInput
            pure (sInput, checkedChannels)

renderShaderList :: MonadUnliftIO m => m ()
renderShaderList = do
    beginGroup
    _ <- withCollapsingHeader "Shaders" [] $ \case
        False -> pure Nothing
        True -> do
            void (selectable "Shader 1" False [])
            pure $ Just ()
    endGroup

renderSettingsWindow :: ( MonadUI m
                        , MonadSettingsWindow m
                        , MonadLogger m
                        ) => Word -> Word -> InputMap -> m ()
renderSettingsWindow sampleRate bufferSize inputs = do
    isOpen <- isSettingsWindowOpen
    selectedInput <- getSelectedInput
    checkedChannels <- getSelectedChannels
    when isOpen $ do
        closed <- openSettingsWindow windowTitle $ do
            -- TODO: Do something with the selected port and shader
            _ <- liftIO $ renderStaticText sampleRate bufferSize
            newSelectedInput <- liftIO $ renderPortBox inputs selectedInput checkedChannels
            case newSelectedInput of
                Nothing     -> pure ()
                Just i@(s, c) -> unless (selectedInput == Just s && checkedChannels == c) $ do
                    logDebugN $ "New selected input: " <> T.pack (show i)
                    setSelectedInput (Just s)
                    setSelectedChannels c
            liftIO renderShaderList
            pure ()
        setSettingsWindowOpen closed
