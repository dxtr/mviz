{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}

module Mviz.UI.SettingsWindow
  ( HasSettingsWindow (..)
  , MonadSettingsWindow (..)
  , renderSettingsWindow
  , selectedPorts
  ) where

import           Control.Monad             (void)
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
import           Mviz.UI.UIWindow          (SettingsWindow (SettingsWindow, settingsCheckedChannels, settingsSelectedInput))
import           Mviz.Utils.Inputs         (combinePortNames)
import           UnliftIO                  (MonadUnliftIO, readIORef)

class HasSettingsWindow a where
    getSettingsWindow :: a -> SettingsWindow

class Monad m => MonadSettingsWindow m where
    openSettingsWindow :: T.Text -> m Bool -> m (Bool, Maybe Bool)
    isSettingsWindowOpen :: m Bool
    setSettingsWindowOpen :: Bool -> m ()
    setSelectedInput :: Maybe T.Text -> m ()
    getSelectedInput :: m (Maybe T.Text)
    setSelectedChannels :: [T.Text] -> m ()
    getSelectedChannels :: m [T.Text]

selectedPorts :: (MonadIO m) => SettingsWindow -> m [T.Text]
selectedPorts SettingsWindow{ settingsSelectedInput = selectedInput
                            , settingsCheckedChannels = checkedChannels } = do
    channels <- liftIO $ readIORef checkedChannels
    maybe [] (`combinePortNames` channels) <$> readIORef selectedInput

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

renderPortBox :: (MonadUnliftIO m) => InputMap -> Maybe T.Text -> [T.Text] -> m (Maybe (T.Text, [T.Text]))
renderPortBox inputs selectedInput selectedChannels = withCollapsingHeader "Ports" [] $ \case
    False -> pure Nothing
    True -> do
        ImVec2 regionSizeX _ <- contentRegionAvail
        ImVec2 spacingX _ <- itemSpacing
        let sizeX = regionSizeX * 0.5
        let calcSize = flip subtract (sizeX - spacingX)
        ImVec2 inputsLabelSize _ <- calcTextSize inputLabel True
        ImVec2 channelsLabelSize _ <- calcTextSize channelsLabel True
        let sortedInputs = sort $ getInputs inputs

        runMaybeT $ do
            newSelectedInput <- MaybeT . liftIO $ withListBox inputLabel (ImVec2 (calcSize inputsLabelSize) 0.0) $ do
                items <- uncons . filter snd <$> mapM (\p -> (p, ) <$> selectable p (Just p == selectedInput) []) sortedInputs
                case items of
                    Nothing     -> pure selectedInput
                    Just (i, _) -> pure . Just . fst $ i
            sameLine
            let channels = maybe [] (sort . getChannels inputs) newSelectedInput
            checkedChannels <- MaybeT . liftIO $ withListBox channelsLabel (ImVec2 (calcSize channelsLabelSize) 0.0) $ do
                chans <- mapM (\c -> (c, ) <$> checkbox c (c `elem` selectedChannels)) channels
                pure . map fst $ filter (\(_, (_, c)) -> c) chans
            sInput <- hoistMaybe newSelectedInput
            pure (sInput, checkedChannels)
    where inputLabel = "Input"
          channelsLabel = "Channels"

renderShaderList :: MonadUnliftIO m => [T.Text] -> m ()
renderShaderList shaders = do
    beginGroup
    _ <- withCollapsingHeader "Shaders" [] $ \case
        False -> pure Nothing
        True -> do
          mapM_ (\s -> selectable s False []) shaders
--            void (selectable "Shader 1" False [])
          pure $ Just ()
    endGroup

renderSettingsWindow :: ( MonadUI m
                        , MonadSettingsWindow m
                        , MonadLogger m
                        ) => Word -> Word -> InputMap -> [T.Text] -> m Bool
renderSettingsWindow sampleRate bufferSize inputs shaders = do
    isOpen <- isSettingsWindowOpen
    selectedInput <- getSelectedInput
    checkedChannels <- getSelectedChannels
    if isOpen then do
        (closed, changed) <- openSettingsWindow windowTitle $ do
            -- TODO: Do something with the selected port and shader
            _ <- liftIO $ renderStaticText sampleRate bufferSize
            newSelectedInput <- liftIO $ renderPortBox inputs selectedInput checkedChannels
            inputChanged <- case newSelectedInput of
                Nothing       -> pure False
                Just i@(s, c) ->
                    if selectedInput == Just s && checkedChannels == c
                    then pure False
                    else do
                        logDebugN $ "New selected input: " <> T.pack (show i)
                        setSelectedInput (Just s)
                        setSelectedChannels c
                        pure True
            liftIO $ renderShaderList shaders
            pure inputChanged
        setSettingsWindowOpen closed
        pure $ fromMaybe False changed
        else pure False
