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

import           Control.Monad              (liftM, void)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Logger       (MonadLogger, logDebugN)
import           Control.Monad.Reader       (MonadReader, ask, asks, runReaderT)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe  (MaybeT (MaybeT, runMaybeT),
                                             hoistMaybe)
import           Data.List                  (sort, uncons)
import           Data.Maybe                 (fromMaybe, isJust)
import qualified Data.Text                  as T
import           ImGui                      (ImVec2 (ImVec2), beginGroup,
                                             calcTextSize, checkbox,
                                             contentRegionAvail, endGroup,
                                             itemSpacing, sameLine, selectable,
                                             textUnformatted,
                                             withCollapsingHeader, withListBox)
import           Mviz.Audio.Inputs          (InputMap, getChannels, getInputs)
import           Mviz.UI.Types              (MonadUI (..))
import           Mviz.UI.UIWindow           (SettingsWindow (..))
import           Mviz.Utils.Inputs          (combinePortNames)
import           UnliftIO                   (MonadUnliftIO, readIORef)

instance (MonadUI m) => MonadUI (StateT SettingsWindow m) where
  isUIShown = lift isUIShown
  toggleUI = lift toggleUI
  renderUI = lift renderUI

instance (MonadSettingsWindow m) => MonadSettingsWindow (StateT SettingsWindow m) where
  openSettingsWindow label func =
    get >>= \s -> lift (openSettingsWindow label $ evalStateT func s)
  getSelectedInput = lift getSelectedInput
  getSelectedChannels = lift getSelectedChannels
  isSettingsChanged = lift isSettingsChanged
  getSettingsShader = lift getSettingsShader

class HasSettingsWindow a where
  getSettingsWindow :: a -> IO SettingsWindow
  setSettingsWindow :: a -> SettingsWindow -> IO ()

class Monad m => MonadSettingsWindow m where
  openSettingsWindow :: T.Text -> m Bool -> m (Bool, Maybe Bool)
  getSelectedInput :: m (Maybe T.Text)
  getSelectedChannels :: m [T.Text]
  isSettingsChanged :: m Bool
  getSettingsShader :: m T.Text

selectedPorts :: (MonadIO m) => SettingsWindow -> m [T.Text]
selectedPorts SettingsWindow{ settingsSelectedInput = selectedInput
                            , settingsCheckedChannels = checkedChannels } = do
    pure $ maybe [] (`combinePortNames` checkedChannels) selectedInput

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

data PortBox = PortBox
  { portBoxInputs           :: InputMap
  , portBoxSelectedInput    :: Maybe T.Text
  , portBoxSelectedChannels :: [T.Text]
  }

instance Eq PortBox where
  x == y = (portBoxSelectedInput x == portBoxSelectedInput y) &&
    (portBoxSelectedChannels x == portBoxSelectedChannels y)

renderPortBox :: ( MonadUnliftIO m
                 , MonadReader PortBox m
                 ) => m (Bool, PortBox)
renderPortBox = withCollapsingHeader "Ports" [] $ \case
  False -> do
    pb <- ask
    return (False, pb)
  True -> do
    pb <- ask
    ImVec2 regionSizeX _ <- contentRegionAvail
    ImVec2 spacingX _ <- itemSpacing
    ImVec2 inputsLabelSize _ <- calcTextSize inputLabel True
    ImVec2 channelsLabelSize _ <- calcTextSize channelsLabel True
    let sizeX = regionSizeX * 0.5
        calcSize = flip subtract (sizeX - spacingX)
        inputs = portBoxInputs pb
        sortChannels = sort . getChannels inputs
        sortedInputs = sort $ getInputs inputs
        selectedInput = portBoxSelectedInput pb
        selectedChannels = portBoxSelectedChannels pb
    s <- runMaybeT $ do
      newSelectedInput <- MaybeT . liftIO $ withListBox inputLabel (ImVec2 (calcSize inputsLabelSize) 0.0) $ do
        items <- uncons . map fst . filter snd <$> mapM (\p -> (p, ) <$> selectable p (Just p == selectedInput) []) sortedInputs
        pure $ maybe selectedInput (Just . fst) items
      sameLine
      let channels = maybe [] sortChannels newSelectedInput
      checkedChannels <- MaybeT . liftIO $ withListBox channelsLabel (ImVec2 (calcSize channelsLabelSize) 0.0) $ do
        chans <- mapM (\c -> (c, ) <$> checkbox c (c `elem` selectedChannels)) channels
        pure . map fst $ filter (\(_, (_, c)) -> c) chans
      pure $ pb { portBoxSelectedInput = newSelectedInput
                , portBoxSelectedChannels = checkedChannels
                }
    return $ maybe (False, pb) (\a -> (a == pb, a)) s
  where inputLabel = "Input"
        channelsLabel = "Channels"

-- renderPortBox :: (MonadUnliftIO m) => InputMap -> Maybe T.Text -> [T.Text] -> m (Maybe (T.Text, [T.Text]))
-- renderPortBox inputs selectedInput selectedChannels = withCollapsingHeader "Ports" [] $ \case
--     False -> pure Nothing
--     True -> do
--       ImVec2 regionSizeX _ <- contentRegionAvail
--       ImVec2 spacingX _ <- itemSpacing
--       ImVec2 inputsLabelSize _ <- calcTextSize inputLabel True
--       ImVec2 channelsLabelSize _ <- calcTextSize channelsLabel True
--       let sizeX = regionSizeX * 0.5
--           calcSize = flip subtract (sizeX - spacingX)
--           sortedInputs = sort $ getInputs inputs

--       runMaybeT $ do
--         newSelectedInput <- MaybeT . liftIO $ withListBox inputLabel (ImVec2 (calcSize inputsLabelSize) 0.0) $ do
--           items <- uncons . filter snd <$> mapM (\p -> (p, ) <$> selectable p (Just p == selectedInput) []) sortedInputs
--             case items of
--               Nothing     -> pure selectedInput
--               Just (i, _) -> pure . Just . fst $ i
--           sameLine
--           let channels = maybe [] (sort . getChannels inputs) newSelectedInput
--           checkedChannels <- MaybeT . liftIO $ withListBox channelsLabel (ImVec2 (calcSize channelsLabelSize) 0.0) $ do
--             chans <- mapM (\c -> (c, ) <$> checkbox c (c `elem` selectedChannels)) channels
--             pure . map fst $ filter (\(_, (_, c)) -> c) chans
--           sInput <- hoistMaybe newSelectedInput
--           pure (sInput, checkedChannels)
--     where inputLabel = "Input"
--           channelsLabel = "Channels"

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

-- TODO: Shaders
renderSettingsWindow :: ( MonadUI m
                        , MonadState SettingsWindow m
                        , MonadSettingsWindow m
                        ) => Word -> Word -> InputMap -> [T.Text] -> m Bool
renderSettingsWindow sampleRate bufferSize inputs shaders = do
  wnd <- get
  let isOpen = settingsWindowOpen wnd
      selectedInput = settingsSelectedInput wnd
      checkedChannels = settingsCheckedChannels wnd
  if isOpen then do
    (closed, changed) <- openSettingsWindow windowTitle $ do
      _ <- liftIO $ renderStaticText sampleRate bufferSize
      (changed, pb) <- liftIO $ runReaderT renderPortBox $
        PortBox { portBoxInputs = inputs
                , portBoxSelectedInput = selectedInput
                , portBoxSelectedChannels = checkedChannels
                }
      put $ wnd { settingsSelectedInput = portBoxSelectedInput pb
                , settingsCheckedChannels = portBoxSelectedChannels pb
                }
      return changed
    put $ wnd { settingsWindowOpen = not closed }
    return $ fromMaybe False changed
    else return False
