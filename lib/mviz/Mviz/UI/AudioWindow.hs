{-# LANGUAGE OverloadedStrings #-}

module Mviz.UI.AudioWindow
  ( HasAudioWindow (..)
  , MonadAudioWindow (..)
  ) where

import qualified Data.Text        as T
import           Mviz.Types       (MonadUI)
import           Mviz.UI.UIWindow (AudioWindow)

class HasAudioWindow a where
    getAudioWindow :: a -> AudioWindow

class Monad m => MonadAudioWindow m where
    openAudioWindow :: T.Text -> m () -> m Bool
    isAudioWindowOpen :: m Bool
    setAudioWindowOpen :: Bool -> m ()

windowId :: T.Text
windowId = "audiowindow"

windowTitle :: T.Text
windowTitle = "Audio##" <> windowId

renderAudioWindow :: (MonadUI m, MonadAudioWindow m) => m ()
renderAudioWindow = do
    isOpen <- isAudioWindowOpen
    when isOpen $ do

