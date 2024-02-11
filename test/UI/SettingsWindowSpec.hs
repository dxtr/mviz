{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module UI.SettingsWindowSpec (spec) where

import           Data.IORef             (newIORef)
import           Mviz.UI.SettingsWindow (selectedPorts)
import           Mviz.UI.UIWindow       (SettingsWindow (..))
import           Test.Hspec             (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
    describe "selectedPorts" $ do
        it "returns selected ports" $ do
            let sw = SettingsWindow { settingsCheckedChannels = ["foo", "bar"]
                                    , settingsSelectedInput = Just "Input"
                                    , settingsWindowOpen = True
                                    , settingsChanged = False
                                    , settingsShader = Nothing
                                    }
            selectedPorts sw `shouldReturn` ["Input:foo", "Input:bar"]
