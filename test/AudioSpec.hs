module AudioSpec where

import           Mviz.Audio (mixChannelBuffers)
import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "mixChannelBuffers" $ do
        it "adds channels" $ do
            mixChannelBuffers [] `shouldBe` []
            mixChannelBuffers [[]] `shouldBe` []
            mixChannelBuffers [[], []] `shouldBe` []
            mixChannelBuffers [[1.0, 1.0]] `shouldBe` [1.0, 1.0]
            mixChannelBuffers [[1.0, 1.0], [1.0, 1.0]] `shouldBe` [2.0, 2.0]
