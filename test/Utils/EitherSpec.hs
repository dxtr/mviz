module Utils.EitherSpec (spec) where
import           Mviz.Utils.Either (maybeToEither)
import           Test.Hspec        (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "maybeToEither" $ do
        it "returns left" $ do
            maybeToEither ("Test" :: String) (Nothing :: Maybe Int) `shouldBe` Left "Test"
        it "returns right" $ do
            maybeToEither "Test" (Just (1 :: Int)) `shouldBe` Right 1
