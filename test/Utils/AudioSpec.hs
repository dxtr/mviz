module Utils.AudioSpec (spec) where

import           Control.Exception  (evaluate)
import           Data.Complex       (Complex ((:+)))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Mviz.Utils.Audio   (fft, ffti)
import           Test.Hspec         (Spec, anyErrorCall, describe, it, shouldBe,
                                     shouldThrow)

spec :: Spec
spec = do
    describe "fft" $ do
        it "calculates fft" $ do
            fft 4 (-1 :| [2,3,0]) `shouldBe` (4.0 :+ 0.0) :| [(-4.0) :+ (-2.0), 0.0 :+ 0.0]
            fft 8 (2 :| [1,-1,5,0,3,0,-4]) `shouldBe` (6.0 :+ 0.0) :| [(-5.7781744) :+ (-3.9497476), 3.0 :+ (-3.0), 9.778174 :+ (-5.9497476), (-4.0) :+ 0.0]
        it "throws error" $ do
            evaluate (fft 0 (-1 :| [])) `shouldThrow` anyErrorCall
    describe "ffti" $ do
        it "calculates the inverse fft" $ do
            ffti 4 (4.0 :+ 0.0 :| [(-4.0) :+ (-2.0), 0.0 :+ 0.0]) `shouldBe` -1 :| [2,3,0]
        it "throws error" $ do
            evaluate (ffti 0 (-1 :| [])) `shouldThrow` anyErrorCall
