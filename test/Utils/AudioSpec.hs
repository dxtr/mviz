module Utils.AudioSpec (spec) where

import           Control.Exception (evaluate)
import           Data.Complex      (Complex ((:+)))
import           Mviz.Utils.Audio  (fft, ffti, frequencies, magnitude)
import           Test.Hspec        (Spec, anyErrorCall, describe, it, shouldBe,
                                    shouldThrow)

spec :: Spec
spec = do
    describe "fft" $ do
        it "calculates fft" $ do
            fft 4 [-1,2,3,0] `shouldBe`  [4.0 :+ 0.0, (-4.0) :+ (-2.0), 0.0 :+ 0.0]
            fft 8 [2, 1,-1,5,0,3,0,-4] `shouldBe`  [6.0 :+ 0.0, (-5.7781744) :+ (-3.9497476), 3.0 :+ (-3.0), 9.778174 :+ (-5.9497476), (-4.0) :+ 0.0]
            fft 0 [] `shouldBe` []
        it "throws error" $ do
            evaluate (fft 0 [-1]) `shouldThrow` anyErrorCall
            evaluate (fft 1 [-1]) `shouldThrow` anyErrorCall
    describe "ffti" $ do
        it "calculates the inverse fft" $ do
            ffti 4 [4.0 :+ 0.0, (-4.0) :+ (-2.0), 0.0 :+ 0.0] `shouldBe` [-1, 2, 3, 0]
        it "throws error" $ do
            evaluate (ffti 0 [-1]) `shouldThrow` anyErrorCall
            evaluate (ffti 1 [-1]) `shouldThrow` anyErrorCall
    describe "frequencies" $ do
        it "throws error" $ do
            evaluate (frequencies 0 1) `shouldThrow` anyErrorCall
            evaluate (frequencies 1 0) `shouldThrow` anyErrorCall
    describe "magnitude" $ do
        it "returns 0" $ do
            magnitude (0.0 :+ 0.0) `shouldBe` sqrt 0
        it "returns 1" $ do
            magnitude (1.0 :+ 0.0) `shouldBe` sqrt 1
            magnitude (0.0 :+ 1.0) `shouldBe` sqrt 1
        it "returns sqrt 2" $ do
            magnitude (1.0 :+ 1.0) `shouldBe` sqrt 2
        it "returns" $ do
            magnitude (2.0 :+ 2.0) `shouldBe` sqrt 8
