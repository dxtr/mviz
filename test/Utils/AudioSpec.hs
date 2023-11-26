module Utils.AudioSpec (spec) where

import           Data.Complex     (Complex ((:+)))
import           Data.Either      (isLeft)
import           Mviz.Utils.Audio (fft, ffti, frequencies, magnitude)
import           Test.Hspec       (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
    describe "fft" $ do
        it "calculates fft" $ do
            fft 4 [-1,2,3,0] `shouldBe` Right [4.0 :+ 0.0, (-4.0) :+ (-2.0), 0.0 :+ 0.0]
            fft 8 [2, 1,-1,5,0,3,0,-4] `shouldBe` Right [6.0 :+ 0.0, (-5.7781744) :+ (-3.9497476), 3.0 :+ (-3.0), 9.778174 :+ (-5.9497476), (-4.0) :+ 0.0]
            fft 0 [] `shouldBe` Right []
            fft 1 [-1] `shouldBe` Right [(-1.0) :+ 0.0]
            fft 0 [-1] `shouldBe` Right []
            fft 2 [-1] `shouldSatisfy` isLeft
            fft 1 [-1, -1] `shouldSatisfy` isLeft
    describe "ffti" $ do
        it "calculates the inverse fft" $ do
            ffti 4 [4.0 :+ 0.0, (-4.0) :+ (-2.0), 0.0 :+ 0.0] `shouldBe` Right [-1, 2, 3, 0]
            ffti 0 [-1] `shouldBe` Right []
            ffti 1 [-1] `shouldBe` Right [-1.0]
    describe "fft symmetry" $ do
        it "calculates fft -> ffti -> fft" $ do
            (fft 4 [-1,2,3,0] >>= ffti 4 >>= fft 4 >>= ffti 4) `shouldBe` Right [-1,2,3,0]
    describe "frequencies" $ do
        it "returns frequencies" $ do
            frequencies 0 1 `shouldBe` []
            frequencies 2 0 `shouldBe` []
            frequencies 48000 3 `shouldBe` [0.0]
            frequencies 48000 4 `shouldBe` [0.0, 12000.0]
            frequencies 48000 5 `shouldBe` [0.0, 9600.0]
            frequencies 48000 6 `shouldBe` [0.0, 8000.0, 16000.0]
            frequencies 44100 10 `shouldBe` [0.0,4410.0,8820.0,13230.0,17640.0]
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
