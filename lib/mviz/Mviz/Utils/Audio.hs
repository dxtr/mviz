module Mviz.Utils.Audio
  ( frequencies
  , magnitude
  , fft
  , ffti
  ) where

import qualified Data.Array.Comfort.Shape    as Shape
import qualified Data.Array.Comfort.Storable as Arr (fromList, toList)
import           Data.Complex                (Complex ((:+)), imagPart,
                                              realPart)
import           Data.List.NonEmpty          (NonEmpty)
import qualified Data.List.NonEmpty          as NE
import           GHC.Stack                   (HasCallStack)
import qualified Numeric.FFTW.Rank1          as FFT
import qualified Numeric.FFTW.Shape          as Spectrum

frequencies :: Int -> Int -> NonEmpty Float
frequencies _ 0 = error "bufferSize cannot be 0"
frequencies 0 _ = error "sampleRate cannot be 0"
frequencies sampleRate bufferSize =
    NE.fromList [ fromIntegral i * (fromIntegral sampleRate / fromIntegral bufferSize) | i <- [0..numFreq] ]
    where numFreq = (bufferSize `quot` 2) - 1

magnitude :: Complex Float -> Float
magnitude (0.0 :+ 0.0) = 0.0
magnitude (1.0 :+ 0.0) = 1.0
magnitude (0.0 :+ 1.0) = 1.0
magnitude (1.0 :+ 1.0) = 1.4142135
magnitude bin =
    sqrt (re*re + im*im)
    where re = realPart bin
          im = imagPart bin

-- TODO: Create a function for amplitude?

fft :: (HasCallStack) => Word -> [Float] -> [Complex Float]
fft _ [] = []
fft bufferSize input
  | bufferSize == 1 = error "buffer size too small"
  | bufferLength /= fromIntegral bufferSize = error $ "input and bufferSize mismatch: " <> show bufferLength <> " " <> show bufferSize
  | otherwise = Arr.toList . FFT.fourierRC $ Arr.fromList shape input
    where shape = Shape.Cyclic bufferSize
          bufferLength = length input

ffti :: (HasCallStack) => Word -> [Complex Float] -> [Float]
ffti bufferSize input
  | bufferSize <= 1 = error "buffer size too small"
--  | length input /= fromIntegral bufferSize = error "input and bufferSize mismatch"
  | otherwise = map (\x -> x / fromIntegral bufferSize) $ Arr.toList . FFT.fourierCR $ Arr.fromList shape input
    where shape = Spectrum.Half bufferSize
