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
import qualified Numeric.FFTW.Rank1          as FFT
import qualified Numeric.FFTW.Shape          as Spectrum

frequencies :: Word -> Word -> [Float]
frequencies 0 _ = []
frequencies _ 0 = []
frequencies sampleRate bufferSize = [ fromIntegral i * (fromIntegral sampleRate / fromIntegral bufferSize) | i <- [0..numFreq] ]
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

fft :: Word -> [Float] -> Either String [Complex Float]
fft 0 _ = Right []
fft _ [] = Right []
fft bufferSize input
  | length input /= fromIntegral bufferSize = Left "buffer size and input length mismatch"
  | otherwise = Right . Arr.toList . FFT.fourierRC $ Arr.fromList shape input
    where shape = Shape.Cyclic bufferSize

ffti :: Word -> [Complex Float] -> Either String [Float]
ffti 0 _ = Right []
ffti _ [] = Right []
ffti bufferSize input
  | length input /= fromIntegral (bufferSize `quot` 2) + 1 = Left "input and bufferSize mismatch"
  | otherwise = Right . map (\x -> x / fromIntegral bufferSize) $ Arr.toList . FFT.fourierCR $ Arr.fromList shape input
    where shape = Spectrum.Half bufferSize
