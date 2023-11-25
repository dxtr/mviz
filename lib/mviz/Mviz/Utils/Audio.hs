module Mviz.Utils.Audio
  ( frequencies
  , magnitude
  , fft
  , ffti
  ) where

import qualified Data.Array.Comfort.Shape    as Shape
import qualified Data.Array.Comfort.Storable as Arr (fromList, toList)
import           Data.Complex                (Complex, imagPart, realPart)
import           Data.List.NonEmpty          (NonEmpty)
import qualified Data.List.NonEmpty          as NE
import qualified Numeric.FFTW.Rank1          as FFT
import qualified Numeric.FFTW.Shape          as Spectrum

frequencies :: (Integral a, RealFloat b) => a -> a -> [b]
frequencies sampleRate bufferSize =
    [ fromIntegral i * (fromIntegral sampleRate / fromIntegral bufferSize) | i <- [0..numFreq] ]
    where numFreq = (bufferSize `quot` 2) - 1

magnitude :: Complex Float -> Float
magnitude bin =
    sqrt (re*re + im*im)
    where re = realPart bin
          im = imagPart bin

-- TODO: Create a function for amplitude?

fft :: Int -> NonEmpty Float -> NonEmpty (Complex Float)
fft 0 _ = error "Size cannot be 0"
fft bufferSize input =
    NE.fromList . Arr.toList . FFT.fourierRC $ Arr.fromList shape inp
    where shape = Shape.Cyclic bufferSize
          inp = NE.toList input

ffti :: Int -> NonEmpty (Complex Float) -> NonEmpty Float
ffti 0 _ = error "Size cannot be 0"
ffti bufferSize input =
  NE.map (\x -> x / fromIntegral bufferSize) $ NE.fromList . Arr.toList . FFT.fourierCR $ Arr.fromList shape inp
  where shape = Spectrum.Half bufferSize
        inp = NE.toList input
