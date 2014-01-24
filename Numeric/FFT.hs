-- | Mixed-radix FFT calculation.
--
-- Arbitrary input vector lengths are handled using a mixed-radix
-- Cooley-Tukey decimation in time algorithm with residual prime
-- length vectors being treated using Rader's algorithm or hand-coded
-- codelets for small primes.
module Numeric.FFT
       ( fft, ifft, fftWith, ifftWith
       , plan, planFromFactors, execute
       , Plan (..), Direction (..), BaseTransform (..)
       ) where

import Prelude hiding (length, map, sum, zipWith)
import Data.Vector
import Data.Complex

import Numeric.FFT.Types
import Numeric.FFT.Plan
import Numeric.FFT.Execute


-- | Forward FFT with embedded plan calculation.
fft :: Vector (Complex Double) -> IO (Vector (Complex Double))
fft xs = do
  p <- plan $ length xs
  return $ fftWith p xs

-- | Inverse FFT with embedded plan calculation.
ifft :: Vector (Complex Double) -> IO (Vector (Complex Double))
ifft xs = do
  p <- plan $ length xs
  return $ ifftWith p xs

-- | Forward FFT with pre-computed plan.
fftWith :: Plan -> Vector (Complex Double) -> Vector (Complex Double)
fftWith p = convert . execute p Forward . convert

-- | Inverse FFT with pre-computed plan.
ifftWith :: Plan -> Vector (Complex Double) -> Vector (Complex Double)
ifftWith p = convert . execute p Inverse . convert
