{-# LANGUAGE FlexibleContexts #-}
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
import Data.Vector.Generic
import Data.Complex

import Numeric.FFT.Types
import Numeric.FFT.Plan
import Numeric.FFT.Execute


-- | Forward FFT with embedded plan calculation.
fft :: Vector v (Complex Double) =>
       v (Complex Double) -> IO (v (Complex Double))
fft xs = do
  p <- plan $ length xs
  return $ fftWith p xs

-- | Inverse FFT with embedded plan calculation.
ifft :: Vector v (Complex Double) =>
        v (Complex Double) -> IO (v (Complex Double))
ifft xs = do
  p <- plan $ length xs
  return $ ifftWith p xs

-- | Forward FFT with pre-computed plan.
fftWith :: Vector v (Complex Double) =>
           Plan -> v (Complex Double) -> v (Complex Double)
fftWith p = convert . execute p Forward . convert

-- | Inverse FFT with pre-computed plan.
ifftWith :: Vector v (Complex Double) =>
            Plan -> v (Complex Double) -> v (Complex Double)
ifftWith p = convert . execute p Inverse . convert
