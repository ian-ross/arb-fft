-- | Mixed-radix FFT calculation.
--
-- Arbitrary input vector lengths are handled using a mixed-radix
-- Cooley-Tukey decimation in time algorithm with residual prime
-- length vectors being treated using Rader's algorithm or hand-coded
-- codelets for small primes.
module Numeric.FFT
       ( fft, ifft, fftWith, ifftWith
       , plan, execute
       , Plan (..), Direction (..), BaseTransform (..)
       , VCD
       , check, dft
       ) where

import Prelude hiding (length, map, sum, zipWith)
import Data.Vector
import Data.Complex

import Numeric.FFT.Types
import Numeric.FFT.Plan
import Numeric.FFT.Execute


-- | Forward FFT with embedded plan calculation.
fft :: VCD -> VCD
fft xs = fftWith (plan $ length xs) xs

-- | Inverse FFT with embedded plan calculation.
ifft :: VCD -> VCD
ifft xs = ifftWith (plan $ length xs) xs

-- | Forward FFT with pre-computed plan.
fftWith :: Plan -> VCD -> VCD
fftWith p = execute p Forward

-- | Inverse FFT with pre-computed plan.
ifftWith :: Plan -> VCD -> VCD
ifftWith p = execute p Inverse



check :: VCD -> VCD
check xs = map abs $ zipWith (-) (dft xs) (fft xs)

omega n = cis (2 * pi / fromIntegral n)

dft h = generate bigN doone
  where bigN = length h
        w = omega bigN
        doone n = sum $
                  zipWith (*) h $ generate bigN (\k -> w^^(n*k))