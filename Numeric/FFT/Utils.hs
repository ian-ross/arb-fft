module Numeric.FFT.Utils
       ( omega, slicevecs, primes, factors
       ) where

import Prelude hiding (head, length, map)
import qualified Prelude as P
import Data.Complex
import Data.Vector

import Numeric.FFT.Types


-- | Roots of unity.
omega :: Int -> Complex Double
omega n = cis (2 * pi / fromIntegral n)


-- | Slice a vector @v@ into equally sized parts, each of length @m@.
slicevecs :: Int -> VCD -> VVCD
slicevecs m v = map (\i -> slice (i * m) m v) $ enumFromN 0 (length v `div` m)


-- | Prime sieve from Haskell wiki.
primes :: [Int]
primes = 2 : primes'
  where primes' = sieve [3, 5 ..] 9 primes'
        sieve (x:xs) q ps@ ~(p:t)
          | x < q = x : sieve xs q ps
          | True  =     sieve [n | n <- xs, rem n p /= 0] (P.head t^2) t


-- | Simple prime factorisation: small factors only; largest/last
-- factor picked out as "special".
factors :: Int -> (Int, Vector Int)
factors n = let (lst, rest) = go n primes in (lst, fromList rest)
  where go cur pss@(p:ps)
          | cur == p         = (p, [])
          | cur `mod` p == 0 = let (lst, rest) = go (cur `div` p) pss
                               in (lst, p : rest)
          | otherwise        = go cur ps
