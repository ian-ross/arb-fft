{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Prelude hiding (length, map, maximum, sum, zipWith)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Control.Applicative ((<$>))
import Data.Complex
import Data.Vector

import Debug.Trace

import Numeric.FFT

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
        [ testProperty "FFT vs. DFT" prop_dft_vs_fft
        , testProperty "FFT/IFFT round-trip" prop_ifft
        ]

-- Clean up number display.
defuzz :: VCD -> VCD
defuzz = map (\(r :+ i) -> df r :+ df i)
  where df x = if abs x < 1.0E-6 then 0 else x

-- Check FFT against DFT.
check :: VCD -> (Double, VCD)
check v = let diff = defuzz $ zipWith (-) (fft v) (basicDFT v)
          in (maximum $ map magnitude diff, diff)

-- QuickCheck property for FFT vs. DFT testing.
prop_dft_vs_fft (v :: VCD) = trace (show $ length v) $ fst (check v) < 1.0E-6

-- QuickCheck property for inverse FFT round-trip testing.
prop_ifft (v :: VCD) = maximum (map magnitude diff) < 1.0E-6
  where diff = zipWith (-) v (ifft $ fft v)

-- Non-zero length arbitrary vectors.
instance Arbitrary VCD where
  arbitrary = fromList <$> listOf1 arbitrary


-- Roots of unity.
omega :: Int -> Complex Double
omega n = cis (2 * pi / fromIntegral n)

-- Naïve DFT.
basicDFT :: VCD -> VCD
basicDFT h = generate n doone
  where n = length h
        w = omega n
        doone i = sum $ zipWith (*) h $ generate n (\k -> w^^(i*k))
