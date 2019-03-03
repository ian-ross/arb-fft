{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Control.Applicative     ((<$>))
import           Data.Complex
import           Data.Vector
import           Prelude                 hiding (length, map, maximum, sum,
                                          zipWith, (++))
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck   hiding (generate)

import           Numeric.FFT

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
        [ testProperty "FFT vs. DFT" prop_dft_vs_fft
        , testProperty "FFT/IFFT round-trip" prop_ifft
        ]

-- Clean up number display.
defuzz :: Vector (Complex Double) -> Vector (Complex Double)
defuzz = map (\(r :+ i) -> df r :+ df i)
  where df x = if abs x < 1.0E-6 then 0 else x

-- Check FFT against DFT.
check :: Vector (Complex Double) -> IO (Double, Vector (Complex Double))
check v = do
  tst <- fft v
  let diff = defuzz $ zipWith (-) tst (basicDFT v)
  return (maximum $ map magnitude diff, diff)

-- QuickCheck property for FFT vs. DFT testing.
prop_dft_vs_fft :: Property
prop_dft_vs_fft = monadicIO $ do
  v <- pick arbitrary
  chk <- run $ check v
  assert $ fst chk < 1.0E-6

-- QuickCheck property for inverse FFT round-trip testing.
prop_ifft :: Property
prop_ifft = monadicIO $ do
  v <- pick arbitrary
  fwd <- run $ fft v
  bwd <- run $ ifft fwd
  let diff = zipWith (-) v bwd
  assert $ maximum (map magnitude diff) < 1.0E-6

-- Non-zero length arbitrary vectors.
instance Arbitrary (Vector (Complex Double)) where
  arbitrary = fromList <$> listOf1 arbitrary


-- Roots of unity.
omega :: Int -> Complex Double
omega n = cis (2 * pi / fromIntegral n)

-- Naïve DFT.
basicDFT :: Vector (Complex Double) -> Vector (Complex Double)
basicDFT h = generate n doone
  where n = length h
        w = omega n
        doone i = sum $ zipWith (*) h $ generate n (\k -> w^^(i*k))
