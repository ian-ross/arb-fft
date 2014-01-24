module Main where

import Criterion.Main
import Data.Complex
import Data.Vector
import qualified Numeric.FFT as FFT

tstvec :: Int -> Vector (Complex Double)
tstvec sz = generate sz (\i -> let ii = fromIntegral i
                               in sin (2*pi*ii/1024) + sin (2*pi*ii/511))

main :: IO ()
main = do
  p <- FFT.plan 256
  run (nf (FFT.fftWith p) (tstvec 256)) 1000
