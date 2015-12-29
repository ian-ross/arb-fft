module Main where

import Criterion
import Criterion.Main.Options
import Criterion.Types
import Data.Complex
import Data.Vector
import qualified Numeric.FFT as FFT

tstvec :: Int -> Vector (Complex Double)
tstvec sz = generate sz (\i -> let ii = fromIntegral i
                               in sin (2*pi*ii/1024) + sin (2*pi*ii/511))

main :: IO ()
main = do
  p <- FFT.plan 256
  benchmarkWith (defaultConfig { resamples = 1000 }) 
    (nf (FFT.fftWith p) (tstvec 256))
