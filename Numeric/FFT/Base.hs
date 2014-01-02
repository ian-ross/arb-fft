module Numeric.FFT.Base ( applyBase ) where

import Prelude hiding (sum, zipWith)
import qualified Prelude as P
import Data.Vector
import qualified Data.IntMap.Strict as IM

import Numeric.FFT.Types
import Numeric.FFT.Special


-- | Apply a base transform to a single vector.
applyBase :: WMap -> BaseTransform -> Int -> VCD -> VCD

-- Simple DFT algorithm.
applyBase wmap (DFTBase sz) sign h = generate sz doone
  where ws = wmap IM.! (sign * sz)
        doone i = sum $ zipWith (*) h $
                  generate sz (\k -> ws ! (i * k `mod` sz))

-- Special hard-coded cases.
applyBase _ (SpecialBase sz) sign h = case IM.lookup sz specialBases of
  Just f -> f sign h
  Nothing -> error "invalid problem size for SpecialBase"

applyBase wmap (RaderBase sz _ _ _ _ _ _) sign h =
  error "RaderBase NOT IMPLEMENTED"
