module Numeric.FFT.Base ( applyBase ) where

import Prelude hiding (sum, zipWith)
import qualified Prelude as P
import Data.Vector
import qualified Data.IntMap.Strict as IM

import Numeric.FFT.Types


-- | Apply a base transform to a single vector.
applyBase :: WMap -> BaseTransform -> Int -> VCD -> VCD
applyBase wmap (DFTBase sz) sign h = generate sz doone
  where ws = wmap IM.! (sign * sz)
        doone i = sum $ zipWith (*) h $
                  generate sz (\k -> ws ! (i * k `mod` sz))
applyBase wmap (SpecialBase sz) sign h = error "SpecialBase NOT IMPLEMENTED"
applyBase wmap (RaderBase sz _ _ _ _) sign h = error "RaderBase NOT IMPLEMENTED"
