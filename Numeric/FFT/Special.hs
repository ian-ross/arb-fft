module Numeric.FFT.Special
       ( specialBases
       , specialBaseSizes
       , maxPrimeSpecialBaseSize ) where

import           Control.Monad.ST
import           Data.IntMap.Strict                (IntMap)
import qualified Data.IntMap.Strict                as IM
import           Data.Vector.Unboxed
import           Prelude                           hiding (filter, maximum)

import           Numeric.FFT.Special.Miscellaneous
import           Numeric.FFT.Special.PowersOfTwo
import           Numeric.FFT.Special.Primes
import           Numeric.FFT.Types
import           Numeric.FFT.Utils


-- | Map from input vector lengths to hard-coded FFT transforms for
-- small problem sizes.  Each function in the map takes a transform
-- direction (+1 for forward, -1 for inverse) and returns the
-- /unscaled/ transform (scaling for inverse transforms is applied at
-- the top-level).
specialBases :: IntMap (Int -> MVCD s -> MVCD s -> ST s ())
specialBases = IM.fromList [ ( 2, special2)
                           , ( 4, special4)
                           , ( 8, special8)
                           , (16, special16)
                           , (32, special32)
                           , (64, special64)
                           , ( 3, special3)
                           , ( 5, special5)
                           , ( 7, special7)
                           , (11, special11)
                           , (13, special13)
                           , ( 6, special6)
                           , ( 9, special9)
                           , (10, special10)
                           , (12, special12)
                           , (14, special14)
                           , (15, special15)
                           , (20, special20)
                           , (25, special25)
                           ]

-- | Available base transform sizes.
specialBaseSizes :: Vector Int
specialBaseSizes = fromList $ IM.keys specialBases

-- | Largest prime for which we have a specialised base transform.
maxPrimeSpecialBaseSize :: Int
maxPrimeSpecialBaseSize = maximum $ filter isPrime specialBaseSizes
