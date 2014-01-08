module Numeric.FFT.Types
       ( VCD, MVCD, VVCD, VMVCD, VVVCD, VI, WMap
       , Direction (..), Plan (..), BaseTransform (..)
       ) where

import Data.IntMap.Strict (IntMap)
import Data.Vector.Unboxed
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Complex


-- | Some useful type synonyms.
type VCD = Vector (Complex Double)
type MVCD s = MV.MVector s (Complex Double)
type VVCD = V.Vector VCD
type VVVCD = V.Vector VVCD
type VMVCD a = V.Vector (MVCD a)
type VI = Vector Int
type WMap = IntMap VCD

-- | Transform direction: 'Forward' is the normal FFT, 'Inverse' is
-- inverse FFT.
data Direction = Forward | Inverse deriving (Eq, Show)

-- | A FFT plan.  This depends only on the problem size and can be
-- pre-computed and reused to transform (and inverse transform) any
-- number of vectors of the given size.
data Plan = Plan { plWMap :: WMap
                   -- ^ Powers of roots of unity: @plWMap IM.! n@ is a
                   -- vector of values of @omega_n^i@ for @0 <= i < n@.
                 , plDLInfo :: V.Vector (Int, Int, VVVCD, VVVCD)
                   -- ^ Size information and diagonal matrix entries
                   -- for Danielson-Lanczos recursive decomposition of
                   -- problem size.
                 , plPermute :: Maybe VI
                   -- ^ Input vector permutation to use before base
                   -- transformation and recursive Danielson-Lanczos
                   -- composition.
                 , plBase :: BaseTransform
                   -- ^ Base transformation used for each sub-vector
                   -- before performing recursive Danielson-Lanczos
                   -- steps to form the full FFT result.
                 } deriving (Eq, Show)

-- | A "base transform" used at the "bottom" of the recursive
-- Cooley-Tukey decomposition of the input problem size: either a
-- simple DFT, a special hard-coded small problem size case, or a
-- Rader prime-length FFT invocation.
data BaseTransform = SpecialBase { baseSize :: Int }
                     -- ^ Hard-coded small-size base transform.
                   | DFTBase { baseSize :: Int }
                     -- ^ Simple DFT base transform.
                   | RaderBase { baseSize :: Int
                               , raderInPerm :: VI
                               , raderOutPerm :: VI
                               , raderBFwd :: VCD
                               , raderBInv :: VCD
                               , raderConvSize :: Int
                               , raderConvPlan :: Plan }
                     -- ^ Prime-length Rader FFT base transform,
                     -- giving problem size, input and output index
                     -- permutations, pre-transformed Rader b sequence
                     -- for forward and inverse problems, padded
                     -- problem size for Rader sequence convolution
                     -- and a (2^N-sized) sub-plan for computing the
                     -- Rader convolution.
                     deriving (Eq, Show)
