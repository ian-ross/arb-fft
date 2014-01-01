module Numeric.FFT.Plan ( plan ) where

import Prelude hiding (concatMap, length, map, reverse, scanl, zip, zipWith)
import qualified Prelude as P
import qualified Data.IntMap.Strict as IM
import Data.List (nub)
import Data.Vector

import Numeric.FFT.Types
import Numeric.FFT.Utils


-- | Plan calculation for a given problem size.
plan :: Int -> Plan
plan n = Plan wmap dlinfo perm base
  where
    -- Factorise input vector length.
    (lastf, fs) = factors n

    -- Input data "digit reversal" permutation.
    perm = digrev n fs

    -- Size information for Danielson-Lanczos steps.
    wfacs = map (n `div`) $ scanl (*) 1 fs
    dlinfo = zip wfacs fs

    -- Base transform.
    -- TODO: SPLIT INTO "SPECIAL", RADER AND ORDINARY.
    base = DFTBase lastf

    -- Map collecting all root of unity powers needed for transform
    -- calculations of this size.
    wmap = makeWMap $ lastf `cons` wfacs


-- | Make integer map giving all powers of roots of unity needed for a
-- particular plan (for both forward and inverse FFTs).
makeWMap :: VI -> WMap
makeWMap ss = IM.fromList $ P.concatMap wvec $ nub $ toList ss
  where wvec n = let w = omega n
                     ws = generate n (w ^)
                 in [(n, ws), (-n, map (1 /) ws)]


-- | Generate digit reversal permutation using elementary "modulo"
-- permutations: last digit is not permuted to match with using a
-- simple DFT or instance of Rader's algorithm at the "bottom" of the
-- overall algorithm.
digrev :: Int -> VI -> VI
digrev n fs = foldl1' (%.%) $ map dupperm subperms
  where
    -- Sizes of the individual permutations that we need, one per
    -- factor.
    sizes = scanl div n fs

    -- Partial sub-permutations, one per factor.
    subperms = reverse $ zipWith perm sizes fs

    -- Duplicate a sub-permutation to fill the required vector length.
    dupperm p =
      let sublen = length p
          shift di = map (+(sublen * di)) p
      in concatMap shift $ enumFromN 0 (n `div` sublen)

    -- Generate a single "modulo" permutation.
    perm sz fac = concatMap doone $ enumFromN 0 fac
      where doone i = generate (sz `div` fac) (\j -> j * fac + i)

    -- Composition of permutations.
    (%.%) :: VI -> VI -> VI
    p1 %.% p2 = backpermute p2 p1
