module Numeric.FFT.Execute ( execute ) where

import Prelude hiding (concatMap, foldr, length, map, null, sum, zip, zipWith)
import qualified Prelude as P
import Data.Complex
import Data.Vector
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M

import Debug.Trace

import Numeric.FFT.Types
import Numeric.FFT.Utils
import Numeric.FFT.Special


-- | Main FFT plan execution driver.
execute :: Plan -> Direction -> VCD -> VCD
execute (Plan wmap dlinfo perm base) dir h =
  if n == 1 then h else rescale $ if null dlinfo
                                  then applyBase wmap base sign h
                                  else fullfft
  where
    n = length h              -- Input vector length.
    bsize = baseSize base     -- Size of base transform.

    -- Root of unity sign and output rescaling.
    (sign, rescale) = case dir of
      Forward -> (1, id)
      Inverse -> (-1, map ((1.0 / fromIntegral n :+ 0) *))

    -- Compose all Danielson-Lanczos steps and base transform.
    recomb = foldr (.) multBase $ map (dl wmap sign) dlinfo

    -- Apply Danielson-Lanczos steps and base transform to digit
    -- reversal ordered input vector.
    fullfft = recomb $ case perm of
      Nothing -> h
      Just p -> backpermute h p

    -- Multiple base transform application for "bottom" of algorithm.
    multBase :: VCD -> VCD
    multBase xm = concatMap (applyBase wmap base sign) $ slicevecs bsize xm


-- | Single Danielson-Lanczos step: process all duplicates and
-- concatenate into a single vector.
dl :: WMap -> Int -> (Int, Int) -> VCD -> VCD
dl wmap sign (wfac, split) h = concatMap doone $ slicevecs wfac h
  where
    -- Size of each diagonal sub-matrix.
    ns = wfac `div` split

    -- Roots of unity for the size of diagonal matrix we need.
    ws = wmap IM.! (sign * wfac)

    -- Basic diagonal entries for a given column.
    ds c = map ((ws !) . (`mod` wfac) . (c *)) $ enumFromN 0 ns

    -- Twiddled diagonal entries in row r, column c (both
    -- zero-indexed), where each row and column if a wfac x wfac
    -- matrix.
    d r c = map ((ws ! ((ns * r * c) `mod` wfac)) *) (ds c)

    -- Process one duplicate by processing all rows and concatenating
    -- the results into a single vector.
    doone v = concatMap single $ enumFromN 0 split
      where vs :: VVCD
            vs = slicevecs ns v
            -- Multiply a single block by its appropriate diagonal
            -- elements.
            mult :: Int -> Int -> VCD
            mult r c = zipWith (*) (d r c) (vs!c)
            -- Multiply all blocks by the corresponding diagonal
            -- elements in a single row.
            single :: Int -> VCD
            single r = foldl1' (zipWith (+)) $ map (mult r) $ enumFromN 0 split


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

-- Rader prime-length FFT.
applyBase wmap (RaderBase sz inperm outperm bfwd binv convsz convplan) sign h =
  generate sz $ \i -> case i of
    0 -> sum h
    _ -> h ! 0 + convmap M.! i
  where
    -- Input values permuted according to group generator indexing.
    as = backpermute h inperm

    -- Padding size.
    pad = convsz - (sz - 1)

    -- Permuted input vector padded to next greater power of two size
    -- for fast convolution.
    apad = generate convsz $
           \i -> if i == 0 then as ! 0
                 else if i > pad then as ! (i - pad) else 0

    -- FFT-based convolution calculation.
    conv = execute convplan Inverse $
           zipWith (*) (execute convplan Forward apad)
                       (if sign == 1 then bfwd else binv)

    -- Map constructed to enable inversion of inverse group generator
    -- index ordering for output.
    convmap = M.fromList $ toList $ zip outperm conv
