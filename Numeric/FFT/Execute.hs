module Numeric.FFT.Execute ( execute ) where

import Prelude hiding (concatMap, foldr, length, map, null, zipWith)
import qualified Prelude as P
import Data.Complex
import Data.Vector
import qualified Data.IntMap.Strict as IM

import Numeric.FFT.Types
import Numeric.FFT.Utils
import Numeric.FFT.Base


-- | Main FFT plan execution driver.
execute :: Plan -> Direction -> VCD -> VCD
execute (Plan wmap dlinfo perm base) dir xs =
  if n == 1 then xs
  else map ((scale :+ 0) *) $ if null dlinfo
                              then applyBase wmap base sign xs
                              else fullfft
  where
    n = length xs             -- Input vector length.
    bsize = baseSize base     -- Size of base transform.

    -- Root of unity sign and output scale.
    (sign, scale) = case dir of
      Forward -> (1, 1.0)
      Inverse -> (-1, 1.0 / fromIntegral n)

    -- Compose all Danielson-Lanczos steps and base transform.
    recomb = foldr (.) multBase $ map (dl wmap sign) dlinfo

    -- Apply Danielson-Lanczos steps and base transform to digit
    -- reversal ordered input vector.
    fullfft = recomb $ backpermute xs perm

    -- Multiple base transform application for "bottom" of algorithm.
    multBase :: VCD -> VCD
    multBase xm = concatMap (applyBase wmap base sign) $ slicevecs bsize xm


-- | Single Danielson-Lanczos step: process all duplicates and
-- concatenate into a single vector.
dl :: WMap -> Int -> (Int, Int) -> VCD -> VCD
dl wmap sign (wfac, split) xs = concatMap doone $ slicevecs wfac xs
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
