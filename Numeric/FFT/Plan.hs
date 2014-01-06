module Numeric.FFT.Plan ( plan ) where

import Prelude hiding (concatMap, enumFromTo, length, map, null, reverse,
                       scanl, zip, zipWith)
import qualified Prelude as P
import qualified Data.IntMap.Strict as IM
import Data.List (nub)
import qualified Data.Vector as V
import Data.Vector.Unboxed

import Numeric.FFT.Types
import Numeric.FFT.Execute
import Numeric.FFT.Utils
import Numeric.FFT.Special


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
    vwfacs = convert wfacs
    vfs = convert fs
    dmatps = V.zipWith (dmat 1) vwfacs vfs
    dmatms = V.zipWith (dmat (-1)) vwfacs vfs
    dlinfo = V.reverse $ V.zip4 vwfacs vfs dmatps dmatms

    -- Calculate diagonal matrix entries used in Danielson-Lanczos steps.
    dmat :: Int -> Int -> Int -> VVVCD
    dmat sign wfac split =
      let ns = wfac `div` split
          w = omega $ sign * wfac
      in V.generate split $
         \r -> V.generate split $
           \c -> map (w^(ns*r*c) *) $ map ((w^^) . (c *)) $ enumFromN 0 ns

    -- Base transform.
    base = makeBase lastf

    -- Map collecting all root of unity powers needed for transform
    -- calculations of this size.
    wmap = makeWMap $ lastf `cons` wfacs


-- | Make base transform for a given sub-problem size.
makeBase :: Int -> BaseTransform
makeBase sz
  | sz `IM.member` specialBases = SpecialBase sz
  | isPrime sz                  = makeRaderBase sz
  | otherwise                   = DFTBase sz


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
digrev :: Int -> VI -> Maybe VI
digrev n fs
  | null fs   = Nothing
  | otherwise = Just $ V.foldl1' (%.%) $ V.map dupperm subperms
  where
    vfs = convert fs

    -- Sizes of the individual permutations that we need, one per
    -- factor.
    sizes = V.scanl div n vfs

    -- Partial sub-permutations, one per factor.
    subperms = V.reverse $ V.zipWith perm sizes vfs

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


-- | Pre-compute plan for prime-length Rader FFT transform.
makeRaderBase :: Int -> BaseTransform
makeRaderBase sz = RaderBase sz inperm outperm bpad bpadinv csz (plan csz)
  where
    -- Convolution length.
    sz1 = sz - 1

    -- Convolution length padded to next greater power of two.
    csz = if sz1 == 2^(log2 sz1)
          then sz1
          else 2 ^ (1 + log2 (2 * sz1 - 3))

    -- Group generator and inverse group generator.
    g = primitiveRoot sz
    ig = invModN sz g

    -- Input value permutation according to group generator indexing.
    inperm = iterateN sz1 (\n -> (g * n) `mod` sz) 1

    -- Index vector based on inverse group generator ordering.
    outperm = iterateN sz1 (\n -> (ig * n) `mod` sz) 1

    -- Root of unity powers based on inverse group generator indexing,
    -- for forward and inverse transform.
    w = omega sz
    bs = backpermute (map (w ^^) $ enumFromTo 0 sz1) outperm
    bsinv = backpermute (map ((w ^^) . negate) $ enumFromTo 0 sz1) outperm

    -- Root of unity powers cyclically repeated to make vector of next
    -- power of two length for fast convolution and FFT transformed
    -- for use in convolution, one for forward transform and one for
    -- inverse transform.
    bpad = fft $ generate csz (\idx -> bs ! (idx `mod` sz1))
    bpadinv = fft $ generate csz (\idx -> bsinv ! (idx `mod` sz1))

    -- Forward FFT with embedded plan calculation.
    fft xs = execute (plan $ length xs) Forward xs
