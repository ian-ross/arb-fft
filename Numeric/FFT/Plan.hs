module Numeric.FFT.Plan ( plan, planFromFactors ) where

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
plan n = planFromFactors n $ factors n

-- | Plan calculation for a given problem factorisation.
planFromFactors :: Int -> (Int, Vector Int) -> Plan
planFromFactors n (lastf, fs) = Plan dlinfo perm base
  where
    -- Input data "digit reversal" permutation.
    digperm = digrev n fs

    -- Include permutation of base transform if needed.
    perm = case (digperm, mextraperm) of
      (Just dp, Just ep) -> Just $ dupperm n ep %.% dp
      (Nothing, Just ep) -> Just $ dupperm n ep
      (Just dp, Nothing) -> Just dp
      (Nothing, Nothing) -> Nothing

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
    (base, mextraperm) = makeBase lastf

-- | Make base transform for a given sub-problem size.
makeBase :: Int -> (BaseTransform, Maybe VI)
makeBase sz
  | sz `IM.member` specialBases = (SpecialBase sz, Nothing)
  | isPrime sz                  = makeRaderBase sz
  | otherwise                   = (makeDFTBase sz, Nothing)

-- | Generate digit reversal permutation using elementary "modulo"
-- permutations: last digit is not permuted to match with using a
-- simple DFT or instance of Rader's algorithm at the "bottom" of the
-- overall algorithm.
digrev :: Int -> VI -> Maybe VI
digrev n fs
  | null fs   = Nothing
  | otherwise = Just $ V.foldl1' (%.%) $ V.map (dupperm n) subperms
  where
    vfs = convert fs

    -- Sizes of the individual permutations that we need, one per
    -- factor.
    sizes = V.scanl div n vfs

    -- Partial sub-permutations, one per factor.
    subperms = V.reverse $ V.zipWith perm sizes vfs

    -- Generate a single "modulo" permutation.
    perm sz fac = concatMap doone $ enumFromN 0 fac
      where doone i = generate (sz `div` fac) (\j -> j * fac + i)

-- | Pre-computation plan for basic DFT transform.
makeDFTBase :: Int -> BaseTransform
makeDFTBase sz = DFTBase sz wsfwd wsinv
  where w = omega sz
        wsfwd = generate sz (w ^)
        wsinv = map (1 /) wsfwd

-- | Pre-compute plan for prime-length Rader FFT transform.
makeRaderBase :: Int -> (BaseTransform, Maybe VI)
makeRaderBase sz = (RaderBase sz outperm bpad bpadinv csz (plan csz),
                    Just inperm)
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
    inperm = 0 `cons` iterateN sz1 (\n -> (g * n) `mod` sz) 1

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
