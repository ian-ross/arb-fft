module Numeric.FFT.Execute ( execute ) where

import Prelude hiding (concatMap, foldr, length, map, mapM_,
                       null, reverse, sum, zip, zipWith)
import qualified Prelude as P
import Control.Monad.ST
import Control.Monad.Primitive (PrimMonad)
import Data.Complex
import Data.STRef
import qualified Data.Vector as V
import Data.Vector.Unboxed
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M

import Numeric.FFT.Types
import Numeric.FFT.Utils
import Numeric.FFT.Special


-- | Main FFT plan execution driver.
execute :: Plan -> Direction -> VCD -> VCD
execute (Plan dlinfo perm base) dir h =
  if n == 1 then h else rescale $
                        if V.null dlinfo
                        then runST $ do
                          mhin <- thaw $ case perm of
                            Nothing -> h
                            Just p -> backpermute h p
                          mhout <- MV.replicate n 0
                          applyBase base sign mhin mhout
                          unsafeFreeze mhout
                        else fullfft
  where
    n = length h              -- Input vector length.
    bsize = baseSize base     -- Size of base transform.

    -- Root of unity sign and output rescaling.
    (sign, rescale) = case dir of
      Forward -> (1, id)
      Inverse -> (-1, map ((1.0 / fromIntegral n :+ 0) *))

    -- Apply Danielson-Lanczos steps and base transform to digit
    -- reversal ordered input vector.
    fullfft = runST $ do
      mhin <- thaw $ case perm of
            Nothing -> h
            Just p -> backpermute h p
      mhtmp <- MV.replicate n 0
      multBase mhin mhtmp
      mhr <- newSTRef (mhtmp, mhin)
      V.forM_ dlinfo $ \dlstep -> do
        (mh0, mh1) <- readSTRef mhr
        dl sign dlstep mh0 mh1
        writeSTRef mhr (mh1, mh0)
      mhs <- readSTRef mhr
      unsafeFreeze $ fst mhs

    -- Multiple base transform application for "bottom" of algorithm.
    multBase :: MVCD s -> MVCD s -> ST s ()
    multBase xmin xmout =
      V.zipWithM_ (applyBase base sign)
                  (slicemvecs bsize xmin) (slicemvecs bsize xmout)


-- | Single Danielson-Lanczos step: process all duplicates and
-- concatenate into a single vector.
dl :: Int -> (Int, Int, VVVCD, VVVCD) -> MVCD s -> MVCD s -> ST s ()
dl sign (wfac, split, dmatp, dmatm) mhin mhout =
  V.zipWithM_ doone (slicemvecs wfac mhin) (slicemvecs wfac mhout)
  where
    -- Twiddled diagonal entries in row r, column c (both
    -- zero-indexed), where each row and column if a wfac x wfac
    -- matrix.
    dmat = if sign == 1 then dmatp else dmatm
    d r c = (dmat V.! r) V.! c

    -- Size of each diagonal sub-matrix.
    ns = wfac `div` split

    -- Process one duplicate by processing all rows and writing the
    -- results into a single output vector.
    doone :: MVCD s -> MVCD s -> ST s ()
    doone vin vout = do
      let vs = (slicemvecs ns vin, slicemvecs ns vout)
      mapM_ (single vs) $ enumFromN 0 split
      where
        -- Multiply a single block by its appropriate diagonal
        -- elements and accumulate the result.
        mult :: VMVCD s -> MVCD s -> Int -> Bool -> Int -> ST s ()
        mult vins vo r first c = do
          let vi = vins V.! c
              dvals = d r c
          forM_ (enumFromN 0 ns) $ \i -> do
            xi <- MV.read vi i
            xo <- if first then return 0 else MV.read vo i
            MV.write vo i (xo + xi * dvals ! i)
        -- Multiply all blocks by the corresponding diagonal
        -- elements in a single row.
        single :: (VMVCD s, VMVCD s) -> Int -> ST s ()
        single (vis, vos) r =
          let m = mult vis (vos V.! r) r
          in do
            m True 0
            mapM_ (m False) $ enumFromN 1 (split-1)


-- | Apply a base transform to a single vector.
applyBase :: BaseTransform -> Int -> MVCD s -> MVCD s -> ST s ()

-- Simple DFT algorithm.
applyBase (DFTBase sz wsfwd wsinv) sign mhin mhout = do
  h <- freeze mhin
  forM_ (enumFromN 0 sz) $ \i -> MV.write mhout i (doone h i)
  where ws = if sign == 1 then wsfwd else wsinv
        doone h i = sum $ zipWith (*) h $
                    generate sz (\k -> ws ! (i * k `mod` sz))

-- Special hard-coded cases.
applyBase (SpecialBase sz) sign mhin mhout =
  case IM.lookup sz specialBases of
    Just f -> f sign mhin mhout
    Nothing -> error "invalid problem size for SpecialBase"

-- Rader prime-length FFT.
applyBase rader sign mhin mhout = do
  h <- freeze mhin
  let tmp = raderWork rader sign h
  forM_ (enumFromN 0 (length h)) $ \i -> MV.write mhout i (tmp ! i)

raderWork (RaderBase sz outperm bfwd binv convsz convplan) sign h =
  generate sz $ \i -> case i of
    0 -> sum h
    _ -> h ! 0 + convmap M.! i
  where
    -- Padding size.
    pad = convsz - (sz - 1)

    -- Permuted input vector padded to next greater power of two size
    -- for fast convolution.
    apad = generate convsz $
           \i -> if i == 0 then h ! 1
                 else if i > pad then h ! (i - pad + 1) else 0

    -- FFT-based convolution calculation.
    conv = execute convplan Inverse $
           zipWith (*) (execute convplan Forward apad)
                       (if sign == 1 then bfwd else binv)

    -- Map constructed to enable inversion of inverse group generator
    -- index ordering for output.
    convmap = M.fromList $ toList $ zip outperm conv
