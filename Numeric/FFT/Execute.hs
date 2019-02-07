module Numeric.FFT.Execute ( execute ) where

import           Control.Monad               (when)
import qualified Control.Monad               as CM
import           Control.Monad.ST
import           Data.Complex
import qualified Data.IntMap.Strict          as IM
import           Data.STRef
import qualified Data.Vector                 as V
import           Data.Vector.Unboxed
import qualified Data.Vector.Unboxed.Mutable as MV
import           Prelude                     hiding (concatMap, foldr, length,
                                              map, mapM_, null, reverse, sum,
                                              zip, zipWith)

import           Numeric.FFT.Special
import           Numeric.FFT.Types
import           Numeric.FFT.Utils


-- | Main FFT plan execution driver.
execute :: Plan -> Direction -> VCD -> VCD
execute (Plan dlinfo perm base) dir h
  | n == 1 = h
  | V.null dlinfo = runST $ do
                          mhin <- case perm of
                            Nothing -> thaw h
                            Just p  -> unsafeThaw $ backpermute h p
                          mhout <- MV.replicate n 0
                          applyBase base sign mhin mhout
                          when (dir == Inverse) $ do
                            let s = 1.0 / fromIntegral n :+ 0
                            CM.forM_ [0..n-1] $ \i -> do
                              x <- MV.unsafeRead mhout i
                              MV.unsafeWrite mhout i $ s * x
                          unsafeFreeze mhout
  | otherwise=  fullfft
  where
    n = length h              -- Input vector length.
    bsize = baseSize base     -- Size of base transform.

    -- Root of unity sign.
    sign = case dir of
      Forward -> 1
      Inverse -> -1

    -- Apply Danielson-Lanczos steps and base transform to digit
    -- reversal ordered input vector.
    fullfft = runST $ do
      mhin <- case perm of
            Nothing -> thaw h
            Just p  -> unsafeThaw $ backpermute h p
      mhtmp <- MV.replicate n 0
      multBase mhin mhtmp
      mhr <- newSTRef (mhtmp, mhin)
      V.forM_ dlinfo $ \dlstep -> do
        (mh0, mh1) <- readSTRef mhr
        dl sign dlstep mh0 mh1
        writeSTRef mhr (mh1, mh0)
      mhs <- readSTRef mhr
      let vout = fst mhs
      when (dir == Inverse) $ do
        let s = 1.0 / fromIntegral n :+ 0
        CM.forM_ [0..n-1] $ \i -> do
          x <- MV.unsafeRead vout i
          MV.unsafeWrite vout i $ s * x
      unsafeFreeze vout

    -- Multiple base transform application for "bottom" of algorithm.
    multBase :: MVCD s -> MVCD s -> ST s ()
    multBase xmin xmout =
      V.zipWithM_ (applyBase base sign)
                  (slicemvecs bsize xmin) (slicemvecs bsize xmout)


-- | Monadic FFT plan execution driver -- used by Rader's algorithm
-- for convolutions.
executeM :: Plan -> Direction -> MVCD s -> MVCD s -> ST s ()
executeM (Plan dlinfo perm base) dir hin hout =
  if n == 1
  then MV.copy hout hin
  else do
    htmp <- MV.replicate n 0

    -- Input permutation.
    case perm of
      Nothing -> MV.copy htmp hin
      Just p  -> backpermuteM n p hin htmp

    -- Apply Danielson-Lanczos steps and base transform to digit
    -- reversal ordered input vector.
    multBase htmp hout
    mhr <- newSTRef (hout, htmp)
    V.forM_ dlinfo $ \dlstep -> do
      (mh0, mh1) <- readSTRef mhr
      dl sign dlstep mh0 mh1
      writeSTRef mhr (mh1, mh0)
    when (odd $ V.length dlinfo) $ MV.copy hout htmp

    -- Output scaling for inverse transform.
    when (dir == Inverse) $ do
      let s = 1.0 / fromIntegral n :+ 0
      forM_ (enumFromN 0 n) $ \i -> do
        x <- MV.unsafeRead hout i
        MV.unsafeWrite hout i $ s * x
  where
    n = MV.length hin         -- Input vector length.
    bsize = baseSize base     -- Size of base transform.

    -- Root of unity sign.
    sign = case dir of
      Forward -> 1
      Inverse -> -1

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

    -- Index vectors.
    nsidxs = enumFromN 0 ns
    splitidxs = enumFromN 1 (split-1)

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
          forM_ nsidxs $ \i -> do
            xi <- MV.unsafeRead vi i
            xo <- if first then return 0 else MV.unsafeRead vo i
            MV.unsafeWrite vo i (xo + xi * dvals ! i)
        -- Multiply all blocks by the corresponding diagonal
        -- elements in a single row.
        single :: (VMVCD s, VMVCD s) -> Int -> ST s ()
        single (vis, vos) r = do
          mult vis (vos V.! r) r True 0
          mapM_ (mult vis (vos V.! r) r False) splitidxs
        -- single (vis, vos) r =
        --   let m = mult vis (vos V.! r) r
        --   in do
        --     m True 0
        --     mapM_ (m False) splitidxs


-- | Apply a base transform to a single vector.
applyBase :: BaseTransform -> Int -> MVCD s -> MVCD s -> ST s ()

-- Simple DFT algorithm.
applyBase (DFTBase sz wsfwd wsinv) sign mhin mhout = do
  h <- freeze mhin
  forM_ (enumFromN 0 sz) $ \i -> MV.unsafeWrite mhout i (doone h i)
  where ws = if sign == 1 then wsfwd else wsinv
        doone h i = sum $ zipWith (*) h $
                    generate sz (\k -> ws ! (i * k `mod` sz))

-- Special hard-coded cases.
applyBase (SpecialBase sz) sign mhin mhout =
  case IM.lookup sz specialBases of
    Just f  -> f sign mhin mhout
    Nothing -> error "invalid problem size for SpecialBase"

-- Rader prime-length FFT.
applyBase (RaderBase sz outperm bfwd binv csz cplan) sign mhin mhout = do
  -- Padding size.
  let pad = csz - (sz - 1)

  -- Permuted input vector padded to next greater power of two size
  -- for fast convolution.
  apad <- MV.replicate csz 0
  forM_ (enumFromN 0 csz) $ \i -> do
    val <- if i == 0 then MV.unsafeRead mhin 1
           else if i > pad
                then MV.unsafeRead mhin $ i - pad + 1
                else return 0
    MV.unsafeWrite apad i val

  -- FFT-based convolution calculation.
  convtmp <- MV.replicate csz 0
  executeM cplan Forward apad convtmp
  let bmult = if sign == 1 then bfwd else binv
  forM_ (enumFromN 0 csz) $ \i -> do
    x <- MV.unsafeRead convtmp i
    MV.unsafeWrite convtmp i $ x * (bmult ! i)
  executeM cplan Inverse convtmp apad
  conv <- unsafeFreeze apad

  -- Input vector sum.
  sumhref <- newSTRef 0
  forM_ (enumFromN 0 sz) $ \i -> do
    val <- MV.unsafeRead mhin i
    modifySTRef sumhref (+ val)
  sumh <- readSTRef sumhref

  -- Write output based on output generator index ordering.
  h0 <- MV.unsafeRead mhin 0
  forM_ (enumFromN 0 sz) $ \i -> do
    let (idx, val) = case i of
          0 -> (0, sumh)
          _ -> (outperm ! (i - 1), h0 + conv ! (i - 1))
    MV.unsafeWrite mhout idx val
