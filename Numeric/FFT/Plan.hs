module Numeric.FFT.Plan ( plan, planFromFactors ) where

import qualified Control.Monad          as CM
import           Control.Monad.IO.Class
import           Criterion.Measurement  (measure)
import           Criterion.Types        hiding (measure)
import           Data.Complex
import           Data.Function          (on)
import qualified Data.IntMap.Strict     as IM
import           Data.List              ((\\))
import qualified Data.List              as L
import qualified Data.Set               as S
import qualified Data.Vector            as V
import           Data.Vector.Unboxed
import           Prelude                hiding (any, concatMap, enumFromTo,
                                         filter, length, map, maximum, null,
                                         reverse, scanl, sum, zip, zipWith,
                                         (++))
import qualified Prelude                as P
import           System.Directory
import           System.Environment
import           System.FilePath

import           Numeric.FFT.Execute
import           Numeric.FFT.Special
import           Numeric.FFT.Types
import           Numeric.FFT.Utils


-- | Number of plans to test empirically.
nTestPlans :: Int
nTestPlans = 50

-- | Plan calculation for a given problem size.
plan :: Int -> IO Plan
plan 1 = return $ Plan V.empty Nothing (SpecialBase 1)
plan n = do
  wis <- readWisdom n
  let fixRader p = case plBase p of
        bpl@(RaderBase _ _ _ _ csz _) -> do
          cplan <- liftIO $ plan csz
          return $ p { plBase = bpl { raderConvPlan = cplan } }
        _ -> return p
  pret <- case wis of
    Just (p, _) -> planFromFactors n p
    Nothing -> do
      let ps = testPlans n nTestPlans
          v = generate n (\i -> sin (2 * pi * fromIntegral i / 511) :+ 0)
      tps <- CM.forM ps $ \p -> do
        ptest <- liftIO $ planFromFactors n p >>= fixRader
        let niters = fromIntegral $ 50000 `div` n
        (meas, _) <- measure (nf (execute ptest Forward) v) niters
        let t = measTime meas / fromIntegral (measIters meas)
            _iters = measIters meas
        return (t, p)
      let (rest, resp) = L.minimumBy (compare `on` fst) tps
      liftIO $ writeWisdom n resp rest
      liftIO $ planFromFactors n resp
  fixRader pret

-- | Get execution time for plan for a given problem size.
planTime :: Int -> IO Double
planTime n = do
  wis <- readWisdom n
  case wis of
    Just (_, t) -> return t
    Nothing -> do
      -- Force generation of wisdom if it's not already there.
      _ <- plan n
      Just (_, t) <- readWisdom n
      return t

-- | Plan calculation for a given problem factorisation.
planFromFactors :: Int -> (Int, Vector Int) -> IO Plan
planFromFactors n (lastf, fs) = do
  -- Base transform.
  (base, mextraperm) <- makeBase lastf

  -- Include permutation of base transform if needed.
  let perm = case (digperm, mextraperm) of
        (Just dp, Just ep) -> Just $ dupperm n ep %.% dp
        (Nothing, Just ep) -> Just $ dupperm n ep
        (Just dp, Nothing) -> Just dp
        (Nothing, Nothing) -> Nothing

  return $ Plan dlinfo perm base
  where
    -- Input data "digit reversal" permutation.
    digperm = digrev n fs

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
           \c -> map ((w^(ns*r*c) *) . ((w^^) . (c *))) $ enumFromN 0 ns

-- | Read from wisdom for a given problem size.
readWisdom :: Int -> IO (Maybe ((Int, Vector Int), Double))
readWisdom n = do
  home <- getEnv "HOME"
  let wisf = home </> ".fft-plan" </> show n
  ex <- doesFileExist wisf
  if ex then
    (do wist <- readFile wisf
        let ((wisb, wisfs), wistim) = read wist :: ((Int, [Int]), Double)
        return $ Just ((wisb, fromList wisfs), wistim))
    else return Nothing

-- | Write wisdom for a given problem size.
writeWisdom :: Int -> (Int, Vector Int) -> Double -> IO ()
writeWisdom n (b, fs) tim = do
  home <- getEnv "HOME"
  let wisd = home </> ".fft-plan"
      wisf = wisd </> show n
  createDirectoryIfMissing True wisd
  writeFile wisf $ show ((b, toList fs), tim) P.++ "\n"

-- | Make base transform for a given sub-problem size.
makeBase :: Int -> IO (BaseTransform, Maybe VI)
makeBase sz
  | sz `IM.member` specialBases = return (SpecialBase sz, Nothing)
  | isPrime sz                  = makeRaderBase sz
  | otherwise                   = return (makeDFTBase sz, Nothing)

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
makeRaderBase :: Int -> IO (BaseTransform, Maybe VI)
makeRaderBase sz = do
  -- Forward FFT with embedded plan calculation.
  let fft p xs = execute p Forward xs

  -- Times for unpadded and padded convolution transforms.
  unpadtime <- planTime sz1
  padtime <- planTime pow2sz

  -- Should we use a padded or an unpadded transform for the
  -- convolution?
  let pad = padtime < unpadtime
      csz = if pad then pow2sz else sz1

  -- Plan for convolution transforms.
  cplan <- plan csz

  -- FFT transforms of b sequences for use in convolution, one for
  -- forward transform and one for inverse transform.  Either just the
  -- basic root of unity powers for the convolution (if we're not
  -- padding), or the powers cyclically repeated to make a vector of
  -- the next power-of-two length.
  let convb =
        fft cplan $ if pad
                    then generate csz (\idx -> bs ! (idx `mod` sz1))
                    else bs
      convbinv =
        fft cplan $ if pad
                    then generate csz (\idx -> bsinv ! (idx `mod` sz1))
                    else bsinv

  return (RaderBase sz outperm convb convbinv csz cplan, Just inperm)
  where
    -- Convolution length.
    sz1 = sz - 1

    -- Convolution length padded to next greater power of two.
    pow2sz = if sz1 == 2^ log2 sz1
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

-- | Base transform type with heuristic ordering.
data BaseType = Special Int | Rader Int deriving (Eq, Show)

-- | Newtype wrapper for custom sorting.
newtype SPlan = SPlan (BaseType, Vector Int) deriving (Eq, Show)

-- | Base transform size.
bSize :: BaseType -> Int
bSize (Special b) = b
bSize (Rader b)   = b

-- | Heuristic ordering for base transform types: special bases come
-- first, then prime bases using Rader's algorithm, ordered according
-- to size compensating for padding needed in the Rader's algorithm
-- convolution.
instance Ord BaseType where
  compare (Special _)  (Rader _)    = LT
  compare (Rader _)    (Special _)  = GT
  compare (Special s1) (Special s2) = compare s2 s1
  compare (Rader r1)   (Rader r2)   = case (isPow2 $ r1 - 1, isPow2 $ r2 - 1) of
    (True, True)   -> compare r1 r2
    (True, False)  -> compare r1 (2 * r2)
    (False, True)  -> compare (2 * r1) r2
    (False, False) -> compare r1 r2

-- | Heuristic ordering for full plans, based first on base type, then
-- on the maximum size of Danielson-Lanczos step.
instance Ord SPlan where
  compare (SPlan (b1, fs1)) (SPlan (b2, fs2)) = case compare b1 b2 of
    LT -> LT
    EQ -> compare (maximum fs2) (maximum fs1)
    GT -> GT

-- | Generate test plans for a given input size, sorted in heuristic
-- order.
testPlans :: Int -> Int -> [(Int, Vector Int)]
testPlans n nplans = L.take nplans $ L.map clean $ L.sort okplans
  where vfs = allFactors n
        bs = usableBases n vfs
        doone b = basePlans n vfs b
        clean (SPlan (b, fs)) = (bSize b, fs)
        allplans = P.concatMap doone bs
        okplans = case L.filter (not . ridiculous) allplans of
          []  -> L.filter (not . reallyRidiculous) allplans
          oks -> oks
        ridiculous (SPlan (_, fs)) = any (> 128) fs
        reallyRidiculous (SPlan (_, fs)) =
          any (> 128) $ filter (not . isPrime) fs

-- | List plans from a single base.
basePlans :: Int -> Vector Int -> BaseType -> [SPlan]
basePlans _n vfs bt = if null lfs
                     then [SPlan (bt, empty)]
                     else P.map (\v -> SPlan (bt, v)) $ leftOvers lfs
  where lfs = fromList $ toList vfs \\ toList (allFactors b)
        b = bSize bt

-- | Produce all distinct permutations and compositions constructable
-- from a given list of factors.
leftOvers :: Vector Int -> [Vector Int]
leftOvers fs =
  if null fs
  then []
  else S.toList $ L.foldl' go S.empty (multisetPerms fs)
  where n = length fs
        go fset perm = foldl' doone fset (enumFromN 0 (2^(n - 1)))
          where doone s i = S.insert (makeComp perm i) s

-- | Usable base transform sizes.
usableBases :: Int -> Vector Int -> [BaseType]
usableBases n fs = P.map Special bs P.++ P.map Rader ps
  where bs = toList $ filter ((== 0) . (n `mod`)) specialBaseSizes
        ps = toList $ filter isPrime $ filter (> maxPrimeSpecialBaseSize) fs
