module Numeric.FFT.Utils
       ( omega, slicevecs, slicemvecs, primes, isPrime
       , allFactors, factors
       , primitiveRoot, invModN, log2, isPow2, dupperm, (%.%)
       , compositions, makeComp, multisetPerms
       ) where

import Prelude hiding (all, concatMap, dropWhile, enumFromTo,
                       filter, head, length, map, maximum, null, reverse)
import qualified Prelude as P
import Data.Bits
import Data.Complex
import Data.Vector.Unboxed
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.List (nub)
import qualified Data.List as L

import Numeric.FFT.Types


-- | Roots of unity.
omega :: Int -> Complex Double
omega n = cis (2 * pi / fromIntegral n)

-- | Slice a vector @v@ into equally sized parts, each of length @m@.
slicevecs :: Int -> VCD -> VVCD
slicevecs m v = V.map (\i -> slice (i * m) m v) $
                V.enumFromN 0 (length v `div` m)

-- | Slice a mutable vector @v@ into equally sized parts, each of
-- length @m@.
slicemvecs :: Int -> MVCD a -> VMVCD a
slicemvecs m v = V.map (\i -> MV.slice (i * m) m v) $
                 V.enumFromN 0 (MV.length v `div` m)

-- | Determine primitive roots modulo n.
--
-- From Wikipedia (https://en.wikipedia.org/wiki/Primitive_root_modulo_n):
--
-- No simple general formula to compute primitive roots modulo n is
-- known.  There are however methods to locate a primitive root that
-- are faster than simply trying out all candidates.  If the
-- multiplicative order of a number m modulo n is equal to phi(n) (the
-- order of Z_n^x), then it is a primitive root.  In fact the converse
-- is true: if m is a primitive root modulo n, then the multiplicative
-- order of m is phi(n).  We can use this to test for primitive roots.
-- [Here, phi(n) is Euler's totient function, and Z_n^x is the
-- multiplicative group of integers modulo n.]
--
-- First, compute phi(n).  Then determine the different prime factors
-- of phi(n), say p1, ..., pk.  Now, for every element m of Z_n^x,
-- compute
--
--    m^(phi(n) / pi) mod n   for i = 1, ..., k
--
-- using a fast algorithm for modular exponentiation such as
-- exponentiation by squaring.  A number m for which these k results
-- are all different from 1 is a primitive root.
--
-- [In our case, n is restricted to being prime, and phi(p) = p - 1
-- for prime p.]
--
primitiveRoot :: Int -> Int
primitiveRoot p
  | isPrime p =
    let tot = p - 1
        -- ^ Euler's totient function for prime values.
        totpows = map (tot `div`) $ fromList $ nub $ toList $ allFactors tot
        -- ^ Powers to check.
        check n = all (/=1) $ map (expt p n) totpows
        -- ^ All powers are different from 1 => primitive root.
    in fromIntegral $ head $ dropWhile (not . check) $ fromList [1..p-1]
  | otherwise = error "Attempt to take primitive root of non-prime value"

-- | Fast exponentation modulo n by squaring.
expt :: Int -> Int -> Int -> Int
expt n b pow = fromIntegral $ go pow
  where bb = fromIntegral b
        nb = fromIntegral n
        go :: Int -> Integer
        go p
          | p == 0 = 1
          | p `mod` 2 == 1 = (bb * go (p - 1)) `mod` nb
          | otherwise = let h = go (p `div` 2) in (h * h) `mod` nb

-- | Find inverse element in multiplicative integer group modulo n.
invModN :: Int -> Int -> Int
invModN n g = head $ filter (\iv -> (g * iv) `mod` n == 1) $ enumFromTo 1 (n-1)

-- | Prime sieve from Haskell wiki.
primes :: Integral a => [a]
primes = 2 : primes'
  where primes' = sieve [3, 5 ..] 9 primes'
        sieve (x:xs) q ps@ ~(p:t)
          | x < q = x : sieve xs q ps
          | True  =     sieve [n | n <- xs, rem n p /= 0] (P.head t^2) t

-- | Naive primality testing.
isPrime :: Integral a => a -> Bool
isPrime n = n `P.elem` P.takeWhile (<= n) primes

-- | Simple prime factorisation.
allFactors :: (Integral a, Unbox a) => a -> Vector a
allFactors n = fromList $ go n primes
  where go cur pss@(p:ps)
          | cur == p         = [p]
          | cur `mod` p == 0 = p : go (cur `div` p) pss
          | otherwise        = go cur ps

-- | Simple prime factorisation: small factors only; largest/last
-- factor picked out as "special".
factors :: (Integral a, Unbox a) => a -> (a, Vector a)
factors n = let (lst, rest) = go n primes in (lst, fromList rest)
  where go cur pss@(p:ps)
          | cur == p         = (p, [])
          | cur `mod` p == 0 = let (lst, rest) = go (cur `div` p) pss
                               in (lst, p : rest)
          | otherwise        = go cur ps

-- | Base-2 logarithm.
log2 :: Int -> Int
log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

-- | Check for powers of two.
isPow2 :: Int -> Bool
isPow2 1 = True
isPow2 n
  | n `mod` 2 == 0 = isPow2 $ n `div` 2
  | otherwise      = False

-- | Duplicate a sub-permutation to fill a given vector length.
dupperm :: Int -> VI -> VI
dupperm n p =
  let sublen = length p
      shift di = map (+(sublen * di)) p
  in concatMap shift $ enumFromN 0 (n `div` sublen)

-- | Composition of permutations.
(%.%) :: VI -> VI -> VI
p1 %.% p2 = backpermute p2 p1

-- | Generate all compositions of a given integer.
compositions :: Int -> V.Vector (Vector Int)
compositions 0 = V.empty
compositions n = let fs = allFactors n
                 in V.reverse $ V.map (makeComp fs) $ V.enumFromN 0 (2^(n-1))

-- | Generate a single composition of a given integer.
makeComp :: Vector Int -> Int -> Vector Int
makeComp fs i = fromList $ foldOps (toList fs) $ makeOps (length fs) i
  where foldOps :: [Int] -> [Bool] -> [Int]
        foldOps (f:fs) ops = go f fs ops
          where go acc [] [] = [acc]
                go acc (f:fs) (op:ops) = if op then go (acc * f) fs ops
                                         else acc : go f fs ops
        makeOps :: Int -> Int -> [Bool]
        makeOps n i = P.replicate (n - 1 - P.length bs) False P.++ bs
          where bs = P.dropWhile not $ P.reverse $
                     P.map (testBit i) [0..bitSize i-1]

-- | Generate all distinct permutations of a multiset in lexicographic
-- order.
multisetPerms :: Vector Int -> [Vector Int]
multisetPerms idp = sidp : L.unfoldr step sidp
  where sidp = fromList $ L.sort $ toList idp
        step v = case permStep v of
          Nothing -> Nothing
          Just p -> Just (p, p)
        permStep :: Vector Int -> Maybe (Vector Int)
        permStep v =
          if null ks
          then Nothing
          else let k = maximum ks
                   ls = filter (\i -> v ! k < v ! i) $ enumFromN 0 n
                   l = maximum ls
               in Just $ revEnd k (swap k l)
          where n = length v
                ks = filter (\i -> v ! i < v ! (i+1)) $ enumFromN 0 (n-1)
                swap a b = generate n $ \i ->
                  if i == a then v ! b
                  else if i == b then v ! a
                       else v ! i
                revEnd f vv = generate n $ \i ->
                  if i <= f
                  then vv ! i
                  else vv ! (n - i + f)
