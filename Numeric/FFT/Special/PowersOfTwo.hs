module Numeric.FFT.Special.PowersOfTwo
       ( special2, special4, special8, special16, special32, special64
       ) where

import Control.Monad.ST
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Complex
import Data.Vector.Unboxed
import qualified Data.Vector.Unboxed.Mutable as MV

import Numeric.FFT.Types
import Numeric.FFT.Utils


-- | Length 2 hard-coded FFT.
special2 :: Int -> MVCD s -> MVCD s -> ST s ()
special2 _ xsin xsout = do
  a <- MV.unsafeRead xsin 0
  b <- MV.unsafeRead xsin 1
  MV.unsafeWrite xsout 0 $ a + b
  MV.unsafeWrite xsout 1 $ a - b

-- | Length 4 hard-coded FFT.
special4 :: Int -> MVCD s -> MVCD s -> ST s ()
special4 sign xsin xsout = do
  xr0 :+ xi0 <- MV.unsafeRead xsin 0 ; xr1 :+ xi1 <- MV.unsafeRead xsin 1
  xr2 :+ xi2 <- MV.unsafeRead xsin 2 ; xr3 :+ xi3 <- MV.unsafeRead xsin 3
  let tb = xr0 - xr2 ; t3 = xr0 + xr2 ; tf = xi0 + xi2 ; t9 = xi0 - xi2
      t6 = xr1 + xr3 ; ta = xr1 - xr3 ; te = xi1 - xi3 ; tg = xi1 + xi3
      r3 = (tb + te) :+ (t9 - ta)
      r2 = (t3 - t6) :+ (tf - tg)
      r1 = (tb - te) :+ (ta + t9)
  MV.unsafeWrite xsout 0 $ (t3 + t6) :+ (tf + tg)
  MV.unsafeWrite xsout 1 $ if sign == 1 then r1 else r3
  MV.unsafeWrite xsout 2 $ if sign == 1 then r2 else r2
  MV.unsafeWrite xsout 3 $ if sign == 1 then r3 else r1

-- | Length 8 hard-coded FFT.
kp707106781 :: Double
kp707106781 = 0.707106781186547524400844362104849039284835938
special8 :: Int -> MVCD s -> MVCD s -> ST s ()
special8 sign xsin xsout = do
  xr0 :+ xi0 <- MV.unsafeRead xsin 0 ; xr1 :+ xi1 <- MV.unsafeRead xsin 1
  xr2 :+ xi2 <- MV.unsafeRead xsin 2 ; xr3 :+ xi3 <- MV.unsafeRead xsin 3
  xr4 :+ xi4 <- MV.unsafeRead xsin 4 ; xr5 :+ xi5 <- MV.unsafeRead xsin 5
  xr6 :+ xi6 <- MV.unsafeRead xsin 6 ; xr7 :+ xi7 <- MV.unsafeRead xsin 7
  let tn = xr0 - xr4 ; t3 = xr0 + xr4 ; tC = xi0 - xi4 ; ti = xi0 + xi4
      tB = xr2 - xr6 ; t6 = xr2 + xr6 ; to = xi2 - xi6 ; tl = xi2 + xi6
      td = xr7 + xr3 ; tv = xr7 - xr3 ; tN = xi7 + xi3 ; ty = xi7 - xi3
      tz = tv - ty ; tH = tv + ty ; ta = xr1 + xr5 ; tq = xr1 - xr5
      tt = xi1 - xi5 ; tM = xi1 + xi5 ; tL = t3 - t6 ; t7 = t3 + t6
      tG = tt - tq ; tu = tq + tt ; te = ta + td ; tf = td - ta
      tm = ti - tl ; tP = ti + tl ; tQ = tM + tN ; tO = tM - tN
      tF = tn - to ; tp = tn + to ; tA = tu + tz ; tE = tz - tu
      tD = tB + tC ; tJ = tC - tB ; tK = tG + tH ; tI = tG - tH
      r7 = (tp + kp707106781 * tA) :+ (tJ + kp707106781 * tK)
      r6 = (tL + tO) :+ (tf + tm)
      r5 = (tF + kp707106781 * tI) :+ (tD + kp707106781 * tE)
      r4 = (t7 - te) :+ (tP - tQ)
      r3 = (tp - kp707106781 * tA) :+ (tJ - kp707106781 * tK)
      r2 = (tL - tO) :+ (tm - tf)
      r1 = (tF - kp707106781 * tI) :+ (tD - kp707106781 * tE)
  MV.unsafeWrite xsout 0 $ (t7 + te) :+ (tP + tQ)
  MV.unsafeWrite xsout 1 $ if sign == 1 then r1 else r7
  MV.unsafeWrite xsout 2 $ if sign == 1 then r2 else r6
  MV.unsafeWrite xsout 3 $ if sign == 1 then r3 else r5
  MV.unsafeWrite xsout 4 $ if sign == 1 then r4 else r4
  MV.unsafeWrite xsout 5 $ if sign == 1 then r5 else r3
  MV.unsafeWrite xsout 6 $ if sign == 1 then r6 else r2
  MV.unsafeWrite xsout 7 $ if sign == 1 then r7 else r1

-- | Length 16 hard-coded FFT.
kp923879532, kp414213562 :: Double
--kp707106781 :: Double
kp923879532 = 0.923879532511286756128183189396788286822416626
kp414213562 = 0.414213562373095048801688724209698078569671875
--kp707106781 = 0.707106781186547524400844362104849039284835938
special16 :: Int -> MVCD s -> MVCD s -> ST s ()
special16 sign xsin xsout = do
  xr0  :+ xi0  <- MV.unsafeRead xsin 0  ; xr1  :+  xi1 <- MV.unsafeRead xsin 1
  xr2  :+ xi2  <- MV.unsafeRead xsin 2  ; xr3  :+  xi3 <- MV.unsafeRead xsin 3
  xr4  :+ xi4  <- MV.unsafeRead xsin 4  ; xr5  :+  xi5 <- MV.unsafeRead xsin 5
  xr6  :+ xi6  <- MV.unsafeRead xsin 6  ; xr7  :+  xi7 <- MV.unsafeRead xsin 7
  xr8  :+ xi8  <- MV.unsafeRead xsin 8  ; xr9  :+  xi9 <- MV.unsafeRead xsin 9
  xr10 :+ xi10 <- MV.unsafeRead xsin 10 ; xr11 :+ xi11 <- MV.unsafeRead xsin 11
  xr12 :+ xi12 <- MV.unsafeRead xsin 12 ; xr13 :+ xi13 <- MV.unsafeRead xsin 13
  xr14 :+ xi14 <- MV.unsafeRead xsin 14 ; xr15 :+ xi15 <- MV.unsafeRead xsin 15
  let tL = xr0 - xr8 ; t3 = xr0 + xr8 ; t1k = xi0 - xi8 ; ty = xi0 + xi8
      t1j = xr4 - xr12 ; t6 = xr4 + xr12 ; tM = xi4 - xi12 ; tB = xi4 + xi12
      t1l = t1j + t1k ; t1H = t1k - t1j ; t1R = t3 - t6 ; t7 = t3 + t6
      t1x = tL + tM ; tN = tL - tM ; tC = ty + tB ; t25 = ty - tB
      t1c = xr15 - xr7 ; tp = xr15 + xr7 ; t20 = xi15 + xi7 ; t1a = xi15 - xi7
      t17 = xr3 - xr11 ; ts = xr3 + xr11 ; t21 = xi3 + xi11 ; t1f = xi3 - xi11
      t1E = t1a - t17 ; t1b = t17 + t1a ; t1Z = tp - ts ; tt = tp + ts
      t2h = t20 + t21 ; t22 = t20 - t21 ; t1D = t1c + t1f ; t1g = t1c - t1f
      tP = xr2 - xr10 ; ta = xr2 + xr10 ; tO = xi2 - xi10 ; tF = xi2 + xi10
      t1n = tP + tO ; tQ = tO - tP ; tR = xr14 - xr6 ; td = xr14 + xr6
      tS = xi14 - xi6 ; tI = xi14 + xi6 ; te = ta + td ; t26 = td - ta
      tT = tR + tS ; t1m = tR - tS ; tJ = tF + tI ; t1S = tF - tI
      t11 = xr1 - xr9 ; ti = xr1 + xr9 ; t1V = xi1 + xi9 ; tZ = xi1 - xi9
      t2f = t7 - te ; tf = t7 + te ; tW = xr5 - xr13 ; tl = xr5 + xr13
      t1W = xi5 + xi13 ; t14 = xi5 - xi13 ; t1B = tZ - tW ; t10 = tW + tZ
      t1U = ti - tl ; tm = ti + tl ; t2g = t1V + t1W ; t1X = t1V - t1W
      t1A = t11 + t14 ; t15 = t11 - t14 ; tu = tm + tt ; tv = tt - tm
      tK = tC - tJ ; t2j = tC + tJ ; t2k = t2g + t2h ; t2i = t2g - t2h
      t29 = t1R - t1S ; t1T = t1R + t1S ; t27 = t25 - t26 ; t2d = t26 + t25
      t2a = t1X - t1U ; t1Y = t1U + t1X ; t23 = t1Z - t22 ; t2b = t1Z + t22
      t28 = t23 - t1Y ; t24 = t1Y + t23 ; t1I = tQ + tT ; tU = tQ - tT
      t2e = t2a + t2b ; t2c = t2a - t2b
      tV = tN + kp707106781 * tU ; t1v = tN - kp707106781 * tU
      t1o = t1m - t1n ; t1y = t1n + t1m
      t1t = t15 - kp414213562 * t10 ; t16 = t10 + kp414213562 * t15
      t1h = t1b - kp414213562 * t1g ; t1s = t1g + kp414213562 * t1b
      t1r = t1l + kp707106781 * t1o ; t1p = t1l - kp707106781 * t1o
      t1q = t16 + t1h ; t1i = t16 - t1h ; t1w = t1t + t1s ; t1u = t1s - t1t
      t1z = t1x + kp707106781 * t1y ; t1L = t1x - kp707106781 * t1y
      t1M = t1B - kp414213562 * t1A ; t1C = t1A + kp414213562 * t1B
      t1F = t1D - kp414213562 * t1E ; t1N = t1E + kp414213562 * t1D
      t1P = t1H + kp707106781 * t1I ; t1J = t1H - kp707106781 * t1I
      t1K = t1F - t1C ; t1G = t1C + t1F ; t1O = t1M - t1N ; t1Q = t1M + t1N
      r15 = (t1z + kp923879532 * t1G) :+ (t1P + kp923879532 * t1Q)
      r14 = (t1T + kp707106781 * t24) :+ (t2d + kp707106781 * t2e)
      r13 = (tV + kp923879532 * t1i) :+ (t1r + kp923879532 * t1u)
      r12 = (t2f + t2i) :+ (tv + tK)
      r11 = (t1L + kp923879532 * t1O) :+ (t1J + kp923879532 * t1K)
      r10 = (t29 + kp707106781 * t2c) :+ (t27 + kp707106781 * t28)
      r9 = (t1v - kp923879532 * t1w) :+ (t1p - kp923879532 * t1q)
      r8 = (tf - tu) :+ (t2j - t2k)
      r7 = (t1z - kp923879532 * t1G) :+ (t1P - kp923879532 * t1Q)
      r6 = (t1T - kp707106781 * t24) :+ (t2d - kp707106781 * t2e)
      r5 = (tV - kp923879532 * t1i) :+ (t1r - kp923879532 * t1u)
      r4 = (t2f - t2i) :+ (tK - tv)
      r3 = (t1L - kp923879532 * t1O) :+ (t1J - kp923879532 * t1K)
      r2 = (t29 - kp707106781 * t2c) :+ (t27 - kp707106781 * t28)
      r1 = (t1v + kp923879532 * t1w) :+ (t1p + kp923879532 * t1q)
  MV.unsafeWrite xsout  0 $ (tf + tu) :+ (t2j + t2k)
  MV.unsafeWrite xsout  1 $ if sign == 1 then r1 else r15
  MV.unsafeWrite xsout  2 $ if sign == 1 then r2 else r14
  MV.unsafeWrite xsout  3 $ if sign == 1 then r3 else r13
  MV.unsafeWrite xsout  4 $ if sign == 1 then r4 else r12
  MV.unsafeWrite xsout  5 $ if sign == 1 then r5 else r11
  MV.unsafeWrite xsout  6 $ if sign == 1 then r6 else r10
  MV.unsafeWrite xsout  7 $ if sign == 1 then r7 else r9
  MV.unsafeWrite xsout  8 $ if sign == 1 then r8 else r8
  MV.unsafeWrite xsout  9 $ if sign == 1 then r9 else r7
  MV.unsafeWrite xsout 10 $ if sign == 1 then r10 else r6
  MV.unsafeWrite xsout 11 $ if sign == 1 then r11 else r5
  MV.unsafeWrite xsout 12 $ if sign == 1 then r12 else r4
  MV.unsafeWrite xsout 13 $ if sign == 1 then r13 else r3
  MV.unsafeWrite xsout 14 $ if sign == 1 then r14 else r2
  MV.unsafeWrite xsout 15 $ if sign == 1 then r15 else r1

-- | Length 32 hard-coded FFT.
kp980785280, kp198912367, kp831469612, kp668178637 :: Double
--kp923879532, kp707106781, kp414213562 :: Double
kp980785280 = 0.980785280403230449126182236134239036973933731
kp198912367 = 0.198912367379658006911597622644676228597850501
kp831469612 = 0.831469612302545237078788377617905756738560812
kp668178637 = 0.668178637919298919997757686523080761552472251
--kp923879532 = 0.923879532511286756128183189396788286822416626
--kp707106781 = 0.707106781186547524400844362104849039284835938
--kp414213562 = 0.414213562373095048801688724209698078569671875
special32 :: Int -> MVCD s -> MVCD s -> ST s ()
special32 sign xsin xsout = do
  xr0  :+ xi0  <- MV.unsafeRead xsin 0  ; xr1  :+  xi1 <- MV.unsafeRead xsin 1
  xr2  :+ xi2  <- MV.unsafeRead xsin 2  ; xr3  :+  xi3 <- MV.unsafeRead xsin 3
  xr4  :+ xi4  <- MV.unsafeRead xsin 4  ; xr5  :+  xi5 <- MV.unsafeRead xsin 5
  xr6  :+ xi6  <- MV.unsafeRead xsin 6  ; xr7  :+  xi7 <- MV.unsafeRead xsin 7
  xr8  :+ xi8  <- MV.unsafeRead xsin 8  ; xr9  :+  xi9 <- MV.unsafeRead xsin 9
  xr10 :+ xi10 <- MV.unsafeRead xsin 10 ; xr11 :+ xi11 <- MV.unsafeRead xsin 11
  xr12 :+ xi12 <- MV.unsafeRead xsin 12 ; xr13 :+ xi13 <- MV.unsafeRead xsin 13
  xr14 :+ xi14 <- MV.unsafeRead xsin 14 ; xr15 :+ xi15 <- MV.unsafeRead xsin 15
  xr16 :+ xi16 <- MV.unsafeRead xsin 16 ; xr17 :+ xi17 <- MV.unsafeRead xsin 17
  xr18 :+ xi18 <- MV.unsafeRead xsin 18 ; xr19 :+ xi19 <- MV.unsafeRead xsin 19
  xr20 :+ xi20 <- MV.unsafeRead xsin 20 ; xr21 :+ xi21 <- MV.unsafeRead xsin 21
  xr22 :+ xi22 <- MV.unsafeRead xsin 22 ; xr23 :+ xi23 <- MV.unsafeRead xsin 23
  xr24 :+ xi24 <- MV.unsafeRead xsin 24 ; xr25 :+ xi25 <- MV.unsafeRead xsin 25
  xr26 :+ xi26 <- MV.unsafeRead xsin 26 ; xr27 :+ xi27 <- MV.unsafeRead xsin 27
  xr28 :+ xi28 <- MV.unsafeRead xsin 28 ; xr29 :+ xi29 <- MV.unsafeRead xsin 29
  xr30 :+ xi30 <- MV.unsafeRead xsin 30 ; xr31 :+ xi31 <- MV.unsafeRead xsin 31
  let t1x = xr0-xr16 ; t3 = xr0+xr16 ; t2R = xi0-xi16 ; t14 = xi0+xi16
      t2S = xr8-xr24 ; t6 = xr8+xr24 ; t1y = xi8-xi24 ; t17 = xi8+xi24
      t2T = t2R-t2S ; t3T = t2S+t2R ; t4r = t3-t6 ; t7 = t3+t6
      t3t = t1x-t1y ; t1z = t1x+t1y ; t18 = t14+t17 ; t4Z = t14-t17
      t1A = xr4-xr20 ; ta = xr4+xr20 ; t1B = xi4-xi20 ; t1b = xi4+xi20
      t1C = t1A+t1B ; t2U = t1B-t1A ; t1D = xr28-xr12 ; td = xr28+xr12
      t1E = xi28-xi12 ; t1e = xi28+xi12 ; te = ta+td ; t50 = td-ta
      t1F = t1D-t1E ; t2V = t1D+t1E ; t4s = t1b-t1e ; t1f = t1b+t1e
      t2W = t2U+t2V ; t3u = t2U-t2V ; t3U = t1F-t1C ; t1G = t1C+t1F
      t1L = xr2-xr18 ; ti = xr2+xr18 ; t1I = xi2-xi18 ; t1j = xi2+xi18
      t1J = xr10-xr26 ; tl = xr10+xr26 ; t1M = xi10-xi26 ; t1m = xi10+xi26
      t3w = t1J+t1I ; t1K = t1I-t1J ; t4v = ti-tl ; tm = ti+tl
      t3x = t1L-t1M ; t1N = t1L+t1M ; t4u = t1j-t1m ; t1n = t1j+t1m
      t3X = t3x - kp414213562 * t3w ; t3y = t3w + kp414213562 * t3x
      t2Z = t1N + kp414213562 * t1K ; t1O = t1K - kp414213562 * t1N
      t53 = t4v+t4u ; t4w = t4u-t4v ; t1S = xr30-xr14 ; tp = xr30+xr14
      t1P = xi30-xi14 ; t1q = xi30+xi14 ; t1Q = xr6-xr22 ; ts = xr6+xr22
      t1T = xi6-xi22 ; t1t = xi6+xi22 ; t3z = t1Q+t1P ; t1R = t1P-t1Q
      t4x = tp-ts ; tt = tp+ts ; t3A = t1S-t1T ; t1U = t1S+t1T
      t4y = t1q-t1t ; t1u = t1q+t1t
      t3W = t3A + kp414213562 * t3z ; t3B = t3z - kp414213562 * t3A
      t2Y = t1U - kp414213562 * t1R ; t1V = t1R + kp414213562 * t1U
      t52 = t4x-t4y ; t4z = t4x+t4y ; t2G = xr31-xr15 ; tN = xr31+xr15
      t4N = xi31+xi15 ; t2r = xi31-xi15 ; t2s = xr7-xr23 ; tQ = xr7+xr23
      t4O = xi7+xi23 ; t2J = xi7-xi23 ; t2x = xr3-xr19 ; tU = xr3+xr19
      t4T = xi3+xi19 ; t2w = xi3-xi19 ; t3O = t2s+t2r ; t2t = t2r-t2s
      t2z = xr27-xr11 ; tX = xr27+xr11 ; t4U = xi27+xi11 ; t2C = xi27-xi11
      t3L = t2G-t2J ; t2K = t2G+t2J ; t4S = tN-tQ ; tR = tN+tQ
      tY = tU+tX ; t4Q = tX-tU ; t4P = t4N-t4O ; t5G = t4N+t4O
      t5H = t4T+t4U ; t4V = t4T-t4U ; t5F = tR-tY ; tZ = tR+tY
      t5I = t5G-t5H ; t5X = t5G+t5H ; t2L = t2x+t2w ; t2y = t2w-t2x
      t2D = t2z+t2C ; t2M = t2z-t2C ; t4R = t4P-t4Q ; t5k = t4Q+t4P
      t3M = t2D-t2y ; t2E = t2y+t2D ; t5j = t4S+t4V ; t4W = t4S-t4V
      t3P = t2L-t2M ; t2N = t2L+t2M ; t2f = xr1-xr17 ; ty = xr1+xr17
      t4C = xi1+xi17 ; t20 = xi1-xi17 ; t21 = xr9-xr25 ; tB = xr9+xr25
      t4D = xi9+xi25 ; t2i = xi9-xi25 ; t26 = xr5-xr21 ; tF = xr5+xr21
      t4I = xi5+xi21 ; t25 = xi5-xi21 ; t3H = t21+t20 ; t22 = t20-t21
      t28 = xr29-xr13 ; tI = xr29+xr13 ; t4J = xi29+xi13 ; t2b = xi29-xi13
      t3E = t2f-t2i ; t2j = t2f+t2i ; t4H = ty-tB ; tC = ty+tB
      tJ = tF+tI ; t4F = tI-tF ; t4E = t4C-t4D ; t5B = t4C+t4D
      t5C = t4I+t4J ; t4K = t4I-t4J ; t5A = tC-tJ ; tK = tC+tJ
      t5D = t5B-t5C ; t5W = t5B+t5C ; t2k = t26+t25 ; t27 = t25-t26
      t2c = t28+t2b ; t2l = t28-t2b ; t4G = t4E-t4F ; t5h = t4F+t4E
      t3F = t2c-t27 ; t2d = t27+t2c ; t5d = t4r+t4s ; t4t = t4r-t4s
      t5g = t4H+t4K ; t4L = t4H-t4K ; t3I = t2k-t2l ; t2m = t2k+t2l
      t4A = t4w-t4z ; t5o = t4w+t4z
      t4X = t4R - kp414213562 * t4W ; t58 = t4W + kp414213562 * t4R
      t59 = t4L - kp414213562 * t4G ; t4M = t4G + kp414213562 * t4L
      t5b = t4t - kp707106781 * t4A ; t4B = t4t + kp707106781 * t4A
      t5c = t59+t58 ; t5a = t58-t59 ; t5n = t50+t4Z ; t51 = t4Z-t50
      t54 = t52-t53 ; t5e = t53+t52 ; t56 = t4M+t4X ; t4Y = t4M-t4X
      t57 = t51 + kp707106781 * t54 ; t55 = t51 - kp707106781 * t54
      t5i = t5g + kp414213562 * t5h ; t5s = t5h - kp414213562 * t5g
      t5t = t5k + kp414213562 * t5j ; t5l = t5j - kp414213562 * t5k
      t5r = t5d - kp707106781 * t5e ; t5f = t5d + kp707106781 * t5e
      t5w = t5s+t5t ; t5u = t5s-t5t ; t5q = t5l-t5i ; t5m = t5i+t5l
      t5v = t5n + kp707106781 * t5o ; t5p = t5n - kp707106781 * t5o
      tf = t7+te ; t5x = t7-te ; t5y = t1n-t1u ; t1v = t1n+t1u
      t5E = t5A+t5D ; t5Q = t5D-t5A ; t5R = t5F+t5I ; t5J = t5F-t5I
      t5P = t5x-t5y ; t5z = t5x+t5y ; t5U = t5Q+t5R ; t5S = t5Q-t5R
      t1g = t18+t1f ; t5L = t18-t1f ; t5M = tt-tm ; tu = tm+tt
      t5O = t5J-t5E ; t5K = t5E+t5J ; t5T = t5M+t5L ; t5N = t5L-t5M
      t5V = tf-tu ; tv = tf+tu ; t60 = t5W+t5X ; t5Y = t5W-t5X
      t11 = tZ-tK ; t10 = tK+tZ ; t5Z = t1g+t1v ; t1w = t1g-t1v
      t39 = t1z + kp707106781 * t1G ; t1H = t1z - kp707106781 * t1G
      t1W = t1O-t1V ; t3k = t1O+t1V
      t3j = t2T + kp707106781 * t2W ; t2X = t2T - kp707106781 * t2W
      t30 = t2Y-t2Z ; t3a = t2Z+t2Y
      t3d = t22 + kp707106781 * t2d ; t2e = t22 - kp707106781 * t2d
      t37 = t1H - kp923879532 * t1W ; t1X = t1H + kp923879532 * t1W
      t33 = t2X + kp923879532 * t30 ; t31 = t2X - kp923879532 * t30
      t2n = t2j - kp707106781 * t2m ; t3c = t2j + kp707106781 * t2m
      t3g = t2t + kp707106781 * t2E ; t2F = t2t - kp707106781 * t2E
      t2O = t2K - kp707106781 * t2N ; t3f = t2K + kp707106781 * t2N
      t47 = t3t - kp707106781 * t3u ; t3v = t3t + kp707106781 * t3u
      t35 = t2n - kp668178637 * t2e ; t2o = t2e + kp668178637 * t2n
      t34 = t2O + kp668178637 * t2F ; t2P = t2F - kp668178637 * t2O
      t3C = t3y-t3B ; t4i = t3y+t3B
      t4h = t3T - kp707106781 * t3U ; t3V = t3T + kp707106781 * t3U
      t38 = t35+t34 ; t36 = t34-t35 ; t32 = t2o+t2P ; t2Q = t2o-t2P
      t41 = t3v - kp923879532 * t3C ; t3D = t3v + kp923879532 * t3C
      t3Y = t3W-t3X ; t48 = t3X+t3W
      t4b = t3E + kp707106781 * t3F ; t3G = t3E - kp707106781 * t3F
      t3J = t3H - kp707106781 * t3I ; t4a = t3H + kp707106781 * t3I
      t4e = t3L + kp707106781 * t3M ; t3N = t3L - kp707106781 * t3M
      t45 = t3V + kp923879532 * t3Y ; t3Z = t3V - kp923879532 * t3Y
      t42 = t3J - kp668178637 * t3G ; t3K = t3G + kp668178637 * t3J
      t3Q = t3O - kp707106781 * t3P ; t4d = t3O + kp707106781 * t3P
      t43 = t3Q + kp668178637 * t3N ; t3R = t3N - kp668178637 * t3Q
      t4p = t47 + kp923879532 * t48 ; t49 = t47 - kp923879532 * t48
      t44 = t42-t43 ; t46 = t42+t43 ; t40 = t3R-t3K ; t3S = t3K+t3R
      t4l = t4h - kp923879532 * t4i ; t4j = t4h + kp923879532 * t4i
      t4n = t4b - kp198912367 * t4a ; t4c = t4a + kp198912367 * t4b
      t4m = t4e + kp198912367 * t4d ; t4f = t4d - kp198912367 * t4e
      t3n = t39 - kp923879532 * t3a ; t3b = t39 + kp923879532 * t3a
      t4q = t4n+t4m ; t4o = t4m-t4n ; t4k = t4c+t4f ; t4g = t4c-t4f
      t3r = t3j + kp923879532 * t3k ; t3l = t3j - kp923879532 * t3k
      t3o = t3d - kp198912367 * t3c ; t3e = t3c + kp198912367 * t3d
      t3h = t3f - kp198912367 * t3g ; t3p = t3g + kp198912367 * t3f
      t3s = t3o+t3p ; t3q = t3o-t3p ; t3i = t3e+t3h ; t3m = t3h-t3e
      r31 = (t3b + kp980785280 * t3i) :+ (t3r + kp980785280 * t3s)
      r30 = (t5f + kp923879532 * t5m) :+ (t5v + kp923879532 * t5w)
      r29 = (t3D + kp831469612 * t3S) :+ (t45 + kp831469612 * t46)
      r28 = (t5z + kp707106781 * t5K) :+ (t5T + kp707106781 * t5U)
      r27 = (t1X + kp831469612 * t2Q) :+ (t33 + kp831469612 * t36)
      r26 = (t4B + kp923879532 * t4Y) :+ (t57 + kp923879532 * t5a)
      r25 = (t49 + kp980785280 * t4g) :+ (t4l + kp980785280 * t4o)
      r24 = (t5V+t5Y)                 :+ (t11+t1w)
      r23 = (t3n + kp980785280 * t3q) :+ (t3l + kp980785280 * t3m)
      r22 = (t5r + kp923879532 * t5u) :+ (t5p + kp923879532 * t5q)
      r21 = (t41 + kp831469612 * t44) :+ (t3Z + kp831469612 * t40)
      r20 = (t5P + kp707106781 * t5S) :+ (t5N + kp707106781 * t5O)
      r19 = (t37 - kp831469612 * t38) :+ (t31 - kp831469612 * t32)
      r18 = (t5b - kp923879532 * t5c) :+ (t55 - kp923879532 * t56)
      r17 = (t4p - kp980785280 * t4q) :+ (t4j - kp980785280 * t4k)
      r16 = (tv-t10)                  :+ (t5Z-t60)
      r15 = (t3b - kp980785280 * t3i) :+ (t3r - kp980785280 * t3s)
      r14 = (t5f - kp923879532 * t5m) :+ (t5v - kp923879532 * t5w)
      r13 = (t3D - kp831469612 * t3S) :+ (t45 - kp831469612 * t46)
      r12 = (t5z - kp707106781 * t5K) :+ (t5T - kp707106781 * t5U)
      r11 = (t1X - kp831469612 * t2Q) :+ (t33 - kp831469612 * t36)
      r10 = (t4B - kp923879532 * t4Y) :+ (t57 - kp923879532 * t5a)
      r9 = (t49 - kp980785280 * t4g) :+ (t4l - kp980785280 * t4o)
      r8 = (t5V-t5Y)                 :+ (t1w-t11)
      r7 = (t3n - kp980785280 * t3q) :+ (t3l - kp980785280 * t3m)
      r6 = (t5r - kp923879532 * t5u) :+ (t5p - kp923879532 * t5q)
      r5 = (t41 - kp831469612 * t44) :+ (t3Z - kp831469612 * t40)
      r4 = (t5P - kp707106781 * t5S) :+ (t5N - kp707106781 * t5O)
      r3 = (t37 + kp831469612 * t38) :+ (t31 + kp831469612 * t32)
      r2 = (t5b + kp923879532 * t5c) :+ (t55 + kp923879532 * t56)
      r1 = (t4p + kp980785280 * t4q) :+ (t4j + kp980785280 * t4k)
  MV.unsafeWrite xsout  0 $ (tv+t10) :+ (t5Z+t60)
  MV.unsafeWrite xsout  1 $ if sign == 1 then r1 else r31
  MV.unsafeWrite xsout  2 $ if sign == 1 then r2 else r30
  MV.unsafeWrite xsout  3 $ if sign == 1 then r3 else r29
  MV.unsafeWrite xsout  4 $ if sign == 1 then r4 else r28
  MV.unsafeWrite xsout  5 $ if sign == 1 then r5 else r27
  MV.unsafeWrite xsout  6 $ if sign == 1 then r6 else r26
  MV.unsafeWrite xsout  7 $ if sign == 1 then r7 else r25
  MV.unsafeWrite xsout  8 $ if sign == 1 then r8 else r24
  MV.unsafeWrite xsout  9 $ if sign == 1 then r9 else r23
  MV.unsafeWrite xsout 10 $ if sign == 1 then r10 else r22
  MV.unsafeWrite xsout 11 $ if sign == 1 then r11 else r21
  MV.unsafeWrite xsout 12 $ if sign == 1 then r12 else r20
  MV.unsafeWrite xsout 13 $ if sign == 1 then r13 else r19
  MV.unsafeWrite xsout 14 $ if sign == 1 then r14 else r18
  MV.unsafeWrite xsout 15 $ if sign == 1 then r15 else r17
  MV.unsafeWrite xsout 16 $ if sign == 1 then r16 else r16
  MV.unsafeWrite xsout 17 $ if sign == 1 then r17 else r15
  MV.unsafeWrite xsout 18 $ if sign == 1 then r18 else r14
  MV.unsafeWrite xsout 19 $ if sign == 1 then r19 else r13
  MV.unsafeWrite xsout 20 $ if sign == 1 then r20 else r12
  MV.unsafeWrite xsout 21 $ if sign == 1 then r21 else r11
  MV.unsafeWrite xsout 22 $ if sign == 1 then r22 else r10
  MV.unsafeWrite xsout 23 $ if sign == 1 then r23 else r9
  MV.unsafeWrite xsout 24 $ if sign == 1 then r24 else r8
  MV.unsafeWrite xsout 25 $ if sign == 1 then r25 else r7
  MV.unsafeWrite xsout 26 $ if sign == 1 then r26 else r6
  MV.unsafeWrite xsout 27 $ if sign == 1 then r27 else r5
  MV.unsafeWrite xsout 28 $ if sign == 1 then r28 else r4
  MV.unsafeWrite xsout 29 $ if sign == 1 then r29 else r3
  MV.unsafeWrite xsout 30 $ if sign == 1 then r30 else r2
  MV.unsafeWrite xsout 31 $ if sign == 1 then r31 else r1

-- | Length 64 hard-coded FFT.
kp956940335, kp881921264, kp534511135, kp303346683 :: Double
kp995184726, kp773010453, kp820678790, kp098491403 :: Double
--kp980785280, kp831469612, kp668178637, kp198912367 :: Double
--kp923879532, kp707106781, kp414213562 :: Double
kp956940335 = 0.956940335732208864935797886980269969482849206
kp881921264 = 0.881921264348355029712756863660388349508442621
kp534511135 = 0.534511135950791641089685961295362908582039528
kp303346683 = 0.303346683607342391675883946941299872384187453
kp995184726 = 0.995184726672196886244836953109479921575474869
kp773010453 = 0.773010453362736960810906609758469800971041293
kp820678790 = 0.820678790828660330972281985331011598767386482
kp098491403 = 0.098491403357164253077197521291327432293052451
--kp980785280 = 0.980785280403230449126182236134239036973933731
--kp831469612 = 0.831469612302545237078788377617905756738560812
--kp668178637 = 0.668178637919298919997757686523080761552472251
--kp198912367 = 0.198912367379658006911597622644676228597850501
--kp923879532 = 0.923879532511286756128183189396788286822416626
--kp707106781 = 0.707106781186547524400844362104849039284835938
--kp414213562 = 0.414213562373095048801688724209698078569671875
special64 :: Int -> MVCD s -> MVCD s -> ST s ()
special64 sign xsin xsout = do
  xr0  :+ xi0  <- MV.unsafeRead xsin 0  ; xr1  :+  xi1 <- MV.unsafeRead xsin 1
  xr2  :+ xi2  <- MV.unsafeRead xsin 2  ; xr3  :+  xi3 <- MV.unsafeRead xsin 3
  xr4  :+ xi4  <- MV.unsafeRead xsin 4  ; xr5  :+  xi5 <- MV.unsafeRead xsin 5
  xr6  :+ xi6  <- MV.unsafeRead xsin 6  ; xr7  :+  xi7 <- MV.unsafeRead xsin 7
  xr8  :+ xi8  <- MV.unsafeRead xsin 8  ; xr9  :+  xi9 <- MV.unsafeRead xsin 9
  xr10 :+ xi10 <- MV.unsafeRead xsin 10 ; xr11 :+ xi11 <- MV.unsafeRead xsin 11
  xr12 :+ xi12 <- MV.unsafeRead xsin 12 ; xr13 :+ xi13 <- MV.unsafeRead xsin 13
  xr14 :+ xi14 <- MV.unsafeRead xsin 14 ; xr15 :+ xi15 <- MV.unsafeRead xsin 15
  xr16 :+ xi16 <- MV.unsafeRead xsin 16 ; xr17 :+ xi17 <- MV.unsafeRead xsin 17
  xr18 :+ xi18 <- MV.unsafeRead xsin 18 ; xr19 :+ xi19 <- MV.unsafeRead xsin 19
  xr20 :+ xi20 <- MV.unsafeRead xsin 20 ; xr21 :+ xi21 <- MV.unsafeRead xsin 21
  xr22 :+ xi22 <- MV.unsafeRead xsin 22 ; xr23 :+ xi23 <- MV.unsafeRead xsin 23
  xr24 :+ xi24 <- MV.unsafeRead xsin 24 ; xr25 :+ xi25 <- MV.unsafeRead xsin 25
  xr26 :+ xi26 <- MV.unsafeRead xsin 26 ; xr27 :+ xi27 <- MV.unsafeRead xsin 27
  xr28 :+ xi28 <- MV.unsafeRead xsin 28 ; xr29 :+ xi29 <- MV.unsafeRead xsin 29
  xr30 :+ xi30 <- MV.unsafeRead xsin 30 ; xr31 :+ xi31 <- MV.unsafeRead xsin 31
  xr32 :+ xi32 <- MV.unsafeRead xsin 32 ; xr33 :+ xi33 <- MV.unsafeRead xsin 33
  xr34 :+ xi34 <- MV.unsafeRead xsin 34 ; xr35 :+ xi35 <- MV.unsafeRead xsin 35
  xr36 :+ xi36 <- MV.unsafeRead xsin 36 ; xr37 :+ xi37 <- MV.unsafeRead xsin 37
  xr38 :+ xi38 <- MV.unsafeRead xsin 38 ; xr39 :+ xi39 <- MV.unsafeRead xsin 39
  xr40 :+ xi40 <- MV.unsafeRead xsin 40 ; xr41 :+ xi41 <- MV.unsafeRead xsin 41
  xr42 :+ xi42 <- MV.unsafeRead xsin 42 ; xr43 :+ xi43 <- MV.unsafeRead xsin 43
  xr44 :+ xi44 <- MV.unsafeRead xsin 44 ; xr45 :+ xi45 <- MV.unsafeRead xsin 45
  xr46 :+ xi46 <- MV.unsafeRead xsin 46 ; xr47 :+ xi47 <- MV.unsafeRead xsin 47
  xr48 :+ xi48 <- MV.unsafeRead xsin 48 ; xr49 :+ xi49 <- MV.unsafeRead xsin 49
  xr50 :+ xi50 <- MV.unsafeRead xsin 50 ; xr51 :+ xi51 <- MV.unsafeRead xsin 51
  xr52 :+ xi52 <- MV.unsafeRead xsin 52 ; xr53 :+ xi53 <- MV.unsafeRead xsin 53
  xr54 :+ xi54 <- MV.unsafeRead xsin 54 ; xr55 :+ xi55 <- MV.unsafeRead xsin 55
  xr56 :+ xi56 <- MV.unsafeRead xsin 56 ; xr57 :+ xi57 <- MV.unsafeRead xsin 57
  xr58 :+ xi58 <- MV.unsafeRead xsin 58 ; xr59 :+ xi59 <- MV.unsafeRead xsin 59
  xr60 :+ xi60 <- MV.unsafeRead xsin 60 ; xr61 :+ xi61 <- MV.unsafeRead xsin 61
  xr62 :+ xi62 <- MV.unsafeRead xsin 62 ; xr63 :+ xi63 <- MV.unsafeRead xsin 63
  let t35 = xr0-xr32 ; t3 = xr0+xr32 ; t5Y = xi0-xi32 ; t26 = xi0+xi32
      t5X = xr16-xr48 ; t6 = xr16+xr48 ; t36 = xi16-xi48 ; t29 = xi16+xi48
      t39 = xr8-xr40 ; ta = xr8+xr40 ; t38 = xi8-xi40 ; t2d = xi8+xi40
      t7B = t35+t36 ; t37 = t35-t36 ; t3b = xr56-xr24 ; td = xr56+xr24
      t3c = xi56-xi24 ; t2g = xi56+xi24 ; t5Z = t5X+t5Y ; t8F = t5Y-t5X
      taf = t3-t6 ; t7 = t3+t6 ; te = ta+td ; tbz = td-ta
      tbA = t26-t29 ; t2a = t26+t29 ; t3d = t3b+t3c ; t60 = t3b-t3c
      td9 = t7-te ; tf = t7+te ; tcB = tbA-tbz ; tbB = tbz+tbA
      t61 = t39+t38 ; t3a = t38-t39 ; t2h = t2d+t2g ; tag = t2d-t2g
      t7C = t61+t60 ; t62 = t60-t61 ; tdH = t2a-t2h ; t2i = t2a+t2h
      tcb = taf-tag ; tah = taf+tag ; t8G = t3a+t3d ; t3e = t3a-t3d
      t3j = xr4-xr36 ; ti = xr4+xr36 ; t3h = xi4-xi36 ; t2l = xi4+xi36
      t3g = xr20-xr52 ; tl = xr20+xr52 ; t3k = xi20-xi52 ; t2o = xi20+xi52
      t3q = xr60-xr28 ; tp = xr60+xr28 ; t3o = xi60-xi28 ; t2s = xi60+xi28
      tai = ti-tl ; tm = ti+tl ; t3n = xr12-xr44 ; ts = xr12+xr44
      t3r = xi12-xi44 ; t2v = xi12+xi44 ; tal = tp-ts ; tt = tp+ts
      taj = t2l-t2o ; t2p = t2l+t2o ; tam = t2s-t2v ; t2w = t2s+t2v
      tu = tm+tt ; tdI = tt-tm ; tak = tai+taj ; tbC = taj-tai
      tbD = tal+tam ; tan = tal-tam ; t7F = t3h-t3g ; t3i = t3g+t3h
      t3l = t3j-t3k ; t7E = t3j+t3k ; tda = t2p-t2w ; t2x = t2p+t2w
      t65 = t3l-kp414213562*t3i ; t3m = t3i+kp414213562*t3l
      t3s = t3q-t3r ; t7H = t3q+t3r ; t7I = t3o-t3n ; t3p = t3n+t3o
      t8I = t7F-kp414213562*t7E ; t7G = t7E+kp414213562*t7F
      t8J = t7I+kp414213562*t7H ; t7J = t7H-kp414213562*t7I
      t64 = t3s+kp414213562*t3p ; t3t = t3p-kp414213562*t3s
      t3H = xr2-xr34 ; ty = xr2+xr34 ; t3x = xi2-xi34 ; t2B = xi2+xi34
      t3w = xr18-xr50 ; tB = xr18+xr50 ; t3I = xi18-xi50 ; t2E = xi18+xi50
      t3C = xr58-xr26 ; tI = xr58+xr26 ; t3D = xi58-xi26 ; t2L = xi58+xi26
      t3z = xr10-xr42 ; tF = xr10+xr42 ; t3E = t3C-t3D ; t3K = t3C+t3D
      t2I = xi10+xi42 ; t3A = xi10-xi42 ; tat = ty-tB ; tC = ty+tB
      tJ = tF+tI ; taq = tI-tF ; t3L = t3A-t3z ; t3B = t3z+t3A
      tdd = tC-tJ ; tK = tC+tJ ; tar = t2B-t2E ; t2F = t2B+t2E
      tau = t2I-t2L ; t2M = t2I+t2L ; tce = tar-taq ; tas = taq+tar
      tcf = tat-tau ; tav = tat+tau ; t7M = t3x-t3w ; t3y = t3w+t3x
      t3F = t3B-t3E ; t7Q = t3B+t3E ; tdc = t2F-t2M ; t2N = t2F+t2M
      t6G = t3y+kp707106781*t3F ; t3G = t3y-kp707106781*t3F
      t7N = t3L+t3K ; t3M = t3K-t3L ; t3J = t3H-t3I ; t7P = t3H+t3I
      t9k = t7M-kp707106781*t7N ; t7O = t7M+kp707106781*t7N
      t9l = t7P-kp707106781*t7Q ; t7R = t7P+kp707106781*t7Q
      t6H = t3J+kp707106781*t3M ; t3N = t3J-kp707106781*t3M
      t5I = xr63-xr31 ; t1z = xr63+xr31 ; tb8 = xi63+xi31 ; t56 = xi63-xi31
      t53 = xr15-xr47 ; t1C = xr15+xr47 ; tb9 = xi15+xi47 ; t5L = xi15-xi47
      t5d = xr55-xr23 ; t1J = xr55+xr23 ; t5g = xi55-xi23 ; tbq = xi55+xi23
      t58 = xr7-xr39 ; t1G = xr7+xr39 ; t5N = t5d+t5g ; t5h = t5d-t5g
      tbp = xi7+xi39 ; t5b = xi7-xi39 ; tbo = t1z-t1C ; t1D = t1z+t1C
      t1K = t1G+t1J ; tb7 = t1J-t1G ; t5c = t58+t5b ; t5O = t5b-t58
      tdA = t1D-t1K ; t1L = t1D+t1K ; tbr = tbp-tbq ; tdw = tbp+tbq
      tba = tb8-tb9 ; tdv = tb8+tb9 ; t8l = t56-t53 ; t57 = t53+t56
      tct = tbo-tbr ; tbs = tbo+tbr ; teo = tdv+tdw ; tdx = tdv-tdw
      t5i = t5c-t5h ; t8x = t5c+t5h ; t8w = t5I+t5L ; t5M = t5I-t5L
      t5P = t5N-t5O ; t8m = t5O+t5N
      t6Y = t57+kp707106781*t5i ; t5j = t57-kp707106781*t5i
      t6V = t5M+kp707106781*t5P ; t5Q = t5M-kp707106781*t5P
      t9z = t8w-kp707106781*t8x ; t8y = t8w+kp707106781*t8x
      tcw = tba-tb7 ; tbb = tb7+tba
      t9C = t8l-kp707106781*t8m ; t8n = t8l+kp707106781*t8m
      t40 = xr62-xr30 ; tN = xr62+xr30 ; t3Q = xi62-xi30 ; t2Q = xi62+xi30
      t3P = xr14-xr46 ; tQ = xr14+xr46 ; t41 = xi14-xi46 ; t2T = xi14+xi46
      t3V = xr54-xr22 ; tX = xr54+xr22 ; t3W = xi54-xi22 ; t30 = xi54+xi22
      t3S = xr6-xr38 ; tU = xr6+xr38 ; t3X = t3V-t3W ; t43 = t3V+t3W
      t2X = xi6+xi38 ; t3T = xi6-xi38 ; taA = tN-tQ ; tR = tN+tQ
      tY = tU+tX ; tax = tX-tU ; t44 = t3T-t3S ; t3U = t3S+t3T
      tdf = tR-tY ; tZ = tR+tY ; tay = t2Q-t2T ; t2U = t2Q+t2T
      taB = t2X-t30 ; t31 = t2X+t30 ; tch = tay-tax ; taz = tax+tay
      tci = taA-taB ; taC = taA+taB ; t7T = t3Q-t3P ; t3R = t3P+t3Q
      t3Y = t3U-t3X ; t7X = t3U+t3X ; tdg = t2U-t31 ; t32 = t2U+t31
      t6J = t3R+kp707106781*t3Y ; t3Z = t3R-kp707106781*t3Y
      t7U = t44+t43 ; t45 = t43-t44 ; t42 = t40-t41 ; t7W = t40+t41
      t9n = t7T-kp707106781*t7U ; t7V = t7T+kp707106781*t7U
      t9o = t7W-kp707106781*t7X ; t7Y = t7W+kp707106781*t7X
      t6K = t42+kp707106781*t45 ; t46 = t42-kp707106781*t45
      t4P = xr1-xr33 ; t14 = xr1+xr33 ; taH = xi1+xi33 ; t4d = xi1-xi33
      t4a = xr17-xr49 ; t17 = xr17+xr49 ; taI = xi17+xi49 ; t4S = xi17-xi49
      t4k = xr57-xr25 ; t1e = xr57+xr25 ; t4n = xi57-xi25 ; taZ = xi57+xi25
      t4f = xr9-xr41 ; t1b = xr9+xr41 ; t4U = t4k+t4n ; t4o = t4k-t4n
      taY = xi9+xi41 ; t4i = xi9-xi41 ; taX = t14-t17 ; t18 = t14+t17
      t1f = t1b+t1e ; taG = t1e-t1b ; t4j = t4f+t4i ; t4V = t4i-t4f
      tdp = t18-t1f ; t1g = t18+t1f ; tb0 = taY-taZ ; tdl = taY+taZ
      taJ = taH-taI ; tdk = taH+taI ; t82 = t4d-t4a ; t4e = t4a+t4d
      tcm = taX-tb0 ; tb1 = taX+tb0 ; tej = tdk+tdl ; tdm = tdk-tdl
      t4p = t4j-t4o ; t8e = t4j+t4o ; t8d = t4P+t4S ; t4T = t4P-t4S
      t4W = t4U-t4V ; t83 = t4V+t4U
      t6R = t4e+kp707106781*t4p ; t4q = t4e-kp707106781*t4p
      t6O = t4T+kp707106781*t4W ; t4X = t4T-kp707106781*t4W
      t9s = t8d-kp707106781*t8e ; t8f = t8d+kp707106781*t8e
      tcp = taJ-taG ; taK = taG+taJ
      t9v = t82-kp707106781*t83 ; t84 = t82+kp707106781*t83
      t4C = xr5-xr37 ; t1j = xr5+xr37 ; taL = xi5+xi37 ; t4K = xi5-xi37
      t4H = xr21-xr53 ; t1m = xr21+xr53 ; t85 = t4K-t4H ; t4L = t4H+t4K
      taO = t1j-t1m ; t1n = t1j+t1m ; t4F = xi21-xi53 ; taM = xi21+xi53
      tdq = taL+taM ; taN = taL-taM ; t86 = t4C+t4F ; t4G = t4C-t4F
      t4r = xr61-xr29 ; t1q = xr61+xr29 ; taR = xi61+xi29 ; t4z = xi61-xi29
      t4w = xr13-xr45 ; t1t = xr13+xr45 ; t88 = t4z-t4w ; t4A = t4w+t4z
      taQ = t1q-t1t ; t1u = t1q+t1t ; t4u = xi13-xi45 ; taS = xi13+xi45
      tb2 = taO+taN ; taP = taN-taO ; tdr = taR+taS ; taT = taR-taS
      t89 = t4r+t4u ; t4v = t4r-t4u ; tdn = t1u-t1n ; t1v = t1n+t1u
      tb3 = taQ-taT ; taU = taQ+taT
      t4Z = t4A-kp414213562*t4v ; t4B = t4v+kp414213562*t4A
      tcq = tb2-tb3 ; tb4 = tb2+tb3 ; tek = tdq+tdr ; tds = tdq-tdr
      t4M = t4G-kp414213562*t4L ; t4Y = t4L+kp414213562*t4G
      t87 = t85-kp414213562*t86 ; t8g = t86+kp414213562*t85
      t6P = t4M+t4B ; t4N = t4B-t4M ; t6S = t4Y+t4Z ; t50 = t4Y-t4Z
      t8h = t89-kp414213562*t88 ; t8a = t88+kp414213562*t89
      t9w = t8g-t8h ; t8i = t8g+t8h ; tcn = taU-taP ; taV = taP+taU
      t9t = t8a-t87 ; t8b = t87+t8a ; t5v = xr3-xr35 ; t1O = xr3+xr35
      tbc = xi3+xi35 ; t5D = xi3-xi35 ; t5A = xr19-xr51 ; t1R = xr19+xr51
      t8o = t5D-t5A ; t5E = t5A+t5D ; tbf = t1O-t1R ; t1S = t1O+t1R
      t5y = xi19-xi51 ; tbd = xi19+xi51 ; tdB = tbc+tbd ; tbe = tbc-tbd
      t8p = t5v+t5y ; t5z = t5v-t5y ; t5k = xr59-xr27 ; t1V = xr59+xr27
      tbi = xi59+xi27 ; t5s = xi59-xi27 ; t5p = xr11-xr43 ; t1Y = xr11+xr43
      t8r = t5s-t5p ; t5t = t5p+t5s ; tbh = t1V-t1Y ; t1Z = t1V+t1Y
      t5n = xi11-xi43 ; tbj = xi11+xi43 ; tbt = tbf+tbe ; tbg = tbe-tbf
      tdC = tbi+tbj ; tbk = tbi-tbj ; t8s = t5k+t5n ; t5o = t5k-t5n
      tdy = t1Z-t1S ; t20 = t1S+t1Z ; tbu = tbh-tbk ; tbl = tbh+tbk
      t5S = t5t-kp414213562*t5o ; t5u = t5o+kp414213562*t5t
      tcx = tbt-tbu ; tbv = tbt+tbu ; tep = tdB+tdC ; tdD = tdB-tdC
      t5F = t5z-kp414213562*t5E ; t5R = t5E+kp414213562*t5z
      t8q = t8o-kp414213562*t8p ; t8z = t8p+kp414213562*t8o
      t6W = t5F+t5u ; t5G = t5u-t5F ; t6Z = t5R+t5S ; t5T = t5R-t5S
      t8A = t8s-kp414213562*t8r ; t8t = t8r+kp414213562*t8s
      t9D = t8z-t8A ; t8B = t8z+t8A ; tcu = tbl-tbg ; tbm = tbg+tbl
      tef = tf-tu ; tv = tf+tu ; t10 = tK+tZ ; teu = tZ-tK
      tel = tej-tek ; teE = tej+tek ; t9A = t8t-t8q ; t8u = t8q+t8t
      teD = tv-t10 ; t11 = tv+t10 ; teF = teo+tep ; teq = teo-tep
      tei = t1g-t1v ; t1w = t1g+t1v ; t21 = t1L+t20 ; ten = t1L-t20
      tet = t2i-t2x ; t2y = t2i+t2x ; teI = teE+teF ; teG = teE-teF
      t23 = t21-t1w ; t22 = t1w+t21 ; t33 = t2N+t32 ; teg = t2N-t32
      t34 = t2y-t33 ; teH = t2y+t33 ; tex = tef-teg ; teh = tef+teg
      teB = teu+tet ; tev = tet-teu ; tey = tel-tei ; tem = tei+tel
      tdV = td9+tda ; tdb = td9-tda ; tdJ = tdH-tdI ; te5 = tdI+tdH
      tez = ten+teq ; ter = ten-teq ; tdL = tdd+tdc ; tde = tdc-tdd
      teA = tey-tez ; teC = tey+tez ; tew = ter-tem ; tes = tem+ter
      tdh = tdf+tdg ; tdK = tdf-tdg ; tdE = tdA-tdD ; te1 = tdA+tdD
      te2 = tdy+tdx ; tdz = tdx-tdy ; te6 = tde+tdh ; tdi = tde-tdh
      teb = te2+kp414213562*te1 ; te3 = te1-kp414213562*te2
      tdZ = tdn+tdm ; tdo = tdm-tdn ; tdt = tdp-tds ; tdY = tdp+tds
      tdW = tdL+tdK ; tdM = tdK-tdL
      tdR = tdt-kp414213562*tdo ; tdu = tdo+kp414213562*tdt
      tdT = tdb-kp707106781*tdi ; tdj = tdb+kp707106781*tdi
      tea = tdZ-kp414213562*tdY ; te0 = tdY+kp414213562*tdZ
      tdQ = tdE+kp414213562*tdz ; tdF = tdz-kp414213562*tdE
      tdP = tdJ+kp707106781*tdM ; tdN = tdJ-kp707106781*tdM
      tdS = tdQ-tdR ; tdU = tdR+tdQ ; tdO = tdu+tdF ; tdG = tdu-tdF
      te9 = tdV-kp707106781*tdW ; tdX = tdV+kp707106781*tdW
      te4 = te0+te3 ; te8 = te3-te0
      te7 = te5-kp707106781*te6 ; ted = te5+kp707106781*te6
      tee = tea+teb ; tec = tea-teb ; tbE = tbC+tbD ; tcc = tbC-tbD
      tcC = tan-tak ; tao = tak+tan
      tcF = tcf-kp414213562*tce ; tcg = tce+kp414213562*tcf
      tcP = tcb-kp707106781*tcc ; tcd = tcb+kp707106781*tcc
      tcZ = tcB-kp707106781*tcC ; tcD = tcB+kp707106781*tcC
      tcj = tch-kp414213562*tci ; tcE = tci+kp414213562*tch
      tcy = tcw-kp707106781*tcx ; tcV = tcw+kp707106781*tcx
      tcW = tct+kp707106781*tcu ; tcv = tct-kp707106781*tcu
      tcT = tcm+kp707106781*tcn ; tco = tcm-kp707106781*tcn
      td0 = tcg+tcj ; tck = tcg-tcj
      td4 = tcW+kp198912367*tcV ; tcX = tcV-kp198912367*tcW
      tcr = tcp-kp707106781*tcq ; tcS = tcp+kp707106781*tcq
      tcK = tcr-kp668178637*tco ; tcs = tco+kp668178637*tcr
      tcQ = tcF+tcE ; tcG = tcE-tcF
      tcJ = tcd-kp923879532*tck ; tcl = tcd+kp923879532*tck
      td5 = tcT-kp198912367*tcS ; tcU = tcS+kp198912367*tcT
      tcL = tcy+kp668178637*tcv ; tcz = tcv-kp668178637*tcy
      tcN = tcD+kp923879532*tcG ; tcH = tcD-kp923879532*tcG
      tcO = tcK+tcL ; tcM = tcK-tcL ; tcI = tcz-tcs ; tcA = tcs+tcz
      td7 = tcP+kp923879532*tcQ ; tcR = tcP-kp923879532*tcQ
      tcY = tcU-tcX ; td2 = tcU+tcX
      td1 = tcZ+kp923879532*td0 ; td3 = tcZ-kp923879532*td0
      td6 = td4-td5 ; td8 = td5+td4
      tbH = tav+kp414213562*tas ; taw = tas-kp414213562*tav
      tbR = tah+kp707106781*tao ; tap = tah-kp707106781*tao
      tc1 = tbB+kp707106781*tbE ; tbF = tbB-kp707106781*tbE
      taD = taz+kp414213562*taC ; tbG = taC-kp414213562*taz
      tbw = tbs-kp707106781*tbv ; tbX = tbs+kp707106781*tbv
      tbY = tbb+kp707106781*tbm ; tbn = tbb-kp707106781*tbm
      tbV = taK+kp707106781*taV ; taW = taK-kp707106781*taV
      tc2 = taw+taD ; taE = taw-taD
      tc7 = tbY+kp198912367*tbX ; tbZ = tbX-kp198912367*tbY
      tb5 = tb1-kp707106781*tb4 ; tbU = tb1+kp707106781*tb4
      tbN = tb5-kp668178637*taW ; tb6 = taW+kp668178637*tb5
      tbS = tbH+tbG ; tbI = tbG-tbH
      tbP = tap-kp923879532*taE ; taF = tap+kp923879532*taE
      tc6 = tbV-kp198912367*tbU ; tbW = tbU+kp198912367*tbV
      tbM = tbw+kp668178637*tbn ; tbx = tbn-kp668178637*tbw
      tbL = tbF+kp923879532*tbI ; tbJ = tbF-kp923879532*tbI
      tbO = tbM-tbN ; tbQ = tbN+tbM ; tbK = tb6+tbx ; tby = tb6-tbx
      tc5 = tbR-kp923879532*tbS ; tbT = tbR+kp923879532*tbS
      tc0 = tbW+tbZ ; tc4 = tbZ-tbW
      tc3 = tc1-kp923879532*tc2 ; tc9 = tc1+kp923879532*tc2
      tca = tc6+tc7 ; tc8 = tc6-tc7
      t3f = t37+kp707106781*t3e ; t6D = t37-kp707106781*t3e
      t6E = t65+t64 ; t66 = t64-t65
      t6T = t6R-kp923879532*t6S ; t7k = t6R+kp923879532*t6S
      t7h = t6D+kp923879532*t6E ; t6F = t6D-kp923879532*t6E
      t7l = t6O+kp923879532*t6P ; t6Q = t6O-kp923879532*t6P
      t70 = t6Y-kp923879532*t6Z ; t7n = t6Y+kp923879532*t6Z
      t7o = t6V+kp923879532*t6W ; t6X = t6V-kp923879532*t6W
      t77 = t6H-kp198912367*t6G ; t6I = t6G+kp198912367*t6H
      t7x = t7l-kp098491403*t7k ; t7m = t7k+kp098491403*t7l
      t7w = t7o+kp098491403*t7n ; t7p = t7n-kp098491403*t7o
      t6L = t6J-kp198912367*t6K ; t76 = t6K+kp198912367*t6J
      t63 = t5Z+kp707106781*t62 ; t73 = t5Z-kp707106781*t62
      t7s = t6I+t6L ; t6M = t6I-t6L
      t7c = t6T-kp820678790*t6Q ; t6U = t6Q+kp820678790*t6T
      t74 = t3m+t3t ; t3u = t3m-t3t
      t7r = t73+kp923879532*t74 ; t75 = t73-kp923879532*t74
      t7i = t77+t76 ; t78 = t76-t77
      t7b = t6F-kp980785280*t6M ; t6N = t6F+kp980785280*t6M
      t7f = t75+kp980785280*t78 ; t79 = t75-kp980785280*t78
      t71 = t6X-kp820678790*t70 ; t7d = t70+kp820678790*t6X
      t7z = t7h+kp980785280*t7i ; t7j = t7h-kp980785280*t7i
      t7q = t7m-t7p ; t7u = t7m+t7p ; t7g = t7c+t7d ; t7e = t7c-t7d
      t72 = t6U+t71 ; t7a = t71-t6U
      t7t = t7r+kp980785280*t7s ; t7v = t7r-kp980785280*t7s
      t7y = t7w-t7x ; t7A = t7x+t7w
      t7D = t7B+kp707106781*t7C ; t9h = t7B-kp707106781*t7C
      t9i = t8I-t8J ; t8K = t8I+t8J
      t9x = t9v-kp923879532*t9w ; t9Y = t9v+kp923879532*t9w
      t9V = t9h-kp923879532*t9i ; t9j = t9h+kp923879532*t9i
      t9Z = t9s+kp923879532*t9t ; t9u = t9s-kp923879532*t9t
      t9E = t9C-kp923879532*t9D ; ta1 = t9C+kp923879532*t9D
      ta2 = t9z+kp923879532*t9A ; t9B = t9z-kp923879532*t9A
      t9L = t9l-kp668178637*t9k ; t9m = t9k+kp668178637*t9l
      tab = t9Z-kp303346683*t9Y ; ta0 = t9Y+kp303346683*t9Z
      taa = ta2+kp303346683*ta1 ; ta3 = ta1-kp303346683*ta2
      t9p = t9n-kp668178637*t9o ; t9K = t9o+kp668178637*t9n
      t8H = t8F+kp707106781*t8G ; t9H = t8F-kp707106781*t8G
      ta6 = t9m+t9p ; t9q = t9m-t9p
      t9Q = t9x-kp534511135*t9u ; t9y = t9u+kp534511135*t9x
      t9I = t7J-t7G ; t7K = t7G+t7J
      ta5 = t9H-kp923879532*t9I ; t9J = t9H+kp923879532*t9I
      t9W = t9L+t9K ; t9M = t9K-t9L
      t9P = t9j-kp831469612*t9q ; t9r = t9j+kp831469612*t9q
      t9T = t9J+kp831469612*t9M ; t9N = t9J-kp831469612*t9M
      t9F = t9B-kp534511135*t9E ; t9R = t9E+kp534511135*t9B
      tad = t9V+kp831469612*t9W ; t9X = t9V-kp831469612*t9W
      ta4 = ta0-ta3 ; ta8 = ta0+ta3 ; t9U = t9Q+t9R ; t9S = t9Q-t9R
      t9G = t9y+t9F ; t9O = t9F-t9y
      ta7 = ta5+kp831469612*ta6 ; ta9 = ta5-kp831469612*ta6
      tac = taa-tab ; tae = tab+taa
      t51 = t4X-kp923879532*t50 ; t6m = t4X+kp923879532*t50
      t6j = t3f+kp923879532*t3u ; t3v = t3f-kp923879532*t3u
      t6n = t4q+kp923879532*t4N ; t4O = t4q-kp923879532*t4N
      t5U = t5Q-kp923879532*t5T ; t6p = t5Q+kp923879532*t5T
      t6q = t5j+kp923879532*t5G ; t5H = t5j-kp923879532*t5G
      t69 = t3N+kp668178637*t3G ; t3O = t3G-kp668178637*t3N
      t6y = t6n-kp303346683*t6m ; t6o = t6m+kp303346683*t6n
      t6z = t6q+kp303346683*t6p ; t6r = t6p-kp303346683*t6q
      t47 = t3Z+kp668178637*t46 ; t68 = t46-kp668178637*t3Z
      t6u = t3O+t47 ; t48 = t3O-t47
      t6f = t51-kp534511135*t4O ; t52 = t4O+kp534511135*t51
      t6t = t63+kp923879532*t66 ; t67 = t63-kp923879532*t66
      t6k = t69+t68 ; t6a = t68-t69
      t6h = t3v-kp831469612*t48 ; t49 = t3v+kp831469612*t48
      t6d = t67+kp831469612*t6a ; t6b = t67-kp831469612*t6a
      t5V = t5H-kp534511135*t5U ; t6e = t5U+kp534511135*t5H
      t6x = t6j-kp831469612*t6k ; t6l = t6j+kp831469612*t6k
      t6s = t6o+t6r ; t6w = t6r-t6o ; t6g = t6e-t6f ; t6i = t6f+t6e
      t5W = t52-t5V ; t6c = t52+t5V
      t6v = t6t-kp831469612*t6u ; t6B = t6t+kp831469612*t6u
      t6C = t6y+t6z ; t6A = t6y-t6z
      t8j = t8f-kp923879532*t8i ; t90 = t8f+kp923879532*t8i
      t8X = t7D+kp923879532*t7K ; t7L = t7D-kp923879532*t7K
      t91 = t84+kp923879532*t8b ; t8c = t84-kp923879532*t8b
      t8C = t8y-kp923879532*t8B ; t93 = t8y+kp923879532*t8B
      t94 = t8n+kp923879532*t8u ; t8v = t8n-kp923879532*t8u
      t8N = t7R+kp198912367*t7O ; t7S = t7O-kp198912367*t7R
      t9c = t91-kp098491403*t90 ; t92 = t90+kp098491403*t91
      t9d = t94+kp098491403*t93 ; t95 = t93-kp098491403*t94
      t7Z = t7V+kp198912367*t7Y ; t8M = t7Y-kp198912367*t7V
      t98 = t7S+t7Z ; t80 = t7S-t7Z
      t8T = t8j-kp820678790*t8c ; t8k = t8c+kp820678790*t8j
      t97 = t8H+kp923879532*t8K ; t8L = t8H-kp923879532*t8K
      t8Y = t8N+t8M ; t8O = t8M-t8N
      t8V = t7L-kp980785280*t80 ; t81 = t7L+kp980785280*t80
      t8R = t8L+kp980785280*t8O ; t8P = t8L-kp980785280*t8O
      t8D = t8v-kp820678790*t8C ; t8S = t8C+kp820678790*t8v
      t9b = t8X-kp980785280*t8Y ; t8Z = t8X+kp980785280*t8Y
      t96 = t92+t95 ; t9a = t95-t92 ; t8U = t8S-t8T ; t8W = t8T+t8S
      t8E = t8k-t8D ; t8Q = t8k+t8D
      t99 = t97-kp980785280*t98 ; t9f = t97+kp980785280*t98
      t9g = t9c+t9d ; t9e = t9c-t9d
      r63 = (t8Z+kp995184726*t96) :+ (t9f+kp995184726*t9g)
      r62 = (tbT+kp980785280*tc0) :+ (tc9+kp980785280*tca)
      r61 = (t6l+kp956940335*t6s) :+ (t6B+kp956940335*t6C)
      r60 = (tdX+kp923879532*te4) :+ (ted+kp923879532*tee)
      r59 = (t9r+kp881921264*t9G) :+ (t9T+kp881921264*t9U)
      r58 = (tcl+kp831469612*tcA) :+ (tcN+kp831469612*tcO)
      r57 = (t6N+kp773010453*t72) :+ (t7f+kp773010453*t7g)
      r56 = (teh+kp707106781*tes) :+ (teB+kp707106781*teC)
      r55 = (t81+kp773010453*t8E) :+ (t8R+kp773010453*t8U)
      r54 = (taF+kp831469612*tby) :+ (tbL+kp831469612*tbO)
      r53 = (t49+kp881921264*t5W) :+ (t6d+kp881921264*t6g)
      r52 = (tdj+kp923879532*tdG) :+ (tdP+kp923879532*tdS)
      r51 = (t9X+kp956940335*ta4) :+ (ta9+kp956940335*tac)
      r50 = (tcR+kp980785280*tcY) :+ (td3+kp980785280*td6)
      r49 = (t7j+kp995184726*t7q) :+ (t7v+kp995184726*t7y)
      r48 = (teD+teG)             :+ (t23+t34)
      r47 = (t9b+kp995184726*t9e) :+ (t99+kp995184726*t9a)
      r46 = (tc5+kp980785280*tc8) :+ (tc3+kp980785280*tc4)
      r45 = (t6x+kp956940335*t6A) :+ (t6v+kp956940335*t6w)
      r44 = (te9+kp923879532*tec) :+ (te7+kp923879532*te8)
      r43 = (t9P+kp881921264*t9S) :+ (t9N+kp881921264*t9O)
      r42 = (tcJ+kp831469612*tcM) :+ (tcH+kp831469612*tcI)
      r41 = (t7b+kp773010453*t7e) :+ (t79+kp773010453*t7a)
      r40 = (tex+kp707106781*teA) :+ (tev+kp707106781*tew)
      r39 = (t8V-kp773010453*t8W) :+ (t8P-kp773010453*t8Q)
      r38 = (tbP-kp831469612*tbQ) :+ (tbJ-kp831469612*tbK)
      r37 = (t6h-kp881921264*t6i) :+ (t6b-kp881921264*t6c)
      r36 = (tdT-kp923879532*tdU) :+ (tdN-kp923879532*tdO)
      r35 = (tad-kp956940335*tae) :+ (ta7-kp956940335*ta8)
      r34 = (td7-kp980785280*td8) :+ (td1-kp980785280*td2)
      r33 = (t7z-kp995184726*t7A) :+ (t7t-kp995184726*t7u)
      r32 = (t11-t22)             :+ (teH-teI)
      r31 = (t8Z-kp995184726*t96) :+ (t9f-kp995184726*t9g)
      r30 = (tbT-kp980785280*tc0) :+ (tc9-kp980785280*tca)
      r29 = (t6l-kp956940335*t6s) :+ (t6B-kp956940335*t6C)
      r28 = (tdX-kp923879532*te4) :+ (ted-kp923879532*tee)
      r27 = (t9r-kp881921264*t9G) :+ (t9T-kp881921264*t9U)
      r26 = (tcl-kp831469612*tcA) :+ (tcN-kp831469612*tcO)
      r25 = (t6N-kp773010453*t72) :+ (t7f-kp773010453*t7g)
      r24 = (teh-kp707106781*tes) :+ (teB-kp707106781*teC)
      r23 = (t81-kp773010453*t8E) :+ (t8R-kp773010453*t8U)
      r22 = (taF-kp831469612*tby) :+ (tbL-kp831469612*tbO)
      r21 = (t49-kp881921264*t5W) :+ (t6d-kp881921264*t6g)
      r20 = (tdj-kp923879532*tdG) :+ (tdP-kp923879532*tdS)
      r19 = (t9X-kp956940335*ta4) :+ (ta9-kp956940335*tac)
      r18 = (tcR-kp980785280*tcY) :+ (td3-kp980785280*td6)
      r17 = (t7j-kp995184726*t7q) :+ (t7v-kp995184726*t7y)
      r16 = (teD-teG)             :+ (t34-t23)
      r15 = (t9b-kp995184726*t9e) :+ (t99-kp995184726*t9a)
      r14 = (tc5-kp980785280*tc8) :+ (tc3-kp980785280*tc4)
      r13 = (t6x-kp956940335*t6A) :+ (t6v-kp956940335*t6w)
      r12 = (te9-kp923879532*tec) :+ (te7-kp923879532*te8)
      r11 = (t9P-kp881921264*t9S) :+ (t9N-kp881921264*t9O)
      r10 = (tcJ-kp831469612*tcM) :+ (tcH-kp831469612*tcI)
      r9 = (t7b-kp773010453*t7e) :+ (t79-kp773010453*t7a)
      r8 = (tex-kp707106781*teA) :+ (tev-kp707106781*tew)
      r7 = (t8V+kp773010453*t8W) :+ (t8P+kp773010453*t8Q)
      r6 = (tbP+kp831469612*tbQ) :+ (tbJ+kp831469612*tbK)
      r5 = (t6h+kp881921264*t6i) :+ (t6b+kp881921264*t6c)
      r4 = (tdT+kp923879532*tdU) :+ (tdN+kp923879532*tdO)
      r3 = (tad+kp956940335*tae) :+ (ta7+kp956940335*ta8)
      r2 = (td7+kp980785280*td8) :+ (td1+kp980785280*td2)
      r1 = (t7z+kp995184726*t7A) :+ (t7t+kp995184726*t7u)
  MV.unsafeWrite xsout  0 $ (t11+t22) :+ (teH+teI)
  MV.unsafeWrite xsout  1 $ if sign == 1 then r1 else r63
  MV.unsafeWrite xsout  2 $ if sign == 1 then r2 else r62
  MV.unsafeWrite xsout  3 $ if sign == 1 then r3 else r61
  MV.unsafeWrite xsout  4 $ if sign == 1 then r4 else r60
  MV.unsafeWrite xsout  5 $ if sign == 1 then r5 else r59
  MV.unsafeWrite xsout  6 $ if sign == 1 then r6 else r58
  MV.unsafeWrite xsout  7 $ if sign == 1 then r7 else r57
  MV.unsafeWrite xsout  8 $ if sign == 1 then r8 else r56
  MV.unsafeWrite xsout  9 $ if sign == 1 then r9 else r55
  MV.unsafeWrite xsout 10 $ if sign == 1 then r10 else r54
  MV.unsafeWrite xsout 11 $ if sign == 1 then r11 else r53
  MV.unsafeWrite xsout 12 $ if sign == 1 then r12 else r52
  MV.unsafeWrite xsout 13 $ if sign == 1 then r13 else r51
  MV.unsafeWrite xsout 14 $ if sign == 1 then r14 else r50
  MV.unsafeWrite xsout 15 $ if sign == 1 then r15 else r49
  MV.unsafeWrite xsout 16 $ if sign == 1 then r16 else r48
  MV.unsafeWrite xsout 17 $ if sign == 1 then r17 else r47
  MV.unsafeWrite xsout 18 $ if sign == 1 then r18 else r46
  MV.unsafeWrite xsout 19 $ if sign == 1 then r19 else r45
  MV.unsafeWrite xsout 20 $ if sign == 1 then r20 else r44
  MV.unsafeWrite xsout 21 $ if sign == 1 then r21 else r43
  MV.unsafeWrite xsout 22 $ if sign == 1 then r22 else r42
  MV.unsafeWrite xsout 23 $ if sign == 1 then r23 else r41
  MV.unsafeWrite xsout 24 $ if sign == 1 then r24 else r40
  MV.unsafeWrite xsout 25 $ if sign == 1 then r25 else r39
  MV.unsafeWrite xsout 26 $ if sign == 1 then r26 else r38
  MV.unsafeWrite xsout 27 $ if sign == 1 then r27 else r37
  MV.unsafeWrite xsout 28 $ if sign == 1 then r28 else r36
  MV.unsafeWrite xsout 29 $ if sign == 1 then r29 else r35
  MV.unsafeWrite xsout 30 $ if sign == 1 then r30 else r34
  MV.unsafeWrite xsout 31 $ if sign == 1 then r31 else r33
  MV.unsafeWrite xsout 32 $ if sign == 1 then r32 else r32
  MV.unsafeWrite xsout 33 $ if sign == 1 then r33 else r31
  MV.unsafeWrite xsout 34 $ if sign == 1 then r34 else r30
  MV.unsafeWrite xsout 35 $ if sign == 1 then r35 else r29
  MV.unsafeWrite xsout 36 $ if sign == 1 then r36 else r28
  MV.unsafeWrite xsout 37 $ if sign == 1 then r37 else r27
  MV.unsafeWrite xsout 38 $ if sign == 1 then r38 else r26
  MV.unsafeWrite xsout 39 $ if sign == 1 then r39 else r25
  MV.unsafeWrite xsout 40 $ if sign == 1 then r40 else r24
  MV.unsafeWrite xsout 41 $ if sign == 1 then r41 else r23
  MV.unsafeWrite xsout 42 $ if sign == 1 then r42 else r22
  MV.unsafeWrite xsout 43 $ if sign == 1 then r43 else r21
  MV.unsafeWrite xsout 44 $ if sign == 1 then r44 else r20
  MV.unsafeWrite xsout 45 $ if sign == 1 then r45 else r19
  MV.unsafeWrite xsout 46 $ if sign == 1 then r46 else r18
  MV.unsafeWrite xsout 47 $ if sign == 1 then r47 else r17
  MV.unsafeWrite xsout 48 $ if sign == 1 then r48 else r16
  MV.unsafeWrite xsout 49 $ if sign == 1 then r49 else r15
  MV.unsafeWrite xsout 50 $ if sign == 1 then r50 else r14
  MV.unsafeWrite xsout 51 $ if sign == 1 then r51 else r13
  MV.unsafeWrite xsout 52 $ if sign == 1 then r52 else r12
  MV.unsafeWrite xsout 53 $ if sign == 1 then r53 else r11
  MV.unsafeWrite xsout 54 $ if sign == 1 then r54 else r10
  MV.unsafeWrite xsout 55 $ if sign == 1 then r55 else r9
  MV.unsafeWrite xsout 56 $ if sign == 1 then r56 else r8
  MV.unsafeWrite xsout 57 $ if sign == 1 then r57 else r7
  MV.unsafeWrite xsout 58 $ if sign == 1 then r58 else r6
  MV.unsafeWrite xsout 59 $ if sign == 1 then r59 else r5
  MV.unsafeWrite xsout 60 $ if sign == 1 then r60 else r4
  MV.unsafeWrite xsout 61 $ if sign == 1 then r61 else r3
  MV.unsafeWrite xsout 62 $ if sign == 1 then r62 else r2
  MV.unsafeWrite xsout 63 $ if sign == 1 then r63 else r1
