module Numeric.FFT.Special ( specialBases ) where

import Control.Monad.ST
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Complex
import Data.Vector.Unboxed
import qualified Data.Vector.Unboxed.Mutable as MV

import Numeric.FFT.Types
import Numeric.FFT.Utils


-- | Map from input vector lengths to hard-coded FFT transforms for
-- small problem sizes.  Each function in the map takes a transform
-- direction (+1 for forward, -1 for inverse) and returns the
-- /unscaled/ transform (scaling for inverse transforms is applied at
-- the top-level).
specialBases :: IntMap (Int -> MVCD s -> MVCD s -> ST s ())
specialBases = IM.fromList [ (2, special2)
                           , (3, special3)
                           , (5, special5)
                           , (7, special7)
                           , (11, special11) ]

-- | Length 2 hard-coded FFT.
special2 :: Int -> MVCD s -> MVCD s -> ST s ()
special2 _ xsin xsout = do
  a <- MV.read xsin 0
  b <- MV.read xsin 1
  MV.write xsout 0 $ a + b
  MV.write xsout 1 $ a - b

-- | Length 3 hard-coded FFT.
kp500000000, kp866025403 :: Double
kp866025403 = 0.866025403784438646763723170752936183471402627
kp500000000 = 0.500000000000000000000000000000000000000000000
special3 :: Int -> MVCD s -> MVCD s -> ST s ()
special3 sign xsin xsout = do
  xr0 :+ xi0 <- MV.read xsin 0 ; xr1 :+ xi1 <- MV.read xsin 1
  xr2 :+ xi2 <- MV.read xsin 2
  let rp = xr1 + xr2 ; rm = xr1 - xr2
      ip = xi1 + xi2 ; im = xi1 - xi2
      tr = xr0 - kp500000000 * rp
      ti = xi0 - kp500000000 * ip
      r1 = (tr - kp866025403 * im) :+ (ti + kp866025403 * rm)
      r2 = (tr + kp866025403 * im) :+ (ti - kp866025403 * rm)
  MV.write xsout 0 $ (xr0 + rp) :+ (xi0 + ip)
  MV.write xsout 1 $ if sign == 1 then r1 else r2
  MV.write xsout 2 $ if sign == 1 then r2 else r1

-- | Length 5 hard-coded FFT.
kp951056516, kp559016994, kp250000000, kp618033988 :: Double
kp951056516 = 0.951056516295153572116439333379382143405698634
kp559016994 = 0.559016994374947424102293417182819058860154590
kp250000000 = 0.250000000000000000000000000000000000000000000
kp618033988 = 0.618033988749894848204586834365638117720309180
special5 :: Int -> MVCD s -> MVCD s -> ST s ()
special5 sign xsin xsout = do
  xr0 :+ xi0 <- MV.read xsin 0 ; xr1 :+ xi1 <- MV.read xsin 1
  xr2 :+ xi2 <- MV.read xsin 2 ; xr3 :+ xi3 <- MV.read xsin 3
  xr4 :+ xi4 <- MV.read xsin 4
  let ts = xr1 - xr4 ; t4 = xr1 + xr4 ; tt = xr2 - xr3 ; t7 = xr2 + xr3
      t8 = t4 + t7 ; ta = t4 - t7 ; te = xi1 - xi4 ; tm = xi1 + xi4
      tn = xi2 + xi3 ; th = xi2 - xi3 ; to = tm + tn ; tq = tm - tn
      ti = te + kp618033988 * th ; tk = th - kp618033988 * te
      t9 = xr0 - kp250000000 * t8 ; tu = ts + kp618033988 * tt
      tw = tt - kp618033988 * ts ; tp = xi0 - kp250000000 * to
      tb = t9 + kp559016994 * ta ; tj = t9 - kp559016994 * ta
      tr = tp + kp559016994 * tq ; tv = tp - kp559016994 * tq
      r4 = (tb + kp951056516 * ti) :+ (tr - kp951056516 * tu)
      r3 = (tj - kp951056516 * tk) :+ (tv + kp951056516 * tw)
      r2 = (tj + kp951056516 * tk) :+ (tv - kp951056516 * tw)
      r1 = (tb - kp951056516 * ti) :+ (tr + kp951056516 * tu)
  MV.write xsout 0 $ (xr0 + t8) :+ (xi0 + to)
  MV.write xsout 1 $ if sign == 1 then r1 else r4
  MV.write xsout 2 $ if sign == 1 then r2 else r3
  MV.write xsout 3 $ if sign == 1 then r3 else r2
  MV.write xsout 4 $ if sign == 1 then r4 else r1

-- | Length 7 hard-coded FFT.
kp974927912, kp900968867, kp801937735 :: Double
kp692021471, kp356895867, kp554958132 :: Double
kp974927912 = 0.974927912181823607018131682993931217232785801
kp900968867 = 0.900968867902419126236102319507445051165919162
kp801937735 = 0.801937735804838252472204639014890102331838324
kp692021471 = 0.692021471630095869627814897002069140197260599
kp356895867 = 0.356895867892209443894399510021300583399127187
kp554958132 = 0.554958132087371191422194871006410481067288862
special7 :: Int -> MVCD s -> MVCD s -> ST s ()
special7 sign xsin xsout = do
  xr0 :+ xi0 <- MV.read xsin 0 ; xr1 :+ xi1 <- MV.read xsin 1
  xr2 :+ xi2 <- MV.read xsin 2 ; xr3 :+ xi3 <- MV.read xsin 3
  xr4 :+ xi4 <- MV.read xsin 4 ; xr5 :+ xi5 <- MV.read xsin 5
  xr6 :+ xi6 <- MV.read xsin 6
  let tI = xr6 - xr1 ; t4 = xr1 + xr6 ; tG = xr4 - xr3 ; ta = xr3 + xr4
      tT = tI + kp554958132 * tG ; tp = ta - kp356895867 * t4
      tH = xr5 - xr2 ; t7 = xr2 + xr5
      tJ = tH - kp554958132 * tI ; tO = tG + kp554958132 * tH
      tu = t7 - kp356895867 * ta ; tb = t4 - kp356895867 * t7
      tB = xi2 + xi5 ; tg = xi2 - xi5
      tC = xi3 + xi4 ; tm = xi3 - xi4 ; tA = xi1 + xi6 ; tj = xi1 - xi6
      tD = tB - kp356895867 * tC ; ts = tm + kp554958132 * tg
      tL = tC - kp356895867 * tA ; tQ = tA - kp356895867 * tB
      tx = tg - kp554958132 * tj ; tn = tj + kp554958132 * tm
      tc = ta - kp692021471 * tb ; tU = tH + kp801937735 * tT
      to = tg + kp801937735 * tn ; tR = tC - kp692021471 * tQ
      td = xr0 - kp900968867 * tc ; tt = tj - kp801937735 * ts
      tq = t7 - kp692021471 * tp ; tS = xi0 - kp900968867 * tR
      tr = xr0 - kp900968867 * tq ; tP = tI - kp801937735 * tO
      tM = tB - kp692021471 * tL ; ty = tm - kp801937735 * tx
      tv = t4 - kp692021471 * tu ; tK = tG - kp801937735 * tJ
      tN = xi0 - kp900968867 * tM ; tE = tA - kp692021471 * tD
      tw = xr0 - kp900968867 * tv ; tF = xi0 - kp900968867 * tE
      r6 = (td + kp974927912 * to) :+ (tS + kp974927912 * tU)
      r5 = (tr + kp974927912 * tt) :+ (tN + kp974927912 * tP)
      r4 = (tw + kp974927912 * ty) :+ (tF + kp974927912 * tK)
      r3 = (tw - kp974927912 * ty) :+ (tF - kp974927912 * tK)
      r2 = (tr - kp974927912 * tt) :+ (tN - kp974927912 * tP)
      r1 = (td - kp974927912 * to) :+ (tS - kp974927912 * tU)
  MV.write xsout 0 $ (xr0 + t4 + t7 + ta) :+ (xi0 + tA + tB + tC)
  MV.write xsout 1 $ if sign == 1 then r1 else r6
  MV.write xsout 2 $ if sign == 1 then r2 else r5
  MV.write xsout 3 $ if sign == 1 then r3 else r4
  MV.write xsout 4 $ if sign == 1 then r4 else r3
  MV.write xsout 5 $ if sign == 1 then r5 else r2
  MV.write xsout 6 $ if sign == 1 then r6 else r1

-- | Length 11 hard-coded FFT.
kp989821441, kp959492973, kp918985947, kp876768831, kp830830026 :: Double
kp778434453, kp715370323, kp634356270, kp342584725, kp521108558 :: Double
kp989821441 = 0.989821441880932732376092037776718787376519372
kp959492973 = 0.959492973614497389890368057066327699062454848
kp918985947 = 0.918985947228994779780736114132655398124909697
kp876768831 = 0.876768831002589333891339807079336796764054852
kp830830026 = 0.830830026003772851058548298459246407048009821
kp778434453 = 0.778434453334651800608337670740821884709317477
kp715370323 = 0.715370323453429719112414662767260662417897278
kp634356270 = 0.634356270682424498893150776899916060542806975
kp342584725 = 0.342584725681637509502641509861112333758894680
kp521108558 = 0.521108558113202722944698153526659300680427422
special11 :: Int -> MVCD s -> MVCD s -> ST s ()
special11 sign xsin xsout = do
  xr0 :+ xi0 <- MV.read xsin 0 ; xr1 :+ xi1 <- MV.read xsin 1
  xr2 :+ xi2 <- MV.read xsin 2 ; xr3 :+ xi3 <- MV.read xsin 3
  xr4 :+ xi4 <- MV.read xsin 4 ; xr5 :+ xi5 <- MV.read xsin 5
  xr6 :+ xi6 <- MV.read xsin 6 ; xr7 :+ xi7 <- MV.read xsin 7
  xr8 :+ xi8 <- MV.read xsin 8 ; xr9 :+ xi9 <- MV.read xsin 9
  xr10 :+ xi10 <- MV.read xsin 10
  let t1u = xr10 - xr1 ; t4 = xr1 + xr10 ; t1q = xr6 - xr5 ; tg = xr5 + xr6;
      t1t = xr9 - xr2 ; t7 = xr2 + xr9 ; t1s = xr8 - xr3 ; ta = xr3 + xr8;
      t25 = t1u + kp521108558 * t1q ; t1W = t1q + kp521108558 * t1s
      tO = ta - kp342584725 * t4 ; th = t7 - kp342584725 * ta
      td = xr4 + xr7 ; t1r = xr7 - xr4
      tP = tg - kp634356270 * tO ; t1X = t1t - kp715370323 * t1W
      t26 = t1r + kp715370323 * t25 ; tF = t4 - kp342584725 * td
      ti = td - kp634356270 * th ; t1N = t1r - kp521108558 * t1t
      t1v = t1t - kp521108558 * t1u ; tG = t7 - kp634356270 * tF
      tX = tg - kp342584725 * t7 ; t1O = t1q + kp715370323 * t1N
      t1w = t1s - kp715370323 * t1v ; t1E = t1s + kp521108558 * t1r
      tY = t4 - kp634356270 * tX ; t16 = td - kp342584725 * tg
      t1F = t1u + kp715370323 * t1E ; t17 = ta - kp634356270 * t16
      to = xi3 - xi8 ; t1i = xi3 + xi8 ; t1k = xi5 + xi6 ; tA = xi5 - xi6
      t1h = xi2 + xi9 ; tr = xi2 - xi9 ; t1j = xi4 + xi7 ; tu = xi4 - xi7
      t20 = t1h - kp342584725 * t1i ; tK = tA + kp521108558 * to
      tT = tu - kp521108558 * tr ; t1g = xi1 + xi10 ; tx = xi1 - xi10
      t21 = t1j - kp634356270 * t20 ; tU = tA + kp715370323 * tT
      tL = tr - kp715370323 * tK ; tB = tx + kp521108558 * tA
      t1R = t1g - kp342584725 * t1j ; t1I = t1i - kp342584725 * t1g
      t1l = t1j - kp342584725 * t1k ; tC = tu + kp715370323 * tB
      t1S = t1h - kp634356270 * t1R ; t1J = t1k - kp634356270 * t1I
      t1m = t1i - kp634356270 * t1l ; t12 = to + kp521108558 * tu
      t1z = t1k - kp342584725 * t1h ; t1b = tr - kp521108558 * tx
      t13 = tx + kp715370323 * t12 ; t1A = t1g - kp634356270 * t1z
      t1c = to - kp715370323 * t1b ; tj = t4 - kp778434453 * ti
      tD = tr + kp830830026 * tC ; t22 = t1g - kp778434453 * t21
      t27 = t1t + kp830830026 * t26 ; tk = tg - kp876768831 * tj
      tE = to + kp918985947 * tD ; t23 = t1k - kp876768831 * t22
      t28 = t1s + kp918985947 * t27 ; tl = xr0 - kp959492973 * tk
      t1T = t1k - kp778434453 * t1S ; t24 = xi0 - kp959492973 * t23
      t1Y = t1u + kp830830026 * t1X ; t1U = t1i - kp876768831 * t1T
      t1Z = t1r - kp918985947 * t1Y ; t1V = xi0 - kp959492973 * t1U
      tH = tg - kp778434453 * tG ; tM = tx + kp830830026 * tL
      tQ = td - kp778434453 * tP ; tI = ta - kp876768831 * tH
      tN = tu - kp918985947 * tM ; tR = t7 - kp876768831 * tQ
      tV = to - kp830830026 * tU ; tJ = xr0 - kp959492973 * tI
      t1K = t1j - kp778434453 * t1J ; tS = xr0 - kp959492973 * tR
      tW = tx - kp918985947 * tV ; t1L = t1h - kp876768831 * t1K
      t1P = t1s - kp830830026 * t1O ; t1M = xi0 - kp959492973 * t1L
      tZ = ta - kp778434453 * tY ; t14 = tA - kp830830026 * t13
      t1Q = t1u - kp918985947 * t1P ; t1B = t1i - kp778434453 * t1A
      t10 = td - kp876768831 * tZ ; t15 = tr + kp918985947 * t14
      t11 = xr0 - kp959492973 * t10 ; t1C = t1j - kp876768831 * t1B
      t1G = t1q - kp830830026 * t1F ; t1n = t1h - kp778434453 * t1m
      t1D = xi0 - kp959492973 * t1C ; t1H = t1t + kp918985947 * t1G
      t1o = t1g - kp876768831 * t1n ; t1x = t1r - kp830830026 * t1w
      t18 = t7 - kp778434453 * t17 ; t1p = xi0 - kp959492973 * t1o
      t1y = t1q - kp918985947 * t1x ; t19 = t4 - kp876768831 * t18
      t1d = tu - kp830830026 * t1c ; t1a = xr0 - kp959492973 * t19
      t1e = tA - kp918985947 * t1d
      r10 = (tl + kp989821441 * tE) :+ (t24 + kp989821441 * t28)
      r9 = (tJ - kp989821441 * tN) :+ (t1V - kp989821441 * t1Z)
      r8 = (tS + kp989821441 * tW) :+ (t1M + kp989821441 * t1Q)
      r7 = (t11 - kp989821441 * t15) :+ (t1D - kp989821441 * t1H)
      r6 = (t1a + kp989821441 * t1e) :+ (t1p + kp989821441 * t1y)
      r5 = (t1a - kp989821441 * t1e) :+ (t1p - kp989821441 * t1y)
      r4 = (t11 + kp989821441 * t15) :+ (t1D + kp989821441 * t1H)
      r3 = (tS - kp989821441 * tW) :+ (t1M - kp989821441 * t1Q)
      r2 = (tJ + kp989821441 * tN) :+ (t1V + kp989821441 * t1Z)
      r1 = (tl - kp989821441 * tE) :+ (t24 - kp989821441 * t28)
  MV.write xsout 0 $ (xr0+t4+t7+ta+td+tg) :+ (xi0+t1g+t1h+t1i+t1j+t1k)
  MV.write xsout 1 $ if sign == 1 then r1 else r10
  MV.write xsout 2 $ if sign == 1 then r2 else r9
  MV.write xsout 3 $ if sign == 1 then r3 else r8
  MV.write xsout 4 $ if sign == 1 then r4 else r7
  MV.write xsout 5 $ if sign == 1 then r5 else r6
  MV.write xsout 6 $ if sign == 1 then r6 else r5
  MV.write xsout 7 $ if sign == 1 then r7 else r4
  MV.write xsout 8 $ if sign == 1 then r8 else r3
  MV.write xsout 9 $ if sign == 1 then r9 else r2
  MV.write xsout 10 $ if sign == 1 then r10 else r1

-- | Length 13 hard-coded FFT.


-- | Length 4 hard-coded FFT.
-- | Length 8 hard-coded FFT.
-- | Length 16 hard-coded FFT.
-- | Length 32 hard-coded FFT.
-- | Length 64 hard-coded FFT.


-- | Length 6 hard-coded FFT.
-- | Length 9 hard-coded FFT.
-- | Length 10 hard-coded FFT.
-- | Length 12 hard-coded FFT.
-- | Length 14 hard-coded FFT.
-- | Length 15 hard-coded FFT.
-- | Length 20 hard-coded FFT.
-- | Length 25 hard-coded FFT.
