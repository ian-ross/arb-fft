module Numeric.FFT.Special.Miscellaneous
       ( special6, special9, special10, special12, special14
       , special15, special20, special25
       ) where

import           Control.Monad.ST
import           Data.Complex
import qualified Data.Vector.Unboxed.Mutable as MV

import           Numeric.FFT.Types


-- | Length 6 hard-coded FFT.
kp866025403, kp500000000 :: Double
kp866025403 = 0.866025403784438646763723170752936183471402627
kp500000000 = 0.500000000000000000000000000000000000000000000
special6 :: Int -> MVCD s -> MVCD s -> ST s ()
special6 sign xsin xsout = do
  xr0 :+ xi0 <- MV.unsafeRead xsin 0 ; xr1 :+ xi1 <- MV.unsafeRead xsin 1
  xr2 :+ xi2 <- MV.unsafeRead xsin 2 ; xr3 :+ xi3 <- MV.unsafeRead xsin 3
  xr4 :+ xi4 <- MV.unsafeRead xsin 4 ; xr5 :+ xi5 <- MV.unsafeRead xsin 5
  let tb = xr0 + xr3 ; t3 = xr0 - xr3 ; tx = xi0 + xi3 ; tp = xi0 - xi3
      tc = xr2 + xr5 ; t6 = xr2 - xr5 ; td = xr4 + xr1 ; t9 = xr4 - xr1
      te = tc + td ; tA = td - tc ; ts = t9 - t6 ; ta = t6 + t9
      tu = xi2 + xi5 ; ti = xi2 - xi5 ; tf = t3 - kp500000000 * ta
      tv = xi4 + xi1 ; tl = xi4 - xi1 ; tt = tb - kp500000000 * te
      ty = tu + tv ; tw = tu - tv ; tq = ti + tl ; tm = ti - tl
      tr = tp - kp500000000 * tq ; tz = tx - kp500000000 * ty
      r5 = (tf + kp866025403 * tm) :+ (tr + kp866025403 * ts)
      r4 = (tt - kp866025403 * tw) :+ (tz - kp866025403 * tA)
      r3 = (t3 + ta) :+ (tp + tq)
      r2 = (tt + kp866025403 * tw) :+ (tz + kp866025403 * tA)
      r1 = (tf - kp866025403 * tm) :+ (tr - kp866025403 * ts)
  MV.unsafeWrite xsout 0 $ (tb + te) :+ (tx + ty)
  MV.unsafeWrite xsout 1 $ if sign == 1 then r1 else r5
  MV.unsafeWrite xsout 2 $ if sign == 1 then r2 else r4
  MV.unsafeWrite xsout 3 $ if sign == 1 then r3 else r3
  MV.unsafeWrite xsout 4 $ if sign == 1 then r4 else r2
  MV.unsafeWrite xsout 5 $ if sign == 1 then r5 else r1

-- | Length 9 hard-coded FFT.
kp954188894, kp363970234, kp852868531, kp984807753 :: Double
kp492403876, kp777861913, kp839099631, kp176326980 :: Double
--kp866025403, kp500000000 :: Double
kp954188894 = 0.954188894138671133499268364187245676532219158
kp363970234 = 0.363970234266202361351047882776834043890471784
kp852868531 = 0.852868531952443209628250963940074071936020296
kp984807753 = 0.984807753012208059366743024589523013670643252
kp492403876 = 0.492403876506104029683371512294761506835321626
kp777861913 = 0.777861913430206160028177977318626690410586096
kp839099631 = 0.839099631177280011763127298123181364687434283
kp176326980 = 0.176326980708464973471090386868618986121633062
--kp866025403 = 0.866025403784438646763723170752936183471402627
--kp500000000 = 0.500000000000000000000000000000000000000000000
special9 :: Int -> MVCD s -> MVCD s -> ST s ()
special9 sign xsin xsout = do
  xr0 :+ xi0 <- MV.unsafeRead xsin 0 ; xr1 :+ xi1 <- MV.unsafeRead xsin 1
  xr2 :+ xi2 <- MV.unsafeRead xsin 2 ; xr3 :+ xi3 <- MV.unsafeRead xsin 3
  xr4 :+ xi4 <- MV.unsafeRead xsin 4 ; xr5 :+ xi5 <- MV.unsafeRead xsin 5
  xr6 :+ xi6 <- MV.unsafeRead xsin 6 ; xr7 :+ xi7 <- MV.unsafeRead xsin 7
  xr8 :+ xi8 <- MV.unsafeRead xsin 8
  let t4 = xr3 + xr6 ; tm = xr6 - xr3 ; tM = xi3 - xi6 ; tk = xi3 + xi6
      tL = xr0 - kp500000000 * t4 ; t5 = xr0 + t4
      tl = xi0 - kp500000000 * tk ; t1f = xi0 + tk
      tE = xr4 - xr7 ; t9 = xr4 + xr7 ; tH = xi7 - xi4 ; tC = xi4 + xi7
      ta = xr1 + t9 ; tG = xr1 - kp500000000 * t9
      t1c = xi1 + tC ; tD = xi1 - kp500000000 * tC
      tI = tG - kp866025403 * tH ; tX = tG + kp866025403 * tH
      tF = tD - kp866025403 * tE ; tW = tD + kp866025403 * tE
      t17 = tl - kp866025403 * tm ; tn = tl + kp866025403 * tm
      tw = xr8 - xr5 ; te = xr5 + xr8 ; tu = xi5 + xi8 ; tr = xi5 - xi8
      tN = tL + kp866025403 * tM ; tV = tL - kp866025403 * tM
      tf = xr2 + te ; to = xr2 - kp500000000 * te
      t1d = xi2 + tu ; tv = xi2 - kp500000000 * tu
      ts = to + kp866025403 * tr ; tZ = to - kp866025403 * tr
      tg = ta + tf ; t1i = tf - ta
      tx = tv + kp866025403 * tw ; t10 = tv - kp866025403 * tw
      t1e = t1c - t1d ; t1g = t1c + t1d
      t1b = t5 - kp500000000 * tg ; t1h = t1f - kp500000000 * t1g
      tO = tx + kp176326980 * ts ; ty = ts - kp176326980 * tx
      tJ = tF - kp839099631 * tI ; tP = tI + kp839099631 * tF
      tS = ty + kp777861913 * tJ ; tK = ty - kp777861913 * tJ
      tU = tO - kp777861913 * tP ; tQ = tO + kp777861913 * tP
      tT = tn + kp492403876 * tK ; tR = tN - kp492403876 * tQ
      t14 = tX - kp176326980 * tW ; tY = tW + kp176326980 * tX
      t11 = tZ - kp363970234 * t10 ; t15 = t10 + kp363970234 * tZ
      t12 = tY - kp954188894 * t11 ; t1a = tY + kp954188894 * t11
      t16 = t14 - kp954188894 * t15 ; t18 = t14 + kp954188894 * t15
      t13 = tV - kp492403876 * t12 ; t19 = t17 + kp492403876 * t18
      r8 = (tN + kp984807753 * tQ) :+ (tn - kp984807753 * tK)
      r7 = (tV + kp984807753 * t12) :+ (t17 - kp984807753 * t18)
      r6 = (t1b + kp866025403 * t1e) :+ (t1h + kp866025403 * t1i)
      r5 = (tR + kp852868531 * tS) :+ (tT + kp852868531 * tU)
      r4 = (t13 - kp852868531 * t16) :+ (t19 - kp852868531 * t1a)
      r3 = (t1b - kp866025403 * t1e) :+ (t1h - kp866025403 * t1i)
      r2 = (tR - kp852868531 * tS) :+ (tT - kp852868531 * tU)
      r1 = (t13 + kp852868531 * t16) :+ (t19 + kp852868531 * t1a)
  MV.unsafeWrite xsout 0 $ (t5 + tg) :+ (t1f + t1g)
  MV.unsafeWrite xsout 1 $ if sign == 1 then r1 else r8
  MV.unsafeWrite xsout 2 $ if sign == 1 then r2 else r7
  MV.unsafeWrite xsout 3 $ if sign == 1 then r3 else r6
  MV.unsafeWrite xsout 4 $ if sign == 1 then r4 else r5
  MV.unsafeWrite xsout 5 $ if sign == 1 then r5 else r4
  MV.unsafeWrite xsout 6 $ if sign == 1 then r6 else r3
  MV.unsafeWrite xsout 7 $ if sign == 1 then r7 else r2
  MV.unsafeWrite xsout 8 $ if sign == 1 then r8 else r1

-- | Length 10 hard-coded FFT.
kp951056516, kp559016994, kp250000000, kp618033988 :: Double
kp951056516 = 0.951056516295153572116439333379382143405698634
kp559016994 = 0.559016994374947424102293417182819058860154590
kp250000000 = 0.250000000000000000000000000000000000000000000
kp618033988 = 0.618033988749894848204586834365638117720309180
special10 :: Int -> MVCD s -> MVCD s -> ST s ()
special10 sign xsin xsout = do
  xr0 :+ xi0 <- MV.unsafeRead xsin 0 ; xr1 :+ xi1 <- MV.unsafeRead xsin 1
  xr2 :+ xi2 <- MV.unsafeRead xsin 2 ; xr3 :+ xi3 <- MV.unsafeRead xsin 3
  xr4 :+ xi4 <- MV.unsafeRead xsin 4 ; xr5 :+ xi5 <- MV.unsafeRead xsin 5
  xr6 :+ xi6 <- MV.unsafeRead xsin 6 ; xr7 :+ xi7 <- MV.unsafeRead xsin 7
  xr8 :+ xi8 <- MV.unsafeRead xsin 8 ; xr9 :+ xi9 <- MV.unsafeRead xsin 9
  let tj = xr0 + xr5 ; t3 = xr0 - xr5 ; t1b = xi0 + xi5 ; tN = xi0 - xi5
      tk = xr2 + xr7 ; t6 = xr2 - xr7 ; to = xr6 + xr1 ; tg = xr6 - xr1
      tl = xr8 + xr3 ; t9 = xr8 - xr3 ; tn = xr4 + xr9 ; td = xr4 - xr9
      tm = tk + tl ; t1j = tk - tl ; ta = t6 + t9 ; tU = t6 - t9
      tp = tn + to ; t1i = tn - to ; th = td + tg ; tV = td - tg
      tq = tm + tp ; t10 = tm - tp ; ti = ta + th ; ts = ta - th
      tw = xi2 - xi7 ; t15 = xi2 + xi7 ; t13 = xi6 + xi1 ; tG = xi6 - xi1
      t16 = xi8 + xi3 ; tz = xi8 - xi3 ; t12 = xi4 + xi9 ; tD = xi4 - xi9
      t1c = t15 + t16 ; t17 = t15 - t16 ; tO = tw + tz ; tA = tw - tz
      t1d = t12 + t13 ; t14 = t12 - t13 ; tP = tD + tG ; tH = tD - tG
      t1e = t1c + t1d ; t1g = t1c - t1d ; tQ = tO + tP ; tS = tO - tP
      tK = tH - kp618033988 * tA ; tI = tA + kp618033988 * tH
      tr = t3 - kp250000000 * ti ; tY = tV - kp618033988 * tU
      tW = tU + kp618033988 * tV ; tR = tN - kp250000000 * tQ
      tJ = tr - kp559016994 * ts ; tt = tr + kp559016994 * ts
      t1a = t17 + kp618033988 * t14 ; t18 = t14 - kp618033988 * t17
      tX = tR - kp559016994 * tS ; tT = tR + kp559016994 * tS
      tZ = tj - kp250000000 * tq ; t1m = t1j + kp618033988 * t1i
      t1k = t1i - kp618033988 * t1j ; t1f = t1b - kp250000000 * t1e
      t19 = tZ + kp559016994 * t10 ; t11 = tZ - kp559016994 * t10
      t1h = t1f - kp559016994 * t1g ; t1l = t1f + kp559016994 * t1g
      r9 = (tt + kp951056516 * tI) :+ (tT - kp951056516 * tW)
      r8 = (t11 - kp951056516 * t18) :+ (t1h + kp951056516 * t1k)
      r7 = (tJ + kp951056516 * tK) :+ (tX - kp951056516 * tY)
      r6 = (t19 - kp951056516 * t1a) :+ (t1l + kp951056516 * t1m)
      r5 = (t3 + ti) :+ (tN + tQ)
      r4 = (t19 + kp951056516 * t1a) :+ (t1l - kp951056516 * t1m)
      r3 = (tJ - kp951056516 * tK) :+ (tX + kp951056516 * tY)
      r2 = (t11 + kp951056516 * t18) :+ (t1h - kp951056516 * t1k)
      r1 = (tt - kp951056516 * tI) :+ (tT + kp951056516 * tW)
  MV.unsafeWrite xsout 0 $ (tj + tq) :+ (t1b + t1e)
  MV.unsafeWrite xsout 1 $ if sign == 1 then r1 else r9
  MV.unsafeWrite xsout 2 $ if sign == 1 then r2 else r8
  MV.unsafeWrite xsout 3 $ if sign == 1 then r3 else r7
  MV.unsafeWrite xsout 4 $ if sign == 1 then r4 else r6
  MV.unsafeWrite xsout 5 $ if sign == 1 then r5 else r5
  MV.unsafeWrite xsout 6 $ if sign == 1 then r6 else r4
  MV.unsafeWrite xsout 7 $ if sign == 1 then r7 else r3
  MV.unsafeWrite xsout 8 $ if sign == 1 then r8 else r2
  MV.unsafeWrite xsout 9 $ if sign == 1 then r9 else r1

-- | Length 12 hard-coded FFT.
--kp866025403, kp500000000 :: Double
--kp866025403 = 0.866025403784438646763723170752936183471402627
--kp500000000 = 0.500000000000000000000000000000000000000000000
special12 :: Int -> MVCD s -> MVCD s -> ST s ()
special12 sign xsin xsout = do
  xr0  :+ xi0  <- MV.unsafeRead xsin 0  ; xr1  :+ xi1  <- MV.unsafeRead xsin 1
  xr2  :+ xi2  <- MV.unsafeRead xsin 2  ; xr3  :+ xi3  <- MV.unsafeRead xsin 3
  xr4  :+ xi4  <- MV.unsafeRead xsin 4  ; xr5  :+ xi5  <- MV.unsafeRead xsin 5
  xr6  :+ xi6  <- MV.unsafeRead xsin 6  ; xr7  :+ xi7  <- MV.unsafeRead xsin 7
  xr8  :+ xi8  <- MV.unsafeRead xsin 8  ; xr9  :+ xi9  <- MV.unsafeRead xsin 9
  xr10 :+ xi10 <- MV.unsafeRead xsin 10 ; xr11 :+ xi11 <- MV.unsafeRead xsin 11
  let t4 = xr4 + xr8 ; tA = xr8 - xr4 ; tS = xi4 - xi8 ; tr = xi4 + xi8
      tR = xr0 - kp500000000 * t4 ; t5 = xr0 + t4 ; ts = xi0 + tr
      tz = xi0 - kp500000000 * tr ; t9 = xr10 + xr2 ; tD = xr2 - xr10
      tV = xi10 - xi2 ; tw = xi10 + xi2 ; tU = xr6 - kp500000000 * t9
      ta = xr6 + t9 ; tx = xi6 + tw ; tC = xi6 - kp500000000 * tw
      tf = xr7 + xr11 ; t1d = xr11 - xr7 ; tJ = xi7 - xi11 ; t1b = xi7 + xi11
      tG = xr3 - kp500000000 * tf ; tg = xr3 + tf ; t1u = xi3 + t1b
      t1c = xi3 - kp500000000 * t1b ; tk = xr1 + xr5 ; t1i = xr5 - xr1
      t1t = t5 - ta ; tb = t5 + ta ; tO = xi1 - xi5 ; t1g = xi1 + xi5
      tL = xr9 - kp500000000 * tk ; tl = xr9 + tk ; t1x = ts + tx
      ty = ts - tx ; t1v = xi9 + t1g ; t1h = xi9 - kp500000000 * t1g
      tn = tg - tl ; tm = tg + tl ; t1y = t1u + t1v ; t1w = t1u - t1v
      tB = tz - kp866025403 * tA ; tZ = tz + kp866025403 * tA
      t10 = tC + kp866025403 * tD ; tE = tC - kp866025403 * tD
      t1o = t1c - kp866025403 * t1d ; t1e = t1c + kp866025403 * t1d
      t1l = tZ + t10 ; t11 = tZ - t10 ; t1j = t1h + kp866025403 * t1i
      t1p = t1h - kp866025403 * t1i ; tK = tG - kp866025403 * tJ
      t12 = tG + kp866025403 * tJ ; t13 = tL + kp866025403 * tO
      tP = tL - kp866025403 * tO ; tT = tR - kp866025403 * tS
      t15 = tR + kp866025403 * tS ; t1m = t1e + t1j ; t1k = t1e - t1j
      t18 = t12 + t13 ; t14 = t12 - t13 ; t16 = tU + kp866025403 * tV
      tW = tU - kp866025403 * tV ; t17 = t15 + t16 ; t19 = t15 - t16
      t1r = tB + tE ; tF = tB - tE ; t1s = t1o + t1p ; t1q = t1o - t1p
      tY = tK + tP ; tQ = tK - tP ; tX = tT + tW ; t1n = tT - tW
      r11 = (t19 + t1k) :+ (t11 - t14)
      r10 = (tX - tY) :+ (t1r - t1s)
      r9 = (t1t - t1w) :+ (tn + ty)
      r8 = (t17 + t18) :+ (t1l + t1m)
      r7 = (t1n + t1q) :+ (tF - tQ)
      r6 = (tb - tm) :+ (t1x - t1y)
      r5 = (t19 - t1k) :+ (t11 + t14)
      r4 = (tX + tY) :+ (t1r + t1s)
      r3 = (t1t + t1w) :+ (ty - tn)
      r2 = (t17 - t18) :+ (t1l - t1m)
      r1 = (t1n - t1q) :+ (tF + tQ)
  MV.unsafeWrite xsout 0 $ (tb + tm) :+ (t1x + t1y)
  MV.unsafeWrite xsout 1 $ if sign == 1 then r1 else r11
  MV.unsafeWrite xsout 2 $ if sign == 1 then r2 else r10
  MV.unsafeWrite xsout 3 $ if sign == 1 then r3 else r9
  MV.unsafeWrite xsout 4 $ if sign == 1 then r4 else r8
  MV.unsafeWrite xsout 5 $ if sign == 1 then r5 else r7
  MV.unsafeWrite xsout 6 $ if sign == 1 then r6 else r6
  MV.unsafeWrite xsout 7 $ if sign == 1 then r7 else r5
  MV.unsafeWrite xsout 8 $ if sign == 1 then r8 else r4
  MV.unsafeWrite xsout 9 $ if sign == 1 then r9 else r3
  MV.unsafeWrite xsout 10 $ if sign == 1 then r10 else r2
  MV.unsafeWrite xsout 11 $ if sign == 1 then r11 else r1

-- | Length 14 hard-coded FFT.
kp974927912, kp801937735, kp900968867 :: Double
kp554958132, kp692021471, kp356895867 :: Double
kp974927912 = 0.974927912181823607018131682993931217232785801
kp801937735 = 0.801937735804838252472204639014890102331838324
kp900968867 = 0.900968867902419126236102319507445051165919162
kp554958132 = 0.554958132087371191422194871006410481067288862
kp692021471 = 0.692021471630095869627814897002069140197260599
kp356895867 = 0.356895867892209443894399510021300583399127187
special14 :: Int -> MVCD s -> MVCD s -> ST s ()
special14 sign xsin xsout = do
  xr0  :+ xi0  <- MV.unsafeRead xsin 0  ; xr1  :+ xi1  <- MV.unsafeRead xsin 1
  xr2  :+ xi2  <- MV.unsafeRead xsin 2  ; xr3  :+ xi3  <- MV.unsafeRead xsin 3
  xr4  :+ xi4  <- MV.unsafeRead xsin 4  ; xr5  :+ xi5  <- MV.unsafeRead xsin 5
  xr6  :+ xi6  <- MV.unsafeRead xsin 6  ; xr7  :+ xi7  <- MV.unsafeRead xsin 7
  xr8  :+ xi8  <- MV.unsafeRead xsin 8  ; xr9  :+ xi9  <- MV.unsafeRead xsin 9
  xr10 :+ xi10 <- MV.unsafeRead xsin 10 ; xr11 :+ xi11 <- MV.unsafeRead xsin 11
  xr12 :+ xi12 <- MV.unsafeRead xsin 12 ; xr13 :+ xi13 <- MV.unsafeRead xsin 13
  let tp = xr0 + xr7 ; t3 = xr0 - xr7 ; t1x = xi0 + xi7 ; t1b = xi0 - xi7
      tq = xr2 + xr9 ; t6 = xr2 - xr9 ; tr = xr12 + xr5 ; t9 = xr12 - xr5
      tx = xr8 + xr1 ; tn = xr8 - xr1 ; tw = xr6 + xr13 ; tk = xr6 - xr13
      to = tk + tn ; t1i = tn - tk ; tu = xr10 + xr3 ; tg = xr10 - xr3
      tt = xr4 + xr11 ; td = xr4 - xr11 ; t1M = tr - tq ; ts = tq + tr
      ta = t6 + t9 ; t1k = t9 - t6 ; t1L = tt - tu ; tv = tt + tu
      th = td + tg ; t1j = tg - td ; t1K = tw - tx ; ty = tw + tx
      tZ = to - kp356895867 * ta ; t14 = th - kp356895867 * to
      tz = ta - kp356895867 * th ; t1Z = ty - kp356895867 * ts
      t27 = ts - kp356895867 * tv ; t2c = tv - kp356895867 * ty
      t1B = xi4 + xi11 ; tE = xi4 - xi11 ; t1C = xi10 + xi3 ; tH = xi10 - xi3
      t1F = xi8 + xi1 ; tV = xi8 - xi1 ; t1E = xi6 + xi13 ; tS = xi6 - xi13
      t1z = xi12 + xi5 ; tO = xi12 - xi5 ; t1d = tE + tH ; tI = tE - tH
      t23 = t1F - t1E ; t1G = t1E + t1F ; t1D = t1B + t1C ; t24 = t1C - t1B
      t1y = xi2 + xi9 ; tL = xi2 - xi9 ; tW = tS - tV ; t1e = tS + tV
      t22 = t1y - t1z ; t1A = t1y + t1z ; tP = tL - tO ; t1c = tL + tO
      t1n = t1e - kp356895867 * t1c ; t1s = t1c - kp356895867 * t1d
      t1f = t1d - kp356895867 * t1e ; t1P = t1G - kp356895867 * t1A
      t1U = t1A - kp356895867 * t1D ; t1H = t1D - kp356895867 * t1G
      tA = to - kp692021471 * tz ; tX = tP + kp554958132 * tW
      t1t = t1e - kp692021471 * t1s ; t1v = t1k + kp554958132 * t1i
      tB = t3 - kp900968867 * tA ; tY = tI + kp801937735 * tX
      t1u = t1b - kp900968867 * t1t ; t1w = t1j + kp801937735 * t1v
      t10 = th - kp692021471 * tZ ; t11 = t3 - kp900968867 * t10
      t12 = tW + kp554958132 * tI ; t1o = t1d - kp692021471 * t1n
      t1q = t1i + kp554958132 * t1j ; t15 = ta - kp692021471 * t14
      t13 = tP - kp801937735 * t12 ; t1p = t1b - kp900968867 * t1o
      t1r = t1k - kp801937735 * t1q ; t16 = t3 - kp900968867 * t15
      t17 = tI - kp554958132 * tP ; t1g = t1c - kp692021471 * t1f
      t1l = t1j - kp554958132 * t1k ; t1I = t1A - kp692021471 * t1H
      t18 = tW - kp801937735 * t17 ; t1h = t1b - kp900968867 * t1g
      t1m = t1i - kp801937735 * t1l ; t1J = t1x - kp900968867 * t1I
      t1N = t1L + kp554958132 * t1M ; t2d = ts - kp692021471 * t2c
      t2f = t24 + kp554958132 * t22 ; t1Q = t1D - kp692021471 * t1P
      t1O = t1K - kp801937735 * t1N ; t2e = tp - kp900968867 * t2d
      t2g = t23 - kp801937735 * t2f ; t1R = t1x - kp900968867 * t1Q
      t1S = t1K + kp554958132 * t1L ; t20 = tv - kp692021471 * t1Z
      t25 = t23 + kp554958132 * t24 ; t1V = t1G - kp692021471 * t1U
      t1T = t1M + kp801937735 * t1S ; t21 = tp - kp900968867 * t20
      t26 = t22 + kp801937735 * t25 ; t1W = t1x - kp900968867 * t1V
      t1X = t1M - kp554958132 * t1K ; t28 = ty - kp692021471 * t27
      t2a = t22 - kp554958132 * t23 ; t1Y = t1L - kp801937735 * t1X
      t29 = tp - kp900968867 * t28 ; t2b = t24 - kp801937735 * t2a
      r13 = (tB + kp974927912 * tY) :+ (t1u + kp974927912 * t1w)
      r12 = (t21 + kp974927912 * t26) :+ (t1R + kp974927912 * t1T)
      r11 = (t16 + kp974927912 * t18) :+ (t1h + kp974927912 * t1m)
      r10 = (t2e + kp974927912 * t2g) :+ (t1J + kp974927912 * t1O)
      r9 = (t11 - kp974927912 * t13) :+ (t1p - kp974927912 * t1r)
      r8 = (t29 + kp974927912 * t2b) :+ (t1W + kp974927912 * t1Y)
      r7 = (t3 + ta + th + to) :+ (t1b + t1c + t1d + t1e)
      r6 = (t29 - kp974927912 * t2b) :+ (t1W - kp974927912 * t1Y)
      r5 = (t11 + kp974927912 * t13) :+ (t1p + kp974927912 * t1r)
      r4 = (t2e - kp974927912 * t2g) :+ (t1J - kp974927912 * t1O)
      r3 = (t16 - kp974927912 * t18) :+ (t1h - kp974927912 * t1m)
      r2 = (t21 - kp974927912 * t26) :+ (t1R - kp974927912 * t1T)
      r1 = (tB - kp974927912 * tY) :+ (t1u - kp974927912 * t1w)
  MV.unsafeWrite xsout 0 $ (tp + ts + tv + ty) :+ (t1x + t1A + t1D + t1G)
  MV.unsafeWrite xsout 1 $ if sign == 1 then r1 else r13
  MV.unsafeWrite xsout 2 $ if sign == 1 then r2 else r12
  MV.unsafeWrite xsout 3 $ if sign == 1 then r3 else r11
  MV.unsafeWrite xsout 4 $ if sign == 1 then r4 else r10
  MV.unsafeWrite xsout 5 $ if sign == 1 then r5 else r9
  MV.unsafeWrite xsout 6 $ if sign == 1 then r6 else r8
  MV.unsafeWrite xsout 7 $ if sign == 1 then r7 else r7
  MV.unsafeWrite xsout 8 $ if sign == 1 then r8 else r6
  MV.unsafeWrite xsout 9 $ if sign == 1 then r9 else r5
  MV.unsafeWrite xsout 10 $ if sign == 1 then r10 else r4
  MV.unsafeWrite xsout 11 $ if sign == 1 then r11 else r3
  MV.unsafeWrite xsout 12 $ if sign == 1 then r12 else r2
  MV.unsafeWrite xsout 13 $ if sign == 1 then r13 else r1

-- | Length 15 hard-coded FFT.
--kp951056516, kp559016994, kp618033988 :: Double
--kp250000000, kp866025403, kp500000000 :: Double
--kp951056516 = 0.951056516295153572116439333379382143405698634
--kp559016994 = 0.559016994374947424102293417182819058860154590
--kp618033988 = 0.618033988749894848204586834365638117720309180
--kp250000000 = 0.250000000000000000000000000000000000000000000
--kp866025403 = 0.866025403784438646763723170752936183471402627
--kp500000000 = 0.500000000000000000000000000000000000000000000
special15 :: Int -> MVCD s -> MVCD s -> ST s ()
special15 sign xsin xsout = do
  xr0  :+ xi0  <- MV.unsafeRead xsin 0  ; xr1  :+ xi1  <- MV.unsafeRead xsin 1
  xr2  :+ xi2  <- MV.unsafeRead xsin 2  ; xr3  :+ xi3  <- MV.unsafeRead xsin 3
  xr4  :+ xi4  <- MV.unsafeRead xsin 4  ; xr5  :+ xi5  <- MV.unsafeRead xsin 5
  xr6  :+ xi6  <- MV.unsafeRead xsin 6  ; xr7  :+ xi7  <- MV.unsafeRead xsin 7
  xr8  :+ xi8  <- MV.unsafeRead xsin 8  ; xr9  :+ xi9  <- MV.unsafeRead xsin 9
  xr10 :+ xi10 <- MV.unsafeRead xsin 10 ; xr11 :+ xi11 <- MV.unsafeRead xsin 11
  xr12 :+ xi12 <- MV.unsafeRead xsin 12 ; xr13 :+ xi13 <- MV.unsafeRead xsin 13
  xr14 :+ xi14 <- MV.unsafeRead xsin 14
  let t1y = xr10 - xr5 ; t4 = xr5 + xr10 ; t1w = xi5 + xi10 ; tw = xi5 - xi10
      t5 = xr0 + t4 ; tt = xr0 - kp500000000 * t4
      t2l = xi0 + t1w ; t1x = xi0 - kp500000000 * t1w
      tx = tt - kp866025403 * tw ; tV = tt + kp866025403 * tw
      t1z = t1x + kp866025403 * t1y ; t1X = t1x - kp866025403 * t1y
      tk = xr11 + xr1 ; t1k = xr1 - xr11 ; tM = xi11 - xi1 ; t1i = xi11 + xi1
      tJ = xr6 - kp500000000 * tk ; tl = xr6 + tk ; t2c = xi6 + t1i
      t1j = xi6 - kp500000000 * t1i ; t1p = xr4 - xr14 ; tp = xr14 + xr4
      tN = tJ - kp866025403 * tM ; tZ = tJ + kp866025403 * tM
      tO = xr9 - kp500000000 * tp ; tq = xr9 + tp ; t1n = xi14 + xi4
      tR = xi14 - xi4 ; t2s = tl - tq ; tr = tl + tq
      t10 = tO + kp866025403 * tR ; tS = tO - kp866025403 * tR
      t1o = xi9 - kp500000000 * t1n ; t2d = xi9 + t1n
      t1O = t1j - kp866025403 * t1k ; t1l = t1j + kp866025403 * t1k
      t24 = tN - tS ; tT = tN + tS ; t1P = t1o - kp866025403 * t1p
      t1q = t1o + kp866025403 * t1p ; t2e = t2c - t2d ; t2n = t2c + t2d
      t1Z = t1O + t1P ; t1Q = t1O - t1P ; t1r = t1l - t1q ; t1B = t1l + t1q
      t11 = tZ + t10 ; t1H = tZ - t10 ; t9 = xr8 + xr13 ; t19 = xr13 - xr8
      tB = xi8 - xi13 ; t17 = xi8 + xi13 ; ty = xr3 - kp500000000 * t9
      ta = xr3 + t9 ; t2f = xi3 + t17 ; t18 = xi3 - kp500000000 * t17
      t1e = xr7 - xr2 ; te = xr2 + xr7 ; tC = ty - kp866025403 * tB
      tW = ty + kp866025403 * tB ; tD = xr12 - kp500000000 * te
      tf = xr12 + te ; t1c = xi2 + xi7 ; tG = xi2 - xi7 ; t2t = ta - tf
      tg = ta + tf ; tX = tD + kp866025403 * tG
      tH = tD - kp866025403 * tG ; t1d = xi12 - kp500000000 * t1c
      t2g = xi12 + t1c ; t1R = t18 - kp866025403 * t19
      t1a = t18 + kp866025403 * t19 ; t25 = tC - tH ; tI = tC + tH
      t1S = t1d - kp866025403 * t1e ; t1f = t1d + kp866025403 * t1e
      t2h = t2f - t2g ; t2m = t2f + t2g ; t1Y = t1R + t1S ; t1T = t1R - t1S
      t1g = t1a - t1f ; t1A = t1a + t1f ; t2a = tg - tr ; ts = tg + tr
      tY = tW + tX ; t1G = tW - tX ; t29 = t5 - kp250000000 * ts
      t2o = t2m + t2n ; t2q = t2m - t2n ; t2k = t2h + kp618033988 * t2e
      t2i = t2e - kp618033988 * t2h ; t2b = t29 - kp559016994 * t2a
      t2j = t29 + kp559016994 * t2a ; t2p = t2l - kp250000000 * t2o
      tU = tI + tT ; t1M = tI - tT ; t2r = t2p - kp559016994 * t2q
      t2v = t2p + kp559016994 * t2q ; t2w = t2t + kp618033988 * t2s
      t2u = t2s - kp618033988 * t2t ; t1L = tx - kp250000000 * tU
      t20 = t1Y + t1Z ; t22 = t1Y - t1Z ; t1N = t1L - kp559016994 * t1M
      t1V = t1L + kp559016994 * t1M ; t1W = t1T + kp618033988 * t1Q
      t1U = t1Q - kp618033988 * t1T ; t21 = t1X - kp250000000 * t20
      t1C = t1A + t1B ; t1E = t1A - t1B ; t23 = t21 - kp559016994 * t22
      t27 = t21 + kp559016994 * t22 ; t28 = t25 + kp618033988 * t24
      t26 = t24 - kp618033988 * t25 ; t1D = t1z - kp250000000 * t1C
      t12 = tY + t11 ; t14 = tY - t11 ; t1F = t1D + kp559016994 * t1E
      t1J = t1D - kp559016994 * t1E ; t1K = t1H - kp618033988 * t1G
      t1I = t1G + kp618033988 * t1H ; t13 = tV - kp250000000 * t12
      t1t = t13 - kp559016994 * t14 ; t15 = t13 + kp559016994 * t14
      t1s = t1g + kp618033988 * t1r ; t1u = t1r - kp618033988 * t1g
      r14 = (t15 + kp951056516 * t1s) :+ (t1F - kp951056516 * t1I)
      r13 = (t1N - kp951056516 * t1U) :+ (t23 + kp951056516 * t26)
      r12 = (t2b + kp951056516 * t2i) :+ (t2r - kp951056516 * t2u)
      r11 = (t15 - kp951056516 * t1s) :+ (t1F + kp951056516 * t1I)
      r10 = (tx + tU) :+ (t1X + t20)
      r9 = (t2j + kp951056516 * t2k) :+ (t2v - kp951056516 * t2w)
      r8 = (t1t - kp951056516 * t1u) :+ (t1J + kp951056516 * t1K)
      r7 = (t1N + kp951056516 * t1U) :+ (t23 - kp951056516 * t26)
      r6 = (t2j - kp951056516 * t2k) :+ (t2v + kp951056516 * t2w)
      r5 = (tV + t12) :+ (t1z + t1C)
      r4 = (t1V + kp951056516 * t1W) :+ (t27 - kp951056516 * t28)
      r3 = (t2b - kp951056516 * t2i) :+ (t2r + kp951056516 * t2u)
      r2 = (t1t + kp951056516 * t1u) :+ (t1J - kp951056516 * t1K)
      r1 = (t1V - kp951056516 * t1W) :+ (t27 + kp951056516 * t28)
  MV.unsafeWrite xsout 0 $ (t5 + ts) :+ (t2l + t2o)
  MV.unsafeWrite xsout 1 $ if sign == 1 then r1 else r14
  MV.unsafeWrite xsout 2 $ if sign == 1 then r2 else r13
  MV.unsafeWrite xsout 3 $ if sign == 1 then r3 else r12
  MV.unsafeWrite xsout 4 $ if sign == 1 then r4 else r11
  MV.unsafeWrite xsout 5 $ if sign == 1 then r5 else r10
  MV.unsafeWrite xsout 6 $ if sign == 1 then r6 else r9
  MV.unsafeWrite xsout 7 $ if sign == 1 then r7 else r8
  MV.unsafeWrite xsout 8 $ if sign == 1 then r8 else r7
  MV.unsafeWrite xsout 9 $ if sign == 1 then r9 else r6
  MV.unsafeWrite xsout 10 $ if sign == 1 then r10 else r5
  MV.unsafeWrite xsout 11 $ if sign == 1 then r11 else r4
  MV.unsafeWrite xsout 12 $ if sign == 1 then r12 else r3
  MV.unsafeWrite xsout 13 $ if sign == 1 then r13 else r2
  MV.unsafeWrite xsout 14 $ if sign == 1 then r14 else r1

-- | Length 20 hard-coded FFT.
--kp951056516, kp559016994, kp618033988, kp250000000 :: Double
--kp951056516 = 0.951056516295153572116439333379382143405698634
--kp559016994 = 0.559016994374947424102293417182819058860154590
--kp618033988 = 0.618033988749894848204586834365638117720309180
--kp250000000 = 0.250000000000000000000000000000000000000000000
special20 :: Int -> MVCD s -> MVCD s -> ST s ()
special20 sign xsin xsout = do
  xr0  :+ xi0  <- MV.unsafeRead xsin 0  ; xr1  :+ xi1  <- MV.unsafeRead xsin 1
  xr2  :+ xi2  <- MV.unsafeRead xsin 2  ; xr3  :+ xi3  <- MV.unsafeRead xsin 3
  xr4  :+ xi4  <- MV.unsafeRead xsin 4  ; xr5  :+ xi5  <- MV.unsafeRead xsin 5
  xr6  :+ xi6  <- MV.unsafeRead xsin 6  ; xr7  :+ xi7  <- MV.unsafeRead xsin 7
  xr8  :+ xi8  <- MV.unsafeRead xsin 8  ; xr9  :+ xi9  <- MV.unsafeRead xsin 9
  xr10 :+ xi10 <- MV.unsafeRead xsin 10 ; xr11 :+ xi11 <- MV.unsafeRead xsin 11
  xr12 :+ xi12 <- MV.unsafeRead xsin 12 ; xr13 :+ xi13 <- MV.unsafeRead xsin 13
  xr14 :+ xi14 <- MV.unsafeRead xsin 14 ; xr15 :+ xi15 <- MV.unsafeRead xsin 15
  xr16 :+ xi16 <- MV.unsafeRead xsin 16 ; xr17 :+ xi17 <- MV.unsafeRead xsin 17
  xr18 :+ xi18 <- MV.unsafeRead xsin 18 ; xr19 :+ xi19 <- MV.unsafeRead xsin 19
  let t1N = xr0 - xr10 ; t3 = xr0 + xr10 ; t2L = xi0 + xi10 ; tN = xi0 - xi10
      tO = xr5 - xr15 ; t6 = xr5 + xr15 ; t2M = xi5 + xi15 ; t1Q = xi5 - xi15
      t1d = tO + tN ; tP = tN - tO ; tD = t3 + t6 ; t7 = t3 - t6
      t3b = t2L + t2M ; t2N = t2L - t2M ; t2f = t1N + t1Q ; t1R = t1N - t1Q
      t1o = xr8 - xr18 ; tp = xr8 + xr18 ; t2u = xi8 + xi18 ; t13 = xi8 - xi18
      t14 = xr13 - xr3 ; ts = xr13 + xr3 ; t2v = xi13 + xi3 ; t1r = xi13 - xi3
      t1t = xr12 - xr2 ; tw = xr12 + xr2 ; t2x = xi12 + xi2 ; t18 = xi12 - xi2
      tH = tp + ts ; tt = tp - ts ; t19 = xr17 - xr7 ; tz = xr17 + xr7
      t2y = xi17 + xi7 ; t1w = xi17 - xi7 ; t2w = t2u - t2v ; t35 = t2u + t2v
      tI = tw + tz ; tA = tw - tz ; t2z = t2x - t2y ; t36 = t2x + t2y
      t2U = tt - tA ; tB = tt + tA ; t2P = t2w + t2z ; t2A = t2w - t2z
      t3d = t35 + t36 ; t37 = t35 - t36 ; t15 = t13 - t14 ; t1h = t14 + t13
      t1i = t19 + t18 ; t1a = t18 - t19 ; t1s = t1o - t1r ; t29 = t1o + t1r
      t3j = tH - tI ; tJ = tH + tI ; t1x = t1t - t1w ; t2a = t1t + t1w
      t2n = t15 - t1a ; t1b = t15 + t1a ; t1T = t1s + t1x ; t1y = t1s - t1x
      t2b = t29 - t2a ; t2h = t29 + t2a ; t1j = t1h + t1i ; t1Y = t1h - t1i
      ta = xr4 + xr14 ; t1z = xr4 - xr14 ; t2B = xi4 + xi14 ; tS = xi4 - xi14
      tT = xr9 - xr19 ; td = xr9 + xr19 ; t2C = xi9 + xi19 ; t1C = xi9 - xi19
      t1E = xr16 - xr6 ; th = xr16 + xr6 ; t2E = xi16 + xi6 ; tX = xi16 - xi6
      tE = ta + td ; te = ta - td ; tY = xr1 - xr11 ; tk = xr1 + xr11
      t2F = xi1 + xi11 ; t1H = xi1 - xi11 ; t2D = t2B - t2C ; t32 = t2B + t2C
      tF = th + tk ; tl = th - tk ; t2G = t2E - t2F ; t33 = t2E + t2F
      t2V = te - tl ; tm = te + tl ; t2O = t2D + t2G ; t2H = t2D - t2G
      t3c = t32 + t33 ; t34 = t32 - t33 ; tU = tS - tT ; t1e = tT + tS
      t1f = tY + tX ; tZ = tX - tY ; t1D = t1z - t1C ; t26 = t1z + t1C
      t3i = tE - tF ; tG = tE + tF ; t1I = t1E - t1H ; t27 = t1E + t1H
      t2m = tU - tZ ; t10 = tU + tZ ; t1S = t1D + t1I ; t1J = t1D - t1I
      t28 = t26 - t27 ; t2g = t26 + t27 ; t2s = tm - tB ; tC = tm + tB
      t1g = t1e + t1f ; t1Z = t1e - t1f ; t2r = t7 - kp250000000 * tC
      t2Q = t2O + t2P ; t2S = t2O - t2P ; t2K = t2H + kp618033988 * t2A
      t2I = t2A - kp618033988 * t2H ; t2t = t2r - kp559016994 * t2s
      t2J = t2r + kp559016994 * t2s ; t2R = t2N - kp250000000 * t2Q
      tK = tG + tJ ; t30 = tG - tJ ; t2T = t2R - kp559016994 * t2S
      t2X = t2R + kp559016994 * t2S ; t2Y = t2V + kp618033988 * t2U
      t2W = t2U - kp618033988 * t2V ; t2Z = tD - kp250000000 * tK
      t3e = t3c + t3d ; t3g = t3c - t3d ; t31 = t2Z + kp559016994 * t30
      t39 = t2Z - kp559016994 * t30 ; t3a = t37 - kp618033988 * t34
      t38 = t34 + kp618033988 * t37 ; t3f = t3b - kp250000000 * t3e
      t1c = t10 + t1b ; t24 = t10 - t1b ; t3h = t3f + kp559016994 * t3g
      t3l = t3f - kp559016994 * t3g ; t3m = t3j - kp618033988 * t3i
      t3k = t3i + kp618033988 * t3j ; t23 = tP - kp250000000 * t1c
      t2i = t2g + t2h ; t2k = t2g - t2h ; t25 = t23 + kp559016994 * t24
      t2d = t23 - kp559016994 * t24 ; t2e = t2b - kp618033988 * t28
      t2c = t28 + kp618033988 * t2b ; t2j = t2f - kp250000000 * t2i
      t1k = t1g + t1j ; t1m = t1g - t1j ; t2l = t2j + kp559016994 * t2k
      t2p = t2j - kp559016994 * t2k ; t2q = t2n - kp618033988 * t2m
      t2o = t2m + kp618033988 * t2n ; t1l = t1d - kp250000000 * t1k
      t1U = t1S + t1T ; t1W = t1S - t1T ; t1n = t1l - kp559016994 * t1m
      t1L = t1l + kp559016994 * t1m ; t1M = t1J + kp618033988 * t1y
      t1K = t1y - kp618033988 * t1J ; t1V = t1R - kp250000000 * t1U
      t21 = t1V + kp559016994 * t1W ; t1X = t1V - kp559016994 * t1W
      t20 = t1Y - kp618033988 * t1Z ; t22 = t1Z + kp618033988 * t1Y
      r19 = (t2l + kp951056516 * t2o) :+ (t25 - kp951056516 * t2c)
      r18 = (t2t - kp951056516 * t2I) :+ (t2T + kp951056516 * t2W)
      r17 = (t1X + kp951056516 * t20) :+ (t1n - kp951056516 * t1K)
      r16 = (t31 - kp951056516 * t38) :+ (t3h + kp951056516 * t3k)
      r15 = (t2f + t2i) :+ (tP + t1c)
      r14 = (t2J + kp951056516 * t2K) :+ (t2X - kp951056516 * t2Y)
      r13 = (t1X - kp951056516 * t20) :+ (t1n + kp951056516 * t1K)
      r12 = (t39 + kp951056516 * t3a) :+ (t3l - kp951056516 * t3m)
      r11 = (t2l - kp951056516 * t2o) :+ (t25 + kp951056516 * t2c)
      r10 = (t7 + tC) :+ (t2N + t2Q)
      r9 = (t21 + kp951056516 * t22) :+ (t1L - kp951056516 * t1M)
      r8 = (t39 - kp951056516 * t3a) :+ (t3l + kp951056516 * t3m)
      r7 = (t2p + kp951056516 * t2q) :+ (t2d - kp951056516 * t2e)
      r6 = (t2J - kp951056516 * t2K) :+ (t2X + kp951056516 * t2Y)
      r5 = (t1R + t1U) :+ (t1d + t1k)
      r4 = (t31 + kp951056516 * t38) :+ (t3h - kp951056516 * t3k)
      r3 = (t2p - kp951056516 * t2q) :+ (t2d + kp951056516 * t2e)
      r2 = (t2t + kp951056516 * t2I) :+ (t2T - kp951056516 * t2W)
      r1 = (t21 - kp951056516 * t22) :+ (t1L + kp951056516 * t1M)
  MV.unsafeWrite xsout 0 $ (tD + tK) :+ (t3b + t3e)
  MV.unsafeWrite xsout 1 $ if sign == 1 then r1 else r19
  MV.unsafeWrite xsout 2 $ if sign == 1 then r2 else r18
  MV.unsafeWrite xsout 3 $ if sign == 1 then r3 else r17
  MV.unsafeWrite xsout 4 $ if sign == 1 then r4 else r16
  MV.unsafeWrite xsout 5 $ if sign == 1 then r5 else r15
  MV.unsafeWrite xsout 6 $ if sign == 1 then r6 else r14
  MV.unsafeWrite xsout 7 $ if sign == 1 then r7 else r13
  MV.unsafeWrite xsout 8 $ if sign == 1 then r8 else r12
  MV.unsafeWrite xsout 9 $ if sign == 1 then r9 else r11
  MV.unsafeWrite xsout 10 $ if sign == 1 then r10 else r10
  MV.unsafeWrite xsout 11 $ if sign == 1 then r11 else r9
  MV.unsafeWrite xsout 12 $ if sign == 1 then r12 else r8
  MV.unsafeWrite xsout 13 $ if sign == 1 then r13 else r7
  MV.unsafeWrite xsout 14 $ if sign == 1 then r14 else r6
  MV.unsafeWrite xsout 15 $ if sign == 1 then r15 else r5
  MV.unsafeWrite xsout 16 $ if sign == 1 then r16 else r4
  MV.unsafeWrite xsout 17 $ if sign == 1 then r17 else r3
  MV.unsafeWrite xsout 18 $ if sign == 1 then r18 else r2
  MV.unsafeWrite xsout 19 $ if sign == 1 then r19 else r1

-- | Length 25 hard-coded FFT.
kp803003575, kp554608978, kp248028675, kp726211448 :: Double
kp525970792, kp992114701, kp851038619, kp912575812 :: Double
kp912018591, kp943557151, kp614372930, kp621716863 :: Double
kp994076283, kp734762448, kp772036680, kp126329378 :: Double
kp827271945, kp949179823, kp860541664, kp557913902 :: Double
kp249506682, kp681693190, kp560319534, kp998026728 :: Double
kp906616052, kp968479752, kp845997307, kp470564281 :: Double
kp062914667, kp921177326, kp833417178, kp541454447 :: Double
kp242145790, kp683113946, kp559154169, kp968583161 :: Double
kp904730450, kp831864738, kp871714437, kp939062505 :: Double
kp549754652, kp634619297, kp256756360 :: Double
--kp951056516, kp559016994, kp250000000, kp618033988 :: Double
kp803003575 = 0.803003575438660414833440593570376004635464850
kp554608978 = 0.554608978404018097464974850792216217022558774
kp248028675 = 0.248028675328619457762448260696444630363259177
kp726211448 = 0.726211448929902658173535992263577167607493062
kp525970792 = 0.525970792408939708442463226536226366643874659
kp992114701 = 0.992114701314477831049793042785778521453036709
kp851038619 = 0.851038619207379630836264138867114231259902550
kp912575812 = 0.912575812670962425556968549836277086778922727
kp912018591 = 0.912018591466481957908415381764119056233607330
kp943557151 = 0.943557151597354104399655195398983005179443399
kp614372930 = 0.614372930789563808870829930444362096004872855
kp621716863 = 0.621716863012209892444754556304102309693593202
kp994076283 = 0.994076283785401014123185814696322018529298887
kp734762448 = 0.734762448793050413546343770063151342619912334
kp772036680 = 0.772036680810363904029489473607579825330539880
kp126329378 = 0.126329378446108174786050455341811215027378105
kp827271945 = 0.827271945972475634034355757144307982555673741
kp949179823 = 0.949179823508441261575555465843363271711583843
kp860541664 = 0.860541664367944677098261680920518816412804187
kp557913902 = 0.557913902031834264187699648465567037992437152
kp249506682 = 0.249506682107067890488084201715862638334226305
kp681693190 = 0.681693190061530575150324149145440022633095390
kp560319534 = 0.560319534973832390111614715371676131169633784
kp998026728 = 0.998026728428271561952336806863450553336905220
kp906616052 = 0.906616052148196230441134447086066874408359177
kp968479752 = 0.968479752739016373193524836781420152702090879
kp845997307 = 0.845997307939530944175097360758058292389769300
kp470564281 = 0.470564281212251493087595091036643380879947982
kp062914667 = 0.062914667253649757225485955897349402364686947
kp921177326 = 0.921177326965143320250447435415066029359282231
kp833417178 = 0.833417178328688677408962550243238843138996060
kp541454447 = 0.541454447536312777046285590082819509052033189
kp242145790 = 0.242145790282157779872542093866183953459003101
kp683113946 = 0.683113946453479238701949862233725244439656928
kp559154169 = 0.559154169276087864842202529084232643714075927
kp968583161 = 0.968583161128631119490168375464735813836012403
kp904730450 = 0.904730450839922351881287709692877908104763647
kp831864738 = 0.831864738706457140726048799369896829771167132
kp871714437 = 0.871714437527667770979999223229522602943903653
kp939062505 = 0.939062505817492352556001843133229685779824606
kp549754652 = 0.549754652192770074288023275540779861653779767
kp634619297 = 0.634619297544148100711287640319130485732531031
kp256756360 = 0.256756360367726783319498520922669048172391148
--kp951056516 = 0.951056516295153572116439333379382143405698634
--kp559016994 = 0.559016994374947424102293417182819058860154590
--kp250000000 = 0.250000000000000000000000000000000000000000000
--kp618033988 = 0.618033988749894848204586834365638117720309180
special25 :: Int -> MVCD s -> MVCD s -> ST s ()
special25 sign xsin xsout = do
  xr0  :+ xi0  <- MV.unsafeRead xsin 0  ; xr1  :+ xi1  <- MV.unsafeRead xsin 1
  xr2  :+ xi2  <- MV.unsafeRead xsin 2  ; xr3  :+ xi3  <- MV.unsafeRead xsin 3
  xr4  :+ xi4  <- MV.unsafeRead xsin 4  ; xr5  :+ xi5  <- MV.unsafeRead xsin 5
  xr6  :+ xi6  <- MV.unsafeRead xsin 6  ; xr7  :+ xi7  <- MV.unsafeRead xsin 7
  xr8  :+ xi8  <- MV.unsafeRead xsin 8  ; xr9  :+ xi9  <- MV.unsafeRead xsin 9
  xr10 :+ xi10 <- MV.unsafeRead xsin 10 ; xr11 :+ xi11 <- MV.unsafeRead xsin 11
  xr12 :+ xi12 <- MV.unsafeRead xsin 12 ; xr13 :+ xi13 <- MV.unsafeRead xsin 13
  xr14 :+ xi14 <- MV.unsafeRead xsin 14 ; xr15 :+ xi15 <- MV.unsafeRead xsin 15
  xr16 :+ xi16 <- MV.unsafeRead xsin 16 ; xr17 :+ xi17 <- MV.unsafeRead xsin 17
  xr18 :+ xi18 <- MV.unsafeRead xsin 18 ; xr19 :+ xi19 <- MV.unsafeRead xsin 19
  xr20 :+ xi20 <- MV.unsafeRead xsin 20 ; xr21 :+ xi21 <- MV.unsafeRead xsin 21
  xr22 :+ xi22 <- MV.unsafeRead xsin 22 ; xr23 :+ xi23 <- MV.unsafeRead xsin 23
  xr24 :+ xi24 <- MV.unsafeRead xsin 24
  let t4 = xr5+xr20 ; t1S = xr5-xr20 ; t7 = xr10+xr15 ; t1T = xr10-xr15
      t4Q = t1T-kp618033988*t1S ; t1U = t1S+kp618033988*t1T
      t8 = t4+t7 ; t3a = t4-t7 ; t3c = xi5-xi20 ; t1y = xi5+xi20
      t39 = xr0-kp250000000*t8 ; t9 = xr0+t8
      t1B = xi10+xi15 ; t3d = xi10-xi15
      t3b = t39+kp559016994*t3a ; t45 = t39-kp559016994*t3a
      t3e = t3c+kp618033988*t3d ; t46 = t3d-kp618033988*t3c
      t1C = t1y+t1B ; t1Q = t1y-t1B ; t1P = xi0-kp250000000*t1C
      t1D = xi0+t1C ; t4P = t1P-kp559016994*t1Q
      t1R = t1P+kp559016994*t1Q ; t1Z = xr21-xr6 ; td = xr6+xr21
      t20 = xr16-xr11 ; tg = xr11+xr16 ; th = td+tg ; t24 = td-tg
      t26 = xi6-xi21 ; tT = xi6+xi21 ; tW = xi11+xi16 ; t27 = xi16-xi11
      t1X = tT-tW ; tX = tT+tW ; t2l = xr24-xr9 ; tm = xr9+xr24
      t2m = xr19-xr14 ; tp = xr14+xr19 ; tq = tm+tp ; t2c = tm-tp
      t2e = xi24-xi9 ; t12 = xi9+xi24 ; t15 = xi14+xi19 ; t2f = xi19-xi14
      t23 = xr1-kp250000000*th ; ti = xr1+th ; t2j = t15-t12
      t16 = t12+t15 ; tr = xr4+tq ; t2b = kp250000000 * tq - xr4
      t1W = xi1-kp250000000*tX ; tY = xi1+tX
      t21 = t1Z+kp618033988*t20 ; t4y = t20-kp618033988*t1Z
      t2i = xi4-kp250000000*t16 ; t17 = xi4+t16 ; ts = ti+tr
      t1K = ti-tr ; t18 = tY-t17 ; t1E = tY+t17
      t2n = t2l+kp618033988*t2m ; t4r = t2m-kp618033988*t2l
      t4x = t1W-kp559016994*t1X ; t1Y = t1W+kp559016994*t1X
      t4o = t2f-kp618033988*t2e ; t2g = t2e+kp618033988*t2f
      t4z = t4x+kp951056516*t4y ; t5f = t4x-kp951056516*t4y
      t3z = t1Y-kp951056516*t21 ; t22 = t1Y+kp951056516*t21
      t4q = t2i+kp559016994*t2j ; t2k = t2i-kp559016994*t2j
      t4s = t4q+kp951056516*t4r ; t5b = t4q-kp951056516*t4r
      t3C = t2k-kp951056516*t2n ; t2o = t2k+kp951056516*t2n
      t2d = t2b-kp559016994*t2c ; t4n = t2b+kp559016994*t2c
      t28 = t26-kp618033988*t27 ; t4v = t27+kp618033988*t26
      t3D = t2d-kp951056516*t2g ; t2h = t2d+kp951056516*t2g
      t4p = t4n+kp951056516*t4o ; t5c = t4n-kp951056516*t4o
      t4u = t23-kp559016994*t24 ; t25 = t23+kp559016994*t24
      t4w = t4u-kp951056516*t4v ; t5e = t4u+kp951056516*t4v
      t3A = t25-kp951056516*t28 ; t29 = t25+kp951056516*t28
      t2u = xr22-xr7 ; tw = xr7+xr22 ; t2v = xr17-xr12 ; tz = xr12+xr17
      tA = tw+tz ; t2z = tz-tw ; t2B = xi22-xi7 ; t1c = xi7+xi22
      t1f = xi12+xi17 ; t2C = xi12-xi17 ; t2s = t1f-t1c ; t1g = t1c+t1f
      t2J = xr8-xr23 ; tF = xr8+xr23 ; t2K = xr13-xr18 ; tI = xr13+xr18
      tJ = tF+tI ; t2O = tI-tF ; t2Q = xi23-xi8 ; t1l = xi8+xi23
      t1o = xi13+xi18 ; t2R = xi18-xi13 ; t2y = xr2-kp250000000*tA
      tB = xr2+tA ; t2H = t1o-t1l ; t1p = t1l+t1o ; tK = xr3+tJ
      t2N = xr3-kp250000000*tJ ; t2r = xi2-kp250000000*t1g
      t1h = xi2+t1g ; t2w = t2u+kp618033988*t2v
      t49 = t2v-kp618033988*t2u ; t2G = xi3-kp250000000*t1p
      t1q = xi3+t1p ; tL = tB+tK ; t1L = tB-tK ; t1r = t1h-t1q
      t1F = t1h+t1q ; t2S = t2Q+kp618033988*t2R
      t4j = t2R-kp618033988*t2Q ; t48 = t2r+kp559016994*t2s
      t2t = t2r-kp559016994*t2s ; t4g = t2K-kp618033988*t2J
      t2L = t2J+kp618033988*t2K ; t4a = t48+kp951056516*t49
      t57 = t48-kp951056516*t49 ; t3v = t2t-kp951056516*t2w
      t2x = t2t+kp951056516*t2w ; t4i = t2N+kp559016994*t2O
      t2P = t2N-kp559016994*t2O ; t4k = t4i-kp951056516*t4j
      t55 = t4i+kp951056516*t4j ; t3s = t2P+kp951056516*t2S
      t2T = t2P-kp951056516*t2S ; t2I = t2G-kp559016994*t2H
      t4f = t2G+kp559016994*t2H ; t2D = t2B-kp618033988*t2C
      t4c = t2C+kp618033988*t2B ; t3t = t2I+kp951056516*t2L
      t2M = t2I-kp951056516*t2L ; t4h = t4f-kp951056516*t4g
      t54 = t4f+kp951056516*t4g ; tM = ts+tL ; tO = ts-tL
      t4b = t2y+kp559016994*t2z ; t2A = t2y-kp559016994*t2z
      tN = t9-kp250000000*tM ; t4d = t4b+kp951056516*t4c
      t58 = t4b-kp951056516*t4c ; t3w = t2A+kp951056516*t2D
      t2E = t2A-kp951056516*t2D ; t1s = t18+kp618033988*t1r
      t1u = t1r-kp618033988*t18 ; tP = tN+kp559016994*tO
      t1t = tN-kp559016994*tO ; t1G = t1E+t1F ; t1I = t1E-t1F
      t1H = t1D-kp250000000*t1G ; t1J = t1H+kp559016994*t1I
      t1N = t1H-kp559016994*t1I ; t1M = t1K+kp618033988*t1L
      t1O = t1L-kp618033988*t1K ; t3H = t1R+kp951056516*t1U
      t1V = t1R-kp951056516*t1U ; t3f = t3b+kp951056516*t3e
      t3r = t3b-kp951056516*t3e ; t30 = t29+kp256756360*t22
      t2a = t22-kp256756360*t29 ; t2p = t2h+kp634619297*t2o
      t31 = t2o-kp634619297*t2h ; t33 = t2E+kp549754652*t2x
      t2F = t2x-kp549754652*t2E ; t2U = t2M-kp939062505*t2T
      t34 = t2T+kp939062505*t2M ; t3m = t2a-kp871714437*t2p
      t2q = t2a+kp871714437*t2p ; t3n = t2F-kp831864738*t2U
      t2V = t2F+kp831864738*t2U ; t2W = t2q+kp904730450*t2V
      t2Y = t2q-kp904730450*t2V ; t32 = t30-kp871714437*t31
      t3g = t30+kp871714437*t31 ; t3h = t33+kp831864738*t34
      t35 = t33-kp831864738*t34 ; t3i = t3g+kp904730450*t3h
      t3k = t3g-kp904730450*t3h ; t36 = t32+kp559154169*t35
      t38 = t35-kp683113946*t32 ; t2X = t1V-kp242145790*t2W
      t3o = t3m+kp559154169*t3n ; t3q = t3n-kp683113946*t3m
      t3j = t3f-kp242145790*t3i ; t2Z = t2X+kp541454447*t2Y
      t37 = t2X-kp541454447*t2Y ; t47 = t45+kp951056516*t46
      t53 = t45-kp951056516*t46 ; t3p = t3j-kp541454447*t3k
      t3l = t3j+kp541454447*t3k ; t5j = t4P+kp951056516*t4Q
      t4R = t4P-kp951056516*t4Q ; t5k = t55-kp062914667*t54
      t56 = t54+kp062914667*t55 ; t59 = t57+kp634619297*t58
      t5l = t58-kp634619297*t57 ; t5n = t5c-kp470564281*t5b
      t5d = t5b+kp470564281*t5c ; t5g = t5e+kp549754652*t5f
      t5o = t5f-kp549754652*t5e ; t5u = t56-kp845997307*t59
      t5a = t56+kp845997307*t59 ; t5v = t5d-kp968479752*t5g
      t5h = t5d+kp968479752*t5g ; t5i = t5a+kp906616052*t5h
      t5A = t5a-kp906616052*t5h ; t5D = t5k-kp845997307*t5l
      t5m = t5k+kp845997307*t5l ; t5p = t5n+kp968479752*t5o
      t5C = t5n-kp968479752*t5o ; t5s = t5m+kp906616052*t5p
      t5q = t5m-kp906616052*t5p ; t5w = t5u-kp560319534*t5v
      t5y = t5v+kp681693190*t5u ; t5E = t5C-kp681693190*t5D
      t5G = t5D+kp560319534*t5C ; t5r = t5j+kp249506682*t5q
      t5z = t53-kp249506682*t5i ; t5t = t5r-kp557913902*t5s
      t5x = t5r+kp557913902*t5s ; t5F = t5z+kp557913902*t5A
      t5B = t5z-kp557913902*t5A ; t4J = t4d-kp062914667*t4a
      t4e = t4a+kp062914667*t4d ; t4l = t4h-kp827271945*t4k
      t4K = t4k+kp827271945*t4h ; t4G = t4s-kp126329378*t4p
      t4t = t4p+kp126329378*t4s ; t4A = t4w+kp939062505*t4z
      t4H = t4z-kp939062505*t4w ; t4Y = t4e-kp772036680*t4l
      t4m = t4e+kp772036680*t4l ; t4Z = t4t-kp734762448*t4A
      t4B = t4t+kp734762448*t4A ; t4C = t4m+kp994076283*t4B
      t4E = t4m-kp994076283*t4B ; t4I = t4G+kp734762448*t4H
      t4T = t4G-kp734762448*t4H ; t4S = t4J+kp772036680*t4K
      t4L = t4J-kp772036680*t4K ; t4U = t4S+kp994076283*t4T
      t4W = t4S-kp994076283*t4T ; t4M = t4I-kp621716863*t4L
      t4O = t4L+kp614372930*t4I ; t4D = t47-kp249506682*t4C
      t50 = t4Y+kp614372930*t4Z ; t52 = t4Z-kp621716863*t4Y
      t4V = t4R+kp249506682*t4U ; t4F = t4D-kp557913902*t4E
      t4N = t4D+kp557913902*t4E ; t51 = t4V+kp557913902*t4W
      t4X = t4V-kp557913902*t4W ; t3I = t3t+kp126329378*t3s
      t3u = t3s-kp126329378*t3t ; t3x = t3v-kp470564281*t3w
      t3J = t3w+kp470564281*t3v ; t3L = t3A-kp634619297*t3z
      t3B = t3z+kp634619297*t3A ; t3E = t3C-kp827271945*t3D
      t3M = t3D+kp827271945*t3C ; t3S = t3u+kp912018591*t3x
      t3y = t3u-kp912018591*t3x ; t3T = t3B+kp912575812*t3E
      t3F = t3B-kp912575812*t3E ; t3G = t3y-kp851038619*t3F
      t3Y = t3y+kp851038619*t3F ; t41 = t3I-kp912018591*t3J
      t3K = t3I+kp912018591*t3J ; t3N = t3L+kp912575812*t3M
      t40 = t3L-kp912575812*t3M ; t3Q = t3K-kp851038619*t3N
      t3O = t3K+kp851038619*t3N ; t3U = t3S-kp525970792*t3T
      t3W = t3T+kp726211448*t3S ; t42 = t40-kp726211448*t41
      t44 = t41+kp525970792*t40 ; t3P = t3H+kp248028675*t3O
      t3X = t3r+kp248028675*t3G ; t3R = t3P-kp554608978*t3Q
      t3V = t3P+kp554608978*t3Q ; t3Z = t3X+kp554608978*t3Y
      t43 = t3X-kp554608978*t3Y
      r24 = (t3f+kp968583161*t3i) :+ (t1V+kp968583161*t2W)
      r23 = (t53+kp998026728*t5i) :+ (t5j-kp998026728*t5q)
      r22 = (t47+kp998026728*t4C) :+ (t4R-kp998026728*t4U)
      r21 = (t3r-kp992114701*t3G) :+ (t3H-kp992114701*t3O)
      r20 = (tP+kp951056516*t1s) :+ (t1J-kp951056516*t1M)
      r19 = (t3l+kp921177326*t3o) :+ (t2Z-kp921177326*t36)
      r18 = (t5B-kp860541664*t5E) :+ (t5x+kp860541664*t5y)
      r17 = (t4F+kp943557151*t4M) :+ (t51+kp943557151*t52)
      r16 = (t3Z-kp803003575*t42) :+ (t3V-kp803003575*t3W)
      r15 = (t1t-kp951056516*t1u) :+ (t1N+kp951056516*t1O)
      r14 = (t3p-kp833417178*t3q) :+ (t37+kp833417178*t38)
      r13 = (t5F-kp949179823*t5G) :+ (t5t-kp949179823*t5w)
      r12 = (t4N+kp949179823*t4O) :+ (t4X+kp949179823*t50)
      r11 = (t43-kp943557151*t44) :+ (t3R+kp943557151*t3U)
      r10 = (t1t+kp951056516*t1u) :+ (t1N-kp951056516*t1O)
      r9 = (t3p+kp833417178*t3q) :+ (t37-kp833417178*t38)
      r8 = (t5F+kp949179823*t5G) :+ (t5t+kp949179823*t5w)
      r7 = (t4N-kp949179823*t4O) :+ (t4X-kp949179823*t50)
      r6 = (t43+kp943557151*t44) :+ (t3R-kp943557151*t3U)
      r5 = (tP-kp951056516*t1s) :+ (t1J+kp951056516*t1M)
      r4 = (t3l-kp921177326*t3o) :+ (t2Z+kp921177326*t36)
      r3 = (t5B+kp860541664*t5E) :+ (t5x-kp860541664*t5y)
      r2 = (t4F-kp943557151*t4M) :+ (t51-kp943557151*t52)
      r1 = (t3Z+kp803003575*t42) :+ (t3V+kp803003575*t3W)
  MV.unsafeWrite xsout 0 $ (t9+tM) :+ (t1D+t1G)
  MV.unsafeWrite xsout 1 $ if sign == 1 then r1 else r24
  MV.unsafeWrite xsout 2 $ if sign == 1 then r2 else r23
  MV.unsafeWrite xsout 3 $ if sign == 1 then r3 else r22
  MV.unsafeWrite xsout 4 $ if sign == 1 then r4 else r21
  MV.unsafeWrite xsout 5 $ if sign == 1 then r5 else r20
  MV.unsafeWrite xsout 6 $ if sign == 1 then r6 else r19
  MV.unsafeWrite xsout 7 $ if sign == 1 then r7 else r18
  MV.unsafeWrite xsout 8 $ if sign == 1 then r8 else r17
  MV.unsafeWrite xsout 9 $ if sign == 1 then r9 else r16
  MV.unsafeWrite xsout 10 $ if sign == 1 then r10 else r15
  MV.unsafeWrite xsout 11 $ if sign == 1 then r11 else r14
  MV.unsafeWrite xsout 12 $ if sign == 1 then r12 else r13
  MV.unsafeWrite xsout 13 $ if sign == 1 then r13 else r12
  MV.unsafeWrite xsout 14 $ if sign == 1 then r14 else r11
  MV.unsafeWrite xsout 15 $ if sign == 1 then r15 else r10
  MV.unsafeWrite xsout 16 $ if sign == 1 then r16 else r9
  MV.unsafeWrite xsout 17 $ if sign == 1 then r17 else r8
  MV.unsafeWrite xsout 18 $ if sign == 1 then r18 else r7
  MV.unsafeWrite xsout 19 $ if sign == 1 then r19 else r6
  MV.unsafeWrite xsout 20 $ if sign == 1 then r20 else r5
  MV.unsafeWrite xsout 21 $ if sign == 1 then r21 else r4
  MV.unsafeWrite xsout 22 $ if sign == 1 then r22 else r3
  MV.unsafeWrite xsout 23 $ if sign == 1 then r23 else r2
  MV.unsafeWrite xsout 24 $ if sign == 1 then r24 else r1

