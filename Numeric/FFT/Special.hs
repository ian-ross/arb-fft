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
                           , (11, special11)
                           , (13, special13) ]

-- | Length 2 hard-coded FFT.
special2 :: Int -> MVCD s -> MVCD s -> ST s ()
special2 _ xsin xsout = do
  a <- MV.unsafeRead xsin 0
  b <- MV.unsafeRead xsin 1
  MV.unsafeWrite xsout 0 $ a + b
  MV.unsafeWrite xsout 1 $ a - b

-- | Length 3 hard-coded FFT.
kp500000000, kp866025403 :: Double
kp866025403 = 0.866025403784438646763723170752936183471402627
kp500000000 = 0.500000000000000000000000000000000000000000000
special3 :: Int -> MVCD s -> MVCD s -> ST s ()
special3 sign xsin xsout = do
  xr0 :+ xi0 <- MV.unsafeRead xsin 0 ; xr1 :+ xi1 <- MV.unsafeRead xsin 1
  xr2 :+ xi2 <- MV.unsafeRead xsin 2
  let rp = xr1 + xr2 ; rm = xr1 - xr2
      ip = xi1 + xi2 ; im = xi1 - xi2
      tr = xr0 - kp500000000 * rp
      ti = xi0 - kp500000000 * ip
      r1 = (tr - kp866025403 * im) :+ (ti + kp866025403 * rm)
      r2 = (tr + kp866025403 * im) :+ (ti - kp866025403 * rm)
  MV.unsafeWrite xsout 0 $ (xr0 + rp) :+ (xi0 + ip)
  MV.unsafeWrite xsout 1 $ if sign == 1 then r1 else r2
  MV.unsafeWrite xsout 2 $ if sign == 1 then r2 else r1

-- | Length 5 hard-coded FFT.
kp951056516, kp559016994, kp250000000, kp618033988 :: Double
kp951056516 = 0.951056516295153572116439333379382143405698634
kp559016994 = 0.559016994374947424102293417182819058860154590
kp250000000 = 0.250000000000000000000000000000000000000000000
kp618033988 = 0.618033988749894848204586834365638117720309180
special5 :: Int -> MVCD s -> MVCD s -> ST s ()
special5 sign xsin xsout = do
  xr0 :+ xi0 <- MV.unsafeRead xsin 0 ; xr1 :+ xi1 <- MV.unsafeRead xsin 1
  xr2 :+ xi2 <- MV.unsafeRead xsin 2 ; xr3 :+ xi3 <- MV.unsafeRead xsin 3
  xr4 :+ xi4 <- MV.unsafeRead xsin 4
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
  MV.unsafeWrite xsout 0 $ (xr0 + t8) :+ (xi0 + to)
  MV.unsafeWrite xsout 1 $ if sign == 1 then r1 else r4
  MV.unsafeWrite xsout 2 $ if sign == 1 then r2 else r3
  MV.unsafeWrite xsout 3 $ if sign == 1 then r3 else r2
  MV.unsafeWrite xsout 4 $ if sign == 1 then r4 else r1

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
  xr0 :+ xi0 <- MV.unsafeRead xsin 0 ; xr1 :+ xi1 <- MV.unsafeRead xsin 1
  xr2 :+ xi2 <- MV.unsafeRead xsin 2 ; xr3 :+ xi3 <- MV.unsafeRead xsin 3
  xr4 :+ xi4 <- MV.unsafeRead xsin 4 ; xr5 :+ xi5 <- MV.unsafeRead xsin 5
  xr6 :+ xi6 <- MV.unsafeRead xsin 6
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
  MV.unsafeWrite xsout 0 $ (xr0 + t4 + t7 + ta) :+ (xi0 + tA + tB + tC)
  MV.unsafeWrite xsout 1 $ if sign == 1 then r1 else r6
  MV.unsafeWrite xsout 2 $ if sign == 1 then r2 else r5
  MV.unsafeWrite xsout 3 $ if sign == 1 then r3 else r4
  MV.unsafeWrite xsout 4 $ if sign == 1 then r4 else r3
  MV.unsafeWrite xsout 5 $ if sign == 1 then r5 else r2
  MV.unsafeWrite xsout 6 $ if sign == 1 then r6 else r1

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
  xr0 :+ xi0 <- MV.unsafeRead xsin 0 ; xr1 :+ xi1 <- MV.unsafeRead xsin 1
  xr2 :+ xi2 <- MV.unsafeRead xsin 2 ; xr3 :+ xi3 <- MV.unsafeRead xsin 3
  xr4 :+ xi4 <- MV.unsafeRead xsin 4 ; xr5 :+ xi5 <- MV.unsafeRead xsin 5
  xr6 :+ xi6 <- MV.unsafeRead xsin 6 ; xr7 :+ xi7 <- MV.unsafeRead xsin 7
  xr8 :+ xi8 <- MV.unsafeRead xsin 8 ; xr9 :+ xi9 <- MV.unsafeRead xsin 9
  xr10 :+ xi10 <- MV.unsafeRead xsin 10
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
  MV.unsafeWrite xsout 0 $ (xr0+t4+t7+ta+td+tg) :+ (xi0+t1g+t1h+t1i+t1j+t1k)
  MV.unsafeWrite xsout 1 $ if sign == 1 then r1 else r10
  MV.unsafeWrite xsout 2 $ if sign == 1 then r2 else r9
  MV.unsafeWrite xsout 3 $ if sign == 1 then r3 else r8
  MV.unsafeWrite xsout 4 $ if sign == 1 then r4 else r7
  MV.unsafeWrite xsout 5 $ if sign == 1 then r5 else r6
  MV.unsafeWrite xsout 6 $ if sign == 1 then r6 else r5
  MV.unsafeWrite xsout 7 $ if sign == 1 then r7 else r4
  MV.unsafeWrite xsout 8 $ if sign == 1 then r8 else r3
  MV.unsafeWrite xsout 9 $ if sign == 1 then r9 else r2
  MV.unsafeWrite xsout 10 $ if sign == 1 then r10 else r1

-- | Length 13 hard-coded FFT.
kp875502302, kp520028571, kp575140729 :: Double
kp600477271, kp300462606, kp516520780 :: Double
kp968287244, kp503537032, kp251768516 :: Double
kp581704778, kp859542535, kp083333333 :: Double
kp957805992, kp522026385, kp853480001 :: Double
kp769338817, kp612264650, kp038632954 :: Double
kp302775637, kp514918778, kp686558370 :: Double
kp226109445, kp301479260 :: Double
--kp866025403, kp500000000 :: Double
kp875502302 = 0.875502302409147941146295545768755143177842006
kp520028571 = 0.520028571888864619117130500499232802493238139
kp575140729 = 0.575140729474003121368385547455453388461001608
kp600477271 = 0.600477271932665282925769253334763009352012849
kp300462606 = 0.300462606288665774426601772289207995520941381
kp516520780 = 0.516520780623489722840901288569017135705033622
kp968287244 = 0.968287244361984016049539446938120421179794516
kp503537032 = 0.503537032863766627246873853868466977093348562
kp251768516 = 0.251768516431883313623436926934233488546674281
kp581704778 = 0.581704778510515730456870384989698884939833902
kp859542535 = 0.859542535098774820163672132761689612766401925
kp083333333 = 0.083333333333333333333333333333333333333333333
kp957805992 = 0.957805992594665126462521754605754580515587217
kp522026385 = 0.522026385161275033714027226654165028300441940
kp853480001 = 0.853480001859823990758994934970528322872359049
kp769338817 = 0.769338817572980603471413688209101117038278899
kp612264650 = 0.612264650376756543746494474777125408779395514
kp038632954 = 0.038632954644348171955506895830342264440241080
kp302775637 = 0.302775637731994646559610633735247973125648287
kp514918778 = 0.514918778086315755491789696138117261566051239
kp686558370 = 0.686558370781754340655719594850823015421401653
kp226109445 = 0.226109445035782405468510155372505010481906348
kp301479260 = 0.301479260047709873958013540496673347309208464
--kp866025403 = 0.866025403784438646763723170752936183471402627
--kp500000000 = 0.500000000000000000000000000000000000000000000
special13 :: Int -> MVCD s -> MVCD s -> ST s ()
special13 sign xsin xsout = do
  xr0 :+ xi0 <- MV.unsafeRead xsin 0 ; xr1 :+ xi1 <- MV.unsafeRead xsin 1
  xr2 :+ xi2 <- MV.unsafeRead xsin 2 ; xr3 :+ xi3 <- MV.unsafeRead xsin 3
  xr4 :+ xi4 <- MV.unsafeRead xsin 4 ; xr5 :+ xi5 <- MV.unsafeRead xsin 5
  xr6 :+ xi6 <- MV.unsafeRead xsin 6 ; xr7 :+ xi7 <- MV.unsafeRead xsin 7
  xr8 :+ xi8 <- MV.unsafeRead xsin 8 ; xr9 :+ xi9 <- MV.unsafeRead xsin 9
  xr10 :+ xi10 <- MV.unsafeRead xsin 10 ; xr11 :+ xi11 <- MV.unsafeRead xsin 11
  xr12 :+ xi12 <- MV.unsafeRead xsin 12
  let t2d = xr8 - xr5 ; tf = xr8 + xr5 ; ta = xr10 + xr4 ; tq = xr10 - xr4
      ty = kp500000000 * ta - xr12
      tb = xr12 + ta ; tr = xr9 - xr3 ; t5 = xr3 + xr9 ; t6 = xr1 + t5
      tx = xr1 - kp500000000 * t5
      ti = xr11 + xr6 ; tt = xr11 - xr6 ; tu = xr7 - xr2 ; tl = xr7 + xr2
      tc = t6 + tb ; t2n = t6 - tb ; t2b = ti - tl ; tm = ti + tl
      t2e = tt + tu ; tv = tt - tu ; ts = tq - tr ; t2g = tr + tq
      tz = tx - ty ; t2a = tx + ty
      tA = tf - kp500000000 * tm
      tn = tf + tm
      t2f = t2d - kp500000000 * t2e
      t2o = t2d + t2e ; to = tc + tn ; tH = tc - tn
      t2h = t2f + kp866025403 * t2g ; t2k = t2f - kp866025403 * t2g
      tE = tz - tA ; tB = tz + tA ; tF = ts - tv ; tw = ts + tv
      t2j = t2a - kp866025403 * t2b ; t2c = t2a + kp866025403 * t2b
      t1R = xi8 + xi5 ; tM = xi8 - xi5 ; t17 = xi10 + xi4 ; t10 = xi10 - xi4
      t18 = kp500000000 * t17 - xi12
      t1l = xi12 + t17 ; tX = xi9 - xi3 ; t14 = xi3 + xi9 ; t1k = xi1 + t14
      t15 = xi1 - kp500000000 * t14
      tP = xi11 - xi6 ; t1a = xi11 + xi6 ; t1b = xi7 + xi2 ; tS = xi7 - xi2
      t1Q = t1k + t1l ; t1m = t1k - t1l ; t11 = tX + t10 ; t1W = t10 - tX
      t1X = tP - tS ; tT = tP + tS ; t1S = t1a + t1b ; t1c = t1a - t1b
      t19 = t15 + t18 ; t1Z = t15 - t18 ; t1j = tM + tT
      tU = tM - kp500000000 * tT
      t1T = t1R + t1S
      t20 = t1R - kp500000000 * t1S ; t12 = tU + kp866025403 * t11
      t1f = tU - kp866025403 * t11
      t21 = t1Z + t20 ; t24 = t1Z - t20 ; t27 = t1Q - t1T ; t1U = t1Q + t1T
      t1g = t19 - kp866025403 * t1c ; t1d = t19 + kp866025403 * t1c
      t25 = t1W - t1X ; t1Y = t1W + t1X
      tC = tw + kp301479260 * tB ; t1x = tB - kp226109445 * tw
      t1y = tF + kp686558370 * tE ; tG = tE - kp514918778 * tF
      t1n = t1j - kp302775637 * t1m ; t1G = t1m + kp302775637 * t1j
      t1u = t1d - kp038632954 * t12 ; t1e = t12 + kp038632954 * t1d
      t1h = t1f + kp612264650 * t1g ; t1v = t1g - kp612264650 * t1f
      t1J = t1x + kp769338817 * t1y ; t1z = t1x - kp769338817 * t1y
      t1H = t1u - kp853480001 * t1v ; t1w = t1u + kp853480001 * t1v
      t1I = t1G - kp522026385 * t1H ; t1O = t1H + kp957805992 * t1G
      tp = xr0 - kp083333333 * to ; t1E = t1e + kp853480001 * t1h
      t1i = t1e - kp853480001 * t1h ; t1q = tH - kp859542535 * tG
      tI = tG + kp581704778 * tH ; t1o = t1i + kp957805992 * t1n
      t1s = t1n - kp522026385 * t1i ; t1p = tp - kp251768516 * tC
      tD = tp + kp503537032 * tC ; t1C = t1w - kp968287244 * t1z
      t1A = t1w + kp968287244 * t1z ; tJ = tD + kp516520780 * tI
      t1N = tD - kp516520780 * tI ; t1D = t1p - kp300462606 * t1q
      t1r = t1p + kp300462606 * t1q ; t1t = t1r - kp575140729 * t1s
      t1B = t1r + kp575140729 * t1s ; t1L = t1D - kp520028571 * t1E
      t1F = t1D + kp520028571 * t1E ; t1K = t1I + kp875502302 * t1J
      t1M = t1I - kp875502302 * t1J ; t2D = t21 - kp226109445 * t1Y
      t22 = t1Y + kp301479260 * t21 ; t26 = t24 - kp514918778 * t25
      t2E = t25 + kp686558370 * t24 ; t2v = t2o - kp302775637 * t2n
      t2p = t2n + kp302775637 * t2o ; t2i = t2c - kp038632954 * t2h
      t2s = t2h + kp038632954 * t2c ; t2t = t2k + kp612264650 * t2j
      t2l = t2j - kp612264650 * t2k ; t2F = t2D - kp769338817 * t2E
      t2N = t2D + kp769338817 * t2E ; t2K = t2s + kp853480001 * t2t
      t2u = t2s - kp853480001 * t2t ; t2w = t2u + kp957805992 * t2v
      t2A = t2v - kp522026385 * t2u ; t1V = xi0 - kp083333333 * t1U
      t2m = t2i - kp853480001 * t2l ; t2C = t2i + kp853480001 * t2l
      t28 = t26 + kp581704778 * t27 ; t2y = t27 - kp859542535 * t26
      t2M = t2p - kp522026385 * t2m ; t2q = t2m + kp957805992 * t2p
      t23 = t1V + kp503537032 * t22 ; t2x = t1V - kp251768516 * t22
      t2O = t2M - kp875502302 * t2N ; t2Q = t2M + kp875502302 * t2N
      t2r = t23 + kp516520780 * t28 ; t29 = t23 - kp516520780 * t28
      t2z = t2x + kp300462606 * t2y ; t2J = t2x - kp300462606 * t2y
      t2P = t2J + kp520028571 * t2K ; t2L = t2J - kp520028571 * t2K
      t2B = t2z + kp575140729 * t2A ; t2H = t2z - kp575140729 * t2A
      t2I = t2C + kp968287244 * t2F ; t2G = t2C - kp968287244 * t2F
      r12 = (tJ - kp600477271 * t1o) :+ (t2r + kp600477271 * t2w)
      r11 =  (t1F + kp575140729 * t1K) :+ (t2L - kp575140729 * t2O)
      r10 = (t1t + kp520028571 * t1A) :+ (t2B - kp520028571 * t2G)
      r9 = (t1B + kp520028571 * t1C) :+ (t2H - kp520028571 * t2I)
      r8 = (t1N + kp600477271 * t1O) :+ (t29 - kp600477271 * t2q)
      r7 = (t1L + kp575140729 * t1M) :+ (t2P - kp575140729 * t2Q)
      r6 = (t1F - kp575140729 * t1K) :+ (t2L + kp575140729 * t2O)
      r5 = (t1N - kp600477271 * t1O) :+ (t29 + kp600477271 * t2q)
      r4 = (t1t - kp520028571 * t1A) :+ (t2B + kp520028571 * t2G)
      r3 = (t1B - kp520028571 * t1C) :+ (t2H + kp520028571 * t2I)
      r2 = (t1L - kp575140729 * t1M) :+ (t2P + kp575140729 * t2Q)
      r1 = (tJ + kp600477271 * t1o) :+ (t2r - kp600477271 * t2w)
  MV.unsafeWrite xsout 0 $ (xr0 + to) :+ (xi0 + t1U)
  MV.unsafeWrite xsout 1 $ if sign == 1 then r1 else r12
  MV.unsafeWrite xsout 2 $ if sign == 1 then r2 else r11
  MV.unsafeWrite xsout 3 $ if sign == 1 then r3 else r10
  MV.unsafeWrite xsout 4 $ if sign == 1 then r4 else r9
  MV.unsafeWrite xsout 5 $ if sign == 1 then r5 else r8
  MV.unsafeWrite xsout 6 $ if sign == 1 then r6 else r7
  MV.unsafeWrite xsout 7 $ if sign == 1 then r7 else r6
  MV.unsafeWrite xsout 8 $ if sign == 1 then r8 else r5
  MV.unsafeWrite xsout 9 $ if sign == 1 then r9 else r4
  MV.unsafeWrite xsout 10 $ if sign == 1 then r10 else r3
  MV.unsafeWrite xsout 11 $ if sign == 1 then r11 else r2
  MV.unsafeWrite xsout 12 $ if sign == 1 then r12 else r1


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
