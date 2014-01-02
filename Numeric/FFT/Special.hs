module Numeric.FFT.Special ( specialBases ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Complex
import Data.Vector

import Numeric.FFT.Types
import Numeric.FFT.Utils


-- | Map from input vector lengths to hard-coded FFT transforms for
-- small problem sizes.  Each function in the map takes a transform
-- direction (+1 for forward, -1 for inverse) and returns the
-- /unscaled/ transform (scaling for inverse transforms is applied at
-- the top-level).
specialBases :: IntMap (Int -> VCD -> VCD)
specialBases = IM.fromList [ (2, special2)
                           , (3, special3)
                           , (5, special5) ]

-- | Length 2 hard-coded FFT.
special2 :: Int -> VCD -> VCD
special2 _ xs =
  let a = xs!0 ; b = xs!1
  in generate 2 $ \i -> case i of
    0 -> a + b
    1 -> a - b

-- | Length 3 hard-coded FFT.
kp500000000, kp866025403 :: Double
kp866025403 = 0.866025403784438646763723170752936183471402627
kp500000000 = 0.500000000000000000000000000000000000000000000
special3 :: Int -> VCD -> VCD
special3 sign xs =
  let ar:+ai=xs!0 ; br:+bi=xs!1 ; cr:+ci=xs!2
      rp = br + cr ; rm = br - cr
      ip = bi + ci ; im = bi - ci
      tr = ar - kp500000000 * rp
      ti = ai - kp500000000 * ip
      r1 = (tr - kp866025403 * im) :+ (ti + kp866025403 * rm)
      r2 = (tr + kp866025403 * im) :+ (ti - kp866025403 * rm)
  in generate 3 $ \i -> case i of
    0 -> (ar + rp) :+ (ai + ip)
    1 -> if sign == 1 then r1 else r2
    2 -> if sign == 1 then r2 else r1

-- | Length 5 hard-coded FFT.
kp951056516, kp559016994, kp250000000, kp618033988 :: Double
kp951056516 = 0.951056516295153572116439333379382143405698634
kp559016994 = 0.559016994374947424102293417182819058860154590
kp250000000 = 0.250000000000000000000000000000000000000000000
kp618033988 = 0.618033988749894848204586834365638117720309180
special5 :: Int -> VCD -> VCD
special5 sign xs =
  let ar:+ai=xs!0 ; br:+bi=xs!1 ; cr:+ci=xs!2 ; dr:+di=xs!3 ; er:+ei=xs!4
      ts = br - er ; t4 = br + er ; tt = cr - dr ; t7 = cr + dr
      t8 = t4 + t7 ; ta = t4 - t7 ; te = bi - ei ; tm = bi + ei
      tn = ci + di ; th = ci - di ; to = tm + tn ; tq = tm - tn
      ti = te + kp618033988 * th ; tk = th - kp618033988 * te
      t9 = ar - kp250000000 * t8 ; tu = ts + kp618033988 * tt
      tw = tt - kp618033988 * ts ; tp = ai - kp250000000 * to
      tb = t9 + kp559016994 * ta ; tj = t9 - kp559016994 * ta
      tr = tp + kp559016994 * tq ; tv = tp - kp559016994 * tq
      r4 = (tb + kp951056516 * ti) :+ (tr - kp951056516 * tu)
      r3 = (tj - kp951056516 * tk) :+ (tv + kp951056516 * tw)
      r2 = (tj + kp951056516 * tk) :+ (tv - kp951056516 * tw)
      r1 = (tb - kp951056516 * ti) :+ (tr + kp951056516 * tu)
  in generate 5 $ \i -> case i of
    0 -> (ar + t8) :+ (ai + to)
    1 -> if sign == 1 then r1 else r4
    2 -> if sign == 1 then r2 else r3
    3 -> if sign == 1 then r3 else r2
    4 -> if sign == 1 then r4 else r1
