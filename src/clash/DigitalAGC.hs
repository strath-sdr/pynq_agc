module DigitalAGC where

import Clash.Prelude

import qualified Test.QuickCheck as Q

import LutLog
import Data.Maybe (fromMaybe)

type Ready = Bool
type Valid = Bool
type AxisIn a = (a, Valid)
type AxisOut a = (a, Ready)


-- -- Complex number helpers
-- 
-- type Cmplx a = Vec 2 a
-- 
-- real :: Cmplx a -> a
-- real = (!! 0)
-- 
-- imag :: Cmplx a -> a
-- imag = (!! 1)
-- 
-- toCmplx :: a -> a -> Cmplx a
-- toCmplx i q = i :> q :> Nil
-- 
-- fromCmplx :: (a -> a -> b) -> Cmplx a -> b
-- fromCmplx f x = f (real x) (imag x)

-- Number representation conversion

toSigned :: forall n . KnownNat n => Unsigned n -> Signed (n+1)
toSigned u = let  u' = resize u :: Unsigned (n+1)
             in fromIntegral u'

toUnsigned :: forall n . KnownNat n => Signed (n+1) -> Unsigned n
toUnsigned s = let u = fromIntegral s :: Unsigned (n+1)
               in resize u

scale :: forall n m f . (KnownNat n, KnownNat m,  KnownNat f) => UFixed (n+m) f -> Signed (n+1)
scale = toSigned . unpack . v2bv . takeI . bv2v . pack

toSF :: forall n m . (KnownNat n, KnownNat m) => UFixed n m -> SFixed (n+1) m
toSF = sf (SNat :: SNat m) . toSigned . unUF

toUF :: forall n m . (KnownNat n, KnownNat m) => SFixed (n+1) m -> UFixed n m
toUF = uf (SNat :: SNat m) . toUnsigned . unSF

mul15by16 :: forall rep n . (KnownNat n, Resize rep, Bits (rep (4+n)), Num (rep (4+n)))
             => rep n -> rep n
mul15by16 x =
  let x16 = shiftL (resize x :: rep (n+4)) 4
      x15 = x16 - signExtend x
      x'  = resize $ shiftR x15 4
  in x'

-- Wordlength grows by one but we switch to unsigned and gain one extra +ve bit
powerDetector :: KnownNat n => Signed n -> Signed n -> Unsigned n
powerDetector i q = fromIntegral . mul15by16 $ (max i' q') `add` shiftR (abs $ min i' q') 1
  where i' = abs i
        q' = abs q

simPowerDetector :: Double -> Double -> Double
simPowerDetector i q = sqrt $ (i*i) + (q*q)

prop_approx_power :: Signed 16 -> Signed 16 -> Bool
prop_approx_power i q = let exp = simPowerDetector (fromIntegral i) (fromIntegral q)
                            act = fromIntegral $ powerDetector i q
                            percent_margin = 6
                            lim = 200
                            error = max exp act - min exp act
                        in abs i < lim || abs q < lim || error / exp <= percent_margin / 100
{-
Above test shows that as long as the signal above 0.6% of the full range, we can expect less than 6% error.

Not bad for a sqrt etc. implemented with 2 adds!
-}

intgDump
  :: (HiddenClockResetEnable dom, Num window, Eq window, Num a, NFDataX window, NFDataX a)
  => window -> Signal dom a -> Signal dom (Maybe a)
intgDump window = mealy f (window, 0)
  where f (0,acc) x = ((window, 0    ), Just acc)
        f (n,acc) x = ((n-1   , acc+x), Nothing )

type IRef nSig nWindow = 1 + CLog 2 (nSig + nWindow)
type NCtrl nSig nWindow iAlpha = 2 ^ ((3 + (CLog 2 (nSig + nWindow) + iAlpha)) + 2)

digiAgc :: forall dom fLog nWindow nSig iAlpha fAlpha fGain .
            ( HiddenClockResetEnable dom
            , KnownNat fLog   , KnownNat nWindow
            , KnownNat nSig   , KnownNat fGain
            , KnownNat iAlpha , KnownNat fAlpha
            , 1<=nSig
            , fGain <= NCtrl nSig nWindow iAlpha
            )
          => SNat fLog
          -> SNat fGain
          -> Unsigned nWindow -- ^ I&D window length
          -> Signal dom (UFixed (IRef nSig nWindow) fLog) -- ^ Reference power
          -> Signal dom (UFixed iAlpha fAlpha) -- ^ Alpha
          -> Signal dom (Signed nSig)
          -> Signal dom (Signed nSig)
          -> Signal dom (UFixed (NCtrl nSig nWindow iAlpha - fGain) fGain)
digiAgc fLog _ window ref alpha i q =
  let smoothPower :: Signal dom (Maybe (Unsigned (nSig + nWindow)))
      smoothPower = intgDump window . fmap extend $  liftA2 powerDetector i q

      calcErr (alpha, ref, x) = mul (toSF alpha) . (sub (toSF ref)) .  toSF $ lutLog10 fLog x

      err = fmap (fmap calcErr) . fmap (\(a,b,x) -> fmap (\x->(a,b,x)) x) $ bundle (alpha, ref, smoothPower)

      logCtrl' = liftA2 (\m x -> fmap (boundedAdd x) m)  err logCtrl
      logCtrl = regMaybe 0 logCtrl'

      ctrl = fmap (lutAntilog10 . toUF) logCtrl
  in uf (SNat :: SNat fGain) <$> ctrl

digiAgcMult :: forall dom . (HiddenClockResetEnable dom)
            => Unsigned 7 -- ^ I&D window length
            -> Signal dom (UFixed 6 6) -- ^ Reference power
            -> Signal dom (UFixed 0 4) -- ^ Alpha
            -> Signal dom (Signed 16)
            -> Signal dom (Signed 16)
            -> Signal dom (UFixed 1014 10, Signed 16, Signed 16)
digiAgcMult w r a i q = let g = digiAgc (SNat :: SNat 6) (SNat :: SNat 10) w r a i' q'
                            g' = toSF <$> g
                            preMul x y = unSF (resizeF $ (sf d0 x) `mul` y :: SFixed 16 0)
                            i' = delay 0 $ liftA2 preMul i g'  :: Signal dom (Signed 16)
                            q' = delay 0 $ liftA2 preMul q g'  :: Signal dom (Signed 16)
                        in bundle (g, i', q')


{-
 What are some nice properties to test our AGC?

  1) (Low passed) output power should be nearly constant, as described by our parameters

     Checks that we are 1) normalising the signal's amplitude
                        2) within the expected response time
                        3) to a level controlled by our parameters

  2) Output signal should correlate very strongly to ideal input
     This is the same as saying our gain control signal should be very close to the flipped version of our generated input gain envelope (within 1 response time period)

     Checks that we are 1) actually passing our input signal through the system
                        2) not distorting it so much that it will be hard to process downstream


 These are both essentially saying:
   * Generate a sensible input
   * Generate a slower, random gain envelope
   * Pass this through our AGC
   * Compare our inverted control signal to the envelope
     using parameters to allow differences in time and scaling
-}
