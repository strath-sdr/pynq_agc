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

shiftUF :: (KnownNat n, KnownNat i, KnownNat f, n <= i) => SNat n -> UFixed i f -> UFixed (i-n) (f+n)
shiftUF n x = resizeF $ shiftR x (snatToNum n)

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

-- A new I&D circuit that only supports powers of two windows, but does normalisation of the output via shifts.
intgDumpPow2
  :: forall dom window n . (HiddenClockResetEnable dom, KnownNat window, KnownNat n)
  => Signal dom (Unsigned window) -> Signal dom (Unsigned n) -> Signal dom (Maybe (Unsigned n))
intgDumpPow2 window x = mealy f initState (bundle (x, window))
  where initState :: (Unsigned (2^window), Unsigned (2^window + n))
        initState = (0,0)
        f (0, acc) (x, window) = ( ( setBit zeroBits (fromIntegral window) - 1                     -- window counter
                                   , resize x)                                                     -- accumulator
                                 , Just . resize $ shiftR acc (fromIntegral (boundedSub window 0)) -- output
                                 )
        f (n, acc) (x, window) = ( ( n-1                                                           -- window counter
                                   , acc + (resize x))                                                 -- accumulator
                                 , Nothing                                                         -- output
                                 )

type IRef nSig = CLog 2 nSig
type NCtrl nSig = CLog 2 (10 ^ (2 ^ ((CLog 2 nSig) - 2)))


{-
 CLog 2 (10 ^ (2 ^ ((2 + (CLog 2 nSig + iAlpha)) - 2)))
 ~
(CLog 2 (10 ^ (2 ^ ((2 + (CLog 2 nSig + iAlpha)) - 2))) - fGain) + fGain

-}


{-
if smoothPower is 16 bits,
ref is 4.fLog
Ctrl could be 54 bits... if we resize this to keep 16 msbs... our range is now 2.7e11 -> full
in log domain this is 11.4 -> max! so we're only using the very top of our range



if we want ctrl to be 14 bit (closest to 16)... we need err to be 2.12 or something.
Can we scale the logarithm down? shiftR by 2 or soething?

-}

digiAgc :: forall dom fLog nWindow nSig iAlpha fAlpha fGain .
            ( HiddenClockResetEnable dom
            , KnownNat fLog   , KnownNat nWindow
            , KnownNat nSig   , KnownNat fGain
            , KnownNat iAlpha , KnownNat fAlpha
            , 1<=nSig
            , 2<=IRef nSig
            , fGain <= NCtrl nSig
            )
          => SNat fLog
          -> SNat fGain
          -> Signal dom (Unsigned nWindow) -- ^ I&D window length
          -> Signal dom (UFixed (IRef nSig - 2) (fLog+2)) -- ^ Reference power
          -> Signal dom (UFixed iAlpha fAlpha) -- ^ Alpha
          -> Signal dom (Signed nSig)
          -> Signal dom (Signed nSig)
          -> Signal dom (UFixed (NCtrl nSig - fGain) fGain)
digiAgc fLog _ window ref alpha i q =
  let smoothPower :: Signal dom (Maybe (Unsigned nSig))
      smoothPower = intgDumpPow2 window $ liftA2 powerDetector i q

      --                                           subtracting two +ves, so no need to extend
      calcErr (alpha, ref, x) = ((resizeF $ toSF alpha) * ) $ (\x -> (toSF ref) - x) .  toSF . shiftUF d2 $ lutLog10 fLog x

      err = fmap (fmap calcErr) . fmap (\(a,b,x) -> fmap (\x->(a,b,x)) x) $ bundle (alpha, ref, smoothPower)

      logCtrl' = liftA2 (\m x -> fmap (boundedAdd' x) m)  err logCtrl -- should I be worried about this accumulating negatively?
      logCtrl = regMaybe 0 logCtrl'

      ctrl = fmap (lutAntilog10 . toUF) logCtrl
      boundedAdd' x y = (\z->if z <0 then 0 else z) $ boundedAdd x y
  in uf (SNat :: SNat fGain) <$> ctrl

digiAgcMult :: forall dom . (HiddenClockResetEnable dom)
            => Signal dom (Unsigned 5) -- ^ I&D window length
            -> Signal dom (UFixed 2 10) -- ^ Reference power
            -> Signal dom (UFixed 1 6) -- ^ Alpha (book says this should be 0<=alpha<=2)
            -> Signal dom (Signed 16)
            -> Signal dom (Signed 16)
            -> Signal dom (UFixed 9 5, Signed 16, Signed 16) -- TODO I changed gain from 7.7 to 9.5 to get better results when bumping up gains... re-check my tests.
digiAgcMult w r a i q = let g = digiAgc (SNat :: SNat 8) (SNat :: SNat 5) w r a i' q'
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

{-
OK, what about moddeling the recovery time of the AGC for instantenous changes?

Look at an extreme case... say signal comming in is at 100 "power"
-}

simAgc :: Double -> Double -> Double -> Double -> Double
simAgc alpha ref inPower logState =
  let logPow = (logBase 10 (10**(logState) / 64 * inPower)) / 4
      logState' = logState + alpha * (ref - logPow)
  in logState'

countToStable :: [Double] -> Int
countToStable []       = error "List doesn't converge"
countToStable (x:[])   = error "List doesn't converge"
countToStable (x:y:zs) = if abs (x-y) / y < 0.005 -- Did we move by less than 0.5%?
                         then 0
                         else 1 + countToStable (y:zs)

createDomain vXilinxSystem{vName="XilDom", vResetPolarity=ActiveLow}

-- Getting space state equations from that book and modifying them for our circuit
{-# ANN topEntity
  (Synthesize
    { t_name   = "digitalAgc"
    , t_inputs = [ PortName "clk"
                 , PortName "aresetn"
                 , PortName "en"
                 , PortName "window"
                 , PortName "ref"
                 , PortName "alpha"
                 , PortName "i"
                 , PortName "q"

                 ]
    , t_output = PortProduct "agc"
                   [ PortName "gain"
                   , PortName "out_i"
                   , PortName "out_q"
                   ]
    }) #-}
topEntity clk rst en = exposeClockResetEnable @XilDom gatedAgc clk rst (toEnable $ pure True) en
  where gatedAgc en w r a i q = mux (fmap bitToBool en) (digiAgcMult w r a i q) (bundle (pure 1, i, q))


{-
0.1,56
0.2,40
0.30000000000000004,31
0.4,26
0.5,22
0.6,19
0.7,17
0.7999999999999999,15
0.8999999999999999,14
0.9999999999999999,13
1.0999999999999999,12
1.2,11
1.3,10
1.4000000000000001,10
1.5000000000000002,9
1.6000000000000003,8
1.7000000000000004,8
1.8000000000000005,7
1.9000000000000006,7)
-}
