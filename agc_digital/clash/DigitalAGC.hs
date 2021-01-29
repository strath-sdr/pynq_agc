module DigitalAGC where

import Clash.Prelude
import LutLog
import DataFlow.Extra
import Data.Maybe (fromMaybe)

{- Numeric helper functions -}

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

shiftSF :: (KnownNat n, KnownNat i, KnownNat f, n <= i) => SNat n -> SFixed i f -> SFixed (i-n) (f+n)
shiftSF n x = resizeF $ shiftR x (snatToNum n)

{- Power detector with root of squares approximation -}

{-

Looking at power approximations again.

https://dspguru.com/dsp/tricks/magnitude-estimator/

what a great site. Let's try the 61/64 and 13/32 approximation.
For 61 and 13, let's use spiral https://dspguru.com/dsp/tricks/magnitude-estimator/

61/64, 13/32 looks like a good fit.
-}

mul61by64 :: forall rep n .
             ( KnownNat n, Resize rep
             , Bits (rep (6+n)), Num (rep (6+n)))
             => rep n -> rep n
mul61by64 x =
  let x'  = resize x :: rep (6+n)
      x4  = shiftL x' 2
      x64 = shiftL x' 6
      x61 = x64 - x4 + x'
  in resize $ shiftR x61 6

mul13by32 :: forall rep n .
             ( KnownNat n, Resize rep
             , Bits (rep (4+n)), Num (rep (4+n)))
             => rep n -> rep n
mul13by32 x =
  let x' = resize x :: rep (4+n)
      x4 = shiftL x' 2
      x8 = shiftL x' 3
      x13 = x8 + x4 + x'
  in resize $ shiftR x13 5

powerDetector :: KnownNat n => Signed n -> Signed n -> Unsigned n
powerDetector i q = fromIntegral . mul61by64 $ (max i' q') `add` mul13by32 (abs $ min i' q')
  where i' = abs i
        q' = abs q

dfPowDetect = pureDF (uncurry powerDetector)

{- Integrate and dump -}

intgDumpPow2
  :: forall dom window n . (HiddenClockResetEnable dom, KnownNat window, KnownNat n)
  => Signal dom (Unsigned window) -> Signal dom Bool -> Signal dom (Unsigned n) -> Signal dom (Maybe (Unsigned n))
intgDumpPow2 window en x = mealy f initState (bundle (en, x, window))
  where initState :: (Unsigned (2^window), Unsigned (2^window + n))
        initState = (0,0)
        f  s       (False, _, _    ) = (s, Nothing)
        f (0, acc) (True, x, window) = ( ( setBit zeroBits (fromIntegral window) - 1                     -- window counter
                                         , resize x)                                                     -- accumulator
                                       , Just . resize $ shiftR acc (fromIntegral (boundedSub window 0)) -- output
                                       )
        f (n, acc) (True, x, window) = ( ( n-1                                                           -- window counter
                                         , acc + (resize x))                                             -- accumulator
                                       , Nothing                                                         -- output
                                       )

dfIntgDump window = gatedMaybeToDF (intgDumpPow2 window)

{- Calculating log error -}
type BitsLog10 n = CLog 2 (CLog 10 (2^n))
type NonZeroLog10 n = (1 <= CLog 10 (2^n), 1 <= BitsLog10 n)
type BitsExp10 n = CLog 2 (10 ^ (2 ^ n))

dfLogErr :: forall dom power fRef iAlpha fAlpha
         .  ( HiddenClockResetEnable dom, KnownNat power, KnownNat fRef
            , KnownNat iAlpha, KnownNat fAlpha
            , NonZeroLog10 power
            , 1 <= power)
         => Signal dom (UFixed (BitsLog10 power) fRef) -> Signal dom (UFixed iAlpha fAlpha)
         -> DataFlow dom Bool Bool (Unsigned power) (SFixed (BitsLog10 power + 1) fRef)
dfLogErr ref alpha = gatedToDF (\en x -> liftA3 f ref alpha x)
  where f ref alpha x = let logX = toSF $ lutLog10 (SNat :: SNat fRef) x
                            dif  = toSF ref - logX
                            err  = (toSF $ resizeF alpha) * dif
                        in err

{- Wee accumulator -}

dfAccum :: (HiddenClockResetEnable dom, KnownNat i, KnownNat f)
        => DataFlow dom Bool Bool (SFixed (i+1) f) (UFixed i f)
dfAccum = gatedVToDF f
  where f en err = let x' = fmap (\z->if z <0 then 0 else z) $ liftA2 boundedAdd x err
                       x  = regEn 0 en x'
                   in (pure True, fmap toUF x)

{- Antilog / exponential conversion -}

dfAntilog :: forall dom i f f'
          .  ( HiddenClockResetEnable dom, KnownNat i, KnownNat f, KnownNat f'
             , 1<= i
             , f' <= BitsExp10 i )
          => DataFlow dom Bool Bool (UFixed i f) (UFixed (BitsExp10 i - f') f')
dfAntilog = pureDF (uf (SNat :: SNat f') . lutAntilog10)

{- Constructing the AGC loop -}

dfForward
  :: ( HiddenClockResetEnable dom
     , KnownNat sig, KnownNat fRef, KnownNat window
     , KnownNat iAlpha, KnownNat fAlpha, KnownNat fGain
     , NonZeroLog10 sig
     , 1  <= sig
     , fGain <= BitsExp10 (BitsLog10 sig) )
  => Signal dom (Unsigned window)
  -> Signal dom (UFixed (BitsLog10 sig) fRef)
  -> Signal dom (UFixed iAlpha fAlpha)
  -> DataFlow dom Bool Bool (Signed sig, Signed sig)
                            (UFixed (BitsExp10 (BitsLog10 sig) - fGain) fGain)
dfForward window ref alpha = dfPowDetect
                             `seqDF` dfIntgDump window
                             `seqDF` dfLogErr ref alpha
                             `seqDF` dfAccum
                             `seqDF` dfAntilog

dfGainStage :: forall dom sig iGain fGain
            . (HiddenClockResetEnable dom, KnownNat sig, KnownNat iGain, KnownNat fGain)
            => Signal dom Bool
            -> DataFlow dom Bool Bool ((Signed sig, Signed sig), UFixed iGain fGain)
                                      ((UFixed iGain fGain, (Signed sig, Signed sig)), (Signed sig, Signed sig))
dfGainStage en = gatedToDF (\_ x -> liftA2 f en x)
  where preMul x g = unSF (resizeF $ sf d0 x `mul` toSF g :: SFixed sig 0)
        f en ((i,q),g) = let g' = if en then g else 1
                             i' = preMul i g'
                             q' = preMul q g'
                         in ((g', (i', q')), (i', q'))

dfAgc
  :: ( HiddenClockResetEnable dom
     , KnownNat sig, KnownNat fRef, KnownNat window
     , KnownNat iAlpha, KnownNat fAlpha, KnownNat fGain
     , NonZeroLog10 sig
     , 1  <= sig
     , fGain <= BitsExp10 (BitsLog10 sig) )
  => Signal dom (Unsigned window)
  -> Signal dom (UFixed (BitsLog10 sig) fRef)
  -> Signal dom (UFixed iAlpha fAlpha)
  -> Signal dom Bool
  -> DataFlow dom Bool Bool (Signed sig, Signed sig)
                            (UFixed (BitsExp10 (BitsLog10 sig) - fGain) fGain
                            ,(Signed sig, Signed sig))
dfAgc window ref alpha en = feedbackLoop logic `seqDF` outReg
  where feedbackLoop ip = hideClockResetEnable loopDF d2 Nil ip
        logic           = (idDF `parDF` dfForward window ref alpha) `seqDF` lockStep `seqDF` dfGainStage en `seqDF` stepLock
        outReg          = hideClockResetEnable fifoDF d2 ((def,(def,def)):>(def,(def,def)):>Nil)

{- Top level -}

createDomain vXilinxSystem{vName="XilDom", vResetPolarity=ActiveLow}

{-# ANN topEntity
  (Synthesize
    { t_name   = "digitalAgc"
    , t_inputs = [ PortName "clk"
                 , PortName "aresetn"
                 , PortName "en"
                 , PortName "window"
                 , PortName "ref"
                 , PortName "alpha"
                 , PortName "s_i_axis_tdata"
                 , PortName "s_i_axis_tvalid"
                 , PortName "s_q_axis_tdata"
                 , PortName "s_q_axis_tvalid"
                 , PortName "m_g_axis_tready"
                 , PortName "m_i_axis_tready"
                 , PortName "m_q_axis_tready"
                 ]
    , t_output = PortProduct ""
                   [ PortName "s_i_axis_tready"
                   , PortName "s_q_axis_tready"
                   , PortName "m_g_axis_tdata"
                   , PortName "m_g_axis_tvalid"
                   , PortName "m_i_axis_tdata"
                   , PortName "m_i_axis_tvalid"
                   , PortName "m_q_axis_tdata"
                   , PortName "m_q_axis_tvalid"
                   ]
    }) #-}
topEntity ::
  Clock XilDom
  -> Reset XilDom
  -> Signal XilDom Bit
  -> Signal XilDom (Unsigned 5)
  -> Signal XilDom (UFixed 3 12)
  -> Signal XilDom (UFixed 1 6)
  -> Signal XilDom (Signed 16)
  -> Signal XilDom Bit
  -> Signal XilDom (Signed 16)
  -> Signal XilDom Bit
  -> Signal XilDom Bit
  -> Signal XilDom Bit
  -> Signal XilDom Bit
  -> Signal XilDom (Bit, Bit, UFixed 14 13, Bit, Signed 16, Bit, Signed 16, Bit)
topEntity clk rst en window ref alpha i inIV q inQV outGR outIR outQR =
  let (g,(i',q'))             = (\(a,b)->(a,unbundle b)) $ unbundle outDatas
      (outGV, (outIV, outQV)) = (\(a,b)->(a,unbundle b)) $ unbundle outVs
      (inIR, inQR)            = unbundle inRs
      (outDatas, outVs, inRs) = ip (bundle (i,q))
                                   (bundle (bitToBool <$> inIV, bitToBool <$> inQV))
                                   (bundle (bitToBool <$> outGR, bundle (bitToBool <$> outIR, bitToBool <$> outQR)))
      ip                      = exposeClockResetEnable
                                   (df $ lockStep `seqDF` dfAgc window ref alpha (bitToBool <$> en) `seqDF` stepLock `seqDF` secondDF stepLock)
                                clk rst (toEnable $ pure True)
  in bundle (boolToBit <$> inIR, boolToBit <$> inQR, g, boolToBit <$> outGV, i', boolToBit <$> outIV, q', boolToBit <$> outQV)

-- TODO Make wrapper project with loopback
-- TODO Sketch out ipywidget controlling alpha, ref, window, [input type, length, amplitudes]
-- TODO see if we can avoid saturation issues
