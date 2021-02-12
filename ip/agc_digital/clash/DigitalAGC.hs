module DigitalAGC where

import Clash.Prelude
import CordicLog
import LutLogTests
import DataFlow.Extra
import Data.Maybe (fromMaybe)

import qualified Prelude as P

{- Numeric helper functions -}

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
intgDumpPow2 window' en x = mealy f initState (bundle (en, x, window))
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
        window = register 0 window'

dfIntgDump window = gatedMaybeToDF (intgDumpPow2 window)

{- Calculating log error -}

paramsVec :: Vec 32 (Int, SFixed 5 32)
paramsVec = $(listToVecTH $ P.take 32 (P.zip shiftSequence arctanhs))

eLn2sVec :: Vec 27 (SFixed 6 32)
eLn2sVec = $(listToVecTH $ P.take 27 eLn2s)

kScaling :: SFixed 5 32
kScaling = $$(fLit $ 1 / kValue 32)

dfLogErr :: forall dom iAlpha fAlpha
         .  ( HiddenClockResetEnable dom
            , KnownNat iAlpha, KnownNat fAlpha
            )
         => Signal dom (UFixed 3 12) -> Signal dom (UFixed iAlpha fAlpha)
         -> DataFlow dom Bool Bool (Unsigned 26) (SFixed 4 22)
dfLogErr ref alpha = liftDF (f ref alpha)
  where f ref alpha x iV oR =
          let x' = regEn 0 iV x
              logX = log10 paramsVec eLn2sVec x'
              dif  = register 0 $ logX - ((resizeF . toSF) <$> ref)
              err  = register 0 $ ((toSF . resizeF) <$> alpha) * dif
              oV = last $ iterate d36 (register False) iV
          in (err, oV, oR)
--  where f ref alpha x iV oR =
--          let logX = (\x -> fLitR (logBase 10 (fromIntegral x))) <$> x :: Signal dom (SFixed 4 22)
--              dif  = logX - (toSF <$> ref)
--              err  = ((toSF . resizeF) <$> alpha) * dif
--          in (err, iV, oR)

{- Wee accumulator -}

dfAccum :: (HiddenClockResetEnable dom, KnownNat i, KnownNat f)
        => DataFlow dom Bool Bool (SFixed i f) (SFixed i f)
dfAccum = gatedVToDF f
  where f en err = let x' = liftA2 boundedSub x err
                       x  = regEn 0 en x'
                   in (pure True, x)

{- Antilog / exponential conversion -}

dfAntilog :: forall dom
          .  ( HiddenClockResetEnable dom)
          => DataFlow dom Bool Bool (SFixed 4 22) (UFixed 24 26)
dfAntilog = liftDF f
  where f x iV oR =
          let x' = regEn 0 iV x
              oV = last $ iterate d35 (register False) iV
              y = register 0 $ resizeF <$> pow10 paramsVec kScaling eLn2sVec (resizeF <$> x') :: Signal dom (UFixed 24 26)
          in (y, oV, oR)
--          let y = (\x -> fLitR $ 10 ** (sfToDouble x)) <$> x :: Signal dom (UFixed 24 26) -- This is probably a pain point!
--          in (y, iV, oR)

{- Constructing the AGC loop -}

dfForward
  :: ( HiddenClockResetEnable dom)
  => Signal dom (Unsigned 5)
  -> Signal dom (UFixed 3 12)
  -> Signal dom (UFixed 1 6)
  -> DataFlow dom Bool Bool (Signed 26, Signed 26)
                            (UFixed 24 26)
dfForward window ref alpha = dfPowDetect
                             `seqDF` dfIntgDump window
                             `seqDF` dfLogErr (resizeF <$> ref) alpha
                             `seqDF` dfAccum
                             `seqDF` dfAntilog

dfGainStage :: forall dom sig
            . (HiddenClockResetEnable dom, KnownNat sig)
            => Signal dom Bool
            -> DataFlow dom Bool Bool ((Signed sig, Signed sig), UFixed 10 15)
                                      ((UFixed 10 15, (Signed (sig), Signed (sig))), (Signed (sig+10), Signed (sig+10)))
dfGainStage en = gatedToDF (\_ x -> liftA2 f en x)
  where preMul x g = sf d0 x `mul` toSF g
        f en ((i,q),g) = let g' = if en then g else 1
                             i' = preMul i g'
                             q' = preMul q g'
                             iint = unSF (resizeF i' :: SFixed (sig+10) 0)
                             qint = unSF (resizeF q' :: SFixed (sig+10) 0)
                             iext = unSF (resizeF i' :: SFixed (sig) 0)
                             qext = unSF (resizeF q' :: SFixed (sig) 0)
                         in ((g', (iext, qext)), (iint, qint))

shrink :: (KnownNat a, KnownNat b, KnownNat i, KnownNat f) => SNat b -> SNat i -> SNat f -> UFixed a b -> UFixed i f
shrink _ _ _ = resizeF

dfAgc
  :: forall dom.
  ( HiddenClockResetEnable dom
  )
  => Signal dom (Unsigned 5)
  -> Signal dom (UFixed 3 12)
  -> Signal dom (UFixed 1 6)
  -> Signal dom Bool
  -> DataFlow dom Bool Bool (Signed 16, Signed 16)
                            (UFixed 10 15
                            ,(Signed 16, Signed 16))
dfAgc window ref alpha en = feedbackLoop logic `seqDF` outReg
  where feedbackLoop ip = hideClockResetEnable loopDF d2 Nil ip
        logic           = (idDF `parDF` fwd) `seqDF` lockStep `seqDF` amp `seqDF` stepLock
        outReg          = hideClockResetEnable fifoDF d2 ((def,(def,def)):>(def,(def,def)):>Nil)
        fwd = dfForward window ref alpha `seqDF` pureDF (shrink (SNat :: SNat 26)
                                                                (SNat :: SNat 10)
                                                                (SNat :: SNat 15))
        amp = dfGainStage en


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
  -> Signal XilDom (Bit, Bit, UFixed 10 15, Bit, Signed 16, Bit, Signed 16, Bit)
topEntity clk rst en window ref alpha i inIV q inQV outGR outIR outQR =
  let (g,(i',q'))             = (\(a,b)->(a,unbundle b)) $ unbundle outDatas
      (outGV, (outIV, outQV)) = (\(a,b)->(a,unbundle b)) $ unbundle outVs
      (inIR, inQR)            = unbundle inRs
      (outDatas, outVs, inRs) = ip (bundle (i,q))
                                   (bundle (bitToBool <$> inIV, bitToBool <$> inQV))
                                   (bundle (bitToBool <$> outGR, bundle (bitToBool <$> outIR, bitToBool <$> outQR)))
      ip                      = exposeClockResetEnable
                                   (df $ lockStep `seqDF` hideClockResetEnable fifoDF d2 Nil `seqDF` dfAgc window (register 0 ref) (register 0 alpha) (register False (bitToBool <$> en)) `seqDF` hideClockResetEnable fifoDF d2 Nil `seqDF` stepLock `seqDF` secondDF stepLock)
                                clk rst (toEnable $ pure True)
  in bundle (boolToBit <$> inIR, boolToBit <$> inQR, g, boolToBit <$> outGV, i', boolToBit <$> outIV, q', boolToBit <$> outQV)
