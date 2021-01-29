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

{- Basic AGC building blocks -}

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

intgDump
  :: (HiddenClockResetEnable dom, Num window, Eq window, Num a, NFDataX window, NFDataX a)
  => window -> Signal dom a -> Signal dom (Maybe a)
intgDump window = mealy f (window, 0)
  where f (0,acc) x = ((window, 0    ), Just acc)
        f (n,acc) x = ((n-1   , acc+x), Nothing )

-- A new I&D circuit that only supports powers of two windows, but does normalisation of the output via shifts.
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
                                         , acc + (resize x))                                                 -- accumulator
                                       , Nothing                                                         -- output
                                       )

{- DataFlow / AXI-Stream wrappers -}

dfPowDetect :: KnownNat n
            => DataFlow dom Bool Bool (Signed n, Signed n) (Unsigned n)
dfPowDetect = pureDF (uncurry powerDetector)

dfIntgDump :: (HiddenClockResetEnable dom, KnownNat window, KnownNat n)
           => Signal dom (Unsigned window)
           -> DataFlow dom Bool Bool (Unsigned n) (Unsigned n)
dfIntgDump window = gatedMaybeToDF (intgDumpPow2 window)

dfLogErr :: forall dom shift power fRef iAlpha fAlpha
         .  ( HiddenClockResetEnable dom, KnownNat shift, KnownNat power, KnownNat fRef
            , KnownNat iAlpha, KnownNat fAlpha
            , shift <= CLog 2 power
            , 1 <= power)
         => SNat shift -> Signal dom (UFixed (CLog 2 power) fRef) -> Signal dom (UFixed iAlpha fAlpha)
         -> DataFlow dom Bool Bool (Unsigned power) (SFixed (CLog 2 power - shift + 1) (fRef + shift))
dfLogErr shift ref alpha = gatedToDF (\en x -> liftA3 f ref alpha x)
  where f ref alpha x = let logX = toSF $ lutLog10 (SNat :: SNat fRef) x
                            dif  = toSF ref - logX
                            err  = (toSF $ resizeF alpha) * (shiftSF shift dif)
                        in err
-- ^ Verify my use of gatedToDF here (just need to lift a pure-ish function on Signals to a DF)

dfAccum :: (HiddenClockResetEnable dom, KnownNat i, KnownNat f)
        => DataFlow dom Bool Bool (SFixed (i+1) f) (UFixed i f)
dfAccum = gatedVToDF f
  where f en err = let x' = fmap (\z->if z <0 then 0 else z) $ liftA2 boundedAdd x err
                       x  = regEn 0 en x'
                   in (pure True, fmap toUF x)

dfAntilog :: forall dom i f f'
          .  ( HiddenClockResetEnable dom, KnownNat i, KnownNat f, KnownNat f'
             , 1<= i
             , f' <= CLog 2 (10 ^ (2 ^ i)))
          => DataFlow dom Bool Bool (UFixed i f) (UFixed (CLog 2 (10^(2^i)) - f') f')
dfAntilog = pureDF (uf (SNat :: SNat f') . lutAntilog10)

{- Constructing the AGC loop -}

dfForward
  :: ( HiddenClockResetEnable dom
     , KnownNat sig, KnownNat fRef, KnownNat window
     , KnownNat iAlpha, KnownNat fAlpha, KnownNat fGain
     , 1  <= (CLog 2 sig - 2)
     , 1  <= sig
     , 2  <= CLog 2 sig
     , fGain <= CLog 2 (10 ^ (2 ^ (CLog 2 sig - 2))) )
  => Signal dom (Unsigned window)
  -> Signal dom (UFixed (CLog 2 sig) fRef)
  -> Signal dom (UFixed iAlpha fAlpha)
  -> DataFlow dom Bool Bool (Signed sig, Signed sig)
                            (UFixed (CLog 2 (10 ^ (2 ^ (CLog 2 sig - 2))) - fGain) fGain)
dfForward window ref alpha = dfPowDetect
                             `seqDF` dfIntgDump window
                             `seqDF` dfLogErr d2 ref alpha
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
     , 1  <= (CLog 2 sig - 2)
     , 1  <= sig
     , 2  <= CLog 2 sig
     , fGain <= CLog 2 (10 ^ (2 ^ (CLog 2 sig - 2))) )
  => Signal dom (Unsigned window)
  -> Signal dom (UFixed (CLog 2 sig) fRef)
  -> Signal dom (UFixed iAlpha fAlpha)
  -> Signal dom Bool
  -> DataFlow dom Bool Bool (Signed sig, Signed sig)
                            (UFixed (CLog 2 (10 ^ (2 ^ (CLog 2 sig - 2))) - fGain) fGain
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
  -> Signal XilDom (UFixed 4 8)
  -> Signal XilDom (UFixed 1 6)
  -> Signal XilDom (Signed 16)
  -> Signal XilDom Bit
  -> Signal XilDom (Signed 16)
  -> Signal XilDom Bit
  -> Signal XilDom Bit
  -> Signal XilDom Bit
  -> Signal XilDom Bit
  -> Signal XilDom (Bit, Bit, UFixed 7 7, Bit, Signed 16, Bit, Signed 16, Bit)
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
