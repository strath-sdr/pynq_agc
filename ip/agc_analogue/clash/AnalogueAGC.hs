module AnalogueAGC where

import Clash.Prelude
import CalibrationLUT (lutData)

data GainState = GLow | GHigh | GOk

type TVc   = Unsigned 16
type TLutAddr = Unsigned 11
type TGain = UFixed 2 16
type TGain' = Unsigned 18
type TCycles = Unsigned 32

createDomain vXilinxSystem{vName="XilDom", vResetPolarity=ActiveLow}

parseThresholds :: Bool {-upper-} -> Bool {-lower-} -> GainState
parseThresholds False False = GLow
parseThresholds False True  = GOk
parseThresholds True  True  = GHigh
parseThresholds True  False = undefined

isAtk GLow = True
isAtk _    = False

truncateLSBs :: (KnownNat n, KnownNat m) => Unsigned (n+m) -> Unsigned (n)
truncateLSBs = unpack . v2bv . takeI . bv2v . pack

satUpper limit x = if (x >= limit) then limit else x
satLower limit x = if (x <= limit) then limit else x

pulseGen n = count .==. 0
  where count = delay 0 count'
        count' = mux (count .==. 0)
                     n
                     (count-1)

approxdB :: TGain -> TLutAddr
approxdB g = db'
  where db = sub (mul a g) a
        a = $$(fLit (20.0 / logBase 2 10 * (2**11 / 6.03))) :: UFixed 12 6 -- 18 to fit in DSP48, Extra scaling factor to get it to fit in our 11 bit output
        db' = unUF (resizeF db :: UFixed 11 0)

calibration :: (HiddenClockResetEnable dom, KnownNat n) => Signal dom (Unsigned (11+n)) -> Signal dom (Unsigned 16)
calibration = romPow2 lutData . fmap (truncateLSBs)

agc
  :: (HiddenClockResetEnable dom)
     => Signal dom TGain
     -> Signal dom TCycles
     -> Signal dom TGain
     -> Signal dom TCycles
     -> Signal dom TGain
     -> Signal dom Bool
     -> Signal dom Bool
     -> Signal dom TVc
agc atkStep atkN decStep decN maxG thU thL = vc
  where
  dir = liftA2 parseThresholds thU thL
  n = mux (isAtk <$> dir) atkN decN
  en = pulseGen $ delay 0 n
  vc = calibration $ approxdB <$> gain_lin
  gain_lin  = regEn 1 en (inc <$> dir <*> atkStep <*> decStep <*> maxG <*> gain_lin)
  inc s up down limit g = case s of
                            GLow  -> satUpper limit $ satAdd SatBound g up
                            GHigh -> satLower 1     $ satSub SatBound g down
                            GOk   -> g

gateOutput :: Applicative f => a -> f Bool -> f a -> f a
gateOutput a en dut = mux en dut (pure a)

data PModState = PWritingLow  (Unsigned 16) (Index 16)
               | PWritingHigh (Unsigned 16) (Index 16)
               | PWaitLow
               | PWaitHigh
               | PLoadLow
               | PLoadHigh
               deriving (Generic, NFDataX)

writeDA3 :: HiddenClockResetEnable dom
         => Signal dom (Unsigned 16)
         -> Signal dom (Bit , Bit , Bit, Bit)
         --            (SCLK, DATA, ~CS, ~LDAC)
writeDA3 = moore fNext fOut PWaitLow
  where
  fNext (PWritingLow  x i) _ = PWritingHigh x i
  fNext (PWritingHigh x i) _ = if i == 0 then PWaitLow else PWritingLow x (i-1)
  fNext (PWaitLow        ) _ = PWaitHigh
  fNext (PWaitHigh       ) _ = PLoadLow
  fNext (PLoadLow        ) _ = PLoadHigh
  fNext (PLoadHigh       ) x = PWritingLow x maxBound

  fOut (PWritingLow  x i) = (0, boolToBit . testBit x $ fromIntegral i, 0, 1)
  fOut (PWritingHigh x i) = (1, boolToBit . testBit x $ fromIntegral i, 0, 1)
  fOut (PWaitLow        ) = (0, 0                                     , 1, 1)
  fOut (PWaitHigh       ) = (1, 0                                     , 1, 1)
  fOut (PLoadLow        ) = (0, 0                                     , 1, 0)
  fOut (PLoadHigh       ) = (1, 0                                     , 1, 0)

topLevel
  :: Clock  XilDom
  -> Reset  XilDom
  -> Enable XilDom
  -> Signal XilDom TGain'
  -> Signal XilDom TCycles
  -> Signal XilDom TGain'
  -> Signal XilDom TCycles
  -> Signal XilDom TGain'
  -> Signal XilDom Bool
  -> Signal XilDom Bool
  -> Signal XilDom (TVc, Bit, Bit, Bit, Bit)
topLevel clk rst en atkStep atkN decStep decN maxG thU thL =
  exposeClockResetEnable (
    let (sclk, din, cs, ldac) = unbundle $ writeDA3 vc
        vc = gateOutput 0 (fromEnable en) $
               agc (uf d16 <$> atkStep) atkN
                   (uf d16 <$> decStep) decN
                   (uf d16 <$> maxG)
                   thU thL
    in bundle (vc, sclk, din, cs, ldac)
  ) clk rst (toEnable $ pure True)
  where

{-# ANN topLevel
  (Synthesize
    { t_name   = "analogueAgc"
    , t_inputs = [ PortName "clk"
                 , PortName "aresetn"
                 , PortName "en"
                 , PortName "atk_step"
                 , PortName "atk_n"
                 , PortName "dec_step"
                 , PortName "dec_n"
                 , PortName "max_g"
                 , PortName "thres_high"
                 , PortName "thres_low"
                 ]
    , t_output = PortProduct ""
                   [ PortName "gain"
                   , PortName "da3_sclk"
                   , PortName "da3_data"
                   , PortName "da3_ncs"
                   , PortName "da3_nldac"
                   ]
    }) #-}
