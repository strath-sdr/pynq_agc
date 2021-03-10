module AnalogueAGC where

import Clash.Prelude

data GainState = GLow | GHigh | GOk

type TVc      = Unsigned 6
type TLutAddr = Unsigned 11
type TGain    = UFixed 0 18
type TGain'   = Unsigned 18
type TCycles  = Unsigned 32

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

pulseGen n = register False isZero
  where count = delay 0 count'
        count' = mux isZero n (count-1)
        isZero = count .==. 0

calibration :: (HiddenClockResetEnable dom, KnownNat n) => Signal dom (Unsigned (6+n)) -> Signal dom (Unsigned 6)
calibration = romPow2 lutData . fmap (truncateLSBs)
  where
  lutData :: Vec 64 (Unsigned 6)
  lutData = map unpack $
            0b000000 :> 0b001111 :> 0b010010 :> 0b010100 :>
            0b010110 :> 0b010111 :> 0b011000 :> 0b011001 :>
            0b011010 :> 0b011011 :> 0b011011 :> 0b011100 :>
            0b011100 :> 0b011101 :> 0b011101 :> 0b011110 :>
            0b011110 :> 0b011111 :> 0b011111 :> 0b100000 :>
            0b100000 :> 0b100000 :> 0b100001 :> 0b100001 :>
            0b100001 :> 0b100010 :> 0b100010 :> 0b100010 :>
            0b100011 :> 0b100011 :> 0b100011 :> 0b100100 :>
            0b100100 :> 0b100100 :> 0b100101 :> 0b100101 :>
            0b100101 :> 0b100110 :> 0b100110 :> 0b100110 :>
            0b100111 :> 0b100111 :> 0b101000 :> 0b101000 :>
            0b101000 :> 0b101001 :> 0b101001 :> 0b101010 :>
            0b101010 :> 0b101010 :> 0b101011 :> 0b101011 :>
            0b101100 :> 0b101101 :> 0b101101 :> 0b101110 :>
            0b101111 :> 0b110000 :> 0b110001 :> 0b110010 :>
            0b110011 :> 0b110110 :> 0b111001 :> 0b111111 :> Nil

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
  vc = calibration $ unUF <$> gain_lin
  gain_lin  = regEn 0 en (inc <$> dir <*> atkStep <*> decStep <*> maxG <*> gain_lin)
  inc s up down limit g = case s of
                            GLow  -> satUpper limit $ satAdd SatBound g up
                            GHigh -> satSub SatBound g down
                            GOk   -> g

gateOutput :: Applicative f => a -> f Bool -> f a -> f a
gateOutput a en dut = mux en dut (pure a)

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
  -> Signal XilDom TVc
topLevel clk rst en atkStep atkN decStep decN maxG thU thL =
  exposeClockResetEnable (
        gateOutput 0 (fromEnable en) $
          agc (uf d18 <$> atkStep) atkN
              (uf d18 <$> decStep) decN
              (uf d18 <$> maxG)
              thU thL
  ) clk rst (toEnable $ pure True)

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
    , t_output = PortName "gain"
    }) #-}
