import Clash.Prelude

data GainState = GLow | GHigh | GOk

parseThresholds :: Bool {-upper-} -> Bool {-lower-} -> GainState
parseThresholds False False = GLow
parseThresholds False True  = GOk
parseThresholds True  True  = GHigh
parseThresholds True  False = undefined

isAtk GLow = True
isAtk _    = False

-- TODO, this should be an AXI stream, not a Signal
sigDetect
  :: HiddenClockResetEnable dom
  => Num a
  => Ord a
  => Num b
  => NFDataX b
  => Ord b
  => Signal dom a -> Signal dom b -> Signal dom a -> Signal dom a -> Signal dom Bool
sigDetect noise_floor count i q = floorCount .>=. count
  where floorCount = register 0 floorCount'
        floorCount' = mux ((abs i .<. noise_floor) .&&. (abs q .<. noise_floor))
                          (floorCount + 1)
                           0

pulseGen n = count .==. 0
  where count = delay 0 count'
        count' = mux (count .==. 0)
                     n
                     (count-1)

satWith limit x = if (x >= limit) then limit else x

agc
  :: (HiddenClockResetEnable dom,
      NFDataX cycles, NFDataX sig, NFDataX gain, Ord sig,
      Ord gain, Ord cycles, SaturatingNum gain, Num cycles, Num sig, Num gain,
      Eq cycles)
     => gain
     -> Signal dom gain
     -> Signal dom cycles
     -> Signal dom gain
     -> Signal dom cycles
     -> Signal dom gain
     -> Signal dom sig
     -> Signal dom cycles
     -> Signal dom Bool
     -> Signal dom Bool
     -> Signal dom sig
     -> Signal dom sig
     -> Signal dom gain
agc defG atkStep atkN decStep decN maxG noiseFloor timeout thUpper thLower i q = gain
  where
  state = liftA2 parseThresholds thUpper thLower
  step = mux (isAtk <$> state) atkStep decStep
  n    = mux (isAtk <$> state) atkN decN
  en = pulseGen n -- .&&. sigDetect noiseFloor timeout i q
  gain  = regEn defG en (inc <$> state <*> maxG <*> gain <*> step)
  inc s maxG gain step = case s of
                           GLow  -> satWith maxG $ satAdd SatBound gain step
                           GHigh ->                satSub SatBound gain step
                           GOk   -> gain

createDomain vXilinxSystem{vName="XilDom", vResetPolarity=ActiveLow}

type TGain = Unsigned 6
type TCycles = Unsigned 32
type TSignal = Signed 16

-- TODO we should have two clock domains, one for configuration and one for DATA
agcTopLevel
  :: Clock  XilDom
  -> Reset  XilDom
  -> Signal XilDom Bit
  -> Signal XilDom TGain
  -> Signal XilDom TCycles
  -> Signal XilDom TGain
  -> Signal XilDom TCycles
  -> Signal XilDom TGain
  -> Signal XilDom TSignal
  -> Signal XilDom TCycles
  -> Signal XilDom Bit
  -> Signal XilDom Bit
  -> Signal XilDom TSignal
  -> Signal XilDom TSignal
  -> Signal XilDom TGain
agcTopLevel clk rst en atkStep atkN decStep decN maxG noiseFloor timeout thUpper thLower i q
  = exposeClockResetEnable (agc 31)
    clk rst (toEnable $ bitToBool <$> en) atkStep atkN decStep decN maxG noiseFloor timeout
    (bitToBool <$> thUpper) (bitToBool <$> thLower) i q

{-# ANN agcTopLevel
  (Synthesize
    { t_name   = "agc"
    , t_inputs = [ PortName "clk"
                 , PortName "aresetn"
                 , PortName "en"
                 , PortName "atk_step"
                 , PortName "atk_n"
                 , PortName "dec_step"
                 , PortName "dec_n"
                 , PortName "max_g"
                 , PortName "noise_floor"
                 , PortName "timeout"
                 , PortName "thres_high"
                 , PortName "thres_low"
                 ]
    , t_output = PortName "gain"
    }) #-}
