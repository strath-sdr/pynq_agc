import Clash.Prelude

data GainState = GLow | GHigh | GOk

parseThresholds :: Bool {-upper-} -> Bool {-lower-} -> GainState
parseThresholds False False = GLow
parseThresholds False True  = GOk
parseThresholds True  True  = GHigh
parseThresholds True  False = undefined

isAtk GLow = True
isAtk _    = False

satWith limit x = if (x >= limit) then limit else x

pulseGen n = count .==. 0
  where count = delay 0 count'
        count' = mux (count .==. 0)
                     n
                     (count-1)

agc
  :: (HiddenClockResetEnable dom,
      NFDataX a, SaturatingNum a, Num a, Ord a,
      NFDataX b, SaturatingNum b, Num b, Ord b
     )
     => Signal dom a
     -> Signal dom b
     -> Signal dom a
     -> Signal dom b
     -> Signal dom a
     -> Signal dom Bool
     -> Signal dom Bool
     -> Signal dom a
agc atkStep atkN decStep decN maxG thU thL = z
  where
  dir = liftA2 parseThresholds thU thL
  n = mux (isAtk <$> dir) atkN decN
  en = pulseGen n
  z  = regEn 0 en (inc <$> dir <*> atkStep <*> decStep <*> maxG <*> z)
  inc s up down limit g = case s of
                            GLow  -> satWith limit $ satAdd SatBound g up
                            GHigh -> satSub SatBound g down
                            GOk   -> g

type TGain = Unsigned 6
type TCycles = Unsigned 32
createDomain vXilinxSystem{vName="XilDom", vResetPolarity=ActiveLow}

topLevel
  :: Clock  XilDom
  -> Reset  XilDom
  -> Enable XilDom
  -> Signal XilDom TGain
  -> Signal XilDom TCycles
  -> Signal XilDom TGain
  -> Signal XilDom TCycles
  -> Signal XilDom TGain
  -> Signal XilDom Bool
  -> Signal XilDom Bool
  -> Signal XilDom TGain
topLevel = exposeClockResetEnable agc

{-# ANN topLevel
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
                 , PortName "thres_high"
                 , PortName "thres_low"
                 ]
    , t_output = PortName "gain"
    }) #-}
