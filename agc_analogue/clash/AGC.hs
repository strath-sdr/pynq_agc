import Clash.Prelude

data GainState = GLow | GHigh | GOk

type TVc   = Unsigned 6
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

approxdB :: TGain -> TVc
approxdB g = db'
  where db = sub (mul a g) a
        a = $$(fLit (20.0 / logBase 2 10 * (64 / 6))) :: UFixed 7 11
        db' = unUF (resizeF db :: UFixed 6 0)

calibration :: (HiddenClockResetEnable dom, KnownNat n) => Signal dom (Unsigned (6+n)) -> Signal dom TVc
calibration = romPow2 lut . fmap (truncateLSBs)
  where lut :: Vec (2^6) (Unsigned 6)
        lut =   0 :>   1 :>   6 :>   8 :>  10 :>  12 :>  13 :>  14 :>  15 :>  16 :>  17 :>
               18 :>  19 :>  19 :>  20 :>  20 :>  21 :>  22 :>  22 :>  23 :>  23 :>  24 :>
               24 :>  24 :>  25 :>  25 :>  26 :>  26 :>  27 :>  27 :>  28 :>  28 :>  28 :>
               29 :>  29 :>  30 :>  30 :>  30 :>  31 :>  31 :>  32 :>  32 :>  33 :>  33 :>
               34 :>  34 :>  35 :>  35 :>  36 :>  36 :>  37 :>  38 :>  38 :>  39 :>  40 :>
               41 :>  42 :>  43 :>  44 :>  46 :>  48 :>  51 :>  55 :>  63 :>  Nil

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
  en = pulseGen n
  vc = calibration $ approxdB <$> gain_lin
  gain_lin  = regEn 1 en (inc <$> dir <*> atkStep <*> decStep <*> maxG <*> gain_lin)
  inc s up down limit g = case s of
                            GLow  -> satUpper limit $ satAdd SatBound g up
                            GHigh -> satLower 1     $ satSub SatBound g down
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
  exposeClockResetEnable (gateOutput 0 (fromEnable en) $
                          agc  (uf d16 <$> atkStep) atkN
                               (uf d16 <$> decStep) decN
                               (uf d16 <$> maxG)
                               thU thL)
  clk rst (toEnable $ pure True)

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
