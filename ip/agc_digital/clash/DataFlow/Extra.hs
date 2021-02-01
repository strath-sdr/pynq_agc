module DataFlow.Extra (
  gatedToDF,
  gatedVToDF,
  gatedMaybeToDF,
  regDF,
  liftRegDF,
  counterDF,
  oscillateDF,
  testBufferDF,
  throttleDF,
  module Clash.Prelude.DataFlow
) where

import Clash.Prelude
import Clash.Prelude.DataFlow
import Data.Maybe (isJust, fromMaybe)

-- | Convert a clock-enabled circuit to a DataFlow circuit
gatedToDF
  :: HiddenClockResetEnable dom
  => (Signal dom Bool -> Signal dom a -> Signal dom b) -- ^ Clock-enabled circuit
  -> DataFlow dom Bool Bool a b                        -- ^ DataFlow circuit
gatedToDF f = DF $ \x vi ro -> let en = vi .&&. ro
                               in (f en x, en, ro)

-- | Convert a clock-enabled circuit with a valid-tagged output to a DataFlow
-- circuit. Use this over `gatedToDF` if you need to suppress any invalid
-- ramp-up values, for example, when keeping frame alignment for FFT data.
gatedVToDF
  :: HiddenClockResetEnable dom
  => (Signal dom Bool -> Signal dom a
      -> (Signal dom Bool, Signal dom b))              -- ^ Clock-enabled circuit
  -> DataFlow dom Bool Bool a b                        -- ^ DataFlow circuit
gatedVToDF f = DF $ \x vi ro -> let en = vi .&&. ro
                                    (vo, x') = f en x
                                in  (x', vo, ro)

-- | Convert a clock-enabled circuit with Maybe output to a DataFlow circuit
gatedMaybeToDF
  :: (HiddenClockResetEnable dom, Default b)
  => (Signal dom Bool -> Signal dom a
      -> Signal dom (Maybe b))                         -- ^ Clock-enabled circuit
  -> DataFlow dom Bool Bool a b                        -- ^ DataFlow circuit
gatedMaybeToDF f = let f' = \en a -> (\o->(fmap isJust o, fmap (fromMaybe def) o)) $ f en a
                   in gatedVToDF f'

-- | Register DataFlow circuit for pipelining
regDF
  :: (HiddenClockResetEnable dom, Num a, NFDataX a)
  => DataFlow dom Bool Bool a a
regDF = hideClockResetEnable fifoDF d2 Nil

-- | FIFO DataFlow circuit for buffering test input data
testBufferDF
  :: (HiddenClockResetEnable dom, Num a, NFDataX a, KnownNat n)
  => SNat n -> DataFlow dom Bool Bool a a
testBufferDF n = hideClockResetEnable fifoDF len (Nil)
  where len = powSNat d2 (clogBaseSNat d2 $ addSNat d1 n)

-- | Throttle DataFlow circuit for limiting chain throughput, used mainly for
-- testing
throttleDF
  :: (HiddenClockResetEnable dom, Default a, NFDataX a)
  => SNat n -> DataFlow dom Bool Bool a a
throttleDF n = DF $ \x vi ro -> let readyGate = riseEvery n
                                in (x, vi .&&. readyGate, ro .&&. readyGate)

-- | Lift a circuit into a registered DataFlow wrapper
liftRegDF
  :: (HiddenClockResetEnable dom, Num b, NFDataX b)
  => (Signal dom a                 -- ^ Circuit to lift
      -> Signal dom Bool
      -> Signal dom Bool
      -> (Signal dom b, Signal dom Bool, Signal dom Bool))
  -> DataFlow dom Bool Bool a b    -- ^ Registered DataFlow circuit
liftRegDF ip = liftDF ip `seqDF` hideClockResetEnable fifoDF d2 (repeat @2 0)

-- | Counter DataFlow circuit with wrapping overflow
counterDF
  :: (HiddenClockResetEnable dom, Num a, NFDataX a)
  => DataFlow dom Bool Bool () a
counterDF = gatedToDF $ \en _ -> let x = regEn 0 en $ x+1
                                 in x

-- | Oscillator DataFlow circuit
oscillateDF
  :: (HiddenClockResetEnable dom, Num a, NFDataX a)
  => a                            -- ^ Initial value
  -> DataFlow dom Bool Bool () a  -- ^ Oscillator DataFlow circuit
oscillateDF a = gatedToDF $ \en _ -> let x = regEn a en $ (-1)*x
                                     in x
