module AxisSync where

import Clash.Prelude

createDomain vXilinxSystem{vName="XilDom", vResetPolarity=ActiveLow}

type TDATA = BitVector 128
type TVALID = Bit
type TREADY = Bit
type TLAST  = Bit

risingEdge b =
  let prev = register False b
  in fmap not prev .&&. b

fallingEdge b =
  let prev = register False b
  in fmap not b .&&. prev

syncGate :: (HiddenClockResetEnable dom, KnownNat n)
         => Signal dom Bool -> Signal dom (Unsigned n)
         -> Signal dom Bool
syncGate go n = moore fState fOut 0 $ bundle (go, n)
  where
  fState s (go, n) = if go then n else (boundedSub s 1)
  fOut   s         = s > 0

syncAxis :: (HiddenClockResetEnable dom, KnownNat n)
         => Signal dom Bool -> Signal dom (Unsigned n)
         -> (Signal dom Bool, Signal dom Bool)
syncAxis go n =
  let gate' = syncGate (risingEdge go) n
      gate  = register False gate'
      last = fallingEdge gate'
  in (gate, last)

topLevel
  :: Clock  XilDom
  -> Reset  XilDom
  -- INPUTS
  -- Params
  -> Signal XilDom Bit            -- ^ Go
  -> Signal XilDom (BitVector 32) -- ^ N
  -- TX in
  -> Signal XilDom TDATA
  -> Signal XilDom TVALID
  -- RX in
  -> Signal XilDom TDATA
  -> Signal XilDom TVALID
  -- TX out
  -> Signal XilDom TREADY
  -- RX out
  -> Signal XilDom TREADY

  -- OUTPUTS
  -> Signal XilDom (
    -- TX in
    TREADY
    -- RX in
    ,TREADY
    -- TX out
    ,TDATA
    ,TVALID
    -- RX out
    ,TDATA
    ,TVALID
    ,TLAST
    )
topLevel clk rst go n tid tiv rid riv tor ror =
  let (gatedHold, gatedLast) = exposeClockResetEnable syncAxis clk rst (toEnable $ pure True) (bitToBool <$> go) (unpack <$> n)
      tir = fmap boolToBit gatedHold
      rir = pure 1
      tod = tid
      tov = pure 1
      rod = rid
      rov = fmap boolToBit gatedHold
      rol = fmap boolToBit gatedLast
  in bundle (tir, rir, tod, tov, rod, rov, rol)

{-# ANN topLevel
  (Synthesize
    { t_name   = "axisSync"
    , t_inputs = [ PortName "clk"
                 , PortName "aresetn"
                 , PortName "go"
                 , PortName "n"
                 , PortName "S_AXIS_TX_TDATA"
                 , PortName "S_AXIS_TX_TVALID"
                 , PortName "S_AXIS_RX_TDATA"
                 , PortName "S_AXIS_RX_TVALID"
                 , PortName "M_AXIS_TX_TREADY"
                 , PortName "M_AXIS_RX_TREADY"
                 ]
    , t_output = PortProduct ""
                   [ PortName "S_AXIS_TX_TREADY"
                   , PortName "S_AXIS_RX_TREADY"
                   , PortName "M_AXIS_TX_TDATA"
                   , PortName "M_AXIS_TX_TVALID"
                   , PortName "M_AXIS_RX_TDATA"
                   , PortName "M_AXIS_RX_TVALID"
                   , PortName "M_AXIS_RX_TLAST"
                   ]
    }) #-}
