module AnalogueAGCTests where

import Prelude
import AnalogueAGC
import Clash.Prelude (Unsigned, UFixed, SNat, System, simulate, unbundle)

sim = simulate @System ( uncurry (agc 0.001 2 0.001 2 2.0) . unbundle )

-- take 6500 . sim $ replicate 1500 (False,False) ++ replicate 1500 (False,True) ++ replicate 1500 (True, True)
