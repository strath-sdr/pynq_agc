module DigitalAGCTests where

import Prelude
import Clash.Prelude (Signed)
import DigitalAGC
import Test.QuickCheck

{- Power detector approximation -}

simPowerDetector :: Double -> Double -> Double
simPowerDetector i q = sqrt $ (i*i) + (q*q)

prop_approx_power :: Signed 16 -> Signed 16 -> Bool
prop_approx_power i q = let exp = simPowerDetector (fromIntegral i) (fromIntegral q)
                            act = fromIntegral $ powerDetector i q
                            percent_margin = 6
                            lim = 200
                            error = max exp act - min exp act
                        in abs i < lim || abs q < lim || error / exp <= percent_margin / 100
{-
Above test shows that as long as the signal above 0.6% of the full range, we can expect less than 6% error.

Not bad for a sqrt etc. implemented with 2 adds!
-}


{-
 What are some nice properties to test our AGC?

  1) (Low passed) output power should be nearly constant, as described by our parameters

     Checks that we are 1) normalising the signal's amplitude
                        2) within the expected response time
                        3) to a level controlled by our parameters

  2) Output signal should correlate very strongly to ideal input
     This is the same as saying our gain control signal should be very close to the flipped version of our generated input gain envelope (within 1 response time period)

     Checks that we are 1) actually passing our input signal through the system
                        2) not distorting it so much that it will be hard to process downstream


 These are both essentially saying:
   * Generate a sensible input
   * Generate a slower, random gain envelope
   * Pass this through our AGC
   * Compare our inverted control signal to the envelope
     using parameters to allow differences in time and scaling
-}

{-
OK, what about moddeling the recovery time of the AGC for instantenous changes?

Look at an extreme case... say signal comming in is at 100 "power"
-}

simAgc :: Double -> Double -> Double -> Double -> Double
simAgc alpha ref inPower logState =
  let logPow = (logBase 10 (10**(logState) / 64 * inPower)) / 4
      logState' = logState + alpha * (ref - logPow)
  in logState'

countToStable :: [Double] -> Int
countToStable []       = error "List doesn't converge"
countToStable (x:[])   = error "List doesn't converge"
countToStable (x:y:zs) = if abs (x-y) / y < 0.005 -- Did we move by less than 0.5%?
                         then 0
                         else 1 + countToStable (y:zs)

-- -- Getting space state equations from that book and modifying them for our circuit
{-
0.1,56
0.2,40
0.30000000000000004,31
0.4,26
0.5,22
0.6,19
0.7,17
0.7999999999999999,15
0.8999999999999999,14
0.9999999999999999,13
1.0999999999999999,12
1.2,11
1.3,10
1.4000000000000001,10
1.5000000000000002,9
1.6000000000000003,8
1.7000000000000004,8
1.8000000000000005,7
1.9000000000000006,7)
-}
