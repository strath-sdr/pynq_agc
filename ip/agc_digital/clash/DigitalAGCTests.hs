module DigitalAGCTests where

import Prelude
import Clash.Prelude (Signed, Unsigned, System, simulate, SNat(..), bundle, riseEvery)
import Data.Maybe (catMaybes)
import DigitalAGC
import DataFlow.Extra
import Test.QuickCheck

allEqual xs ys = foldl (&&) True $ zipWith (==) xs ys

{- Power detector approximation -}

refPowerDetector :: Double -> Double -> Double
refPowerDetector i q = sqrt $ (i*i) + (q*q)

prop_PowerDetector :: Signed 26 -> Signed 26 -> Bool
prop_PowerDetector i q =
  let exp = refPowerDetector (fromIntegral i) (fromIntegral q)
      act = fromIntegral $ powerDetector i q
      percent_margin = 6
      error = max exp act - min exp act
      isExtreme x = abs x < 50 || abs x > (maxBound-50)
  in isExtreme i || isExtreme q || error <= percent_margin / 100 * exp
{-
Above test shows that as long as the signal above 0.3% of the full range, we can expect less than 6% error.
-}

refIntgDump :: Int -> [Int] -> [Int]
refIntgDump _ [] = []
refIntgDump n xs = let (a,b) = splitAt n xs
                       window = sum a `div` n
                   in if length a == n
                      then window : refIntgDump n b
                      else []

prop_IntgDump :: Unsigned 5 -> [Unsigned 26] -> Bool
prop_IntgDump w xs =
  let exp = refIntgDump (2^w) $ map fromIntegral xs
      act = map fromIntegral . drop 1 . catMaybes $
            simulate @System (intgDumpPow2 (pure w)) (cycle xs)
  in xs == [] || allEqual exp act

prop_IntgDumpDf :: Unsigned 5 -> [Unsigned 26] -> Bool
prop_IntgDumpDf w xs =
  let exp = refIntgDump (2^w) $ map fromIntegral xs
      act = map fromIntegral . drop 1 . map (\(x,_,_)->x) . filter (\(_,v,r)->v&&r) $
            simulate @System (\x -> bundle $ df (testBufferDF (SNat :: SNat 10000) `seqDF` throttleDF (SNat :: SNat 4) `seqDF` dfIntgDump (pure w)) x (pure True) (riseEvery (SNat :: SNat 3)) ) (cycle xs)
  in xs == [] || length xs > 10000 || allEqual exp act

prop_ToIntgDumpDf :: Unsigned 5 -> [(Signed 26, Signed 26)] -> Bool
prop_ToIntgDumpDf w xs =
  let exp = refIntgDump (2^w) . map (round . uncurry refPowerDetector) $ map (\(i,q) -> (fromIntegral i, fromIntegral q)) xs
      act = map fromIntegral . drop 1 . map (\(x,_,_)->x) . filter (\(_,v,r)->v&&r) $
            simulate @System (\x -> bundle $ df (testBufferDF (SNat :: SNat 10000) `seqDF` throttleDF (SNat :: SNat 4) `seqDF` dfPowDetect `seqDF` dfIntgDump (pure w)) x (pure True) (riseEvery (SNat :: SNat 3)) ) (cycle xs)
  in xs == [] || length xs > 10000 || allEqual exp act

{- We are equal to our reference integrate and dump function for all window sizes
   between 2^0 and 2^15, with a single invalid ramp-up value
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
