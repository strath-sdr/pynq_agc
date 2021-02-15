{-# LANGUAGE OverloadedStrings #-}

module Simulation where

import DigitalAGC
import LutLogTests (sfToDouble, ufToDouble)

import Clash.Prelude (
  UFixed, Unsigned, Signed, SFixed(..), Vec(..), Nat, KnownDomain(..), HiddenClockResetEnable, SNat(..),
  createDomain, vInitBehavior, vName, vSystem, InitBehavior(..), Signal,
  simulate, System,
  shiftR,
  register,
  sf, bundle, df,
  fLitR, KnownNat
  )

import Prelude
import Data.Maybe (fromMaybe)
import Data.List (group)

import Lucid
import Lucid.Html5
import Graphics.Plotly hiding (sort)
import qualified Graphics.Plotly.Base as GPB
import Graphics.Plotly.Simple
import Graphics.Plotly.Lucid
import Lens.Micro
import System.Process
import Data.Array
import Data.Foldable

import qualified Data.Complex as DC
import Numeric.Transform.Fourier.FFT
import DSP.Basic (linspace)

import Test.QuickCheck

import Statistics.Sample
import qualified Data.Vector.Unboxed as V
--import Data.Fixed

--dec :: Int -> [a] -> [a]
--dec n [] = []
--dec n (x : xs) = x : dec n (drop (n-1) xs)

interp :: Int -> [a] -> [a]
interp n [] = []
interp n (x : xs) = replicate n x ++ interp n xs

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = take n xs : chunks n (drop n xs)

quantiseS :: forall i f . (KnownNat i, KnownNat f) => SNat i -> SNat f -> Double -> Double
quantiseS _ _ x = sfToDouble (fLitR x :: SFixed i f)

quantiseU :: forall i f . (KnownNat i, KnownNat f) => SNat i -> SNat f -> Double -> Double
quantiseU _ _ x = ufToDouble (fLitR x :: UFixed i f)

d0 = SNat :: SNat 0

iSig   = SNat :: SNat 26
iGain  = SNat :: SNat 10
fGain  = SNat :: SNat 10
iAlpha = SNat :: SNat 1
fAlpha = SNat :: SNat 6
iRef   = SNat :: SNat 3
fRef   = SNat :: SNat 12
iLog   = SNat :: SNat 4
fLog   = SNat :: SNat 22

simAgc :: Double -> Double -> Double -> [(Double, Double)] -> [(Double, Double,Double)]
simAgc window ref alpha iqs =
  let window' = 2**window
      powers blk = map (\(i,q)-> quantiseU iSig d0 . sqrt $ i^2 + q^2) blk
      smooth blk = quantiseU iSig d0 $ (/window') $ sum  blk
      alg state x = quantiseS iLog fLog (
                      (quantiseS iLog fLog state) -
                      ((quantiseS iLog fLog $ logBase 10 (smooth $ powers x)) - (quantiseU iRef fRef ref)) *
                      (quantiseU iAlpha fAlpha alpha))
      gains = scanl (\state x ->
                quantiseS iLog fLog $
                alg state (map (\(i,q)->(
                  quantiseS iSig d0 $ (i*) $ quantiseU iGain fGain $ 10**state,
                  quantiseS iSig d0 $ (q*) $ quantiseU iGain fGain $ 10**state)
                  ) x)
              ) 0
              (init $ chunks (round window') iqs)
  in zipWith (\(i,q) s -> let g = quantiseU iGain fGain $ 10**s
                          in (g, quantiseS iSig d0 $ i*g, quantiseS iSig d0 $ q*g))
     iqs (interp (round window') gains)


-- My floating point simulation shows us that we _should_ be ok with our
-- approach... make some tests to verify the log and exp DF behaviour.

createDomain vSystem{vName="SyncDefined", vInitBehavior=Defined}

simInput = let iqs = map (\(i,q)->(shiftR i 1, shiftR q 1)) $ sinInputComplex 1 0.01
           in  take 3000 iqs ++ map (\(i,q)->(shiftR i 4, shiftR q 4)) iqs

-- from 1.25 to 0.75... our ref is basically divided by 4, so this makes sense for a 100x range
-- we have 0.5 range with 6 frac bits => resolution of 1/64, so 32 sensible settings for our signal height.
-- This is all with alpha = 0.9375 and a window of 7
-- Window doesn't really affect this too much
-- Alpha seems to?

-- we have a 2.6 log word ( really 4.4)

-- a = 0.9375 and window = 128 ~ 1.5k         1.5k
-- a = 0.5                       3k           3.5k
-- a = 0.25                      4.5k         5k
-- a = 0.125                     7k           8k
-- a = 0.0625                                 10k

--   alpha             recovery samples   diff      log10  log diff
--        1; 5         2                  ----      0.3    --------
--      0.5; 7.3       5.3                3.3       0.72   0.42
--     0.25; 12        9                  3.7       0.95   0.23
--    0.125; 20        17                 8         1.23   0.27
--   0.0625; 33        31                 14        1.49   0.26

-- ref = 1.13 for 32k
-- for 1000, log 10
--sim = let ref = 1.05 :: UFixed 2 10
--          window = 7 :: Unsigned 5
--          alpha = 2.0 :: UFixed 1 6
--          fLog = Clash.d6
--          fGain = Clash.d6
--          out_gain = Clash.simulate @System (uncurry (digiAgcMult (pure window) (pure ref) (pure alpha)). unbundle) simInput
--      in map (zip [1..]) [
--                           map (fromIntegral . (\(_,x,_)->x)) out_gain
--                         , map (fromIntegral . (\(_,_,x)->x)) out_gain
--                         , map (ufToDouble   . (\(x,_,_)->x)) out_gain
--                         ]

{-
Just thinking about sensible wordlengths for RFSoC...

Our input is always 16 bits.

==========

This will likely be after decimation, used inside the user's RX digital logic so we can expect deal with sampling rates well below the 4 GSamples/s... say probably a max of 512 MHz.

How many window bits are needed for a period of 1 ms?

1e-3 * 512e6 = 18.9 ... let's just say 19

==========

Fullscale signal after log will be...

after ID; we need (nWindow + nSig) bits (35!!!)
after log10; we need (1+log2 35) bits (9) and fLog fractional bits, let's say 9.fLog
after sub with ref; we need 10.fLog
after mul with alpha we need (10+ia).(fLog+fa)

{ What should alpha's range be to allow a good selection of response times? }

After the antilog, our wordlength increases exponentially! We should be super
careful about growing our wordlengths in the log domain.

==========

nSig = 16
nWindow = 19
-}

-- Experimentally found recovery cycles for every tenth decimal step in alpha
-- alphas = [0.1,0.2,...1.9]
recoveryCycles :: [(Int, Int)]
recoveryCycles = [(1,56),(2,40),(3,31),(4,26),(5,22),(6,19),(7,17),(8,15),(9,14),(10,13),(11,12),(12,11),(13,10),(14,10),(15,9),(16,8),(17,8),(18,7),(19,7)]

getRecoveryCycles :: Double -> Int
getRecoveryCycles = fromMaybe (error "No entry found for that alpha") . flip lookup recoveryCycles. round . (*10)

newtype InGain = InGain Double deriving Show
newtype InStepTime = InStepTime Int deriving Show

instance Arbitrary InStepTime where
  arbitrary = fmap InStepTime $ choose (1000,10000)

instance Arbitrary InGain where
  arbitrary = fmap (InGain . recip . fromIntegral) $ choose (1::Int,63)

steppedInput :: InGain -> InGain -> InStepTime -> [(Signed 16,Signed 16)]
steppedInput (InGain g1) (InGain g2) (InStepTime n) =
  let a = map (\(i,q)->(multD i g1, multD q g1)) . take n $ sinInputComplex 1 0.01
      b = map (\(i,q)->(multD i g2, multD q g2))           $ sinInputComplex 1 0.01
  in a ++ b
  where
  multD s d = fromIntegral . round $ fromIntegral s * d

splitInto n [] = []
splitInto n xs = let (a,b) = splitAt n xs
                 in a : splitInto n b

isSteady :: Int -> Double -> Double -> [Double] -> Bool
isSteady n ref percent = (<=n) . maximum . map length . filter (\a->False == a!!0) . group . map (\x->abs (x-ref)/ref < (percent/100))

--simDfLogErr ref alpha x =
--  simulate @System (
--    bundle $ df (\x-> dfLogErr (pure ref) (pure alpha)) x (register True (pure False)) (pure True) :: Signal System (SFixed 4 22, Bool, Bool)
--    )
--  (repeat x) :: [(SFixed 4 22, Bool, Bool)]

{--

1) Can I reproduce the variation in simulation? Maybe generating a repeating pattern with a non-integer multiple of the window size

2) Does this also appear in the floating point simulation, because that doesn't have the delays from cordic stuff.

If no to 1) maybe it's to do with my DataFlow implementation?

I'm seeing spikes for 32 cycles in the output. Why is this?! We should only be seeing 512 sample changes because of the window of 2^9


I've fixed it... Two things now:

  1) Are the log10 and pow10 df units *really* returning consistient values?

  2) We can add a feature to set the error state! We can use that in the python instead of feeding it extra samples. Is there anything else we should flush? Maybe just pad with 32ish samples. Wait. isn't that just the reset functionality that should already exist? I've just mapped the areset pin to axi reg 0, bit 1.

--}

simOutPower g1 g2 n =
  let ref = 4.0 :: UFixed 3 12
      alpha = 1.0 :: UFixed 1 6
      window = 9 :: Unsigned 5
      --inputSig = take (10000 + rec_time*(2^window)) $ steppedInput g1 g2 n
      inputSig = take (8000*4) . cycle . take 8000 $ steppedInput g1 g2 n
      rec_time = (2+) . getRecoveryCycles $ ufToDouble alpha
      --out_gain = take 15000 $ simAgc (fromIntegral window) (ufToDouble ref) (ufToDouble alpha) (map (\(i,q)->(fromIntegral i, fromIntegral q)) inputSig)
      --out_pow = map (\(_,i,q)-> sqrt $ (i)**2 + (q)**2) out_gain
      ip x = bundle $ df (dfAgc (pure window) (pure ref) (pure alpha) (pure True)) x (pure True) (pure True) :: Signal System ((UFixed 10 15, (Signed 16, Signed 16)), Bool, Bool)
      --outs = drop 1 . take (10000 + rec_time*(2^window))
      outs = drop 1 . take (8000*4)
             $ simulate @System ip inputSig
      out_gain = map (\((g,(i,q)), v,r)->(g,i,q)) outs
      out_pow = map (\(_,i,q)-> sqrt $ (fromIntegral i)**2 + (fromIntegral q)**2) out_gain
      out_pow_block = map ((/(2^window)) . sum) $ splitInto (2^window) out_pow
      expected_pow = 10**(ufToDouble ref)
  in (inputSig, out_gain, out_pow, out_pow_block, rec_time, expected_pow)

prop_OutPower :: InGain -> InGain -> InStepTime -> Property
prop_OutPower g1 g2 (InStepTime n) =
  let (_, _, _, out_pow_block, rec_time, expected_pow) = simOutPower g1 g2 (InStepTime n)
  in property . isSteady rec_time expected_pow 1 $ drop (ceiling $ (fromIntegral n) / 2**7) out_pow_block

showTest g1 g2 n =
    renderToFile "/tmp/clash/sim.html" $ doctypehtml_ $ do
    head_ $ do meta_ [charset_ "utf-8"]
               plotlyCDN
               reloadCDN
               styleSheet
    body_ $ do
               toHtml $ plotly "time_iq_in" (traceTime simInputTrace)
                          & layout . title ?~ "Time domain I/Q Input"
               toHtml $ plotly "time_iq_out" (traceTime tData)
                          & layout . title ?~ "Time domain I/Q Output"
               toHtml $ plotly "constl_iq_out" (traceConstl tData)
                          & layout . title ?~ "Constellation I/Q"
                          & layout . width ?~ 600
                          & layout . height ?~ 600
               toHtml $ plotly "time_iq_ctrl" (traceTime ctrlData)
                          & layout . title ?~ "Time domain Control"
  where
  (simInput, sim, out_pow, _, _, _) = simOutPower g1 g2 n
  tData = map (zip [1..]) $ [map (\(_,x,_)->fromIntegral x) sim
                            ,map (\(_,_,x)->fromIntegral x) sim
                            ,map (\(x,_,_)->ufToDouble x)   sim
                            --[map (\(_,x,_)-> x) sim
                            --,map (\(_,_,x)-> x) sim
                            --,map (\(x,_,_)->x)   sim
                            ] :: [[(Double, Double)]]
  ctrlData = [tData !! 2, tData !! 2]
  simInputTrace = map (zip [(1::Double)..] . map (sfToDouble . sf (SNat :: SNat 0)))  [map fst simInput, map snd simInput]

corr :: [(Double, Double)] -> Double
corr = correlation . V.fromList

--prop_corrTest g1 g2 n =
--  let (_, outsig, _, _, recTime, _) = simOutPower g1 g2 n
--      insig = steppedInput (InGain 0.3125) (InGain 0.3125) (InStepTime 0)
--      inI  = map (fromIntegral . fst) insig
--      inQ  = map (fromIntegral . snd) insig
--      outI = map (fromIntegral . (\(_,i,_)->i)) outsig
--      outQ = map (fromIntegral . (\(_,_,q)->q)) outsig
--      correlation = corr $ zip (drop recTime inI) (drop recTime outI)
--  in property $ correlation > 0.9

bl10 n = fromIntegral . ceiling $ logBase 2 (logBase 10 (2**n))
be10 n = fromIntegral . ceiling $ logBase 2 (10 ** (2**n))

bitsCalc :: Double -> Double -> Double -> (Double, Double) -> Double
         -> (Double, (Double, Double), (Double, Double))
bitsCalc window sig fLog (iAlpha, fAlpha) fGain =
  let intgInternal = 2**window + sig
      iLog = bl10 sig
      iGain = be10 iLog - fGain
  in (intgInternal, (iLog,fLog), (iGain, fGain))

sinInput :: Double -> Double -> [Signed 16]
sinInput fs dt = map (fromInteger . round . ((2**15-1) * ) .  sin . (2*pi*fs * )) [0.0, dt..]

sinInputComplex :: Double -> Double -> [(Signed 16, Signed 16)]
sinInputComplex fs dt = zip sins coss
  where sins = map (fromInteger . round . ((2**15-1) * ) .  sin . (2*pi*fs * )) [0.0, dt..]
        coss = map (fromInteger . round . ((2**15-1) * ) .  cos . (2*pi*fs * )) [0.0, dt..]

reloadCDN :: Monad m => HtmlT m ()
reloadCDN = script_ [src_ "http://livejs.com/live.js"] $ toHtml (""::String)

styleSheet :: Monad m => HtmlT m ()
styleSheet = style_ [type_ "text/css" ] $
             toHtml (".svg-container {margin: 0 auto !important;}"::String)

traceTime a =  [trace "I" $ a !! 0
               ,trace "Q" $ a !! 1]
  where trace label points = linePlot points
                               & name ?~ label
                               & (GPB.line ?~ (defLine & lineshape ?~ Hv))

traceConstl points = [scatterPlot $ zip (map snd (points!!0)) (map snd (points!!1))]

startServer = callCommand "mkdir -p /tmp/clash; cd /tmp/clash/; python -m http.server > /dev/null 2>&1 &"

return []
runTests = $quickCheckAll

{- TODO

Include bin files in IP make script

Try find what resolution we need to increase in order to better recover very very low signals.

-}
