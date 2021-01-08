{-# LANGUAGE OverloadedStrings #-}

module Simulation where

import DigitalAGC
import LutLog

import qualified Clash.Explicit.Signal as ES
import Prelude
import Clash.Prelude (UFixed, Unsigned, Signed,
  createDomain, simulate, vInitBehavior, vName, vSystem, InitBehavior(..), shiftR,
  snatToNum, SFixed(..), sf, unSF, unbundle, bundle, df, SNat(..), Vec(..), System, simulate_lazy, listToVecTH, Nat, knownDomain, KnownDomain(..), HiddenClockResetEnable)
import Control.Monad (forM_)
import qualified Data.List as L

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

import qualified Clash.Prelude as Clash
import qualified Numeric.FFT as FFT
import Data.List (sort)
import Test.QuickCheck
import Data.Reflection (reifyNat)

createDomain vSystem{vName="SyncDefined", vInitBehavior=Defined}

simInput = let iqs = map (\(i,q)->(shiftR i 1, shiftR q 1)) $ sinInputComplex 1 0.01
           in  L.take 3000 iqs ++ map (\(i,q)->(shiftR i 5, shiftR q 5)) iqs

sim = let ref = 4.5 :: UFixed 4 6
          window = 7 :: Unsigned 5
          alpha = 0.5 :: UFixed 0 4
          fLog = Clash.d6
          fGain = Clash.d10
          out_gain = Clash.simulate @System (uncurry (digiAgcMult (pure window) (pure ref) (pure alpha)). unbundle) simInput
      in map (zip [1..]) [
                           map (fromIntegral . (\(_,x,_)->x)) out_gain
                         , map (fromIntegral . (\(_,_,x)->x)) out_gain
                         , map (ufToDouble   . (\(x,_,_)->x)) out_gain
                         ]

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

-- God holp us

doFFT
  :: Double -> [[(Double, Double)]] -> [(Double, Double)]
doFFT fs t = let x    = zipWith (\(_,i) (_,q)-> i DC.:+ q) (t!!0) (t!!1)
                 arrx = listArray (0, (length x) - 1) x
                 fftshift xs = drop (length xs `div` 2) xs ++ take (length xs `div` 2) xs
                 arrX = zip (linspace ((-fs)/2) (fs/2) (length x) ) $ fftshift . toList $ fft arrx
             in map (\(t,c)->(t,DC.magnitude c)) arrX

sinInput :: Double -> Double -> [Signed 16]
sinInput fs dt = map (fromInteger . round . ((2**15-1) * ) .  sin . (2*pi*fs * )) [0.0, dt..]

sinInputComplex :: Double -> Double -> [(Signed 16, Signed 16)]
sinInputComplex fs dt = zip sins coss
  where sins = map (fromInteger . round . ((2**15-1) * ) .  sin . (2*pi*fs * )) [0.0, dt..]
        coss = map (fromInteger . round . ((2**15-1) * ) .  cos . (2*pi*fs * )) [0.0, dt..]


approxEqual :: Double -> Double -> Bool
approxEqual x y = abs (x - y) < 0.0001


fftToMagPhase :: [DC.Complex Double] -> [[(Double, Double)]]
fftToMagPhase cs = [ zip f_series abs, zip f_series phase ]
  where
  cs' = fftshift cs
  f_series = linspace (-0.5) (0.5) (length cs)
  abs = map DC.magnitude cs'
  phase = map DC.phase cs'
  fftshift xs = drop (length xs `div` 2) xs ++ take (length xs `div` 2) xs

fftToIQ :: [DC.Complex Double] -> [[(Double, Double)]]
fftToIQ cs = [ zip f_series i, zip f_series q ]
  where
  cs' = fftshift cs
  f_series = linspace (-0.5) (0.5) (length cs)
  i = map DC.realPart cs'
  q = map DC.imagPart cs'
  fftshift xs = drop (length xs `div` 2) xs ++ take (length xs `div` 2) xs


-- |`script` tag to go in the header to import the plotly.js javascript from the official CDN
reloadCDN :: Monad m => HtmlT m ()
reloadCDN = script_ [src_ "http://livejs.com/live.js"] $ toHtml (""::String)

styleSheet :: Monad m => HtmlT m ()
styleSheet = style_ [type_ "text/css" ] $
             toHtml (".svg-container {margin: 0 auto !important;}"::String)

main n =
    renderToFile "/tmp/clash/test.html" $ doctypehtml_ $ do
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
--               toHtml $ plotly "fft_mag" (traceFFT $ doFFT fs tData)
--                          & layout . title ?~ "Freq domain"
  where
  tData = map (L.take n) sim :: [[(Double, Double)]]
  ctrlData = [tData !! 2, tData !! 2]
  simInputTrace = map (L.take n . zip [(1::Double)..] . map (sfToDouble . sf Clash.d0))  [map fst simInput, map snd simInput]

traceTime a =  [trace "I" $ a !! 0
               ,trace "Q" $ a !! 1]
  where trace label points = linePlot points
                               & name ?~ label
                               & (GPB.line ?~ (defLine & lineshape ?~ Hv))

traceConstl points = [scatterPlot $ zip (map snd (points!!0)) (map snd (points!!1))]

traceFFT a = [linePlot a & name ?~ "FFT Magnitude"
                         & (GPB.line ?~ (defLine & lineshape ?~ Hv))]

startServer = callCommand "mkdir -p /tmp/clash; cd /tmp/clash/; python -m http.server > /dev/null 2>&1 &"

return []
runTests = $quickCheckAll
