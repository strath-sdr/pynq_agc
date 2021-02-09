{-# LANGUAGE TemplateHaskell #-}
module CordicLogTests where

import CordicLog

import Prelude
import Clash.Prelude (KnownNat, SNat(..), Signed, SFixed, Unsigned, UFixed, pow2SNat, snatToNum, unSF, unUF, simulate, System, listToVecTH, Vec(..), fLit)
import Test.QuickCheck

ufToDouble :: forall n m . (KnownNat n, KnownNat m) => UFixed n m -> Double
ufToDouble u = fromIntegral (unUF u) / (snatToNum $ pow2SNat (SNat :: SNat m))

sfToDouble :: forall n m . (KnownNat n, KnownNat m) => SFixed n m -> Double
sfToDouble u = fromIntegral (unSF u) / (snatToNum $ pow2SNat (SNat :: SNat m))

--prop_comb_ln_32 = forAll (suchThat arbitrary (\x -> x>0 && x<1)) prop
--  where
--  prop :: UFixed 0 32 -> Bool
--  prop x = let exp = log (ufToDouble x)
--               act = sfToDouble $ lnComb32 x
--               percent_margin = 0.01
--               error = max exp act - min exp act
--           in error / exp <= percent_margin / 100
--
--prop_comb_ln_scaled = forAll (suchThat arbitrary (\x-> x>1)) prop
--  where
--  prop :: Unsigned 26 -> Bool
--  prop x = let exp = log (fromIntegral x)
--               act = sfToDouble $ lnCombScaled x
--               percent_margin = 5
--               error = max exp act - min exp act
--           in error / exp <= percent_margin / 100

paramsVec :: Vec 32 (Int, SFixed 5 32)
paramsVec = $(listToVecTH $ take 32 (zip shiftSequence arctanhs))

eLn2sVec :: Vec 27 (UFixed 5 32)
eLn2sVec = $(listToVecTH $ take 27 eLn2s)

kScaling :: SFixed 5 32
kScaling = $$(fLit $ 1 / kValue 32)

prop_ln_norm = forAll (suchThat arbitrary (\x -> x>0.5)) prop
  where
  prop :: UFixed 0 32 -> Bool
  prop x = let exp = log (ufToDouble x)
               act = sfToDouble (simulate @System (lnNorm paramsVec) (repeat x) !! 33)
               percent_margin = 0.01
               error = max exp act - min exp act
           in error / abs exp <= percent_margin / 100

prop_ln_scaled = forAll (suchThat arbitrary (\x -> x>1)) prop
  where
  prop :: Unsigned 26 -> Bool
  prop x = let exp = log (fromIntegral x)
               act = sfToDouble (simulate @System (lnScaled paramsVec eLn2sVec) (repeat x) !! 33)
               percent_margin = 0.01
               error = max exp act - min exp act
           in error / abs exp <= percent_margin / 100

prop_log10 = forAll (suchThat arbitrary (\x -> x>1)) prop
  where
  prop :: Unsigned 26 -> Bool
  prop x = let exp = logBase 10 (fromIntegral x)
               act = sfToDouble (simulate @System (log10 paramsVec eLn2sVec) (repeat x) !! 33)
               percent_margin = 0.1
               error = max exp act - min exp act
           in error / abs exp <= percent_margin / 100

prop_exp_norm = forAll (suchThat arbitrary (\x -> abs x < 0.69)) prop
  where
  prop :: UFixed 0 32 -> Bool
  prop x = let expt = exp (ufToDouble x)
               act = ufToDouble (simulate @System (expNorm paramsVec kScaling) (repeat x) !! 33)
               percent_margin = 0.01
               error = max expt act - min expt act
           in error / abs expt <= percent_margin / 100

prop_exp_scaled = forAll (suchThat arbitrary (\x -> x<16)) prop
  where
  prop :: UFixed 5 22 -> Bool
  prop x = let expt = exp (ufToDouble x)
               act = ufToDouble (simulate @System (expScaled paramsVec kScaling eLn2sVec) (repeat x) !! 33)
               percent_margin = 0.1
               error = max expt act - min expt act
           in error / abs expt <= percent_margin / 100

prop_pow10 = forAll (suchThat arbitrary (\x -> x<6)) prop
  where
  prop :: UFixed 4 22 -> Bool
  prop x = let expt = 10 ** (ufToDouble x)
               act = ufToDouble (simulate @System (pow10 paramsVec kScaling eLn2sVec) (repeat x) !! 33)
               percent_margin = 1
               error = max expt act - min expt act
           in error / abs expt <= percent_margin / 100

return []
runTests = $quickCheckAll
