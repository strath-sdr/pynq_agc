module LutLogTests where

import LutLog

import Prelude
import Clash.Prelude (KnownNat, SNat(..), Unsigned, UFixed, SFixed, pow2SNat, snatToNum, unSF, unUF)
import Test.QuickCheck

ufToDouble :: forall n m . (KnownNat n, KnownNat m) => UFixed n m -> Double
ufToDouble u = fromIntegral (unUF u) / (snatToNum $ pow2SNat (SNat :: SNat m))

sfToDouble :: forall n m . (KnownNat n, KnownNat m) => SFixed n m -> Double
sfToDouble u = fromIntegral (unSF u) / (snatToNum $ pow2SNat (SNat :: SNat m))

prop_equal_float = forAll (suchThat arbitrary (>1)) prop
  where
  prop :: Unsigned 16 -> Bool
  prop x = let exp = logBase 2 (fromIntegral x)
               act = ufToDouble $ lutLog2 (SNat :: SNat 8) x
               percent_margin = 0.5
               error = max exp act - min exp act
           in error / exp <= percent_margin / 100


prop_equal_float_10 = forAll (suchThat arbitrary (>1)) prop
  where
  prop :: Unsigned 16 -> Bool
  prop x = let exp = logBase 10 (fromIntegral x)
               act = ufToDouble $ lutLog10 (SNat :: SNat 8) x
               percent_margin = 2
               error = max exp act - min exp act
           in error / exp <= percent_margin / 100


prop_approx_pow2 :: UFixed 4 10 -> Bool
prop_approx_pow2 x = let exp = 2 ** (ufToDouble x)
                         act = fromIntegral $ lutAntilog2 x
                         percent_margin = 1
                         error = max exp act - min exp act
                     in error < 1 || error / exp <= percent_margin / 100


prop_approx_pow10 :: UFixed 2 10 -> Bool
prop_approx_pow10 x = let exp = 10 ** (ufToDouble x)
                          act = fromIntegral $ lutAntilog10 x
                          percent_margin = 3
                          error = max exp act - min exp act
                      in error < 1 || error / exp <= percent_margin / 100

return []
runTests = $quickCheckAll
