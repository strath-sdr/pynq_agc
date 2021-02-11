module CordicLog where

import Clash.Prelude
import qualified Prelude as P

shiftSequence :: [Int]
shiftSequence = go 1 4
  where go k k'
          | k == k' = k : go k (3*k+1)
          | otherwise = k : go (k+1) k'

arctanhs :: [Double]
arctanhs = P.map func shiftSequence
    where
    func i = atanh (1 / (2 ** fromIntegral i))

eLn2s :: [Double]
eLn2s = [fromIntegral e*log 2 :: Double | e<-[0..]]

kValue :: Int -> Double
kValue i = product $ P.take i $ P.map func shiftSequence
    where
    func i = sqrt (1 - 1 / (2 ** (2*fromIntegral i)))

--hypParams :: (KnownNat i, KnownNat f) => [(Int, SFixed i f)]
hypParams = P.zip shiftSequence $ P.map fLitR arctanhs

-- https://dl.acm.org/doi/10.1145/1478786.1478840 for original paper on unified cordic

-- https://github.com/adamwalker/clash-utils/blob/master/src/Clash/DSP/CORDIC.hs for inspiration

-- https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjXxInysNDuAhUSYsAKHaDvB5EQFjAAegQIARAC&url=https%3A%2F%2Fwww.st.com%2Fresource%2Fen%2Fdesign_tip%2Fdm00441302-coordinate-rotation-digital-computer-algorithm-cordic-to-compute-trigonometric-and-hyperbolic-functions-stmicroelectronics.pdf&usg=AOvVaw2-BweEKvQ5rEuIRknwcHy_
-- Has reference c code for shift sequence

type CordicState a b = (a,a,b)

hypStepVectoring :: (KnownNat i, KnownNat f) =>
        Int -> (SFixed i f) -> CordicState (SFixed i f) (SFixed i f) -> CordicState (SFixed i f) (SFixed i f)
hypStepVectoring shift delta (x,y,z) = (x', y', z')
  where
  addSub sel x y
        | sel       = resizeF $ x `add` y
        | otherwise = resizeF $ x `sub` y
  x' = addSub (y < 0) x (shiftR y shift)
  y' = addSub (y < 0) y (shiftR x shift)
  z' = addSub (not $ y < 0) z delta

hypStepRotation :: (KnownNat i, KnownNat f) =>
        Int -> (SFixed i f) -> CordicState (SFixed i f) (SFixed i f) -> CordicState (SFixed i f) (SFixed i f)
hypStepRotation shift delta (x,y,z) = (x', y', z')
  where
  addSub sel x y
        | sel       = resizeF $ x `add` y
        | otherwise = resizeF $ x `sub` y--
  x' = addSub (z >= 0) x (shiftR y shift)--
  y' = addSub (z >= 0) y (shiftR x shift)--
  z' = addSub (not $ z >= 0) z delta



toSigned :: forall n . KnownNat n => Unsigned n -> Signed (n+1)
toSigned u = let  u' = resize u :: Unsigned (n+1)
             in fromIntegral u'

toUnsigned :: forall n . KnownNat n => Signed (n+1) -> Unsigned n
toUnsigned s = let u = fromIntegral s :: Unsigned (n+1)
               in resize u

toSF :: forall n m . (KnownNat n, KnownNat m) => UFixed n m -> SFixed (n+1) m
toSF = sf (SNat :: SNat m) . toSigned . unUF

toUF :: forall n m . (KnownNat n, KnownNat m) => SFixed (n+1) m -> UFixed n m
toUF = uf (SNat :: SNat m) . toUnsigned . unSF


--lnComb :: Int -> SFixed 2 32 -> SFixed 2 32 -> SFixed 5 32
--lnComb iters x y = (\(_,_,n)->2*n) $ P.foldl (flip $ uncurry hypStep) (resizeF x, resizeF y, 0) (P.take iters hypParams)
--
--lnComb32 :: UFixed 0 32 -> SFixed 5 32
--lnComb32 u = lnComb 32 (x+1) (x-1)
--  where x = resizeF $ toSF u
--
--lnCombScaled :: Unsigned 26 -> SFixed 7 33
--lnCombScaled u =
--  let u' = resizeF . toSF $ uf d26 (shiftL u (25-ms1))
--      ms1 = fromIntegral $ ifoldl (\acc i b -> if bitToBool b then i else acc) 0
--            (reverse . bv2v $ pack u) -- most significant one
--      x   = u' + 1
--      y   = u' - 1
--      ln32b = fLitR $ fromIntegral (26-(25-ms1)) * log 2 :: SFixed 6 33
--  in (lnComb 32 x y) `add` ln32b


lnNorm :: HiddenClockResetEnable dom
       => Vec n (Int, SFixed 5 32)
       -> Signal dom (UFixed 0 32)
       -> Signal dom (SFixed 5 32)
lnNorm args u = fmap (\(_,_,z)->shiftL z 1) $
                foldl (\x params -> delay (0,0,0) $ fmap ((uncurry hypStepVectoring) params) x)
                (bundle (s+1, s-1, 0))
                args
  where
  s = (resizeF . toSF) <$> u

lnScaled :: (HiddenClockResetEnable dom)
         => Vec n (Int, SFixed 5 32)
         -> Vec 27 (SFixed 6 32)
         -> Signal dom (Unsigned 26)
         -> Signal dom (SFixed 7 32) --TODO tighten bits
lnScaled args eLn2s u =
  let scaleDown u shift = resizeF $ uf d26 (shiftL u shift)
      iOfMS1 u = fromIntegral $ ifoldl (\acc i b -> if bitToBool b then i else acc) 0
                 (reverse . bv2v $ pack u)
      ms1 = iOfMS1 <$> u
      eLn2 = (\i -> eLn2s !! fromIntegral i) <$> (26-(25-ms1))
  in liftA2 add eLn2 (lnNorm args (liftA2 scaleDown u (25-ms1)))

log10 :: forall dom n . (HiddenClockResetEnable dom)
         => Vec n (Int, SFixed 5 32)
         -> Vec 27 (SFixed 6 32)
         -> Signal dom (Unsigned 26)
         -> Signal dom (SFixed 4 22)
log10 paramsVec eLn2sVec x =
  let lnX = resizeF <$> lnScaled paramsVec eLn2sVec x :: Signal dom (SFixed 6 21)
      scaling = $$(fLit $ 1 / (log 10)) :: SFixed 7 11
      log10X = mul scaling <$> lnX
  in resizeF <$> log10X


expNorm :: HiddenClockResetEnable dom
       => Vec n (Int, SFixed 5 32)
       -> SFixed 5 32
       -> Signal dom (SFixed 1 32)
       -> Signal dom (UFixed 4 32)
expNorm args init u = fmap (\(x,_,_)->toUF x) $
                      foldl (\x params -> delay (0,0,0) $ fmap ((uncurry hypStepRotation) params) x)
                      (bundle (pure init, pure init, s))
                      args
  where
  s = resizeF <$> u

expScaled :: forall n dom . (HiddenClockResetEnable dom)
         => Vec n (Int, SFixed 5 32)
         -> SFixed 5 32
         -> Vec 27 (SFixed 6 32)
         -> Signal dom (SFixed 6 22)
         -> Signal dom (UFixed 24 21) --TODO tighten bits
expScaled args init eLn2s u =
  let getQIndex u = foldl (\acc (i,b) -> if b then i else acc) 0 $ imap (\i x -> (fromIntegral i, abs u>x)) eLn2s
      s = resizeF <$> u
      iQ = getQIndex <$> s
      qLn2 = (eLn2s !!) <$> iQ
      s' = mux (s .>. 0) (s - qLn2) (s + qLn2)
      shift s expD iQ = if s > 0
                        then resizeF $ shiftL expD iQ
                        else resizeF $ shiftR expD iQ
      expD = resizeF <$> (expNorm args init (resizeF <$> s')) :: Signal dom (UFixed 24 32)
      res = liftA3 shift s expD iQ
  in res

pow10 :: forall n dom . (HiddenClockResetEnable dom)
         => Vec n (Int, SFixed 5 32)
         -> SFixed 5 32
         -> Vec 27 (SFixed 6 32)
         -> Signal dom (SFixed 5 22)
         -> Signal dom (UFixed 24 32) --TODO tighten bits
pow10 args init eLn2s u =
  let scaling = $$(fLit $ log 10) :: SFixed 3 16
      x' = resizeF <$> mul scaling <$> u
      expX = resizeF <$> expScaled args init eLn2s x'
  in expX
{-
For unsigned 26 in

log10 :: 0 -> 7.8

we need to accept SFixed 4 22 at input to scaled exp
exp (q*ln2 + x) = 2^q * exp x

we need to find how many times x is than 0.69?
could be up to 12x

10^ will need prescaling by log10 though. This gets us to 27x again

find ms1 and try to put that in the 0.0X position, so we'll always be between 0.25 and 0.4999
-}
