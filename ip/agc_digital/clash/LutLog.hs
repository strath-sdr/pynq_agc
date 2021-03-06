module LutLog where

import Clash.Prelude
import qualified Data.List as L
import qualified Test.QuickCheck as Q

-- Our version of fLitR that adds rounding
fLitR' :: forall rep int frac . (KnownNat frac, Bounded (rep (int + frac)), Integral (rep (int + frac)))
       => Double -> Fixed rep int frac
fLitR' = fLitR . (+ (1 / (snatToNum $ pow2SNat m)))
  where m = SNat :: SNat (frac+1)

-- Our version of resizeUF' that adds rounding
resizeUF'
  :: forall int1 frac1 int2 frac2 .
     (KnownNat int1, KnownNat frac1,
      KnownNat int2, KnownNat frac2) =>
      UFixed int1 frac1 -> UFixed int2 frac2
resizeUF' = resizeF . add halfstep
  where halfstep :: UFixed 0 (frac2 + 1)
        halfstep = succ 0

-- Log 2

lutLog2I :: forall n m . (KnownNat n, KnownNat m, FracUFixedC 1 m, 1 <= n) => Unsigned n -> UFixed ( CLog 2 n) m
lutLog2I x = resizeF logInt + resizeF logFrac
  where
        ms1 = ifoldl (\acc i b -> if bitToBool b then i else acc) 0
              (reverse . bv2v $ pack x) -- most significant one
        addr :: Unsigned m
        addr = if ms1 > snatToNum m
                 then resize $ shiftR x (fromIntegral ms1 - snatToNum m)
                 else resize $ shiftL x (snatToNum m - fromIntegral ms1)
        logInt  :: UFixed (CLog 2 n) 0
        logInt  = uf d0 (unpack (pack ms1))
        logFrac :: UFixed 0 m
        logFrac = uf m . unpack $ asyncRomFile (pow2SNat m) (logBinFileName m) addr
        m = SNat :: SNat m

lutLog2 :: forall n m . (KnownNat n, KnownNat m, FracUFixedC 1 m, 1 <= n) => SNat m -> Unsigned n -> UFixed (CLog 2 n) m
lutLog2 _ = lutLog2I

logBinFileName :: SNat m -> String
logBinFileName m = "loglut.mem"
                   --"loglut_"   -- Can't synthesize string concat in clash... What about using TH?
                   --L.++ show m
                   --L.++ ".bin"

writeLutLogFile :: forall m . KnownNat m => SNat m -> IO ()
writeLutLogFile m = do
  let msqr = snatToNum $ pow2SNat m
      ufs  = [fLitR' (logBase 2 (1+i/msqr)) :: UFixed 0 m | i<-[0 .. msqr - 1]]
      bvs = L.map (filter (/= '_') . show . pack) ufs
  writeFile (logBinFileName m) (unlines bvs)

-- Log 10

lutLog10I
  :: forall n m . (KnownNat n, KnownNat m, 1 <= n, 1<=CLog 10 (2^n))
     => Unsigned n -> Fixed Unsigned (CLog 2 (CLog 10 (2^n))) m
lutLog10I = resizeUF' . mul coef . lutLog2 (SNat :: SNat m)  -- truncation with resizef a source of error
  where coef :: UFixed 0 m
        coef = resizeUF' ( $$(fLit . recip $ logBase 2 10) :: UFixed 0 128 )
        -- bit of a hack here... will only work up to 128, but seems like I need
        -- to give a constant width to keep fLit's TH magic happy.

lutLog10
  :: forall n m . (KnownNat n, KnownNat m, 1 <= n, 1<=CLog 10 (2^n))
     => SNat m -> Unsigned n -> Fixed Unsigned (CLog 2 (CLog 10 (2^n))) m
lutLog10 _ = lutLog10I

{-
What about inverse log?

we have log_2(x) and log_10(x)

these invert to 2^x 10^x

... 2^x is easy just set the xth bit. Any fractional bits can have a lookup.

We can change the base with a^b= c^{b * log_c(a)}

For us, 10^b = 2^{b * log_2(10)}
i.e., we multiply out input by a constant and then do our 2^x

-}

-- Antilog 2

lutAntilog2 :: forall n m . (KnownNat n, KnownNat m, FracUFixedC 1 m, 1 <= n)
            => UFixed n m -> Unsigned (2^n)
lutAntilog2 x = y
  where
        m = SNat :: SNat m
        xInt  = fromIntegral . unUF $ (resizeF x :: UFixed n 0)
        xFrac = pack (resize $ unUF x :: Unsigned m)
        yHead = setBit (fromIntegral xInt) zeroBits :: Unsigned (2^n)
        yTail = uf m . unpack $ asyncRomFile (pow2SNat m) (antilogBinFileName m) xFrac :: UFixed 0 m
        y = setBit (unUF (
              resizeF $ shiftL (
                  resizeF yTail :: UFixed (2^n) m
              ) xInt :: UFixed (2^n) 0)) xInt

antilogBinFileName :: SNat m -> String
antilogBinFileName m = "antiloglut.mem"
                       --"antiloglut_"
                       --L.++ show m
                       --L.++ ".bin"

writeLutAntilogFile :: forall m . KnownNat m => SNat m -> IO ()
writeLutAntilogFile m = do
  let msqr = snatToNum $ pow2SNat m
      ufs  = [fLitR' ((2 ** x) - 1) :: UFixed 0 m | i<-[0 .. msqr - 1], let x = (i/msqr)]
      bvs = L.map (filter (/= '_') . show . pack) ufs
  writeFile (antilogBinFileName m) (unlines bvs)

 -- Antilog 10

lutAntilog10
  :: forall n m . (KnownNat n, KnownNat m, 1 <= n)
  => UFixed n m -> Unsigned (CLog 2 (10^(2^n)))
lutAntilog10 = resize . lutAntilog2 . trunc . mul coef
  where coef :: UFixed 2 m
        coef = resizeUF' ( $$(fLit $ logBase 2 10) :: UFixed 2 128 )
        trunc :: (KnownNat a, KnownNat b) => UFixed a b -> UFixed (n+2) m
        trunc = resizeUF'
