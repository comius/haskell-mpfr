{-# OPTIONS_GHC -F -pgmF script/cpphs -optF--layout -optF--hashes #-}
{- -# LANGUAGE CPP #-}
{- -# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-} -- foreign import prim
{-# LANGUAGE MagicHash #-} -- postfix # on identifiers
{-# LANGUAGE UnboxedTuples #-} -- (#  #)
{-# LANGUAGE UnliftedFFITypes #-}  -- argument type of foreign import
{-# LANGUAGE BangPatterns #-}  -- !

module Data.Approximate.MPFRLowLevel ( 
-- * Types
  RoundMode(..), Precision, Rounded,

#  include "MPFR/export.hhs"
-- * Assignment functions
  set,
  posInf, negInf, zero, negZero, nan,  
  fromInt, fromIntegerA, fromDouble, fromRationalA,

-- * Conversion functions  
  toRationalA, toDoubleA, toDouble2Exp,  toInteger2Exp,
  toRawStringExp, toStringHex, toStringBin, toStringSci, toStringFix, toString, toStringReadback,
  
#  include "MPFR/conversion.h"

-- * Arithmetic functions
#  include "MPFR/arithmetics.h"
  mul2i, 

-- * Comparison functions
  isNaN, isInfinite, isZero,
# include "MPFR/comparison.h"

-- * Special functions
# include "MPFR/special.h"
  facw, zetaw, jn, yn,

-- * Integer functions
# include "MPFR/integer.h"
   
-- * Miscellaneous functions
  getPrec,
  getExp,
) where
import Prelude as Prelude hiding (isNaN, isInfinite, div, sqrt, exp, log, sin, cos, tan, asin, acos, atan, pi, abs, min, max, floor, round, sinh, cosh,tanh, acosh, asinh, atanh, atan2)
import qualified Prelude(floor, max)
import Data.Bits
import Data.List (isInfixOf)
import Data.Ratio
import GHC.CString -- unpackCString
import GHC.Int -- Int32#
import GHC.Integer.GMP.Internals -- #S, #J
import GHC.Prim -- Int#, ByteArray#,
import GHC.Types -- Word
import GHC.Integer.GMP.Prim (int2Integer#)
import Data.Approximate.MPFR.Types

{- 5.2 Assignment Functions -}

foreign import prim "mpfr_cmm_set" mpfrFromMpfr#
  :: Unary

foreign import prim "mpfr_cmm_init_si" mpfrFromInt#
  :: CRounding# -> CPrecision# -> Int# -> RoundedOut#

foreign import prim "mpfr_cmm_init_z" mpfrFromInteger#
  :: CRounding# -> CPrecision# -> Int# -> ByteArray# -> RoundedOut#

foreign import prim "mpfr_cmm_init_q" mpfrFromRational#
  :: CRounding# -> CPrecision# -> Int# -> ByteArray# -> Int# -> ByteArray# -> RoundedOut#

foreign import prim "mpfr_cmm_init_d" mfprFromDouble#
  :: CRounding# -> CPrecision# -> Double# -> RoundedOut#

foreign import prim "mpfr_cmm_init_z_2exp" mpfrEncode#
  :: CRounding# -> CPrecision# -> CExp# -> Int# -> ByteArray# -> RoundedOut#

set :: RoundMode -> Precision -> Rounded -> Rounded
set = unary mpfrFromMpfr#

--set_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)

posInf :: Rounded
posInf = Rounded s (-0x8000000000000000# +# 3#) l
  where  (!Rounded s _ l) = zero
negInf :: Rounded
negInf = Rounded s (-0x8000000000000000# +# 3#) l
  where (!Rounded s _ l) = fromInt Near 2 (-1)

nan = Rounded s (-0x8000000000000000# +# 2#) l
  where (!Rounded s _ l) = zero

zero :: Rounded
zero = fromInt Near 2 0

negZero :: Rounded
negZero = negate zero

fromInt :: RoundMode -> Precision -> Int -> Rounded
fromInt r p (I# i#) = Rounded s e l where
    (# s, e, l #) = mpfrFromInt# (mode# r) (prec# p) i#

--fromInt_ :: RoundMode -> Precision -> Int -> (Rounded, Int)

fromIntegerA :: RoundMode -> Precision -> Integer -> Rounded
fromIntegerA r p (S# i) = Rounded s e l where
    (# s, e, l #) = mpfrFromInt# (mode# r) (prec# p) i
fromIntegerA r p (J# i xs) = Rounded s e l where
    (# s, e, l #) = mpfrFromInteger# (mode# r) (prec# p) i xs

--fromString :: String -> Precision -> GHC.Types.Word -> Rounded
--fromWord :: RoundMode -> Precision -> GHC.Types.Word -> Rounded
--fromWord_ :: RoundMode -> Precision -> GHC.Types.Word -> (Rounded, Int)

-- | Construct a rounded floating point number directly from a 'Double'.
fromDouble :: RoundMode -> Precision -> Double -> Rounded
fromDouble r p (D# d) = Rounded s e l where
    (# s, e, l #) = mfprFromDouble# (mode# r) (prec# p) d

--fromDouble_ :: RoundMode -> Precision -> Double -> (Rounded, Int)
--stringToMPFR :: RoundMode -> Precision -> GHC.Types.Word -> String -> Rounded
--stringToMPFR_ :: RoundMode -> Precision -> GHC.Types.Word -> String -> (Rounded, Int)
--strtofr ::RoundMode -> Precision -> GHC.Types.Word -> String -> (Rounded, String)
--strtofr_ :: RoundMode  -> Precision -> GHC.Types.Word -> String -> (Rounded, String, Int)
toInt# :: Integer -> (# Int#, ByteArray# #)
toInt# (S# x#) = int2Integer# x#
toInt# (J# x# xs#) =  (# x#, xs# #)
fromRationalA :: RoundMode -> Precision -> Rational -> Rounded
fromRationalA r p rat = Rounded s e l where
    !(# n, ns #) = toInt# $ numerator rat
    !(# d, ds #) = toInt# $ denominator rat
    (# s, e, l #) = mpfrFromRational# (mode# r) (prec# p) n ns d ds

{-
int2i :: RoundMode -> Precision -> Int -> Int -> Rounded
int2i_ :: RoundMode -> Precision -> Int -> Int -> (Rounded, Int)
int2w :: RoundMode -> Precision -> GHC.Types.Word -> Int -> Rounded
int2w_ :: RoundMode -> Precision -> GHC.Types.Word -> Int -> (Rounded, Int)
-}

{- 5.4 Conversion Functions -}

{-
toDouble :: RoundMode -> Rounded -> Double
toDouble2exp :: RoundMode -> Rounded -> (Double, Int)
toInt :: RoundMode -> Rounded -> Int
toWord :: RoundMode -> Rounded -> GHC.Types.Word
-}

foreign import prim "mpfr_cmm_get_z_2exp" mpfrDecode#
  :: CSignPrec# -> CExp# -> ByteArray# -> (# CExp#, Int#, ByteArray# #)

toInteger2Exp :: Rounded -> (Integer, Int)
toInteger2Exp (Rounded sp e l) = case mpfrDecode# sp e l of (# i, s, d #) -> (J# s d, I# i)

toRationalA :: Rounded -> Rational
toRationalA r
   | e > 0     = fromIntegral (s `shiftL` e)
   | otherwise = s % (1 `shiftL` negate e)
   where (s, e) = toInteger2Exp r

foreign import prim "mpfr_cmm_get_d" mpfrGetDouble#
   :: CRounding# ->
      CSignPrec# -> CExp# -> ByteArray# ->
      Double#

foreign import prim "mpfr_cmm_get_d_2exp" mpfrGetDouble2Exp#
   :: CRounding# ->
      CSignPrec# -> CExp# -> ByteArray# ->
      (# Double#, Int# #)

toDoubleA :: RoundMode -> Rounded -> Double
toDoubleA r (Rounded sp e l) =
    let d = mpfrGetDouble# (mode# r) sp e l in D# d

toDouble2Exp :: RoundMode -> Rounded -> (Double, Int)
toDouble2Exp r (Rounded sp e l) =
    let (# d, exp #) = mpfrGetDouble2Exp# (mode# r) sp e l in (D# d, I# exp) 

foreign import prim "mpfr_cmm_asprintf" mpfrASPrintf#
  :: Addr# -> Int# -> CRounding# ->
     CSignPrec# -> CExp# -> ByteArray# -> ByteArray#

foreign import prim "mpfr_cmm_readbackstr" mpfrReadback#
  :: CSignPrec# -> CExp# -> ByteArray# -> ByteArray#


mpfrASPrintf :: Addr# -> Int -> RoundMode -> Rounded -> String
mpfrASPrintf f (I# p) r (Rounded s e l) = unpackCString# (byteArrayContents# str) where
   str = mpfrASPrintf# f p (mode# r) s e l

toStringReadback ::  Rounded -> String
toStringReadback (Rounded s e l) = unpackCString# (byteArrayContents# str) where
   str = mpfrReadback# s e l

toStringHex  r p d = mpfrASPrintf "%.*R*a"# p r d
toStringBin r p d = mpfrASPrintf "%.*R*b"# p r d
toStringSci r p d = mpfrASPrintf "%.*R*e"# p r d
toStringFix r p d = mpfrASPrintf "%.*R*f"# p r d
toString r p d = mpfrASPrintf "%.*R*g"# p r d

foreign import prim "mpfr_cmm_get_str" mpfrGetStr#
  :: CRounding# -> Int# -> Int# ->
     CSignPrec# -> CExp# -> ByteArray# ->
     (# Int#, ByteArray# #)


toRawStringExp :: RoundMode -> Int -> Int -> Rounded -> (String, Exp)
toRawStringExp r (I# n) (I# d) (Rounded s e l) = (unpackCString# (byteArrayContents# str), I64# i) where
   (# i, str #) = mpfrGetStr# (mode# r) n d s e l


#include "MPFR/types.hhs"
#include "MPFR/conversion.h"

instance Show Rounded where
    show = toStringReadback

{- 5.5 Basic Arithmetic Functions -}


#include "MPFR/arithmetics.h"

absD = Data.Approximate.MPFRLowLevel.abs

mul2i :: RoundMode -> Precision -> Rounded -> Int -> Rounded
mul2i r p (Rounded s e l) (I# i) = Rounded s (e +# i) l

{-
addd :: RoundMode -> Precision -> Rounded -> Double -> Rounded
addd_ :: RoundMode -> Precision -> Rounded -> Double -> (Rounded, Int)
addi :: RoundMode -> Precision -> Rounded -> Int -> Rounded
addi_ :: RoundMode -> Precision -> Rounded -> Int -> (Rounded, Int)
addw :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> Rounded
addw_ :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> (Rounded, Int)
-}

{-
subd :: RoundMode -> Precision -> Rounded -> Double -> Rounded
subd_ :: RoundMode -> Precision -> Rounded -> Double -> (Rounded, Int)
subi :: RoundMode -> Precision -> Rounded -> Int -> Rounded
subi_ :: RoundMode -> Precision -> Rounded -> Int -> (Rounded, Int)
subw :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> Rounded
subw_ :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> (Rounded, Int)
dsub :: RoundMode -> Precision -> Double -> Rounded -> Rounded
dsub_ :: RoundMode -> Precision -> Double -> Rounded -> (Rounded, Int)
isub :: RoundMode -> Precision -> Int -> Rounded -> Rounded
isub_ :: RoundMode -> Precision -> Int -> Rounded -> (Rounded, Int)
wsub :: RoundMode -> Precision -> GHC.Types.Word -> Rounded -> Rounded
wsub_ :: RoundMode -> Precision -> GHC.Types.Word -> Rounded -> (Rounded, Int)
-}

{-
mul2i_ :: RoundMode -> Precision -> Rounded -> Int -> (Rounded, Int)
muld :: RoundMode -> Precision -> Rounded -> Double -> Rounded
muld_ :: RoundMode -> Precision -> Rounded -> Double -> (Rounded, Int)
muli :: RoundMode -> Precision -> Rounded -> Int -> Rounded
muli_ :: RoundMode -> Precision -> Rounded -> Int -> (Rounded, Int)
mulw :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> Rounded
mulw_ :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> (Rounded, Int)
mul2w :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> Rounded
mul2w_ :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> (Rounded, Int)

divd :: RoundMode -> Precision -> Rounded -> Double -> Rounded
divd_ :: RoundMode -> Precision -> Rounded -> Double -> (Rounded, Int)
divi :: RoundMode -> Precision -> Rounded -> Int -> Rounded
divi_ :: RoundMode -> Precision -> Rounded -> Int -> (Rounded, Int)
divw :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> Rounded
divw_ :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> (Rounded, Int)
ddiv :: RoundMode -> Precision -> Double -> Rounded -> Rounded
ddiv_ :: RoundMode -> Precision -> Double -> Rounded -> (Rounded, Int)
idiv :: RoundMode -> Precision -> Int -> Rounded -> Rounded
idiv_ :: RoundMode -> Precision -> Int -> Rounded -> (Rounded, Int)
wdiv :: RoundMode -> Precision -> GHC.Types.Word -> Rounded -> Rounded
wdiv_ :: RoundMode -> Precision -> GHC.Types.Word -> Rounded -> (Rounded, Int)
div2i :: RoundMode -> Precision -> Rounded -> Int -> Rounded
div2i_ :: RoundMode -> Precision -> Rounded -> Int -> (Rounded, Int)
div2w :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> Rounded
div2w_ :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> (Rounded, Int)
-}

{-
sqrtw :: RoundMode -> Precision -> GHC.Types.Word -> Rounded
sqrtw_ :: RoundMode -> Precision -> GHC.Types.Word -> (Rounded, Int)
root :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> Rounded
root_ ::
  RoundMode -> Precision -> Rounded -> GHC.Types.Word -> (Rounded, Int)
powi :: RoundMode -> Precision -> Rounded -> Int -> Rounded
powi_ :: RoundMode -> Precision -> Rounded -> Int -> (Rounded, Int)
poww :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> Rounded
poww_ :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> (Rounded, Int)

wpow :: RoundMode -> Precision -> GHC.Types.Word -> Rounded -> Rounded
wpow_ ::
  RoundMode -> Precision -> GHC.Types.Word -> Rounded -> (Rounded, Int)
wpoww ::
  RoundMode -> Precision -> GHC.Types.Word -> GHC.Types.Word -> Rounded
wpoww_ ::
  RoundMode
  -> Precision -> GHC.Types.Word -> GHC.Types.Word -> (Rounded, Int)
-}

{- 5.6 Comparison Functions -}

foreign import prim "mpfr_cmm_cmp" mpfrCmp# :: CSignPrec# -> CExp# -> ByteArray#
                       -> CSignPrec# -> CExp# -> ByteArray# -> Int#

foreign import prim "mpfr_cmm_sgn" mpfrSgn# :: CSignPrec# -> CExp# -> ByteArray# -> Int#

#include "MPFR/comparison.h"

greatereq = greaterequal
lesseq = lessequal
notequal = lessgreater
minD = Data.Approximate.MPFRLowLevel.min
maxD = Data.Approximate.MPFRLowLevel.max

instance Eq Rounded where
  (==) = equal
  (/=) = notequal

instance Ord Rounded where
  compare (Rounded s e l) (Rounded s' e' l') = compare (fromIntegral (I# (mpfrCmp# s e l s' e' l'))) (0 :: Int32) -- TODO opt
  (<=) = lesseq
  (>=) = greatereq
  (<) = less
  (>) = greater
  {- TODO
  min = binary mpfrMin#
  max = binary mpfrMax#
-}

isNaN :: Rounded -> Bool
isNaN (Rounded _ e _) = isTrue# (e ==# -0x8000000000000000# +# 2#)

isInfinite :: Rounded -> Bool
isInfinite (Rounded _ e _) = isTrue# (e ==# -0x8000000000000000# +# 3#)

isZero :: Rounded -> Bool
isZero (Rounded _ e _) = isTrue# (e ==# -0x8000000000000000# +# 1#)

{-cmp :: Rounded -> Rounded -> Maybe Ordering

cmpd :: Rounded -> Double -> Maybe Ordering
cmpi :: Rounded -> Int -> Maybe Ordering
cmpw :: Rounded -> GHC.Types.Word -> Maybe Ordering
cmp2i :: Rounded -> Int -> Exp -> Maybe Ordering
cmp2w :: Rounded -> GHC.Types.Word -> Exp -> Maybe Ordering

cmpabs :: Rounded -> Rounded -> Maybe Ordering

unordered :: Rounded -> Rounded -> Maybe Bool

isInteger :: Rounded -> Bool
isNumber :: Rounded -> Bool
sgn :: Rounded -> Maybe Int

--}

{- 5.7 Special Functions -}

#include "MPFR/special.h"

foreign import prim "mpfr_cmm_fac" mpfrFac#
  :: CRounding# -> CPrecision# -> Word# -> RoundedOut#

foreign import prim "mpfr_cmm_zetaw" mpfrZeta#
  :: CRounding# -> CPrecision# -> Word# -> RoundedOut#

foreign import prim "mpfr_cmm_jn" mpfrJn#
  :: CRounding# -> CPrecision# -> Int# -> CSignPrec# -> CExp# -> ByteArray# -> RoundedOut#

foreign import prim "mpfr_cmm_yn" mpfrYn#
  :: CRounding# -> CPrecision# -> Int# -> CSignPrec# -> CExp# -> ByteArray# -> RoundedOut#


facw :: RoundMode -> Precision -> GHC.Types.Word -> Rounded
facw r p (W# x) = Rounded s' e' l' where
    (# s', e', l' #) = mpfrFac# (mode# r) (prec# p) x 

zetaw :: RoundMode -> Precision -> GHC.Types.Word -> Rounded
zetaw r p (W# x) = Rounded s' e' l' where
    (# s', e', l' #) = mpfrZeta# (mode# r) (prec# p) x 

jn :: RoundMode -> Precision -> Int -> Rounded -> Rounded
jn r p (I# x) (Rounded s e l) = Rounded s' e' l' where
    (# s', e', l' #) = mpfrJn# (mode# r) (prec# p) x s e l 


yn :: RoundMode -> Precision -> Int -> Rounded -> Rounded
yn r p (I# x) (Rounded s e l) = Rounded s' e' l' where
    (# s', e', l' #) = mpfrYn# (mode# r) (prec# p) x s e l 

{- 5.10 Integer and Remainder related functions -}

#include "MPFR/integer.h"

{-
remquo :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)
remquo_ ::
  RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int, Int)
-}

{- 5.11 Rounding related functions -}
{-
maxPrec :: Rounded -> Rounded -> Precision
-}

{- 5.12 Misc functions-}

{-
nextToward :: Rounded -> Rounded -> Rounded
nextAbove :: Rounded -> Rounded
nextBelow :: Rounded -> Rounded

-- newRandomStatePointer ::
--  GHC.Ptr.Ptr hmpfr-0.3.3.5:FFIhelper.GmpRandState
--urandomb ::
--  GHC.Ptr.Ptr hmpfr-0.3.3.5:FFIhelper.GmpRandState
--  -> Precision -> Rounded

-}


getExp :: Rounded -> Exp
getExp (Rounded _ e# _) = I64# e#

{-
setExp :: Rounded -> Exp -> Rounded

signbit :: Rounded -> Bool


bitsInInteger :: Num a => Integer -> a

compose :: RoundMode -> Precision -> (Integer, Int) -> Rounded
decompose :: Rounded -> (Integer, Exp)

--freeCache :: IO ()

getMantissa :: Rounded -> Integer

one :: Rounded

-}
