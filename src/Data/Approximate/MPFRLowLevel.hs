{-# OPTIONS_GHC -F -pgmF script/cpphs -optF--layout -optF--hashes #-}
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
  fromInt, fromIntegerA, fromDouble, fromRationalA, fromInteger2Exp, readRounded,

-- * Conversion functions  
  toRationalA, toDoubleA, toDouble2Exp,  toInteger2Exp,
  toRawStringExp, toStringHex, toStringBin, toStringSci, toStringFix, toString, toStringReadback,
  
#  include "MPFR/conversion.h"

-- * Arithmetic functions
#  include "MPFR/arithmetics.h"
  mul2i, div2i, root,

-- * Comparison functions
  isNaN, isInfinite, isZero, cmp, cmpAbs, sgn,
# include "MPFR/comparison.h"

-- * Special functions
# include "MPFR/special.h"
  facw, zetaw, jn, yn, lgamma,

-- * Integer functions
# include "MPFR/integer.h"
  remquo,
   
-- * Miscellaneous functions
  nextAbove, nextBelow, nextToward, copySign,
  getPrec,
  getExp,
) where
import Prelude as Prelude hiding (isNaN, isInfinite, div, sqrt, exp, log, sin, cos, tan, asin, acos, atan, pi, abs, min, max, floor, round, sinh, cosh,tanh, acosh, asinh, atanh, atan2)
import Data.Bits
import Data.Ratio
import GHC.CString -- unpackCString
import GHC.Int -- Int32#
import GHC.Integer.GMP.Internals -- #S, #J
import GHC.Prim -- Int#, ByteArray#,
import GHC.Types -- Word
import GHC.Integer.GMP.Prim (int2Integer#)
import GHC.Pack
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

foreign import prim "mpfr_cmm_strtofr" mpfrFromStr#
  :: CRounding# -> CPrecision# -> Int# -> Addr# -> (# CSignPrec#, CExp#, ByteArray#, Addr#, Int# #) 

{-|
    Read a floating-point number from a string in base with given 'Precision' and rounded with 'RoundMode';
    __base__ must be either 0 (to detect the base, as described below) or a number from 2 to 62 
    (otherwise the behavior is undefined). If string starts with valid data, the result and remaining
    string (just after the valid data) is returned; otherwise zero and the whole input string is returned
    (for consistency with strtod). The usual ternary value is returned.

    Parsing follows the standard C @strtod@ function with some extensions. After optional leading
    whitespace, one has a subject sequence consisting of an optional sign (+ or -), and either numeric 
    data or special data. The subject sequence is defined as the longest initial subsequence of the input
    string, starting with the first non-whitespace character, that is of the expected form.

    The form of numeric data is a non-empty sequence of significand digits with an optional decimal point,
    and an optional exponent consisting of an exponent prefix followed by an optional sign and a non-empty
    sequence of decimal digits. A significand digit is either a decimal digit or a Latin letter 
    (62 possible characters), with A = 10, B = 11, …, Z = 35; case is ignored in bases less or equal to
    36, in bases larger than 36, a = 36, b = 37, …, z = 61. The value of a significand digit must be 
    strictly less than the base. The decimal point can be either the one defined by the current locale or
    the period (the first one is accepted for consistency with the C standard and the practice, the second
    one is accepted to allow the programmer to provide MPFR numbers from strings in a way that does not
    depend on the current locale). The exponent prefix can be e or E for bases up to 10, or \@ in any base;
    it indicates a multiplication by a power of the base. In bases 2 and 16, the exponent prefix can also
    be p or P, in which case the exponent, called __binary exponent__, indicates a multiplication by a power
    of 2 instead of the base (there is a difference only for base 16); in base 16 for example 1p2 
    represents 4 whereas 1\@2 represents 256. The value of an exponent is always written in base 10.

    If the argument __base__ is 0, then the base is automatically detected as follows. If the significand 
    starts with 0b or 0B, base 2 is assumed. If the significand starts with 0x or 0X, base 16 is assumed.
    Otherwise base 10 is assumed.

    Note: The exponent (if present) must contain at least a digit. Otherwise the possible exponent prefix
    and sign are not part of the number (which ends with the significand). Similarly, if 0b, 0B, 0x or 0X
    is not followed by a binary/hexadecimal digit, then the subject sequence stops at the character 0, 
    thus 0 is read.

    Special data (for infinities and NaN) can be @\@inf\@@ or @\@nan\@(n-char-sequence-opt)@, and if /base <= 16/, 
    it can also be @infinity@, @inf@, @nan@ or @nan(n-char-sequence-opt)@, all case insensitive. A 
    @n-char-sequence-opt@ is a possibly empty string containing only digits, Latin letters and the 
    underscore (0, 1, 2, …, 9, a, b, …, z, A, B, …, Z, _). Note: one has an optional sign for all data,
    even NaN. For example, @-\@nAn\@(This_Is_Not_17)@ is a valid representation for NaN in base 17.
 -}
readRounded :: RoundMode -> Precision -> Int -> ReadS Rounded
readRounded  r p (I# i) str = [(Rounded s e l, unpackCString# a)] where
  (# s, e, l, a, _ #) = mpfrFromStr# (mode# r) (prec# p) i (byteArrayContents# (packCString# str)) 

{-| Returns the value of op with given Precision and rounded with RoundMode. -}
set :: RoundMode -> Precision -> Rounded -> Rounded
set = unary mpfrFromMpfr#

{-| Positive infinity. -}
posInf :: Rounded
posInf = Rounded s (-0x8000000000000000# +# 3#) l
  where  (!Rounded s _ l) = zero

{-| Negative infinity. -}
negInf :: Rounded
negInf = Rounded s (-0x8000000000000000# +# 3#) l
  where (!Rounded s _ l) = fromInt Near 2 (-1)

{-| Not a number. -}
nan :: Rounded
nan = Rounded s (-0x8000000000000000# +# 2#) l
  where (!Rounded s _ l) = zero

{-| Positive zero. -}
zero :: Rounded
zero = fromInt Near 2 0

{-| Negative zero. -}
negZero :: Rounded
negZero = neg Down 2 zero

{-| Constructs 'Rounded' from 'Int' with given Precision and rounded with RoundMode.
  Note that the input 0 is converted to +0 regardless of the rounding mode. -}
fromInt :: RoundMode -> Precision -> Int -> Rounded
fromInt r p (I# i#) = Rounded s e l where
    (# s, e, l #) = mpfrFromInt# (mode# r) (prec# p) i#

{-| Constructs 'Rounded' from 'Integer' with given Precision and rounded with RoundMode.
  Note that the input 0 is converted to +0 regardless of the rounding mode. -}
fromIntegerA :: RoundMode -> Precision -> Integer -> Rounded
fromIntegerA r p (S# i) = Rounded s e l where
    (# s, e, l #) = mpfrFromInt# (mode# r) (prec# p) i
fromIntegerA r p (J# i xs) = Rounded s e l where
    (# s, e, l #) = mpfrFromInteger# (mode# r) (prec# p) i xs

--fromWord :: RoundMode -> Precision -> GHC.Types.Word -> Rounded

{-| Constructs 'Rounded' from 'Double' with given Precision and rounded with RoundMode.
  If the system does not support the IEEE 754 standard this function might not preserve the signed zeros.-}
fromDouble :: RoundMode -> Precision -> Double -> Rounded
fromDouble r p (D# d) = Rounded s e l where
    (# s, e, l #) = mfprFromDouble# (mode# r) (prec# p) d

toInt# :: Integer -> (# Int#, ByteArray# #)
toInt# (S# x#) = int2Integer# x#
toInt# (J# x# xs#) =  (# x#, xs# #)

{-| Constructs 'Rounded' from 'Rational' with given Precision and rounded with RoundMode.
  Note that the input 0 is converted to +0 regardless of the rounding mode.
  Function might fail if the numerator (or the denominator) can not be represented as a Rounded.-}
fromRationalA :: RoundMode -> Precision -> Rational -> Rounded
fromRationalA r p rat = Rounded s e l where
    !(# n, ns #) = toInt# $ numerator rat
    !(# d, ds #) = toInt# $ denominator rat
    (# s, e, l #) = mpfrFromRational# (mode# r) (prec# p) n ns d ds

{-| Constructs 'Rounded' from 'Integer' multiplied by two to the power 'Exp'
   with given Precision and rounded with RoundMode.
    Note that the input 0 is converted to +0. -} 
fromInteger2Exp :: RoundMode -> Precision -> Integer -> Exp -> Rounded 
fromInteger2Exp r p i (I64# ex) = Rounded s e l where
  !(# n, ns #) = toInt# i
  (# s, e, l #) = mpfrEncode# (mode# r) (prec# p) ex n ns

{-
int2i :: RoundMode -> Precision -> Int -> Int -> Rounded
int2i_ :: RoundMode -> Precision -> Int -> Int -> (Rounded, Int)
int2w :: RoundMode -> Precision -> GHC.Types.Word -> Int -> Rounded
int2w_ :: RoundMode -> Precision -> GHC.Types.Word -> Int -> (Rounded, Int)
-}

{- 5.4 Conversion Functions -}

{-
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
toDouble2Exp r (Rounded sp e l) = (D# d, I# ex)
    where (# d, ex #) = mpfrGetDouble2Exp# (mode# r) sp e l

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

toStringHex, toStringBin, toStringSci, toStringFix, toString :: RoundMode -> Int -> Rounded -> String
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

{-| Returns __/@op1@ times 2 raised to @op2@/__. -}
mul2i :: Rounded -> Int -> Rounded
mul2i (Rounded s e l) (I# i) = Rounded s (e +# i) l

{- | Returns __/@op1@ divided by 2 raised to @op2@/__. -}
div2i :: Rounded -> Int -> Rounded
div2i (Rounded s e l) (I# i) = Rounded s (e -# i) l



foreign import prim "mpfr_cmm_root" mpfrRoot#
  :: CRounding# -> CPrecision# -> Int# -> CSignPrec# -> CExp# -> ByteArray# -> RoundedOut#

{-| Returns __/the kth root of @op@/__ with given Precision and rounded with RoundMode.
    For k odd (resp. even) and op negative (including -Inf), returns a negative number (resp. NaN).
    The kth root of -0 is defined to be -0, whatever the parity of k. -}
root :: RoundMode -> Precision -> Rounded -> Int -> Rounded
root r p  (Rounded s e l) (I# x) = Rounded s' e' l' where
    (# s', e', l' #) = mpfrRoot# (mode# r) (prec# p) x s e l

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

foreign import prim "mpfr_cmm_cmpabs" mpfrCmpAbs# :: CSignPrec# -> CExp# -> ByteArray#
                       -> CSignPrec# -> CExp# -> ByteArray# -> Int#

#include "MPFR/comparison.h"

cmp :: Rounded -> Rounded -> Maybe Ordering
cmp a b | unordered a b  = Nothing
cmp (Rounded s e l) (Rounded s' e' l') = Just (compare (fromIntegral (I# (mpfrCmp# s e l s' e' l'))) (0 :: Int32)) 

cmpAbs :: Rounded -> Rounded -> Maybe Ordering
cmpAbs a b | unordered a b  = Nothing
cmpAbs (Rounded s e l) (Rounded s' e' l') = Just (compare (fromIntegral (I# (mpfrCmpAbs# s e l s' e' l'))) (0 :: Int32)) 


instance Eq Rounded where
  (==) = equal
  (/=) = notEqual

instance Ord Rounded where
  compare (Rounded s e l) (Rounded s' e' l') = compare (fromIntegral (I# (mpfrCmp# s e l s' e' l'))) (0 :: Int32) -- TODO opt
  (<=) = lessEq
  (>=) = greaterEq
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

TEST(sgn#,sgn)

sgn :: Rounded -> Maybe Int
sgn r | isNaN r = Nothing
sgn (Rounded s e l) = Just $ I# (mpfrsgn# s e l)

{-
cmpd :: Rounded -> Double -> Maybe Ordering
cmpi :: Rounded -> Int -> Maybe Ordering
cmpw :: Rounded -> GHC.Types.Word -> Maybe Ordering
cmp2i :: Rounded -> Int -> Exp -> Maybe Ordering
cmp2w :: Rounded -> GHC.Types.Word -> Exp -> Maybe Ordering
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

foreign import prim "mpfr_cmm_lgamma" mpfrLGamma#
  :: CRounding# -> CPrecision# -> CSignPrec# -> CExp# -> ByteArray# -> RoundedOut_#

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

lgamma :: RoundMode -> Precision -> Rounded -> (Int32, Rounded)
lgamma r p (Rounded s e l) = (fromIntegral (I# i), Rounded s' e' l') where
    (# s', e', l', i #) = mpfrLGamma# (mode# r) (prec# p) s e l


{- 5.10 Integer and Remainder related functions -}

#include "MPFR/integer.h"

foreign import prim "mpfr_cmm_remquo" mpfrRemquo#
  :: CRounding# -> CPrecision# -> CSignPrec# -> CExp# -> ByteArray# -> CSignPrec# -> CExp# -> ByteArray# -> RoundedOut_#

remquo :: RoundMode -> Precision -> Rounded -> Rounded -> (Int, Rounded)
remquo r p (Rounded s e l) (Rounded s2 e2 l2) = (I# i, Rounded s' e' l') where
    (# s', e', l', i #) = mpfrRemquo# (mode# r) (prec# p) s e l s2 e2 l2



{- 5.11 Rounding related functions -}
{-
maxPrec :: Rounded -> Rounded -> Precision
-}

{- 5.12 Misc functions-}

type Inplace = CSignPrec# -> CExp# -> ByteArray# -> (# CSignPrec#, CExp#, ByteArray# #)

foreign import prim "mpfr_cmm_nextabove" mpfrNextAbove# :: Inplace
foreign import prim "mpfr_cmm_nextbelow" mpfrNextBelow# :: Inplace

inplace :: Inplace -> Rounded -> Rounded
inplace f (Rounded s e l) = Rounded s' e' l'
  where (# s', e', l' #) = f s e l
{-# INLINE inplace #-}

nextAbove, nextBelow :: Rounded -> Rounded
nextAbove = inplace mpfrNextAbove#
nextBelow = inplace mpfrNextBelow#


type Inplace2 = CSignPrec# -> CExp# -> ByteArray# -> CSignPrec# -> CExp# -> ByteArray# -> (# CSignPrec#, CExp#, ByteArray# #)

foreign import prim "mpfr_cmm_nexttoward" mpfrNextToward# :: Inplace2
foreign import prim "mpfr_cmm_copysign" mpfrCopySign# :: Inplace2

inplace2 :: Inplace2 -> Rounded -> Rounded -> Rounded
inplace2 f (Rounded s e l) (Rounded s2 e2 l2) = Rounded s' e' l'
  where (# s', e', l' #) = f s e l s2 e2 l2
{-# INLINE inplace2 #-}

nextToward :: Rounded -> Rounded -> Rounded
nextToward = inplace2 mpfrNextToward#

copySign :: Rounded -> Rounded -> Rounded
copySign = inplace2 mpfrCopySign#

getExp :: Rounded -> Exp
getExp (Rounded _ e# _) = I64# e#

