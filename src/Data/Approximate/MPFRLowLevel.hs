{- -# LANGUAGE CPP #-}
{- -# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-} -- foreign import prim
{-# LANGUAGE MagicHash #-} -- postfix # on identifiers
{-# LANGUAGE UnboxedTuples #-} -- (#  #)
{-# LANGUAGE UnliftedFFITypes #-}  -- argument type of foreign import
{-# LANGUAGE BangPatterns #-}  -- !

module Data.Approximate.MPFRLowLevel (
  RoundMode(..), Precision, Rounded,
  getPrec,
  set,
  posInf, negInf, zero, naN,
  fromInt, fromIntegerA, fromDouble, fromRationalA,
  add, sub, mul, mul2i, sqr, div, pow, neg, sqrt,
  exp, log, sin, cos, tan, asin, acos, atan,
  pi,
  isNaN, isInfinite, isZero,
  getExp,
  toRationalA, toDoubleA
) where
import Prelude hiding (isNaN, isInfinite, div, sqrt, exp, log, sin, cos, tan, asin, acos, atan, pi)
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
import Data.Approximate.MPFR.Special

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

naN = Rounded s (-0x8000000000000000# +# 2#) l
  where (!Rounded s _ l) = zero

zero :: Rounded
zero = fromInt Near 2 0

--negZero :: Rounded
--negZero = negate zero

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
toString :: GHC.Types.Word -> Rounded -> String
toStringExp :: GHC.Types.Word -> Rounded -> String
toWord :: RoundMode -> Rounded -> GHC.Types.Word
-}

foreign import prim "mpfr_cmm_get_z_2exp" mpfrDecode#
  :: CSignPrec# -> CExp# -> ByteArray# -> (# CExp#, Int#, ByteArray# #)

decodeFloat' :: Rounded -> (Integer, Int)
decodeFloat' (Rounded sp e l) = case mpfrDecode# sp e l of (# i, s, d #) -> (J# s d, I# i)

toRationalA :: Rounded -> Rational
toRationalA r
   | e > 0     = fromIntegral (s `shiftL` e)
   | otherwise = s % (1 `shiftL` negate e)
   where (s, e) = decodeFloat' r

foreign import prim "mpfr_cmm_get_d" mpfrGetDouble#
   :: CRounding# ->
      CSignPrec# -> CExp# -> ByteArray# ->
      Double#

toDoubleA :: RoundMode -> Rounded -> Double
toDoubleA r (Rounded sp e l) =
    let d = mpfrGetDouble# (mode# r) sp e l in D# d

foreign import prim "mpfr_cmm_get_str" mpfrGetStr#
  :: CRounding# -> Int# -> Int# ->
     CSignPrec# -> CExp# -> ByteArray# ->
     (# Int#, ByteArray# #)

mpfrToString :: RoundMode -> Int -> Int -> Rounded -> (String, Exp)
mpfrToString r (I# n) (I# d) (Rounded s e l) = (unpackCString# (byteArrayContents# str), I64# i) where
   (# i, str #) = mpfrGetStr# (mode# r) n d s e l

-- | Output a string in base 10 rounded to Near in exponential form.
toStringExp       :: Int -- ^ number of digits
                  -> Rounded -> String
toStringExp dec d | isInfixOf "NaN" ss = "NaN"
                  | isInfixOf "Inf" ss = s ++ "Infinity"
                  | isZero d = "0"
                  | e > 0              =
                      s ++ if Prelude.floor prec <= dec
                           then
                               take e ss ++
                               let bt = backtrim (drop e ss)
                               in if null bt
                                  then ""
                                  else '.' : bt
                           else head ss : '.' :
                                let bt = (backtrim . tail) ss
                                in (if null bt then "0" else bt)
                                   ++ "e" ++ show (pred e)
                  | otherwise =
                      s ++ (head ss : '.' :
                               (let bt = (backtrim . tail) ss in
                                if null bt then "0"
                                else bt )
                               ++ "e" ++ show (pred e))
                    where (str, e') = mpfrToString Near n 10 d
                          e = fromIntegral e'
                          n        = max dec 5
                          (s, ss) = case head str of
                                      '-' -> ("-", tail str)
                                      _   -> ("" , str)
                          backtrim = reverse . dropWhile (== '0') . reverse
                          prec = logBase 10 2 * fromIntegral (getExp d) :: Double

-- | Output a string in base 10 rounded to Near. The difference from @toStringExp@ is that
-- it won't output in exponential form if it is sensible to do so.
toString       :: Int -> Rounded -> String
toString dec d | isInfixOf "NaN" ss = "NaN"
               | isInfixOf "Inf" ss = s ++ "Infinity"
               | otherwise          =
                   s ++ case compare 0 e of
                          LT -> take e ss ++
                                (let bt = all (== '0') (drop e ss)
                                 in if bt then "" else '.' : drop e ss)
                                ++ (if fromIntegral n - e < 0
                                    then 'e' : show (e - fromIntegral n)
                                    else "")
                          GT -> let ee = fromIntegral dec + e in
                                if ee <= 0 then "0" else
                                   head ss : '.' : (backtrim . tail . take ee) ss
                                            ++ "e" ++ show (pred e)
                          EQ -> "0." ++ let bt = all (== '0') ss
                                        in if bt then "0" else ss
                  where (str, e') = mpfrToString Near n 10 d
                        n        = max dec 5
                        e = fromIntegral e'
                        (s, ss) = case head str of
                                    '-' -> ("-", tail str)
                                    _   -> ("" , str)
                        backtrim = reverse . dropWhile (== '0') . reverse
{-


fitsSInt :: RoundMode -> Rounded -> Bool
fitsSLong :: RoundMode -> Rounded -> Bool
fitsSShort :: RoundMode -> Rounded -> Bool
fitsUInt :: RoundMode -> Rounded -> Bool
fitsULong :: RoundMode -> Rounded -> Bool
fitsUShort :: RoundMode -> Rounded -> Bool

-}
instance Show Rounded where
    show = toStringExp 16

{- 5.5 Basic Arithmetic Functions -}

foreign import prim "mpfr_cmm_add" mpfrAdd# :: Binary
foreign import prim "mpfr_cmm_sub" mpfrSub# :: Binary
foreign import prim "mpfr_cmm_mul" mpfrMul# :: Binary
foreign import prim "mpfr_cmm_div" mpfrDiv# :: Binary
foreign import prim "mpfr_cmm_neg" mpfrNeg# :: Unary
foreign import prim "mpfr_cmm_sqr" mpfrSqr# :: Unary
foreign import prim "mpfr_cmm_sqrt" mpfrSqrt# :: Unary
foreign import prim "mpfr_cmm_pow" mpfrPow# :: Binary

add :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded
add = binary mpfrAdd#

{-
add_ :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)
addd :: RoundMode -> Precision -> Rounded -> Double -> Rounded
addd_ :: RoundMode -> Precision -> Rounded -> Double -> (Rounded, Int)
addi :: RoundMode -> Precision -> Rounded -> Int -> Rounded
addi_ :: RoundMode -> Precision -> Rounded -> Int -> (Rounded, Int)
addw :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> Rounded
addw_ :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> (Rounded, Int)
-}

sub :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded
sub = binary mpfrSub#

{-
sub_ :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)
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
ara
        wsub :: RoundMode -> Precision -> GHC.Types.Word -> Rounded -> Rounded
wsub_ :: RoundMode -> Precision -> GHC.Types.Word -> Rounded -> (Rounded, Int)
-}

mul :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded
mul = binary mpfrMul#


mul2i :: RoundMode -> Precision -> Rounded -> Int -> Rounded
mul2i r p (Rounded s e l) (I# i) = Rounded s (e +# i) l

{-
mul2i_ :: RoundMode -> Precision -> Rounded -> Int -> (Rounded, Int)
mul_ :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)
muld :: RoundMode -> Precision -> Rounded -> Double -> Rounded
muld_ :: RoundMode -> Precision -> Rounded -> Double -> (Rounded, Int)
muli :: RoundMode -> Precision -> Rounded -> Int -> Rounded
muli_ :: RoundMode -> Precision -> Rounded -> Int -> (Rounded, Int)
mulw :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> Rounded
mulw_ :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> (Rounded, Int)
mul2w :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> Rounded
mul2w_ :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> (Rounded, Int)
-}

sqr :: RoundMode -> Precision -> Rounded -> Rounded
sqr = unary mpfrSqr#
{-
sqr_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
-}

div :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded
div = binary mpfrDiv#
{-
div_ :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)
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

sqrt :: RoundMode -> Precision -> Rounded -> Rounded
sqrt = unary mpfrSqrt#
--sqrt_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
{-
recSqrt :: RoundMode -> Precision -> Rounded -> Rounded
recSqrt_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)

sqrtw :: RoundMode -> Precision -> GHC.Types.Word -> Rounded
sqrtw_ :: RoundMode -> Precision -> GHC.Types.Word -> (Rounded, Int)

cbrt :: RoundMode -> Precision -> Rounded -> Rounded
cbrt_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
root :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> Rounded
root_ ::
  RoundMode -> Precision -> Rounded -> GHC.Types.Word -> (Rounded, Int)
--}


pow :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded
pow = binary mpfrPow#

{-
pow_ :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)
powi :: RoundMode -> Precision -> Rounded -> Int -> Rounded
powi_ :: RoundMode -> Precision -> Rounded -> Int -> (Rounded, Int)
poww :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> Rounded
poww_ :: RoundMode -> Precision -> Rounded -> GHC.Types.Word -> (Rounded, Int)
-}

neg :: RoundMode -> Precision -> Rounded -> Rounded
neg = unary mpfrNeg#

{- neg_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int) --}
{-
absD :: RoundMode -> Precision -> Rounded -> Rounded
absD_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
-}

{-
dim :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded
dim_ :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)

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

foreign import prim "mpfr_cmm_min" mpfrMin# :: Binary
foreign import prim "mpfr_cmm_max" mpfrMax# :: Binary

foreign import prim "mpfr_cmm_equal_p"         mpfrEqual#        :: Comparison
foreign import prim "mpfr_cmm_lessgreater_p"   mpfrNotEqual#     :: Comparison
foreign import prim "mpfr_cmm_less_p"          mpfrLess#         :: Comparison
foreign import prim "mpfr_cmm_greater_p"       mpfrGreater#      :: Comparison
foreign import prim "mpfr_cmm_lessequal_p"     mpfrLessEqual#    :: Comparison
foreign import prim "mpfr_cmm_greaterequal_p"  mpfrGreaterEqual# :: Comparison

instance Eq Rounded where
  (==) = cmp mpfrEqual#
  (/=) = cmp mpfrNotEqual#

instance Ord Rounded where
  compare (Rounded s e l) (Rounded s' e' l') = compare (fromIntegral (I# (mpfrCmp# s e l s' e' l'))) (0 :: Int32) -- TODO opt
  (<=) = cmp mpfrLessEqual#
  (>=) = cmp mpfrGreaterEqual#
  (<) = cmp mpfrLess#
  (>) = cmp mpfrGreater#
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

greater :: Rounded -> Rounded -> Bool
greatereq :: Rounded -> Rounded -> Bool

less :: Rounded -> Rounded -> Bool
lesseq :: Rounded -> Rounded -> Bool
equal :: Rounded -> Rounded -> Bool

lessgreater :: Rounded -> Rounded -> Maybe Bool
unordered :: Rounded -> Rounded -> Maybe Bool

isInteger :: Rounded -> Bool
isNumber :: Rounded -> Bool
sgn :: Rounded -> Maybe Int

maxD :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded
maxD_ :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)
minD :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded
minD_ :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)

--}


{- 5.10 Integer and Remainder related functions -}

{-
rint :: RoundMode -> Precision -> Rounded -> Rounded
ceil :: Precision -> Rounded -> Rounded
ceil_ :: Precision -> Rounded -> (Rounded, Int)
floor :: Precision -> Rounded -> Rounded
floor_ :: Precision -> Rounded -> (Rounded, Int)
round :: Precision -> Rounded -> Rounded
round_ :: Precision -> Rounded -> (Rounded, Int)
trunc :: Precision -> Rounded -> Rounded
trunc_ :: Precision -> Rounded -> (Rounded, Int)

-}

{-
rint_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
rintCeil :: RoundMode -> Precision -> Rounded -> Rounded
rintCeil_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
rintFloor :: RoundMode -> Precision -> Rounded -> Rounded
rintFloor_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
rintRound :: RoundMode -> Precision -> Rounded -> Rounded
rintRound_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
rintTrunc :: RoundMode -> Precision -> Rounded -> Rounded
rintTrunc_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)

fmod :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded
fmod_ :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)

modf :: RoundMode -> Precision -> Precision -> Rounded -> (Rounded, Rounded)
modf_ ::
  RoundMode -> Precision -> Precision -> Rounded -> (Rounded, Rounded, Int)
frac :: RoundMode -> Precision -> Rounded -> Rounded
frac_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
remainder :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded
remainder_ :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)
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
