{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-} -- foreign import prim
{-# LANGUAGE MagicHash #-} -- postfix # on identifiers
{-# LANGUAGE UnboxedTuples #-} -- (#  #)
{-# LANGUAGE UnliftedFFITypes #-}  -- argument type of foreign import
{-# LANGUAGE BangPatterns #-}  -- !
{-# LANGUAGE QuasiQuotes #-}

module Data.Approximate.MPFR.Special where

import Data.Approximate.MPFR.Types

{- 5.7 Special Functions -}

foreign import prim "mpfr_cmm_exp" mpfrExp# :: Unary
foreign import prim "mpfr_cmm_log" mpfrLog# :: Unary
foreign import prim "mpfr_cmm_sin" mpfrSin# :: Unary
foreign import prim "mpfr_cmm_cos" mpfrCos# :: Unary
foreign import prim "mpfr_cmm_tan" mpfrTan# :: Unary
foreign import prim "mpfr_cmm_acos" mpfrASin# :: Unary
foreign import prim "mpfr_cmm_asin" mpfrACos# :: Unary
foreign import prim "mpfr_cmm_atan" mpfrATan# :: Unary


--exp :: RoundMode -> Precision -> Rounded -> Rounded
--exp = unary mpfrExp#

log :: RoundMode -> Precision -> Rounded -> Rounded
log = unary mpfrLog#

sin :: RoundMode -> Precision -> Rounded -> Rounded
sin = unary mpfrSin#

cos :: RoundMode -> Precision -> Rounded -> Rounded
cos = unary mpfrCos#

tan :: RoundMode -> Precision -> Rounded -> Rounded
tan = unary mpfrTan#

asin :: RoundMode -> Precision -> Rounded -> Rounded
asin = unary mpfrASin#

acos :: RoundMode -> Precision -> Rounded -> Rounded
acos = unary mpfrACos#

atan :: RoundMode -> Precision -> Rounded -> Rounded
atan = unary mpfrATan#
{-
log_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
log10 :: RoundMode -> Precision -> Rounded -> Rounded
log10_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
log2 :: RoundMode -> Precision -> Rounded -> Rounded
log2_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
catalan :: RoundMode -> Precision -> Rounded
catalan_ :: RoundMode -> Precision -> (Rounded, Int)
pi :: RoundMode -> Precision -> Rounded
pi_ :: RoundMode -> Precision -> (Rounded, Int)
euler :: RoundMode -> Precision -> Rounded
euler_ :: RoundMode -> Precision -> (Rounded, Int)


exp_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
exp10 :: RoundMode -> Precision -> Rounded -> Rounded
exp10_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
exp2 :: RoundMode -> Precision -> Rounded -> Rounded
exp2_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)

cos_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
sin_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
tan_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)

sincos :: RoundMode -> Precision -> Precision -> Rounded -> (Rounded, Rounded)
sincos_ :: RoundMode -> Precision -> Precision -> Rounded -> (Rounded, Rounded, Int)

sec :: RoundMode -> Precision -> Rounded -> Rounded
sec_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)

csc :: RoundMode -> Precision -> Rounded -> Rounded
csc_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
cot :: RoundMode -> Precision -> Rounded -> Rounded
cot_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)

acos_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
asin_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
atan_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
atan2 ::
  RoundMode -> Precision -> Rounded -> Rounded -> Rounded
atan2_ :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)

cosh :: RoundMode -> Precision -> Rounded -> Rounded
cosh_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)


acosh :: RoundMode -> Precision -> Rounded -> Rounded
acosh_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
asinh :: RoundMode -> Precision -> Rounded -> Rounded
asinh_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
sinh :: RoundMode -> Precision -> Rounded -> Rounded
sinh_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
tanh :: RoundMode -> Precision -> Rounded -> Rounded
tanh_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)

sinhcosh ::
  RoundMode -> Precision -> Precision -> Rounded -> (Rounded, Rounded)
sinhcosh_ ::
  RoundMode -> Precision -> Precision -> Rounded -> (Rounded, Rounded, Int)

sech :: RoundMode -> Precision -> Rounded -> Rounded
sech_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
csch :: RoundMode -> Precision -> Rounded -> Rounded
csch_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
coth :: RoundMode -> Precision -> Rounded -> Rounded
coth_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)


atanh :: RoundMode -> Precision -> Rounded -> Rounded
atanh_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)

facw :: RoundMode -> Precision -> GHC.Types.Word -> Rounded
facw_ :: RoundMode -> Precision -> GHC.Types.Word -> (Rounded, Int)

expm1 :: RoundMode -> Precision -> Rounded -> Rounded
expm1_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)

eint :: RoundMode -> Precision -> Rounded -> Rounded
eint_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)


li2 :: RoundMode -> Precision -> Rounded -> Rounded
li2_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)

gamma :: RoundMode -> Precision -> Rounded -> Rounded
gamma_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)

lngamma :: RoundMode -> Precision -> Rounded -> Rounded
lngamma_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)

lgamma :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
lgamma_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int, Int)

zeta :: RoundMode -> Precision -> Rounded -> Rounded
zeta_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
zetaw :: RoundMode -> Precision -> GHC.Types.Word -> Rounded
zetaw_ :: RoundMode -> Precision -> GHC.Types.Word -> (Rounded, Int)

erf :: RoundMode -> Precision -> Rounded -> Rounded
erf_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
erfc :: RoundMode -> Precision -> Rounded -> Rounded
erfc_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)


j0 :: RoundMode -> Precision -> Rounded -> Rounded
j0_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
j1 :: RoundMode -> Precision -> Rounded -> Rounded
j1_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
jn :: RoundMode -> Precision -> Int -> Rounded -> Rounded
jn_ :: RoundMode -> Precision -> Int -> Rounded -> (Rounded, Int)


y0 :: RoundMode -> Precision -> Rounded -> Rounded
y0_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
y1 :: RoundMode -> Precision -> Rounded -> Rounded
y1_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)
yn :: RoundMode -> Precision -> Int -> Rounded -> Rounded
yn_ :: RoundMode -> Precision -> Int -> Rounded -> (Rounded, Int)

fma :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded -> Rounded
fma_ ::
  RoundMode -> Precision -> Rounded -> Rounded -> Rounded -> (Rounded, Int)

fms :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded -> Rounded
fms_ ::
  RoundMode -> Precision -> Rounded -> Rounded -> Rounded -> (Rounded, Int)

agm :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded
agm_ :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)


hypot :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded
hypot_ :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)


log2c :: RoundMode -> Precision -> Rounded
log2c_ :: RoundMode -> Precision -> (Rounded, Int)
log1p :: RoundMode -> Precision -> Rounded -> Rounded
log1p_ :: RoundMode -> Precision -> Rounded -> (Rounded, Int)

-}
