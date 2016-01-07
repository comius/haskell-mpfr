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
exp = unary mpfrExp#

foreign import prim "mpfr_cmm_log" mpfrLog# :: Unary
log = unary mpfrLog#

foreign import prim "mpfr_cmm_sin" mpfrSin# :: Unary
sin = unary mpfrSin#

foreign import prim "mpfr_cmm_cos" mpfrCos# :: Unary
cos = unary mpfrCos#

foreign import prim "mpfr_cmm_tan" mpfrTan# :: Unary
tan = unary mpfrTan#

foreign import prim "mpfr_cmm_acos" mpfrASin# :: Unary
asin = unary mpfrASin#

foreign import prim "mpfr_cmm_asin" mpfrACos# :: Unary
acos = unary mpfrACos#

foreign import prim "mpfr_cmm_atan" mpfrATan# :: Unary
atan = unary mpfrATan#

foreign import prim "mpfr_cmm_log10" mpfrLog10# :: Unary
log10 = unary mpfrLog10#

foreign import prim "mpfr_cmm_log2" mpfrLog2# :: Unary
log2 = unary mpfrLog2#

foreign import prim "mpfr_cmm_exp10" mpfrExp10# :: Unary
exp10 = unary mpfrExp10#

foreign import prim "mpfr_cmm_exp2" mpfrExp2# :: Unary
exp2 = unary mpfrExp2#

foreign import prim "mpfr_cmm_sec" mpfrSec# :: Unary
sec = unary mpfrSec#

foreign import prim "mpfr_cmm_csc" mpfrCsc# :: Unary
csc = unary mpfrCsc#

foreign import prim "mpfr_cmm_cot" mpfrCot# :: Unary
cot = unary mpfrCot#

foreign import prim "mpfr_cmm_cosh" mpfrCosh# :: Unary
cosh = unary mpfrCosh#

foreign import prim "mpfr_cmm_acosh" mpfrACosh# :: Unary
acosh = unary mpfrACosh#

foreign import prim "mpfr_cmm_asinh" mpfrASinh# :: Unary
asinh = unary mpfrASinh#

foreign import prim "mpfr_cmm_sinh" mpfrSinh# :: Unary
sinh = unary mpfrSinh#

foreign import prim "mpfr_cmm_tanh" mpfrTanh# :: Unary
tanh = unary mpfrTanh#

foreign import prim "mpfr_cmm_sech" mpfrSech# :: Unary
sech = unary mpfrSech#

foreign import prim "mpfr_cmm_csch" mpfrCsch# :: Unary
csch = unary mpfrCsch#

foreign import prim "mpfr_cmm_coth" mpfrCoth# :: Unary
coth = unary mpfrCoth#

foreign import prim "mpfr_cmm_atanh" mpfrATanh# :: Unary
atanh = unary mpfrATanh#

foreign import prim "mpfr_cmm_expm1" mpfrExpm1# :: Unary
expm1 = unary mpfrExpm1#

foreign import prim "mpfr_cmm_eint" mpfreint# :: Unary
eint = unary mpfreint#

foreign import prim "mpfr_cmm_li2" mpfrli2# :: Unary
li2 = unary mpfrli2#

foreign import prim "mpfr_cmm_gamma" mpfrgamma# :: Unary
gamma = unary mpfrgamma#

foreign import prim "mpfr_cmm_lngamma" mpfrlngamma# :: Unary
lngamma = unary mpfrlngamma#

foreign import prim "mpfr_cmm_lgamma" mpfrlgamma# :: Unary
lgamma = unary mpfrlgamma#

foreign import prim "mpfr_cmm_zeta" mpfrzeta# :: Unary
zeta = unary mpfrzeta#

foreign import prim "mpfr_cmm_zetaw" mpfrzetaw# :: Unary
zetaw = unary mpfrzetaw#

foreign import prim "mpfr_cmm_erf" mpfrerf# :: Unary
erf = unary mpfrerf#

foreign import prim "mpfr_cmm_erfc" mpfrerfc# :: Unary
erfc = unary mpfrerfc#

foreign import prim "mpfr_cmm_j0" mpfrj0# :: Unary
j0 = unary mpfrj0#

foreign import prim "mpfr_cmm_j1" mpfrj1# :: Unary
j1 = unary mpfrj1#

foreign import prim "mpfr_cmm_y0" mpfry0# :: Unary
y0 = unary mpfry0#

foreign import prim "mpfr_cmm_y1" mpfry1# :: Unary
y1 = unary mpfry1#

foreign import prim "mpfr_cmm_log1p" mpfrlog1p# :: Unary
log1p = unary mpfrlog1p#

{-
catalan :: RoundMode -> Precision -> Rounded
catalan_ :: RoundMode -> Precision -> (Rounded, Int)
pi :: RoundMode -> Precision -> Rounded
pi_ :: RoundMode -> Precision -> (Rounded, Int)
euler :: RoundMode -> Precision -> Rounded
euler_ :: RoundMode -> Precision -> (Rounded, Int)

sincos :: RoundMode -> Precision -> Precision -> Rounded -> (Rounded, Rounded)
sincos_ :: RoundMode -> Precision -> Precision -> Rounded -> (Rounded, Rounded, Int)

atan2 ::
  RoundMode -> Precision -> Rounded -> Rounded -> Rounded
atan2_ :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)

sinhcosh ::
  RoundMode -> Precision -> Precision -> Rounded -> (Rounded, Rounded)
sinhcosh_ ::
  RoundMode -> Precision -> Precision -> Rounded -> (Rounded, Rounded, Int)

facw :: RoundMode -> Precision -> GHC.Types.Word -> Rounded
facw_ :: RoundMode -> Precision -> GHC.Types.Word -> (Rounded, Int)

zetaw :: RoundMode -> Precision -> GHC.Types.Word -> Rounded
zetaw_ :: RoundMode -> Precision -> GHC.Types.Word -> (Rounded, Int)

jn :: RoundMode -> Precision -> Int -> Rounded -> Rounded
jn_ :: RoundMode -> Precision -> Int -> Rounded -> (Rounded, Int)

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

-}
