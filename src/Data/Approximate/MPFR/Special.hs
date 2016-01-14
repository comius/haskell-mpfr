{-# OPTIONS_GHC -F -pgmF script/cpphs -optF--layout -optF--hashes #-}
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


#define UNARY2(name, long_name) \
foreign import prim #long_name mpfr##name :: Unary \
name :: RoundMode -> Precision -> Rounded -> Rounded \
name = unary mpfr##name 

#define UNARY(name) UNARY2(name,mpfr_cmm_##name) 

#include "special.h"

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

--foreign import prim "mpfr_cmm_zetaw" mpfrzetaw# :: Unary
--zetaw = unary mpfrzetaw#

-}
