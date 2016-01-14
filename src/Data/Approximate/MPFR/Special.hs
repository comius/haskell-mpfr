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

#define STR(s) #s
#define LONGNAME(name) STR(mpfr_cmm_##name)

#define UNARY(name) \
foreign import prim LONGNAME(name) mpfr##name :: Unary \
name :: RoundMode -> Precision -> Rounded -> Rounded \
name = unary mpfr##name 

#define BINARY(name) \
foreign import prim LONGNAME(name) mpfr##name :: Binary \
name :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded \
name = binary mpfr##name 

#define CONST(name) \
foreign import prim LONGNAME(name) mpfr##name :: Const \
name :: RoundMode -> Precision -> Rounded \
name = constf mpfr##name 

#define UNARY2(name) \
foreign import prim LONGNAME(name) mpfr##name :: Unary2 \
name :: RoundMode -> Precision -> Rounded -> (Rounded, Rounded) \
name = unary2 mpfr##name 


#define TRINARY(name)


#include "special.h"

sincos = sin_cos
sincosh = sinh_cosh
pi=const_pi
euler=const_euler
log2c=const_log2
catalan=const_catalan


{-
facw :: RoundMode -> Precision -> GHC.Types.Word -> Rounded
facw_ :: RoundMode -> Precision -> GHC.Types.Word -> (Rounded, Int)

zetaw :: RoundMode -> Precision -> GHC.Types.Word -> Rounded
zetaw_ :: RoundMode -> Precision -> GHC.Types.Word -> (Rounded, Int)

jn :: RoundMode -> Precision -> Int -> Rounded -> Rounded
jn_ :: RoundMode -> Precision -> Int -> Rounded -> (Rounded, Int)

yn :: RoundMode -> Precision -> Int -> Rounded -> Rounded
yn_ :: RoundMode -> Precision -> Int -> Rounded -> (Rounded, Int)


hypot :: RoundMode -> Precision -> Rounded -> Rounded -> Rounded
hypot_ :: RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)
-}
