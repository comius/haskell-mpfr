{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-} -- foreign import prim
{-# LANGUAGE MagicHash #-} -- postfix # on identifiers
{-# LANGUAGE UnboxedTuples #-} -- (#  #)
{-# LANGUAGE UnliftedFFITypes #-}  -- argument type of foreign import
{-# LANGUAGE BangPatterns #-}
        -- |

module Data.Approximate.MPFR.Types (
   constf, unary, unary2, unary_,binary,binary_,ternary, cmp, Rounded (..), RoundMode (..), Precision, Const, Unary, Unary2, Binary,Ternary, Comparison, CExp#, CPrec#, CSignPrec#, mode#, prec#, RoundedOut#, CRounding#, Exp, CPrecision#, getPrec
) where
import Prelude hiding (isNaN, isInfinite, div, sqrt, exp, log, sin, cos, tan, asin, acos, atan)
import Data.Bits
import GHC.Int -- Int32#
import GHC.Prim -- Int#, ByteArray#,
import GHC.Types -- Word

{- Basic data -}
type CPrec#      = Int#
type CSignPrec#  = Int#
type CPrecision# = Int#
type CExp#       = Int#
type CRounding#  = Int#

data Rounded = Rounded
  { roundedSignPrec :: CSignPrec# -- Sign# << 64/32 | Precision#
  , roundedExp      :: CExp#
  , roundedLimbs    :: ByteArray#
  }

{- TODO is squeezing sign and prec together really faster? -}

{- 4.2 Nomenclature and Types - precision -}

{-
gmp.h:
#if defined (_CRAY) && ! defined (_CRAYMPP)
/* plain `int' is much faster (48 bits) */
#define __GMP_MP_SIZE_T_INT     1
typedef int                     mp_size_t;
typedef int                     mp_exp_t;
#else
#define __GMP_MP_SIZE_T_INT     0            <---
typedef long int                mp_size_t
typedef long int                mp_exp_t;
#endif

mpfr.h:
# if __GMP_MP_SIZE_T_INT == 1
#  define _MPFR_PREC_FORMAT 2
# else
#  define _MPFR_PREC_FORMAT 3  <---
# endif

#elif _MPFR_PREC_FORMAT == 3
typedef long  mpfr_prec_t;

-}
type Precision = Int

prec# :: Precision -> Int#
prec# (I# i#) = i#

prec_bit :: Int
prec_bit
  | b63 == 0  = b31
  | otherwise = b63
  where b63 = bit 63
        b31 = bit 31

getPrec :: Rounded -> Precision
getPrec (Rounded s _ _) = (I# s) .&. complement prec_bit

{- 4.4 Rounding Modes -}

{- Haskel model of MPFR precision

Definition of rounding modes (DON'T USE MPFR_RNDNA!).

typedef enum {
  MPFR_RNDN=0,  /* round to nearest, with ties to even */
  MPFR_RNDZ,    /* round toward zero */
  MPFR_RNDU,    /* round toward +Inf */
  MPFR_RNDD,    /* round toward -Inf */
  MPFR_RNDA,    /* round away from zero */
  MPFR_RNDF,    /* faithful rounding (not implemented yet) */
  MPFR_RNDNA=-1 /* round to nearest, with ties away from zero (mpfr_round) */
} mpfr_rnd_t;
-}
data RoundMode
  = Near
  | Zero
  | Up
  | Down
  | AwayFromZero

instance Enum RoundMode where
  toEnum 0 = Near
  toEnum 1 = Zero
  toEnum 2 = Up
  toEnum 3 = Down
  toEnum 4 = AwayFromZero
  toEnum 5 = error "RoundMode: Not implemented"
  toEnum (-1) = error "RoundMode: Don't use!"
  toEnum _ = error "RoundMode: Unknown"

  fromEnum Near = 0
  fromEnum Zero = 1
  fromEnum Up = 2
  fromEnum Down = 3
  fromEnum AwayFromZero = 4

mode# :: RoundMode -> Int#
mode# r = case fromEnum r of
  I# i# -> i#

{- General types, method signatures -}


type Exp = GHC.Int.Int64

type RoundedOut# = (# CSignPrec#, CExp#, ByteArray# #)
type RoundedOut_# = (# CSignPrec#, CExp#, ByteArray#, Int# #)

type RoundedOut2# = (# CSignPrec#, CExp#, ByteArray#, CSignPrec#, CExp#, ByteArray# #)

type Const
  = CRounding# -> CPrec# -> RoundedOut#

type Unary
  = CRounding# -> CPrec# ->
    CSignPrec# -> CExp# -> ByteArray# -> RoundedOut#

type Unary2
  = CRounding# -> CPrec# ->
    CSignPrec# -> CExp# -> ByteArray# -> RoundedOut2#

type Unary_
  = CRounding# -> CPrec# ->
    CSignPrec# -> CExp# -> ByteArray# -> RoundedOut_#

type Binary
  = CRounding# -> CPrec# ->
    CSignPrec# -> CExp# -> ByteArray# ->
    CSignPrec# -> CExp# -> ByteArray# -> RoundedOut#

type Ternary
  = CRounding# -> CPrec# ->
    CSignPrec# -> CExp# -> ByteArray# ->
    CSignPrec# -> CExp# -> ByteArray# ->
    CSignPrec# -> CExp# -> ByteArray# -> RoundedOut#

type Binary_
  = CRounding# -> CPrec# ->
    CSignPrec# -> CExp# -> ByteArray# ->
    CSignPrec# -> CExp# -> ByteArray# -> RoundedOut_#


type Comparison
  = CSignPrec# -> CExp# -> ByteArray# ->
    CSignPrec# -> CExp# -> ByteArray# ->
    Int#

constf :: Const -> RoundMode -> Precision -> Rounded
constf f r p = Rounded s' e' l' where
    (# s', e', l' #) = f (mode# r) (prec# p)
{-# INLINE constf #-}


unary :: Unary -> RoundMode -> Precision -> Rounded -> Rounded
unary f r p (Rounded s e l) = Rounded s' e' l' where
    (# s', e', l' #) = f (mode# r) (prec# p) s e l
{-# INLINE unary #-}

unary2 :: Unary2 -> RoundMode -> Precision -> Rounded -> (Rounded, Rounded)
unary2 f r p (Rounded s e l) = (Rounded s1' e1' l1', Rounded s2' e2' l2') where
    (# s1', e1', l1', s2', e2', l2' #) = f (mode# r) (prec# p) s e l
{-# INLINE unary2 #-}

unary_ :: Unary_ -> RoundMode -> Precision -> Rounded -> ( Rounded, Int )
unary_ f r p (Rounded s e l) = ( Rounded s' e' l', I# t) where
    (# s', e', l', t #) = f (mode# r) (prec# p) s e l
{-# INLINE unary_ #-}


binary :: Binary -> RoundMode -> Precision -> Rounded -> Rounded -> Rounded
binary f r p (Rounded s e l) (Rounded s' e' l') = Rounded s'' e'' l'' where
    (# s'', e'', l'' #) = f (mode# r) (prec# p) s e l s' e' l'
{-# INLINE binary #-}

binary_ :: Binary_ -> RoundMode -> Precision -> Rounded -> Rounded -> (Rounded, Int)
binary_ f r p (Rounded s e l) (Rounded s' e' l') = (Rounded s'' e'' l'', I# t) where
    (# s'', e'', l'', t #) = f (mode# r) (prec# p) s e l s' e' l'
{-# INLINE binary_ #-}

ternary :: Ternary -> RoundMode -> Precision -> Rounded -> Rounded -> Rounded -> Rounded
ternary f r p (Rounded s e l) (Rounded s' e' l') (Rounded s'' e'' l'') = Rounded s''' e''' l''' where
    (# s''', e''', l''' #) = f (mode# r) (prec# p) s e l s' e' l' s'' e'' l''
{-# INLINE ternary #-}


cmp :: Comparison -> Rounded -> Rounded -> Bool
cmp f (Rounded s e l) (Rounded s' e' l') = I# (f s e l s' e' l') /= 0
{-# INLINE cmp #-}
