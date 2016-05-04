/*DOC
Return @op@ rounded to an integer.
@rint@ rounds to the nearest representable integer in the given direction @rnd@

The returned ternary value is zero when the result is exact, positive when it is
greater than the original value of @op@, and negative when it is smaller.
More precisely, the returned value is 0 when @op@ is a representable integer,
 1 or -1 when @op@ is an integer that is not representable, 2 or -2 when @op@ is
not an integer.

When @op@ is NaN, the NaN flag is set as usual. In the other cases,
the inexact flag is set when result differs from @op@, following
the ISO C99 rule for the @rint@ function. If you want the behavior to
be more like IEEE 754 / ISO TS 18661-1, i.e., the usual behavior where the
round-to-integer function is regarded as any other mathematical function,
you should use one the @rint*@ functions instead (however it is
not possible to round to nearest with the even rounding rule yet).

Note that @round@ is different from @rint@ called with
the rounding to nearest mode (where halfway cases are rounded to an even
integer or significand). Note also that no double rounding is performed; for
instance, 10.5 (1010.1 in binary) is rounded by @rint@ with
rounding to nearest to 12 (1100
in binary) in 2-bit precision, because the two enclosing numbers representable
on two bits are 8 and 12, and the closest is 12.
(If one first rounded to an integer, one would round 10.5 to 10 with
even rounding, and then 10 would be rounded to 8 again with even rounding.) */
UNARY(rint)
/*DOC Return @op@ rounded to an integer.
@ceil@ rounds to to the next higher or equal representable integer.
See @rint@ for special values. */
ROUNDING(ceil)
/*DOC Return @op@ rounded to an integer.
@floor@ rounds to the next lower or equal representable integer.
See @rint@ for special values. */
ROUNDING(floor)
/*DOC Return @op@ rounded to an integer.
@round@ rounds to the nearest representable integer, rounding halfway cases away from zero
(as in the roundTiesToAway mode of IEEE 754-2008).
See @rint@ for special values. */
ROUNDING(round)
/*DOC Return @op@ rounded to an integer.
@trunc@ rounds to the next representable integer toward zero.
See @rint@ for special values. */
ROUNDING(trunc)

/*DOC
Return @op@ rounded to an integer.
@rintCeil@ rounds to the next higher or equal integer.

If the result is not representable, it is rounded in the direction @rnd@.
The returned ternary value is associated with the considered
round-to-integer function (regarded in the same way as any other
mathematical function).


Contrary to @rint@, those functions do perform a double rounding:
first @op@ is rounded to the nearest integer in the direction given by
the function name, then this nearest integer (if not representable) is
rounded in the given direction @rnd@.  Thus these round-to-integer
functions behave more like the other mathematical functions, i.e., the
returned result is the correct rounding of the exact result of the function
in the real numbers.

For example, @rintRound@ with rounding to nearest and a precision
of two bits rounds 6.5 to 7 (halfway cases away from zero), then 7 is
rounded to 8 by the round-even rule, despite the fact that 6 is also
representable on two bits, and is closer to 6.5 than 8. */
UNARY_(rintCeil,rint_ceil)

/*DOC
Return @op@ rounded to an integer.
@rintFloor@ rounds to the next lower or equal integer,

See @rintCeil@ for details.*/
UNARY_(rintFloor,rint_floor)

/*DOC
Return @op@ rounded to an integer.
@rintRound@ rounds to the nearest integer, rounding halfway cases away
from zero.

See @rintCeil@ for details.*/
UNARY_(rintRound,rint_round)

/*DOC Return @op@ rounded to an integer.
@rintTrunc@ rounds to the next integer toward zero.

See @rintCeil@ for details.*/
UNARY_(rintTrunc,rint_trunc)

/*DOC
Return the fractional part of @op@, having the same sign as
@op@, rounded in the direction @rnd@ (unlike in @rint@,
@rnd@ affects only how the exact fractional part is rounded, not how
the fractional part is generated). */
UNARY(frac)

/*DOC Return simultaneously the integral part of @op@ and the fractional part of @op@, 
rounded in the direction @rnd@ with the corresponding precision (equivalent to @trunc@ and @frac@). */ 
UNARY2(modf,modf)

/*DOC Return the value of @x@ - @n@@y@, rounded
according to the direction @rnd@, where @n@ is the integer quotient
of @x@ divided by @y@, defined as follows: @n@ is rounded
toward zero for @fmod@.

Special values are handled as described in Section F.9.7.1 of
the ISO C99 standard:
If @x@ is infinite or @y@ is zero, NaN is returned.
If @y@ is infinite and @x@ is finite, returned is @x@ rounded to the precision of @prec@.
If result is zero, it has the sign of @x@.
The returned ternary value corresponds to the result.*/
BINARY(fmod)

/*DOC Return the value of @x@ - @n@@y@, rounded
according to the direction @rnd@, where @n@ is the integer quotient
of @x@ divided by @y@, defined as follows: @n@ is rounded
 to the nearest integer (ties rounded to even) for @remainder@ and @remquo@.

See @fmod@ for special values.

The @remainder@ and @remquo@ functions are useful for additive argument reduction.*/
BINARY(remainder)

/*DOC Return true iff @op@ is an integer.*/
TEST(isInteger, integer_p)