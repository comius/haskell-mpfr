/*DOC Return the natural logarithm of @op@.
Return +0 if @op@ is 1 (in all rounding modes), for consistency with the ISO C99 and IEEE 754-2008 standards.
Retrun -Inf if @op@ is ±0 (i.e., the sign of the zero has no influence on the result). */
UNARY(log)

/*DOC Return log2(@op@).
Return +0 if @op@ is 1 (in all rounding modes), for consistency with the ISO C99 and IEEE 754-2008 standards.
Retrun -Inf if @op@ is ±0 (i.e., the sign of the zero has no influence on the result). */
UNARY(log2)

/*DOC Return log10(@op@).
Return +0 if @op@ is 1 (in all rounding modes), for consistency with the ISO C99 and IEEE 754-2008 standards.
Retrun -Inf if @op@ is ±0 (i.e., the sign of the zero has no influence on the result). */
UNARY(log10)

/*DOC Return the exponential of @op@. */
UNARY(exp)
/*DOC Return 2^@op@. */
UNARY(exp2)
/*DOC Return 10^@op@. */
UNARY(exp10)

/*DOC Return cosine of @op@. */
UNARY(cos)
/*DOC Return sine of @op@. */
UNARY(sin)
/*DOC Return tangent of @op@. */
UNARY(tan)

/*DOC Return simultaneously sine and cosine of @op@. */
UNARY2(sincos,sin_cos)

/*DOC Return secant of @op@. */
UNARY(sec)
/*DOC Return cosecant of @op@. */
UNARY(csc)
/*DOC Return cotangent of @op@. */
UNARY(cot)

/*DOC
Return the arc-cosine of @op@.
Note that since @acos(-1)@ returns the floating-point number closest to
Pi according to the given rounding mode, this number might not be
in the output range 0 <= @rop@ < Pi of the arc-cosine function;
still, the result lies in the image of the output range
by the rounding function. */
UNARY(acos)
/*DOC Return the arc-sine of @op@.
See acos for values of @asin(-1)@, @asin(1)@.  */
UNARY(asin)
/*DOC Return the arc-tangent of @op@.
See acos for values of @atan(-Inf)@, @atan(+Inf)@ or for @atan(op)@ with large @op@ and small precision. */
UNARY(atan)

/*DOC
Retrun arc-tangent2 of @y@ and @x@:
if @x > 0@, @atan2(y, x) = atan (y/x)@;
if @x < 0@, @atan2(y, x) = sign(y)*(Pi - atan (|y/x|))@,
thus a number from -Pi to Pi.
As for @atan@, in case the exact mathematical result is +Pi or -Pi,
its rounded result might be outside the function output range.

@atan2(y, 0)@ does not raise any floating-point exception.
Special values are handled as described in the ISO C99 and IEEE 754-2008
standards for the @atan2@ function:

 - @atan2(+0, -0)@ returns +Pi.
 - @atan2(-0, -0)@ returns -Pi.
 - @atan2(+0, +0)@ returns +0.
 - @atan2(-0, +0)@ returns -0.
 - @atan2(+0, x)@ returns +Pi for @x < 0@.
 - @atan2(-0, x)@ returns -Pi for @x < 0@.
 - @atan2(+0, x)@ returns +0 for @x > 0@.
 - @atan2(-0, x)@ returns -0 for @x > 0@.
 - @atan2(y, 0)@ returns -Pi/2 for @y < 0@.
 - @atan2(y, 0)@ returns +Pi/2 for @y > 0@.
 - @atan2(+Inf, -Inf)@ returns +3*Pi/4.
 - @atan2(-Inf, -Inf)@ returns -3*Pi/4.
 - @atan2(+Inf, +Inf)@ returns +Pi/4.
 - @atan2(-Inf, +Inf)@ returns -Pi/4.
 - @atan2(+Inf, x)@ returns +Pi/2 for finite @x@.
 - @atan2(-Inf, x)@ returns -Pi/2 for finite @x@.
 - @atan2(y, -Inf)@ returns +Pi for finite @y > 0@.
 - @atan2(y, -Inf)@ returns -Pi for finite @y < 0@.
 - @atan2(y, +Inf)@ returns +0 for finite @y > 0@.
 - @atan2(y, +Inf)@ returns -0 for finite @y < 0@.
*/
BINARY(atan2)

/*DOC Return hyperbolic cosine of @op@. */
UNARY(cosh)
/*DOC Return hyperbolic sine of @op@. */
UNARY(sinh)
/*DOC Return hyperbolic tangent of @op@. */
UNARY(tanh)

/*DOC Return simultaneously hyperbolic sine and cosine of @op@. */
UNARY2(sinhcosh,sinh_cosh)

/*DOC Return hyperbolic secant of @op@. */
UNARY(sech)
/*DOC Return hyperbolic cosecant of @op@. */
UNARY(csch)
/*DOC Return hyperbolic cotangent of @op@. */
UNARY(coth)

/*DOC Return inverse hyperbolic cosine of @op@. */
UNARY(acosh)
/*DOC Return inverse hyperbolic sine of @op@. */
UNARY(asinh)
/*DOC Return inverse hyperbolic tangent of @op@. */
UNARY(atanh)

/*DOC Return the logarithm of one plus @op@. */
UNARY(log1p)

/*DOC Return e^@op@-1. */
UNARY(expm1)    

/*DOC Return the exponential integral of @op@.
For positive @op@,
the exponential integral is the sum of Euler's constant, of the logarithm
of @op@, and of the sum for k from 1 to infinity of @op@^k/(k * k!).

For negative @op@, result is NaN
(this definition for negative argument follows formula 5.1.2 from the
Handbook of Mathematical Functions from Abramowitz and Stegun, a future
version might use another definition). */
UNARY(eint)

/*DOC Return real part of the dilogarithm of @op@.
 MPFR defines the dilogarithm function as the integral of -log(1-t)/t from 0 to @op@. */
UNARY(li2)

/*DOC Return the value of the Gamma function on @op@.
 When @op@ is a negative integer, return NaN. */
UNARY(gamma)

/*DOC Return the value of the logarithm of the Gamma function on @op@.
When @op@ is 1 or 2, return +0 (in all rounding modes).
When @op@ is an infinity or a nonpositive integer, return +Inf,
following the general rules on special values.
When -2@k@-1 < @op@ < -2k, @k@ being a nonnegative integer, return NaN.
See also @lgamma@. */
UNARY(lngamma)

/*DOC Return the value of the Digamma (sometimes also called Psi)
function on @op@.
When @op@ is a negative integer, return NaN. */
UNARY(digamma)

/*DOC Return the value of the Riemann Zeta function on @op@. */
UNARY(zeta)

/*DOC Return the value of the error function on @op@.*/
UNARY(erf)
/*DOC Return the value of the complementary error function on @op@.*/
UNARY(erfc)

/*DOC Return the value of the first kind Bessel function of order 0 on @op@.
When @op@ is NaN, NaN is always returned.
When @op@ is plus or minus Infinity, +0 is returned.
When @op@ is zero, and @n@ is not zero, +0 or -0 is returned depending on the parity and sign of @n@,
and the sign of @op@. */
UNARY(j0)

/*DOC Return the value of the first kind Bessel function of order 1 on @op@.
When @op@ is NaN, NaN is always returned.
When @op@ is plus or minus Infinity, +0 is returned.
When @op@ is zero, and @n@ is not zero, +0 or -0 is returned depending on the parity and sign of @n@,
and the sign of @op@. */
UNARY(j1)

/*DOC Return the value of the second kind Bessel function of order 0 on @op@.
When @op@ is NaN or negative, NaN is returned.
When @op@ is +Inf, +0 is returned.
When @op@ is zero, +Inf or -Inf is returned depending on the parity and sign of @n@. */
UNARY(y0)
/*DOC Return the value of the second kind Bessel function of order 1 on @op@.
When @op@ is NaN or negative, NaN is returned.
When @op@ is +Inf, +0 is returned.
When @op@ is zero, +Inf or -Inf is returned depending on the parity and sign of @n@. */
UNARY(y1)

/*DOC Return @op1@ * @op2@ + @op3@.
Concerning special values (signed zeros, infinities, NaN), these functions behave like a multiplication followed by a
separate addition or subtraction.  That is, the fused operation matters only for rounding. */
TERNARY(fma)

/*DOC Return @op1@ * @op2@ - @op3@.
Concerning special values (signed zeros, infinities, NaN), these functions behave like a multiplication followed by a
separate addition or subtraction.  That is, the fused operation matters only for rounding. */
TERNARY(fms)

/*DOC Return the arithmetic-geometric mean of @op1@ and @op2@.

The arithmetic-geometric mean is the common limit of the sequences
@u@_@n@ and @v@_@n@, where @u@_0=@op1@, @v@_0=@op2@,
@u@_(@n@+1) is the arithmetic mean of @u@_@n@ and @v@_@n@,
and @v@_(@n@+1) is the geometric mean of @u@_@n@ and @v@_@n@.
If any operand is negative, return NaN@. */
BINARY(agm)

/*DOC Return the Euclidean norm of @x@ and @y@, i.e., @sqrt{x^2+y^2}@.

Special values are handled as described in the ISO C99 (Section F.9.4.3)
and IEEE 754-2008 (Section 9.2.1) standards:
If @x@ or @y@ is an infinity, then +Inf is returned, even if the other number is NaN. */ 
BINARY(hypot)

/*DOC Return the value of the Airy function Ai on @x@.
When @x@ is NaN, NaN is returned.
When @x@ is +Inf or -Inf, +0 is returned.

The current implementation is not intended to be used with large arguments.
It works with |@x@| typically smaller than 500. For larger arguments,
other methods should be used and will be implemented in a future version. */
UNARY(ai)

/*DOC Return the logarithm of 2.*/
CONST(log2c,const_log2)
/*DOC Return the value of Pi.*/
CONST(pi,const_pi)
/*DOC Return the value of Euler's constant 0.577... */
CONST(euler,const_euler)
/*DOC Return the value of Catalan's constant 0.915... */
CONST(catalan,const_catalan)
