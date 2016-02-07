/*DOC Returns __/@op1@ + @op2@/__ with given Precision and rounded with RoundMode. */
BINARY(add)

/*DOC Returns __/@op1@ - @op2@/__ with given Precision and rounded with RoundMode. */
BINARY(sub)

/*DOC Returns __/@op1@ times @op2@/__ with given Precision and rounded with RoundMode.
When a result is zero, its sign is the product of the signs of the operands. */
BINARY(mul)

/*DOC Returns __/the square of @op@/__ with given Precision and rounded with RoundMode. */
UNARY(sqr)

/*DOC Returns __/@op1@\/@op2@/__ with given Precision and rounded with RoundMode. 
When a result is zero, its sign is the product of the signs of the operands. */
BINARY(div)

/*DOC Returns __/the square root of @op@/__ with given Precision and rounded with RoundMode (returns -0 if op is -0, to be consistent with the IEEE 754 standard).
Returns NaN if op is negative. */
UNARY(sqrt)

/*DOC Returns __/the reciprocal square root of @op@/__ with given Precision and rounded with RoundMode.
 Returns +Inf if op is ±0, +0 if op is +Inf, and NaN if op is negative.  */
UNARY_(recSqrt,rec_sqrt)

/*DOC Returns __/the cubic root of @op@/__ with given Precision and rounded with RoundMode. 
For op negative (including -Inf), returns a negative number. 
The cubic root of -0 is defined to be -0. */
UNARY(cbrt)

/*DOC Returns __/@op1@ raised to @op2@/__ with given Precision and rounded with RoundMode.
Special values are handled as described in the ISO C99 and IEEE 754-2008 standards for the @pow@ function:

      - @pow@(±0, y) returns plus or minus infinity for /y/ a negative odd integer.
      - @pow@(±0, y) returns plus infinity for /y/ negative and not an odd integer.
      - @pow@(±0, y) returns plus or minus zero for /y/ a positive odd integer.
      - @pow@(±0, y) returns plus zero for /y/ positive and not an odd integer.
      - @pow@(-1, ±Inf) returns 1.
      - @pow@(+1, y) returns 1 for any /y/, even a NaN.
      - @pow@(x, ±0) returns 1 for any /x/, even a NaN.
      - @pow@(x, y) returns NaN for finite negative /x/ and finite non-integer /y/.
      - @pow@(x, -Inf) returns plus infinity for /0 \< abs(x) \< 1/, and plus zero for /abs(x) > 1/.
      - @pow@(x, +Inf) returns plus zero for /0 \< abs(x) \< 1/, and plus infinity for /abs(x) > 1/.
      - @pow@(-Inf, y) returns minus zero for /y/ a negative odd integer.
      - @pow@(-Inf, y) returns plus zero for /y/ negative and not an odd integer.
      - @pow@(-Inf, y) returns minus infinity for /y/ a positive odd integer.
      - @pow@(-Inf, y) returns plus infinity for /y/ positive and not an odd integer.
      - @pow@(+Inf, y) returns plus zero for /y/ negative, and plus infinity for /y/ positive. */
BINARY(pow)

/*DOC Returns __/-@op@/__ with given Precision and rounded with RoundMode.
A rounding might occur if the Precision is less than precision of op. */
UNARY(neg)

/*DOC Returns __/the absolute value of @op@/__ with given Precision and rounded with RoundMode. 
A rounding might occur if the Precision is less than precision of op. */
UNARY(abs)

/*DOC Returns __/the positive difference of @op1@ and @op2@/__, i.e., op1 - op2 with given Precision and rounded with RoundMode 
if op1 > op2, +0 if op1 <= op2, and NaN if op1 or op2 is NaN. */
BINARY(dim)
