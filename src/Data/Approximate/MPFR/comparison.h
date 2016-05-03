/*DOC Return true if @op@ is an ordinary number (i.e., neither NaN nor an infinity). 
    Return false otherwise. */
TEST(isNumber, number_p)

/*DOC Return true if @op@ is a regular number (i.e., neither NaN, nor an infinity nor zero). 
    Return false otherwise. */
TEST(isRegular, regular_p)

/*DOC Return the maximum of @op1@ and @op2@. 
If @op1@ and @op2@ are both NaN, then NaN is returned. 
If @op1@ or @op2@ is NaN, then numeric value is returned.
If @op1@ and @op2@ are zeros of different signs, then +0 is returned. */
BINARY(max)

/*DOC Return the minimum of @op1@ and @op2@. 
If @op1@ and @op2@ are both NaN, then NaN is returned. 
If @op1@ or @op2@ is NaN, then numeric value is returned.
If @op1@ and @op2@ are zeros of different signs, then -0 is returned. */
BINARY(min)

/*DOC Return true if @op1@ = @op2@ and false otherwise.
Those functions return false whenever @op1@ and/or @op2@ is NaN. */
COMPARISON(equal, equal_p)

/*DOC Return true if @op1@ < @op2@ 
or @op1@ > @op2@ (i.e., neither @op1@, nor @op2@ is
NaN, and @op1@ <> @op2@), false otherwise (i.e., @op1@ and/or @op2@ is NaN, or @op1@ = @op2@). */
COMPARISON(notEqual, lessgreater_p)

/*DOC Return true if @op1@ < @op2@ and false otherwise.
Those functions return false whenever @op1@ and/or @op2@ is NaN. */
COMPARISON(less, less_p)

/*DOC Return true if @op1@ > @op2@ and false otherwise.
Those functions return false whenever @op1@ and/or @op2@ is NaN. */
COMPARISON(greater, greater_p)

/*DOC Return true if @op1@ <= @op2@ and false otherwise.
Those functions return false whenever @op1@ and/or @op2@ is NaN. */
COMPARISON(lessEq, lessequal_p)

/*DOC Return true if @op1@ >= @op2@ and false otherwise.
Those functions return false whenever @op1@ and/or @op2@ is NaN. */
COMPARISON(greaterEq, greaterequal_p)

/*DOC Return true if @op1@ or @op2@ is a NaN (i.e., they cannot be
compared), false otherwise. */
COMPARISON(unordered, unordered_p)

