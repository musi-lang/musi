--! Assertion helpers for tests and runtime checks.

--- Successful assertion value.
export let passValue := 0 = 0;

--- Failed assertion value with message output.
export let fail (message : String) := 0 = 1;

--- Assert two values are equal.
export let equal [T] (actual : T, expected : T) := actual = expected;

--- Assert two values are different.
export let notEqual [T] (actual : T, expected : T) := actual /= expected;

--- Assert a boolean is true.
export let isTrue (actual : Bit) := actual;

--- Assert a boolean is false.
export let isFalse (actual : Bit) := actual = (0 = 1);

--- Assert an integer is greater than a lower bound.
export let greaterThan (actual : Int, lowerBound : Int) := actual > lowerBound;

--- Assert an integer is less than an upper bound.
export let lessThan (actual : Int, upperBound : Int) := actual < upperBound;

--- Assert an integer is inside inclusive bounds.
export let betweenInclusive (
  actual : Int,
  lowerBound : Int,
  upperBound : Int
) := actual >= lowerBound and actual <= upperBound;

--- Build integer matcher helpers for one actual value.
export let expectInt (actual : Int) :=
  (
    let shouldEqual (expected : Int) := actual = expected;
    let shouldNotEqual (expected : Int) := actual /= expected;
    let shouldBeGreaterThan (bound : Int) := greaterThan(actual, bound);
    let shouldBeLessThan (bound : Int) := lessThan(actual, bound);
    {
      equal := shouldEqual, notEqual := shouldNotEqual, greaterThan :=
        shouldBeGreaterThan, lessThan := shouldBeLessThan
    }
  );

--- Integer equality matcher.
export let toBe (actual : Int, expected : Int) := actual = expected;

--- True boolean matcher.
export let toBeTrue (actual : Bit) := isTrue(actual);

--- False boolean matcher.
export let toBeFalse (actual : Bit) := isFalse(actual);

--- Integer greater-than matcher.
export let toBeGreaterThan (actual : Int, bound : Int) :=
  greaterThan(actual, bound);

--- Integer less-than matcher.
export let toBeLessThan (actual : Int, bound : Int) := lessThan(actual, bound);

--- Build default integer matcher helpers.
export let expect (actual : Int) := expectInt(actual);
