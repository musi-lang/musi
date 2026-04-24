let pass := 0 = 0;
let failBool (message : String) : Bool := 0 = 1;

--- Successful assertion value.
export let passValue : Bool := pass;

--- Failed assertion value with message output.
export let fail (message : String) : Bool := failBool(message);

--- Assert two values are equal.
export let equal [T] (actual : T, expected : T) : Bool :=
  match () (
  | _ if actual = expected => pass
  | _ => fail("expected values to be equal")
  );

--- Assert two values are different.
export let notEqual [T] (actual : T, expected : T) : Bool :=
  match () (
  | _ if actual /= expected => pass
  | _ => fail("expected values to be different")
  );

--- Assert a boolean is true.
export let isTrue (actual : Bool) : Bool :=
  match () (
  | _ if actual => pass
  | _ => fail("expected condition to be true")
  );

--- Assert a boolean is false.
export let isFalse (actual : Bool) : Bool :=
  match () (
  | _ if actual => fail("expected condition to be false")
  | _ => pass
  );

--- Assert an integer is greater than a lower bound.
export let greaterThan (actual : Int, lowerBound : Int) : Bool :=
  match () (
  | _ if actual > lowerBound => pass
  | _ => fail("expected value to be greater than lower bound")
  );

--- Assert an integer is less than an upper bound.
export let lessThan (actual : Int, upperBound : Int) : Bool :=
  match () (
  | _ if actual < upperBound => pass
  | _ => fail("expected value to be less than upper bound")
  );

--- Assert an integer is inside inclusive bounds.
export let betweenInclusive (
  actual : Int,
  lowerBound : Int,
  upperBound : Int,
) : Bool :=
  match () (
  | _ if actual < lowerBound => fail(
    "expected value to be inside inclusive bounds"
  )
  | _ if actual > upperBound => fail(
    "expected value to be inside inclusive bounds"
  )
  | _ => pass
  );

--- Build integer matcher helpers for one actual value.
export let expectInt (actual : Int) :=
  (
    let shouldEqual (expected : Int) := equal[Int](actual, expected);
    let shouldNotEqual (expected : Int) := notEqual[Int](actual, expected);
    let shouldBeGreaterThan (bound : Int) := greaterThan(actual, bound);
    let shouldBeLessThan (bound : Int) := lessThan(actual, bound);
    {
      equal := shouldEqual, notEqual := shouldNotEqual, greaterThan :=
        shouldBeGreaterThan, lessThan := shouldBeLessThan
    }
  );

--- Matcher alias for integer equality.
export let toBe (actual : Int, expected : Int) : Bool :=
  equal[Int](actual, expected);

--- Matcher alias for integer equality.
export let toEqual (actual : Int, expected : Int) : Bool :=
  equal[Int](actual, expected);

--- Matcher alias for true boolean checks.
export let toBeTrue (actual : Bool) : Bool := isTrue(actual);

--- Matcher alias for false boolean checks.
export let toBeFalse (actual : Bool) : Bool := isFalse(actual);

--- Matcher alias for integer greater-than checks.
export let toBeGreaterThan (actual : Int, bound : Int) : Bool :=
  greaterThan(actual, bound);

--- Matcher alias for integer less-than checks.
export let toBeLessThan (actual : Int, bound : Int) : Bool :=
  lessThan(actual, bound);

--- Build default integer matcher helpers.
export let expect (actual : Int) := expectInt(actual);
