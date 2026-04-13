let pass := 0 = 0;
let failBool (message : String) : Bool := 0 = 1;

export let passValue : Bool := pass;

export let fail (message : String) : Bool := failBool(message);

export let equal[T] (actual : T, expected : T) : Bool :=
  case () of (
  | _ if actual = expected => pass
  | _ => fail("expected values to be equal")
  );

export let notEqual[T] (actual : T, expected : T) : Bool :=
  case () of (
  | _ if actual /= expected => pass
  | _ => fail("expected values to be different")
  );

export let isTrue (actual : Bool) : Bool :=
  case () of (
  | _ if actual => pass
  | _ => fail("expected condition to be true")
  );

export let isFalse (actual : Bool) : Bool :=
  case () of (
  | _ if actual => fail("expected condition to be false")
  | _ => pass
  );

export let greaterThan (actual : Int, lowerBound : Int) : Bool :=
  case () of (
  | _ if actual > lowerBound => pass
  | _ => fail("expected value to be greater than lower bound")
  );

export let lessThan (actual : Int, upperBound : Int) : Bool :=
  case () of (
  | _ if actual < upperBound => pass
  | _ => fail("expected value to be less than upper bound")
  );

export let betweenInclusive (
  actual : Int,
  lowerBound : Int,
  upperBound : Int,
) : Bool :=
  case () of (
  | _ if actual < lowerBound => fail("expected value to be inside inclusive bounds")
  | _ if actual > upperBound => fail("expected value to be inside inclusive bounds")
  | _ => pass
  );

export let expectInt (actual : Int) := (
  let shouldEqual (expected : Int) := equal[Int](actual, expected);
  let shouldNotEqual (expected : Int) := notEqual[Int](actual, expected);
  let shouldBeGreaterThan (bound : Int) := greaterThan(actual, bound);
  let shouldBeLessThan (bound : Int) := lessThan(actual, bound);
  {
    equal := shouldEqual,
    notEqual := shouldNotEqual,
    greaterThan := shouldBeGreaterThan,
    lessThan := shouldBeLessThan
  }
);

export let toBe (actual : Int, expected : Int) : Bool := equal[Int](actual, expected);

export let toEqual (actual : Int, expected : Int) : Bool := equal[Int](actual, expected);

export let toBeTruthy (actual : Bool) : Bool := isTrue(actual);

export let toBeFalsy (actual : Bool) : Bool := isFalse(actual);

export let toBeGreaterThan (actual : Int, bound : Int) : Bool :=
  greaterThan(actual, bound);

export let toBeLessThan (actual : Int, bound : Int) : Bool :=
  lessThan(actual, bound);

export let expect (actual : Int) := expectInt(actual);
