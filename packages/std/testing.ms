let assert := import "@std/assert";
let core := import "musi:core";
let intrinsics := import "musi:test";
let Bool := core.Bool;
let String := core.String;
let Unit := core.Unit;

--- Successful test result.
export let pass : Bool := assert.passValue;

--- Failed assertion value with message output.
export let fail (message : String) : Bool := assert.fail(message);

--- Assert two values are equal.
export let equal [T] (actual : T, expected : T) : Bool :=
  assert.equal[T](actual, expected);
--- Assert two values are different.
export let notEqual [T] (actual : T, expected : T) : Bool :=
  assert.notEqual[T](actual, expected);
--- Assert a boolean is true.
export let isTrue (actual : Bool) : Bool := assert.isTrue(actual);
--- Assert a boolean is false.
export let isFalse (actual : Bool) : Bool := assert.isFalse(actual);
--- Assert an integer is greater than a lower bound.
export let greaterThan (actual : Int, lowerBound : Int) : Bool :=
  assert.greaterThan(actual, lowerBound);
--- Assert an integer is less than an upper bound.
export let lessThan (actual : Int, upperBound : Int) : Bool :=
  assert.lessThan(actual, upperBound);
--- Assert an integer is inside inclusive bounds.
export let betweenInclusive (
  actual : Int,
  lowerBound : Int,
  upperBound : Int,
) : Bool := assert.betweenInclusive(actual, lowerBound, upperBound);
--- Build integer matcher helpers for one actual value.
export let expectInt (actual : Int) := assert.expectInt(actual);

--- Matcher alias for integer equality.
export let toBe (actual : Int, expected : Int) : Bool :=
  assert.toBe(actual, expected);
--- Matcher alias for integer equality.
export let toEqual (actual : Int, expected : Int) : Bool :=
  assert.toEqual(actual, expected);
--- Matcher alias for true boolean checks.
export let toBeTrue (actual : Bool) : Bool := assert.toBeTrue(actual);
--- Matcher alias for false boolean checks.
export let toBeFalse (actual : Bool) : Bool := assert.toBeFalse(actual);
--- Matcher alias for integer greater-than checks.
export let toBeGreaterThan (actual : Int, bound : Int) : Bool :=
  assert.toBeGreaterThan(actual, bound);
--- Matcher alias for integer less-than checks.
export let toBeLessThan (actual : Int, bound : Int) : Bool :=
  assert.toBeLessThan(actual, bound);
--- Build default integer matcher helpers.
export let expect (actual : Int) := assert.expect(actual);

--- Start a named test suite.
export let describe (name : String) : Unit := intrinsics.suiteStart(name);
--- End current test suite.
export let endDescribe () : Unit := intrinsics.suiteEnd();
--- Record one named test case.
export let it (name : String, passed : Bool) : Unit :=
  intrinsics.testCase(name, passed);
