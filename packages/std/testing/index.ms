let core := import "musi:core";
let intrinsics := import "musi:test";
let assert := import "@std/assert";
let Bool := core.Bool;
let String := core.String;
let Unit := core.Unit;

export let pass : Bool := assert.passValue;

export let fail (message : String) : Bool := assert.fail(message);

export let equal[T] (actual : T, expected : T) : Bool := assert.equal[T](actual, expected);
export let notEqual[T] (actual : T, expected : T) : Bool := assert.notEqual[T](actual, expected);
export let isTrue (actual : Bool) : Bool := assert.isTrue(actual);
export let isFalse (actual : Bool) : Bool := assert.isFalse(actual);
export let greaterThan (actual : Int, lowerBound : Int) : Bool :=
  assert.greaterThan(actual, lowerBound);
export let lessThan (actual : Int, upperBound : Int) : Bool :=
  assert.lessThan(actual, upperBound);
export let betweenInclusive (
  actual : Int,
  lowerBound : Int,
  upperBound : Int,
) : Bool := assert.betweenInclusive(actual, lowerBound, upperBound);
export let expectInt (actual : Int) := assert.expectInt(actual);

export let toBe (actual : Int, expected : Int) : Bool := assert.toBe(actual, expected);
export let toEqual (actual : Int, expected : Int) : Bool := assert.toEqual(actual, expected);
export let toBeTruthy (actual : Bool) : Bool := assert.toBeTruthy(actual);
export let toBeFalsy (actual : Bool) : Bool := assert.toBeFalsy(actual);
export let toBeGreaterThan (actual : Int, bound : Int) : Bool :=
  assert.toBeGreaterThan(actual, bound);
export let toBeLessThan (actual : Int, bound : Int) : Bool :=
  assert.toBeLessThan(actual, bound);
export let expect (actual : Int) := assert.expect(actual);

export let describe (name : String) : Unit :=
  intrinsics.suiteStart(name);
export let endDescribe () : Unit :=
  intrinsics.suiteEnd();
export let it (name : String, passed : Bool) : Unit :=
  intrinsics.testCase(name, passed);
