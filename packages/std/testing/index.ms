let Core := import "musi:core";
let Intrinsics := import "musi:test";
let Assert := import "@std/assert";
let Bool := Core.Bool;
let String := Core.String;
let Unit := Core.Unit;

export let pass : Bool := Assert.passValue;

export let fail (message : String) : Bool := Assert.fail(message);

export let equal[T] (actual : T, expected : T) : Bool := Assert.equal[T](actual, expected);
export let notEqual[T] (actual : T, expected : T) : Bool := Assert.notEqual[T](actual, expected);
export let isTrue (actual : Bool) : Bool := Assert.isTrue(actual);
export let isFalse (actual : Bool) : Bool := Assert.isFalse(actual);
export let greaterThan (actual : Int, lowerBound : Int) : Bool :=
  Assert.greaterThan(actual, lowerBound);
export let lessThan (actual : Int, upperBound : Int) : Bool :=
  Assert.lessThan(actual, upperBound);
export let betweenInclusive (
  actual : Int,
  lowerBound : Int,
  upperBound : Int,
) : Bool := Assert.betweenInclusive(actual, lowerBound, upperBound);
export let expectInt (actual : Int) := Assert.expectInt(actual);

export let toBe (actual : Int, expected : Int) : Bool := Assert.toBe(actual, expected);
export let toEqual (actual : Int, expected : Int) : Bool := Assert.toEqual(actual, expected);
export let toBeTruthy (actual : Bool) : Bool := Assert.toBeTruthy(actual);
export let toBeFalsy (actual : Bool) : Bool := Assert.toBeFalsy(actual);
export let toBeGreaterThan (actual : Int, bound : Int) : Bool :=
  Assert.toBeGreaterThan(actual, bound);
export let toBeLessThan (actual : Int, bound : Int) : Bool :=
  Assert.toBeLessThan(actual, bound);
export let expect (actual : Int) := Assert.expect(actual);

export let describe (name : String) : Unit :=
  Intrinsics.suiteStart(name);
export let endDescribe () : Unit :=
  Intrinsics.suiteEnd();
export let it (name : String, passed : Bool) : Unit :=
  Intrinsics.testCase(name, passed);
