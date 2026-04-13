let Core := import "musi:core";
let Intrinsics := import "musi:test";
let Assert := import "@std/assert";
let Bool := Core.Bool;
let String := Core.String;
let Unit := Core.Unit;

export let pass : Bool := Assert.passValue;

export let fail := Assert.fail;

export let equal := Assert.equal;
export let notEqual := Assert.notEqual;
export let isTrue := Assert.isTrue;
export let isFalse := Assert.isFalse;
export let greaterThan := Assert.greaterThan;
export let lessThan := Assert.lessThan;
export let betweenInclusive := Assert.betweenInclusive;
export let expectInt := Assert.expectInt;

export let toBe := Assert.toBe;
export let toEqual := Assert.toEqual;
export let toBeTruthy := Assert.toBeTruthy;
export let toBeFalsy := Assert.toBeFalsy;
export let toBeGreaterThan := Assert.toBeGreaterThan;
export let toBeLessThan := Assert.toBeLessThan;
export let expect := Assert.expect;

export let describe (name : String) :=
  Intrinsics.suiteStart(name);
export let endDescribe () :=
  Intrinsics.suiteEnd();
export let it (name : String, passed : Bool) :=
  Intrinsics.testCase(name, passed);

export let beforeAll (body : Unit -> Unit) : Unit := endDescribe();

export let afterAll (body : Unit -> Unit) : Unit := endDescribe();

export let beforeEach (body : Unit -> Unit) : Unit := endDescribe();

export let afterEach (body : Unit -> Unit) : Unit := endDescribe();
