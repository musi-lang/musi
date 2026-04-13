let Core := import "musi:core";
let Intrinsics := import "musi:test";
let Assert := import "@std/assert";
let Bool := Core.Bool;
let String := Core.String;
let Unit := Core.Unit;

export let pass := Assert.toBe(0, 0);

export let fail := Assert.fail;

export let expect := Assert.expect;
export let toBe := Assert.toBe;
export let toEqual := Assert.toEqual;
export let toBeTruthy := Assert.toBeTruthy;
export let toBeFalsy := Assert.toBeFalsy;
export let toBeGreaterThan := Assert.toBeGreaterThan;
export let toBeLessThan := Assert.toBeLessThan;

export let describe (name : String) :=
  Intrinsics.suiteStart(name);
export let endDescribe () :=
  Intrinsics.suiteEnd();
export let it (name : String, passed : Bool) :=
  Intrinsics.testCase(name, passed);

export let beforeAll (body) : Unit := (;);

export let afterAll (body) : Unit := (;);

export let beforeEach (body) : Unit := (;);

export let afterEach (body) : Unit := (;);
