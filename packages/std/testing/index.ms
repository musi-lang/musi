let Intrinsics := import "musi:test";
let Test := Intrinsics.Test;
let Assert := import "@std/assert";

export let pass := Assert.to_be(0, 0);

export let fail := Assert.fail;

export let expect := Assert.expect;
export let to_be := Assert.to_be;
export let to_equal := Assert.to_equal;
export let to_be_truthy := Assert.to_be_truthy;
export let to_be_falsy := Assert.to_be_falsy;
export let to_be_greater_than := Assert.to_be_greater_than;
export let to_be_less_than := Assert.to_be_less_than;

export let describe (name : String) :=
    perform Test.suiteStart(name);
export let end_describe () :=
    perform Test.suiteEnd();
export let it (name : String, passed : Bool) :=
    perform Test.testCase(name, passed);

export let before_all (body) : Unit := (;);

export let after_all (body) : Unit := (;);

export let before_each (body) : Unit := (;);

export let after_each (body) : Unit := (;);
