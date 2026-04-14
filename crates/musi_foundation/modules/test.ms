let Core := import "musi:core";
let Bool := Core.Bool;
let String := Core.String;
let Unit := Core.Unit;

export opaque let SampleCase[T] := data {
  | Case
};

export opaque let SampleList[T] := data {
  | Nil
};

export opaque let Sample[T] := class { };

export opaque let Test := effect {
  let suiteStart (name : String) : Unit;
  let suiteEnd () : Unit;
  let testCase (name : String, passed : Bool) : Unit;
};

export let suiteStart (name : String) : Unit := request Test.suiteStart(name);

export let suiteEnd () : Unit := request Test.suiteEnd();

export let testCase (name : String, passed : Bool) : Unit := request Test.testCase(name, passed);
