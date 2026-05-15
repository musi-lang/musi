--! Host-backed test primitives.

let Core := import "musi:core";
let Bit := Core.Bit;
let String := Core.String;
let Unit := Core.Unit;

--- Sample test case marker.
export hidden let SampleCase [T] := data {
  | Case
};

--- Sample test list marker.
export hidden let SampleList [T] := data {
  | Nil
};

--- Sample test contract.
export hidden let Sample [T] := shape {
};

@foreign(abi := .musi)
let Musi__suiteStart (name : String) : Unit;

--- Start a named test suite.
export let suiteStart (name : String) := unsafe (Musi__suiteStart(name));

@foreign(abi := .musi)
let Musi__suiteEnd () : Unit;

--- End current test suite.
export let suiteEnd () := unsafe (Musi__suiteEnd());

@foreign(abi := .musi)
let Musi__testCase (name : String, passed : Bit) : Unit;

--- Record one named test case.
export let testCase (name : String, passed : Bit) :=
  unsafe (Musi__testCase(name, passed));
