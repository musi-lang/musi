--! Host-backed random primitives.

let Core := import "musi:core";
let Float := Core.Float;
let Int := Core.Int;
let String := Core.String;

@foreign(abi := .musi)
let Musi__int () : Int;

export let int () := unsafe (Musi__int());

@foreign(abi := .musi)
let Musi__intInRange (lowerBound : Int, upperBound : Int) : Int;

export let intInRange (lowerBound : Int, upperBound : Int) :=
  unsafe (Musi__intInRange(lowerBound, upperBound));

@foreign(abi := .musi)
let Musi__bool () : Int;

export let bool () := unsafe (Musi__bool());

@foreign(abi := .musi)
let Musi__float01 () : Float;

export let float01 () := unsafe (Musi__float01());

@foreign(abi := .musi)
let Musi__entropyHex (count : Int) : String;

export let entropyHex (count : Int) := unsafe (Musi__entropyHex(count));
