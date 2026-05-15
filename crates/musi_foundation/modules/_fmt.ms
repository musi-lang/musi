--! Host-backed formatting primitives.

let Core := import "musi:core";
let Float := Core.Float;
let Int := Core.Int;
let String := Core.String;

@foreign(abi := .musi)
let Musi__int (value : Int) : String;

export let int (value : Int) := unsafe (Musi__int(value));

@foreign(abi := .musi)
let Musi__float (value : Float) : String;

export let float (value : Float) := unsafe (Musi__float(value));
