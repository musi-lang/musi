--! Host-backed JSON primitives.

let Core := import "musi:core";
let Int := Core.Int;
let String := Core.String;

@foreign(abi := .musi)
let Musi__isValid (source : String) : Int;

export let isValid (source : String) := unsafe (Musi__isValid(source));

@foreign(abi := .musi)
let Musi__normalize (source : String) : String;

export let normalize (source : String) := unsafe (Musi__normalize(source));
