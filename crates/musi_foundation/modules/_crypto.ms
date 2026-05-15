--! Host-backed cryptographic primitives.

let Core := import "musi:core";
let String := Core.String;

@foreign(abi := .musi)
let Musi__sha256Hex (source : String) : String;

export let sha256Hex (source : String) := unsafe (Musi__sha256Hex(source));

@foreign(abi := .musi)
let Musi__sha256Base64 (source : String) : String;

export let sha256Base64 (source : String) :=
  unsafe (Musi__sha256Base64(source));
