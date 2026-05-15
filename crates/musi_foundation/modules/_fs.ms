--! Host-backed file system primitives.

let Core := import "musi:core";
let Int := Core.Int;
let String := Core.String;
let Unit := Core.Unit;

@foreign(abi := .musi)
let Musi__readText (path : String) : String;

export let readText (path : String) := unsafe (Musi__readText(path));

@foreign(abi := .musi)
let Musi__writeText (path : String, text : String) : Unit;

export let writeText (path : String, text : String) :=
  unsafe (Musi__writeText(path, text));

@foreign(abi := .musi)
let Musi__exists (path : String) : Int;

export let exists (path : String) := unsafe (Musi__exists(path));

@foreign(abi := .musi)
let Musi__appendText (path : String, text : String) : Int;

export let appendText (path : String, text : String) :=
  unsafe (Musi__appendText(path, text));

@foreign(abi := .musi)
let Musi__remove (path : String) : Int;

export let remove (path : String) := unsafe (Musi__remove(path));

@foreign(abi := .musi)
let Musi__createDirAll (path : String) : Int;

export let createDirAll (path : String) := unsafe (Musi__createDirAll(path));
