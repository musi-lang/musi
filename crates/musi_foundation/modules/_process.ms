--! Host-backed process primitives.

let Core := import "musi:core";
let Int := Core.Int;
let String := Core.String;
let Unit := Core.Unit;

@foreign(abi := .musi)
let Musi__argCount () : Int;

export let argCount () := unsafe (Musi__argCount());

@foreign(abi := .musi)
let Musi__argAt (index : Int) : String;

export let argAt (index : Int) := unsafe (Musi__argAt(index));

@foreign(abi := .musi)
let Musi__cwd () : String;

export let cwd () := unsafe (Musi__cwd());

@foreign(abi := .musi)
let Musi__run (command : String) : Int;

export let run (command : String) := unsafe (Musi__run(command));

@foreign(abi := .musi)
let Musi__exit (code : Int) : Unit;

export let exit (code : Int) := unsafe (Musi__exit(code));
