--! Host-backed input and output primitives.

let Core := import "musi:core";
let String := Core.String;
let Unit := Core.Unit;

@foreign(abi := .musi)
let Musi__write (text : String) : Unit;

export let write (text : String) := unsafe (Musi__write(text));

@foreign(abi := .musi)
let Musi__writeLn (text : String) : Unit;

export let writeLn (text : String) := unsafe (Musi__writeLn(text));

@foreign(abi := .musi)
let Musi__writeErr (text : String) : Unit;

export let writeErr (text : String) := unsafe (Musi__writeErr(text));

@foreign(abi := .musi)
let Musi__writeErrLn (text : String) : Unit;

export let writeErrLn (text : String) := unsafe (Musi__writeErrLn(text));

@foreign(abi := .musi)
let Musi__readLine () : String;

export let readLine () := unsafe (Musi__readLine());
