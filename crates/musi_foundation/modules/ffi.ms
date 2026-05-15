--! Compiler-owned FFI builtin module.

let Core := import "musi:core";

--- Raw FFI pointer type.
export let CPtr := Core.CPtr;

--- C string pointer type.
export let CString := Core.CString;

--- Scoped pin capability type.
export let Pin := Core.Pin;

@foreign(abi := .musi)
let Musi__ptrNull () : CPtr;
@foreign(abi := .musi)
let Musi__ptrIsNull (raw : CPtr) : Core.Bit;

--- Raw null C pointer.
export let ptrNull () := unsafe (Musi__ptrNull());

--- Returns true when raw C pointer is null.
export let ptrIsNull (raw : CPtr) := unsafe (Musi__ptrIsNull(raw));
