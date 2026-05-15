--! Compiler-owned core builtin module.

@musi.builtin(name := "Type")
--- Type universe value.
export let Type := Type;

@musi.builtin(name := "Array")
--- Array type constructor.
export let Array := Array;

@musi.builtin(name := "Any")
--- Any value type.
export let Any := Any;

@musi.builtin(name := "Unknown")
--- Unknown value type.
export let Unknown := Unknown;

@musi.builtin(name := "Syntax")
--- Syntax value type.
export let Syntax := Syntax;

@musi.builtin(name := "Empty")
--- Empty result type.
export let Empty := Empty;

@musi.builtin(name := "Unit")
--- Unit value type.
export let Unit := Unit;

@musi.builtin(name := "Bit")
--- Bit condition type.
export let Bit := Bit;

@musi.builtin(name := "Nat")
--- Natural number type.
export let Nat := Nat;

@musi.builtin(name := "Int")
--- Integer type.
export let Int := Int;

@musi.builtin(name := "Int8")
--- 8-bit signed integer type.
export let Int8 := Int8;

@musi.builtin(name := "Int16")
--- 16-bit signed integer type.
export let Int16 := Int16;

@musi.builtin(name := "Int32")
--- 32-bit signed integer type.
export let Int32 := Int32;

@musi.builtin(name := "Int64")
--- 64-bit signed integer type.
export let Int64 := Int64;

@musi.builtin(name := "Nat8")
--- 8-bit natural number type.
export let Nat8 := Nat8;

@musi.builtin(name := "Nat16")
--- 16-bit natural number type.
export let Nat16 := Nat16;

@musi.builtin(name := "Nat32")
--- 32-bit natural number type.
export let Nat32 := Nat32;

@musi.builtin(name := "Nat64")
--- 64-bit natural number type.
export let Nat64 := Nat64;

@musi.builtin(name := "Float")
--- Floating-point number type.
export let Float := Float;

@musi.builtin(name := "Float32")
--- 32-bit floating-point number type.
export let Float32 := Float32;

@musi.builtin(name := "Float64")
--- 64-bit floating-point number type.
export let Float64 := Float64;

@musi.builtin(name := "String")
--- Text string type.
export let String := String;

@musi.builtin(name := "Rune")
--- Unicode scalar value type.
export let Rune := Rune;

@musi.builtin(name := "Bits")
--- Exact-width bit pattern type constructor.
export let Bits := Bits;

@musi.builtin(name := "Word")
--- Target-sized machine word type.
export let Word := Word;

@musi.builtin(name := "Word8")
--- 8-bit machine word type.
export let Word8 := Word8;

@musi.builtin(name := "Word16")
--- 16-bit machine word type.
export let Word16 := Word16;

@musi.builtin(name := "Word32")
--- 32-bit machine word type.
export let Word32 := Word32;

@musi.builtin(name := "Word64")
--- 64-bit machine word type.
export let Word64 := Word64;

@musi.builtin(name := "Range")
--- Range type.
export let Range := Range;

@musi.builtin(name := "Pin")
--- Scoped pin capability type.
export let Pin := Pin;

@musi.builtin(name := "CString")
--- C string pointer type.
export let CString := CString;

@musi.builtin(name := "CPtr")
--- Raw FFI pointer type.
export let CPtr := CPtr;

--- Maybe value type.
export hidden let Maybe [T] := data {
  | Some(T)
  | None
};

--- Abort capability contract.
export hidden let Abort := shape {
};

@foreign(abi := .musi)
let Musi__abort () : Empty;

--- Abort current computation.
export let abort () := unsafe (Musi__abort());

--- Type that can step through ranges.
export hidden let Rangeable [T] := shape {
  let compare (left : T, right : T) : Int;
  let next (value : T) : Maybe[T];
  let prev (value : T) : Maybe[T];
};

--- Range bounds contract.
export hidden let RangeBounds [T] := shape {
  let defaultLowerBound () : T;
  let defaultUpperBound () : T;
};
