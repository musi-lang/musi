let bytes := import "@std/bytes";
let cmp := import "@std/cmp";
let collections := import "@std/collections";
let option := import "@std/option";
let path := import "@std/path";
let result := import "@std/result";
let core := import "musi:core";

-- --- Type universe ---
--- Prelude alias for core Type.
export let Type := core.Type;
--- Prelude alias for core Array.
export let Array := core.Array;
--- Prelude alias for core Any.
export let Any := core.Any;
--- Prelude alias for core Unknown.
export let Unknown := core.Unknown;
--- Prelude alias for core Syntax.
export let Syntax := core.Syntax;
--- Prelude alias for core Empty.
export let Empty := core.Empty;
--- Prelude alias for core Unit.
export let Unit := core.Unit;
--- Prelude alias for core Bool.
export let Bool := core.Bool;
--- Prelude alias for core Nat.
export let Nat := core.Nat;
--- Prelude alias for core Int.
export let Int := core.Int;
--- Prelude alias for core Int8.
export let Int8 := core.Int8;
--- Prelude alias for core Int16.
export let Int16 := core.Int16;
--- Prelude alias for core Int32.
export let Int32 := core.Int32;
--- Prelude alias for core Int64.
export let Int64 := core.Int64;
--- Prelude alias for core Nat8.
export let Nat8 := core.Nat8;
--- Prelude alias for core Nat16.
export let Nat16 := core.Nat16;
--- Prelude alias for core Nat32.
export let Nat32 := core.Nat32;
--- Prelude alias for core Nat64.
export let Nat64 := core.Nat64;
--- Prelude alias for core Float.
export let Float := core.Float;
--- Prelude alias for core Float32.
export let Float32 := core.Float32;
--- Prelude alias for core Float64.
export let Float64 := core.Float64;
--- Prelude alias for core String.
export let String := core.String;
--- Prelude alias for core Rune.
export let Rune := core.Rune;
--- Prelude alias for core Bits.
export let Bits := core.Bits;
--- Prelude alias for core Range.
export let Range := core.Range;
--- Prelude alias for core Pin.
export let Pin := core.Pin;
--- Prelude alias for core CString.
export let CString := core.CString;
--- Prelude alias for core CPtr.
export let CPtr := core.CPtr;

-- --- Common named data types ---
--- Prelude optional value type.
export let Option := option.Option;
--- Prelude recoverable result type.
export let Result := result.Result;
--- Prelude comparison ordering type.
export let Ordering := cmp.Ordering;
--- Prelude list type.
export let List := collections.list.List;
--- Prelude path type.
export let Path := path.Path;
--- Prelude bytes type.
export let Bytes := bytes.Bytes;

-- --- Common constructors ---
--- Build a present option.
export let someOf [T] (value : T) : Option[T] := option.someOf[T](value);
--- Build an absent option.
export let noneOf [T] () : Option[T] := option.noneOf[T]();
--- Build a successful result.
export let ok [T, E] (value : T) : Result[T, E] := result.ok[T, E](value);
--- Build a failed result.
export let err [T, E] (error : E) : Result[T, E] := result.err[T, E](error);
--- Abort current computation.
export let abort () : Empty := core.abort();

--- Prelude abort effect contract.
export opaque let Abort := core.Abort;
--- Prelude rangeable shape.
export opaque let Rangeable := core.Rangeable;
--- Prelude range bounds shape.
export opaque let RangeBounds := core.RangeBounds;
