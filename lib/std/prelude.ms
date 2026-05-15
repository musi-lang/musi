--! Default std prelude exports.

let bytes := import "@std/bytes";
let cmp := import "@std/cmp";
let collections := import "@std/collections";
let expect := import "@std/expect";
let maybe := import "@std/maybe";
let path := import "@std/path";
let core := import "musi:core";

/- === Type universe === -/
--- Type universe value.
export let Type := core.Type;
--- Array type constructor.
export let Array := core.Array;
--- Any value type.
export let Any := core.Any;
--- Unknown value type.
export let Unknown := core.Unknown;
--- Syntax value type.
export let Syntax := core.Syntax;
--- Empty result type.
export let Empty := core.Empty;
--- Unit value type.
export let Unit := core.Unit;
--- Bit condition type.
export let Bit := core.Bit;
--- Natural number type.
export let Nat := core.Nat;
--- Integer type.
export let Int := core.Int;
--- 8-bit signed integer type.
export let Int8 := core.Int8;
--- 16-bit signed integer type.
export let Int16 := core.Int16;
--- 32-bit signed integer type.
export let Int32 := core.Int32;
--- 64-bit signed integer type.
export let Int64 := core.Int64;
--- 8-bit natural number type.
export let Nat8 := core.Nat8;
--- 16-bit natural number type.
export let Nat16 := core.Nat16;
--- 32-bit natural number type.
export let Nat32 := core.Nat32;
--- 64-bit natural number type.
export let Nat64 := core.Nat64;
--- Floating-point number type.
export let Float := core.Float;
--- 32-bit floating-point number type.
export let Float32 := core.Float32;
--- 64-bit floating-point number type.
export let Float64 := core.Float64;
--- Text string type.
export let String := core.String;
--- Unicode scalar value type.
export let Rune := core.Rune;
--- Exact-width bit pattern type constructor.
export let Bits := core.Bits;
--- Target-sized machine word type.
export let Word := core.Word;
--- 8-bit machine word type.
export let Word8 := core.Word8;
--- 16-bit machine word type.
export let Word16 := core.Word16;
--- 32-bit machine word type.
export let Word32 := core.Word32;
--- 64-bit machine word type.
export let Word64 := core.Word64;
--- Range type.
export let Range := core.Range;

/- === Common named data types === -/
--- Prelude maybe value type.
export let Maybe := maybe.Maybe;
--- Prelude expected success or failure type.
export let Expect := expect.Expect;
--- Prelude comparison ordering type.
export let Ordering := cmp.Ordering;
--- Prelude list type.
export let List := collections.list.List;
--- Prelude path type.
export let Path := path.Path;
--- Prelude bytes type.
export let Bytes := bytes.Bytes;
/- === Common constructors === -/
--- Build Some maybe value.
export let Some [T] (value : T) := maybe.Some[T](value);
--- Build None maybe value.
export let None [T] () := maybe.None[T]();
--- Build successful expected value.
export let Success [T, E] (value : T) := expect.Success[T, E](value);
--- Build failed expected value.
export let Failure [T, E] (error : E) := expect.Failure[T, E](error);
--- Abort current computation.
export let abort () := core.abort();

--- Prelude abort capability contract.
export hidden let Abort := core.Abort;
--- Prelude rangeable shape.
export hidden let Rangeable := core.Rangeable;
--- Prelude range bounds shape.
export hidden let RangeBounds := core.RangeBounds;
