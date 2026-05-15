--! Machine word helpers.

let sys := import "#sys";
let bits := import "@std/bits";
let core := import "musi:core";

let Word8 := core.Word8;
let Word16 := core.Word16;
let Word32 := core.Word32;
let Word64 := core.Word64;

--- Target word width in bits.
export let width := sys.pointerWidth;

--- Build zero-valued 8-bit machine word.
export let zero8 () := bits.zero8();
--- Build zero-valued 16-bit machine word.
export let zero16 () := bits.zero16();
--- Build zero-valued 32-bit machine word.
export let zero32 () := bits.zero32();
--- Build zero-valued 64-bit machine word.
export let zero64 () := bits.zero64();

--- Build all-ones 8-bit machine word.
export let ones8 () := bits.ones8();
--- Build all-ones 16-bit machine word.
export let ones16 () := bits.ones16();
--- Build all-ones 32-bit machine word.
export let ones32 () := bits.ones32();
--- Build all-ones 64-bit machine word.
export let ones64 () := bits.ones64();

--- Convert natural number to 8-bit machine word.
export let fromNat8 (value : Nat) := bits.fromNat8(value);
--- Convert natural number to 16-bit machine word.
export let fromNat16 (value : Nat) := bits.fromNat16(value);
--- Convert natural number to 32-bit machine word.
export let fromNat32 (value : Nat) := bits.fromNat32(value);
--- Convert natural number to 64-bit machine word.
export let fromNat64 (value : Nat) := bits.fromNat64(value);

--- Convert integer to 8-bit machine word.
export let fromInt8 (value : Int) := bits.fromInt8(value);
--- Convert integer to 16-bit machine word.
export let fromInt16 (value : Int) := bits.fromInt16(value);
--- Convert integer to 32-bit machine word.
export let fromInt32 (value : Int) := bits.fromInt32(value);
--- Convert integer to 64-bit machine word.
export let fromInt64 (value : Int) := bits.fromInt64(value);

--- Convert 8-bit machine word to natural number.
export let toNat8 (value : Word8) := bits.toNat8(value);
--- Convert 16-bit machine word to natural number.
export let toNat16 (value : Word16) := bits.toNat16(value);
--- Convert 32-bit machine word to natural number.
export let toNat32 (value : Word32) := bits.toNat32(value);
--- Convert 64-bit machine word to natural number.
export let toNat64 (value : Word64) := bits.toNat64(value);

--- Convert 8-bit machine word to integer.
export let toInt8 (value : Word8) := bits.toInt8(value);
--- Convert 16-bit machine word to integer.
export let toInt16 (value : Word16) := bits.toInt16(value);
--- Convert 32-bit machine word to integer.
export let toInt32 (value : Word32) := bits.toInt32(value);
--- Convert 64-bit machine word to integer.
export let toInt64 (value : Word64) := bits.toInt64(value);

--- Returns true when two 32-bit machine words match.
export let equals32 (left : Word32, right : Word32) :=
  bits.equals32(left, right);

--- Combine 32-bit machine words with bitwise and.
export let logicalAnd32 (left : Word32, right : Word32) :=
  bits.logicalAnd32(left, right);

--- Combine 32-bit machine words with bitwise or.
export let logicalOr32 (left : Word32, right : Word32) :=
  bits.logicalOr32(left, right);

--- Combine 32-bit machine words with bitwise xor.
export let logicalXor32 (left : Word32, right : Word32) :=
  bits.logicalXor32(left, right);

--- Invert 32-bit machine word bits.
export let logicalNot32 (value : Word32) := bits.logicalNot32(value);
