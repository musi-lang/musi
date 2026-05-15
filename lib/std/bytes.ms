--! Byte sequence helpers.

--- Byte buffer backed by integer byte values.
export hidden let Bytes := data {
  | ByteValues(values : []Int)
};

--- Empty byte buffer.
export let empty () := .ByteValues(values := []);

--- Build bytes from raw byte values.
export let fromArray (values : []Int) := .ByteValues(values := [...values]);

--- Function for `toArray`.
let bytesToArray (target : Bytes) :=
  match target (
  | .ByteValues(values := values) => [...values]
  );

--- Return a copy of raw byte values.
export let toArray (target : Bytes) := bytesToArray(target);

--- Function for `copy`.
let copyBytes (target : Bytes) := fromArray(bytesToArray(target));

--- Copy bytes.
export let copy (target : Bytes) := copyBytes(target);

--- Function for `concat`.
let concatBytes (target : Bytes, other : Bytes) :=
  fromArray([...bytesToArray(target), ...bytesToArray(other)]);

--- Concatenate two byte buffers.
export let concat (target : Bytes, other : Bytes) := concatBytes(target, other);

--- Function for `append`.
let appendByte (target : Bytes, value : Int) :=
  fromArray([...bytesToArray(target), value]);

--- Append one byte.
export let append (target : Bytes, value : Int) := appendByte(target, value);

--- Function for `prepend`.
let prependByte (target : Bytes, value : Int) :=
  fromArray([value, ...bytesToArray(target)]);

--- Prepend one byte.
export let prepend (target : Bytes, value : Int) := prependByte(target, value);

--- Function for `equals`.
let bytesEqual (target : Bytes, other : Bytes) :=
  bytesToArray(target) = bytesToArray(other);

--- Compare byte buffers.
export let equals (target : Bytes, other : Bytes) := bytesEqual(target, other);

--- Function for `notEquals`.
let bytesNotEqual (target : Bytes, other : Bytes) :=
  bytesToArray(target) /= bytesToArray(other);

--- Compare byte buffers for inequality.
export let notEquals (target : Bytes, other : Bytes) :=
  bytesNotEqual(target, other);

--- Function for `isEmpty`.
let bytesIsEmpty (target : Bytes) :=
  if bytesToArray(target) = [] then 0 = 0 else 0 = 1;

--- Function for `nonEmpty`.
let bytesNonEmpty (target : Bytes) :=
  if bytesToArray(target) = [] then 0 = 1 else 0 = 0;

--- Return a copy of raw byte values.
export let (self : Bytes).toArray () := bytesToArray(self);

--- Copy bytes.
export let (self : Bytes).copy () := copyBytes(self);

--- Concatenate two byte buffers.
export let (self : Bytes).concat (other : Bytes) := concatBytes(self, other);

--- Append one byte.
export let (self : Bytes).append (value : Int) := appendByte(self, value);

--- Prepend one byte.
export let (self : Bytes).prepend (value : Int) := prependByte(self, value);

--- Compare byte buffers.
export let (self : Bytes).equals (other : Bytes) := bytesEqual(self, other);

--- Compare byte buffers for inequality.
export let (self : Bytes).notEquals (other : Bytes) :=
  bytesNotEqual(self, other);

--- Returns true when byte buffer is empty.
export let (self : Bytes).isEmpty () := bytesIsEmpty(self);

--- Returns true when byte buffer has values.
export let (self : Bytes).nonEmpty () := bytesNonEmpty(self);
