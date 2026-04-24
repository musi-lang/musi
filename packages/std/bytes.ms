--- Byte buffer backed by integer byte values.
export opaque let Bytes := data {
  | ByteValues(values : []Int)
};

--- Empty byte buffer.
export let empty () : Bytes := .ByteValues(values := []);

--- Build bytes from raw byte values.
export let fromArray (values : []Int) : Bytes :=
  .ByteValues(values := [...values]);

--- Core operation for `toArray`.
let bytesToArray (target : Bytes) : []Int :=
  match target (
  | .ByteValues(values := values) =>[...values]
  );

--- Core operation for `toArray`.
export let toArray (target : Bytes) : []Int := bytesToArray(target);

--- Core operation for `copy`.
let copyBytes (target : Bytes) : Bytes := fromArray(bytesToArray(target));

--- Copy bytes.
export let copy (target : Bytes) : Bytes := copyBytes(target);

--- Core operation for `concat`.
let concatBytes (target : Bytes, other : Bytes) : Bytes :=
  fromArray([...bytesToArray(target), ...bytesToArray(other)]);

--- Concatenate two byte buffers.
export let concat (target : Bytes, other : Bytes) : Bytes :=
  concatBytes(target, other);

--- Core operation for `append`.
let appendByte (target : Bytes, value : Int) : Bytes :=
  fromArray([...bytesToArray(target), value]);

--- Append one byte.
export let append (target : Bytes, value : Int) : Bytes :=
  appendByte(target, value);

--- Core operation for `prepend`.
let prependByte (target : Bytes, value : Int) : Bytes :=
  fromArray([value, ...bytesToArray(target)]);

--- Prepend one byte.
export let prepend (target : Bytes, value : Int) : Bytes :=
  prependByte(target, value);

--- Core operation for `equals`.
let bytesEqual (target : Bytes, other : Bytes) : Bool :=
  bytesToArray(target) = bytesToArray(other);

--- Compare byte buffers.
export let equals (target : Bytes, other : Bytes) : Bool :=
  bytesEqual(target, other);

--- Core operation for `notEquals`.
let bytesNotEqual (target : Bytes, other : Bytes) : Bool :=
  bytesToArray(target) /= bytesToArray(other);

--- Compare byte buffers for inequality.
export let notEquals (target : Bytes, other : Bytes) : Bool :=
  bytesNotEqual(target, other);

--- Core operation for `isEmpty`.
let bytesIsEmpty (target : Bytes) : Bool :=
  match bytesToArray(target) (
  | [] => 0 = 0
  | _ => 0 = 1
  );

--- Core operation for `nonEmpty`.
let bytesNonEmpty (target : Bytes) : Bool :=
  match bytesToArray(target) (
  | [] => 0 = 1
  | _ => 0 = 0
  );

--- Return a copy of raw byte values.
export let (self : Bytes).toArray () : []Int := bytesToArray(self);

--- Copy bytes.
export let (self : Bytes).copy () : Bytes := copyBytes(self);

--- Concatenate two byte buffers.
export let (self : Bytes).concat (other : Bytes) : Bytes :=
  concatBytes(self, other);

--- Append one byte.
export let (self : Bytes).append (value : Int) : Bytes :=
  appendByte(self, value);

--- Prepend one byte.
export let (self : Bytes).prepend (value : Int) : Bytes :=
  prependByte(self, value);

--- Compare byte buffers.
export let (self : Bytes).equals (other : Bytes) : Bool :=
  bytesEqual(self, other);

--- Compare byte buffers for inequality.
export let (self : Bytes).notEquals (other : Bytes) : Bool :=
  bytesNotEqual(self, other);

--- Returns true when byte buffer is empty.
export let (self : Bytes).isEmpty () : Bool := bytesIsEmpty(self);

--- Returns true when byte buffer has values.
export let (self : Bytes).nonEmpty () : Bool := bytesNonEmpty(self);
