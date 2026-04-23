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
export let toArray (target : Bytes) : []Int :=
  match target (
  | .ByteValues(values := values) =>[...values]
  );

--- Core operation for `copy`.
export let copy (target : Bytes) : Bytes := fromArray(toArray(target));

--- Core operation for `concat`.
export let concat (target : Bytes, other : Bytes) : Bytes :=
  fromArray([...toArray(target), ...toArray(other)]);

--- Core operation for `append`.
export let append (target : Bytes, value : Int) : Bytes :=
  fromArray([...toArray(target), value]);

--- Core operation for `prepend`.
export let prepend (target : Bytes, value : Int) : Bytes :=
  fromArray([value, ...toArray(target)]);

--- Core operation for `equals`.
export let equals (target : Bytes, other : Bytes) : Bool :=
  toArray(target) = toArray(other);

--- Core operation for `notEquals`.
export let notEquals (target : Bytes, other : Bytes) : Bool :=
  toArray(target) /= toArray(other);

--- Core operation for `isEmpty`.
export let isEmpty (target : Bytes) : Bool :=
  match toArray(target) (
  | [] => 0 = 0
  | _ => 0 = 1
  );

--- Core operation for `nonEmpty`.
export let nonEmpty (target : Bytes) : Bool :=
  match toArray(target) (
  | [] => 0 = 1
  | _ => 0 = 0
  );

--- Return a copy of raw byte values.
export let (self : Bytes).toArray () : []Int := toArray(self);

--- Copy bytes.
export let (self : Bytes).copy () : Bytes := copy(self);

--- Concatenate two byte buffers.
export let (self : Bytes).concat (other : Bytes) : Bytes := concat(self, other);

--- Append one byte.
export let (self : Bytes).append (value : Int) : Bytes := append(self, value);

--- Prepend one byte.
export let (self : Bytes).prepend (value : Int) : Bytes := prepend(self, value);

--- Compare byte buffers.
export let (self : Bytes).equals (other : Bytes) : Bool := equals(self, other);

--- Compare byte buffers for inequality.
export let (self : Bytes).notEquals (other : Bytes) : Bool :=
  notEquals(self, other);

--- Returns true when byte buffer is empty.
export let (self : Bytes).isEmpty () : Bool := isEmpty(self);

--- Returns true when byte buffer has values.
export let (self : Bytes).nonEmpty () : Bool := nonEmpty(self);
