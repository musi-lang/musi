/// Byte buffer backed by integer byte values.
export opaque let Bytes := data {
  | Bytes(values : []Int)
};

/// Empty byte buffer.
export let empty () : Bytes := .Bytes(values := []);

/// Build bytes from raw byte values.
export let fromArray (values : []Int) : Bytes := .Bytes(values := [...values]);

/// Return a copy of raw byte values.
export let (self : Bytes).toArray () : []Int :=
  match self (
  | .Bytes(values := values) => [...values]
  );

/// Copy bytes.
export let (self : Bytes).copy () : Bytes := fromArray(toArray(self));

/// Concatenate two byte buffers.
export let (self : Bytes).concat (other : Bytes) : Bytes := fromArray([...toArray(self), ...toArray(other)]);

/// Append one byte.
export let (self : Bytes).append (value : Int) : Bytes := fromArray([...toArray(self), value]);

/// Prepend one byte.
export let (self : Bytes).prepend (value : Int) : Bytes := fromArray([value, ...toArray(self)]);

/// Compare byte buffers.
export let (self : Bytes).equals (other : Bytes) : Bool := toArray(self) = toArray(other);

/// Compare byte buffers for inequality.
export let (self : Bytes).notEquals (other : Bytes) : Bool := toArray(self) /= toArray(other);

/// Returns true when byte buffer is empty.
export let (self : Bytes).isEmpty () : Bool :=
  match toArray(self) (
  | [] => 0 = 0
  | _ => 0 = 1
  );

/// Returns true when byte buffer has values.
export let (self : Bytes).nonEmpty () : Bool :=
  match toArray(self) (
  | [] => 0 = 1
  | _ => 0 = 0
  );
