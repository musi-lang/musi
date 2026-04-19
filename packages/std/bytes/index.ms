let core := import "./_core.ms";

--- Byte buffer backed by integer byte values.
export let Bytes := core.Bytes;

--- Empty byte buffer.
export let empty := core.empty;

--- Build bytes from raw byte values.
export let fromArray := core.fromArray;

--- Return a copy of raw byte values.
export let(self : Bytes).toArray () : []Int := core.toArray(self);

--- Copy bytes.
export let(self : Bytes).copy () : Bytes := core.copy(self);

--- Concatenate two byte buffers.
export let(self : Bytes).concat (other : Bytes) : Bytes :=
    core.concat(self, other);

--- Append one byte.
export let(self : Bytes).append (value : Int) : Bytes :=
    core.append(self, value);

--- Prepend one byte.
export let(self : Bytes).prepend (value : Int) : Bytes :=
    core.prepend(self, value);

--- Compare byte buffers.
export let(self : Bytes).equals (other : Bytes) : Bool :=
    core.equals(self, other);

--- Compare byte buffers for inequality.
export let(self : Bytes).notEquals (other : Bytes) : Bool :=
    core.notEquals(self, other);

--- Returns true when byte buffer is empty.
export let(self : Bytes).isEmpty () : Bool := core.isEmpty(self);

--- Returns true when byte buffer has values.
export let(self : Bytes).nonEmpty () : Bool := core.nonEmpty(self);
