--- Byte buffer backed by integer byte values.
export opaque let Bytes := data {
  | Bytes(values : []Int)
};

--- Empty byte buffer.
export let empty () : Bytes := .Bytes(values := []);

--- Build bytes from raw byte values.
export let fromArray (values : []Int) : Bytes := .Bytes(values := [...values]);

--- Internal implementation for `toArray`.
export let toArray (target : Bytes) : []Int := match target (
  | .Bytes(values := values) =>[...values]
);

--- Internal implementation for `copy`.
export let copy (target : Bytes) : Bytes := fromArray(toArray(target));

--- Internal implementation for `concat`.
export let concat (target : Bytes, other : Bytes) : Bytes := fromArray([...toArray(target), ...toArray(other)]);

--- Internal implementation for `append`.
export let append (target : Bytes, value : Int) : Bytes := fromArray([...toArray(target), value]);

--- Internal implementation for `prepend`.
export let prepend (target : Bytes, value : Int) : Bytes := fromArray([value, ...toArray(target)]);

--- Internal implementation for `equals`.
export let equals (target : Bytes, other : Bytes) : Bool := toArray(target) = toArray(other);

--- Internal implementation for `notEquals`.
export let notEquals (target : Bytes, other : Bytes) : Bool := toArray(target) /= toArray(other);

--- Internal implementation for `isEmpty`.
export let isEmpty (target : Bytes) : Bool := match toArray(target) (
  |[] => 0 = 0
  | _ => 0 = 1
);

--- Internal implementation for `nonEmpty`.
export let nonEmpty (target : Bytes) : Bool := match toArray(target) (
  |[] => 0 = 1
  | _ => 0 = 0
);
