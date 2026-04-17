/// Build an empty sequence.
export let empty[T] () : []T := [];

/// Build a one-value iterator.
export let once[T] (value : T) : []T := [value];

/// Concatenate two sequences.
export let concat[T] (left : []T, right : []T) : []T := [...left, ...right];

/// Append a trailing value.
export let append[T] (iter : []T, value : T) : []T := [...iter, value];

/// Prepend a leading value.
export let prepend[T] (value : T, iter : []T) : []T := [value, ...iter];

/// Collect iterator values into an array.
export let collect[T] (iter : []T) : []T := [...iter];

/// Compare integer sequences for equality.
export let equalsInt (left : []Int, right : []Int) : Bool := left = right;

/// Returns true when sequence is empty.
export let isEmpty[T] (iter : []T) : Bool :=
  match iter (
  | [] => 0 = 0
  | _ => 0 = 1
  );

/// Invert a boolean value.
export let invert (value : Bool) : Bool :=
  match value (
  | _ if value => 0 = 1
  | _ => 0 = 0
  );

/// Returns true when sequence has values.
export let nonEmpty[T] (iter : []T) : Bool := invert(isEmpty[T](iter));
