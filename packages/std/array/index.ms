/// Build an empty sequence.
export let empty[T] () : []T := [];

/// Build a one-value array.
export let single[T] (value : T) : []T := [value];

/// Build a two-value array.
export let pair[T] (first : T, second : T) : []T := [first, second];

/// Copy sequence values.
export let copy[T] (values : []T) : []T := [...values];

/// Concatenate two sequences.
export let concat[T] (left : []T, right : []T) : []T := [...left, ...right];

/// Append a trailing value.
export let append[T] (values : []T, value : T) : []T := [...values, value];

/// Prepend a leading value.
export let prepend[T] (value : T, values : []T) : []T := [value, ...values];

/// Compare integer sequences for equality.
export let equalsInt (left : []Int, right : []Int) : Bool := left = right;

/// Compare integer arrays for inequality.
export let notEqualsInt (left : []Int, right : []Int) : Bool := left /= right;

/// Returns true when sequence is empty.
export let isEmpty[T] (values : []T) : Bool :=
  match values (
  | [] => 0 = 0
  | _ => 0 = 1
  );

/// Returns true when sequence has values.
export let nonEmpty[T] (values : []T) : Bool :=
  match values (
  | [] => 0 = 1
  | _ => 0 = 0
  );
