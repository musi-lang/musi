/// Build an empty sequence.
export let empty[T] () : []T := [];

/// Copy sequence values.
export let copy[T] (values : []T) : []T := [...values];

/// Concatenate two sequences.
export let concat[T] (left : []T, right : []T) : []T := [...left, ...right];

/// Compare integer sequences for equality.
export let equalsInt (left : []Int, right : []Int) : Bool := left = right;

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
