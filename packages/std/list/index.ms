export let empty[T] () : []T := [];

export let of[T] (value : T) : []T := [value];

export let pair[T] (first : T, second : T) : []T := [first, second];

export let copy[T] (values : []T) : []T := [...values];

export let concat[T] (left : []T, right : []T) : []T := [...left, ...right];

export let append[T] (values : []T, value : T) : []T := [...values, value];

export let prepend[T] (value : T, values : []T) : []T := [value, ...values];

export let equalsInt (left : []Int, right : []Int) : Bool := left = right;

export let isEmpty[T] (values : []T) : Bool :=
  match values (
  | [] => 0 = 0
  | _ => 0 = 1
  );

export let nonEmpty[T] (values : []T) : Bool :=
  match values (
  | [] => 0 = 1
  | _ => 0 = 0
  );
