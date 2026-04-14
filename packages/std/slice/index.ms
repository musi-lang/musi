export let empty[T] () : []T := [];

export let copy[T] (values : []T) : []T := [...values];

export let concat[T] (left : []T, right : []T) : []T := [...left, ...right];

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
