export let empty[T] () : []T := [];

export let once[T] (value : T) : []T := [value];

export let concat[T] (left : []T, right : []T) : []T := [...left, ...right];

export let append[T] (iter : []T, value : T) : []T := [...iter, value];

export let prepend[T] (value : T, iter : []T) : []T := [value, ...iter];

export let collect[T] (iter : []T) : []T := [...iter];

export let equalsInt (left : []Int, right : []Int) : Bool := left = right;

export let isEmpty[T] (iter : []T) : Bool :=
  match iter (
  | [] => 0 = 0
  | _ => 0 = 1
  );

export let invert (value : Bool) : Bool :=
  match value (
  | _ if value => 0 = 1
  | _ => 0 = 0
  );

export let nonEmpty[T] (iter : []T) : Bool := invert(isEmpty[T](iter));
