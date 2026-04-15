export opaque let Option[T] := data {
  | Some(T)
  | None
};

export let some[T] (value : T) : Option[T] := .Some(value);

export let none[T] () : Option[T] := .None;

export let isSome[T] (value : Option[T]) : Bool :=
  match value (
  | .Some(_) => 0 = 0
  | .None => 0 = 1
  );

export let invert (value : Bool) : Bool :=
  match value (
  | _ if value => 0 = 1
  | _ => 0 = 0
  );

export let isNone[T] (value : Option[T]) : Bool := invert(isSome[T](value));

export let unwrapOr[T] (value : Option[T], fallback : T) : T :=
  match value (
  | .Some(item) => item
  | .None => fallback
  );

export let unwrapOrElse[T] (value : Option[T], fallback : T) : T :=
  match value (
  | .Some(item) => item
  | .None => fallback
  );

export let map[T, U] (value : Option[T], f : T -> U) : Option[U] :=
  match value (
  | .Some(item) => some[U](f(item))
  | .None => none[U]()
  );

export let mapOr[T, U] (value : Option[T], fallback : U, f : T -> U) : U :=
  match value (
  | .Some(item) => f(item)
  | .None => fallback
  );

export let mapOrElse[T, U] (value : Option[T], fallback : U, f : T -> U) : U :=
  match value (
  | .Some(item) => f(item)
  | .None => fallback
  );

export let andThen[T, U] (value : Option[T], f : T -> Option[U]) : Option[U] :=
  match value (
  | .Some(item) => f(item)
  | .None => none[U]()
  );

export let orElse[T] (value : Option[T], fallback : Option[T]) : Option[T] :=
  match value (
  | .Some(_) => value
  | .None => fallback
  );

export let filter[T] (value : Option[T], predicate : T -> Bool) : Option[T] :=
  match value (
  | .Some(item) if predicate(item) => value
  | .Some(_) => none[T]()
  | .None => none[T]()
  );

export let fold[T, U] (value : Option[T], onNone : U, onSome : T -> U) : U :=
  match value (
  | .Some(item) => onSome(item)
  | .None => onNone
  );
