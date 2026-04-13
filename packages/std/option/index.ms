export opaque let Option[T] := data {
  | Some : T
  | None
};

export let some[T] (value : T) : Option[T] := .Some(value);

export let none[T] () : Option[T] := .None;

export let isSome[T] (value : Option[T]) : Bool :=
  case value of (
  | .Some(item) => 0 = 0
  | .None => 0 = 1
  );

export let isNone[T] (value : Option[T]) : Bool :=
  case value of (
  | .Some(item) => 0 = 1
  | .None => 0 = 0
  );

export let unwrapOr[T] (value : Option[T], fallback : T) : T :=
  case value of (
  | .Some(item) => item
  | .None => fallback
  );

export let unwrapOrElse[T] (value : Option[T], fallback : T) : T := unwrapOr(value, fallback);

export let map[T, U] (value : Option[T], f : T -> U) : Option[U] :=
  case value of (
  | .Some(item) => some[U](f(item))
  | .None => none[U]()
  );

export let mapOr[T, U] (value : Option[T], fallback : U, f : T -> U) : U :=
  case value of (
  | .Some(item) => f(item)
  | .None => fallback
  );

export let andThen[T, U] (value : Option[T], f : T -> Option[U]) : Option[U] :=
  case value of (
  | .Some(item) => f(item)
  | .None => none[U]()
  );

export let orElse[T] (value : Option[T], fallback : Option[T]) : Option[T] :=
  case value of (
  | .Some(item) => value
  | .None => fallback
  );

export let filter[T] (value : Option[T], predicate : T -> Bool) : Option[T] :=
  case value of (
  | .Some(item) if predicate(item) => value
  | .Some(item) => none[T]()
  | .None => none[T]()
  );

export let match[T, U] (value : Option[T], onNone : U, onSome : T -> U) : U :=
  case value of (
  | .Some(item) => onSome(item)
  | .None => onNone
  );
