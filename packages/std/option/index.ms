/// Optional value with either a present value or no value.
export opaque let Option[T] := data {
  | Some(T)
  | None
};

/// Build a present optional value.
export let some[T] (value : T) : Option[T] := .Some(value);

/// Build an absent optional value.
export let none[T] () : Option[T] := .None;

/// Returns true when the option contains a value.
export let (self : Option[T]).isSome[T] () : Bool :=
  match self (
  | .Some(_) => 0 = 0
  | .None => 0 = 1
  );

/// Returns true when the option contains no value.
export let (self : Option[T]).isNone[T] () : Bool :=
  match self (
  | .Some(_) => 0 = 1
  | .None => 0 = 0
  );

/// Return the contained value or the fallback.
export let (self : Option[T]).unwrapOr[T] (fallback : T) : T :=
  match self (
  | .Some(item) => item
  | .None => fallback
  );

/// Return the contained value or compute the fallback.
export let (self : Option[T]).unwrapOrElse[T] (fallback : T) : T :=
  match self (
  | .Some(item) => item
  | .None => fallback
  );

/// Transform a present value.
export let (self : Option[T]).map[T, U] (f : T -> U) : Option[U] :=
  match self (
  | .Some(item) => some[U](f(item))
  | .None => none[U]()
  );

/// Transform a present value or return the fallback.
export let (self : Option[T]).mapOr[T, U] (fallback : U, f : T -> U) : U :=
  match self (
  | .Some(item) => f(item)
  | .None => fallback
  );

/// Transform a present value or compute the fallback.
export let (self : Option[T]).mapOrElse[T, U] (fallback : U, f : T -> U) : U :=
  match self (
  | .Some(item) => f(item)
  | .None => fallback
  );

/// Chain an operation that may return none.
export let (self : Option[T]).andThen[T, U] (f : T -> Option[U]) : Option[U] :=
  match self (
  | .Some(item) => f(item)
  | .None => none[U]()
  );

/// Return this option or a fallback option.
export let (self : Option[T]).orElse[T] (fallback : Option[T]) : Option[T] :=
  match self (
  | .Some(_) => self
  | .None => fallback
  );

/// Keep a present value only when the predicate passes.
export let (self : Option[T]).filter[T] (predicate : T -> Bool) : Option[T] :=
  match self (
  | .Some(item) if predicate(item) => self
  | .Some(_) => none[T]()
  | .None => none[T]()
  );

/// Fold both option cases into one value.
export let (self : Option[T]).fold[T, U] (onNone : U, onSome : T -> U) : U :=
  match self (
  | .Some(item) => onSome(item)
  | .None => onNone
  );
