--- Optional value with either a present value or no value.
export opaque let Option [T] := data {
  | Some(T)
  | None
};

--- Build a present optional value.
export let someOf [T] (value : T) : Option[T] := .Some(value);

--- Build an absent optional value.
export let noneOf [T] () : Option[T] := .None;

--- Core operation for `isSome`.
let optionIsSome [T] (target : Option[T]) : Bool :=
  match target (
  | .Some(_) => 0 = 0
  | .None => 0 = 1
  );

--- Core operation for `isSome`.
export let isSome [T] (target : Option[T]) : Bool := optionIsSome[T](target);

--- Core operation for `isNone`.
let optionIsNone [T] (target : Option[T]) : Bool :=
  match target (
  | .Some(_) => 0 = 1
  | .None => 0 = 0
  );

--- Core operation for `isNone`.
export let isNone [T] (target : Option[T]) : Bool := optionIsNone[T](target);

--- Core operation for `unwrapOr`.
let optionUnwrapOr [T] (target : Option[T], fallback : T) : T :=
  match target (
  | .Some(item) => item
  | .None => fallback
  );

--- Core operation for `unwrapOr`.
export let unwrapOr [T] (target : Option[T], fallback : T) : T :=
  optionUnwrapOr[T](target, fallback);

--- Core operation for `unwrapOrElse`.
let optionUnwrapOrElse [T] (target : Option[T], fallback : T) : T :=
  match target (
  | .Some(item) => item
  | .None => fallback
  );

--- Core operation for `unwrapOrElse`.
export let unwrapOrElse [T] (target : Option[T], fallback : T) : T :=
  optionUnwrapOrElse[T](target, fallback);

--- Core operation for `map`.
let optionMap [T, U] (target : Option[T], f : T -> U) : Option[U] :=
  match target (
  | .Some(item) => someOf[U](f(item))
  | .None => noneOf[U]()
  );

--- Core operation for `map`.
export let map [T, U] (target : Option[T], f : T -> U) : Option[U] :=
  optionMap[T, U](target, f);

--- Core operation for `mapOr`.
let optionMapOr [T, U] (target : Option[T], fallback : U, f : T -> U) : U :=
  match target (
  | .Some(item) => f(item)
  | .None => fallback
  );

--- Core operation for `mapOr`.
export let mapOr [T, U] (target : Option[T], fallback : U, f : T -> U) : U :=
  optionMapOr[T, U](target, fallback, f);

--- Core operation for `mapOrElse`.
let optionMapOrElse [T, U] (target : Option[T], fallback : U, f : T -> U) : U :=
  match target (
  | .Some(item) => f(item)
  | .None => fallback
  );

--- Core operation for `mapOrElse`.
export let mapOrElse [T, U] (
  target : Option[T],
  fallback : U,
  f : T -> U
) : U := optionMapOrElse[T, U](target, fallback, f);

--- Core operation for `andThen`.
let optionAndThen [T, U] (target : Option[T], f : T -> Option[U]) : Option[U] :=
  match target (
  | .Some(item) => f(item)
  | .None => noneOf[U]()
  );

--- Core operation for `andThen`.
export let andThen [T, U] (
  target : Option[T],
  f : T -> Option[U]
) : Option[U] := optionAndThen[T, U](target, f);

--- Core operation for `orElse`.
let optionOrElse [T] (target : Option[T], fallback : Option[T]) : Option[T] :=
  match target (
  | .Some(_) => target
  | .None => fallback
  );

--- Core operation for `orElse`.
export let orElse [T] (target : Option[T], fallback : Option[T]) : Option[T] :=
  optionOrElse[T](target, fallback);

--- Core operation for `filter`.
let optionFilter [T] (target : Option[T], predicate : T -> Bool) : Option[T] :=
  match target (
  | .Some(item)if predicate(item) => target
  | .Some(_) => noneOf[T]()
  | .None => noneOf[T]()
  );

--- Core operation for `filter`.
export let filter [T] (target : Option[T], predicate : T -> Bool) : Option[T] :=
  optionFilter[T](target, predicate);

--- Core operation for `fold`.
let optionFold [T, U] (
  target : Option[T],
  onNone : U,
  onSome : T -> U
) : U := match target (
| .Some(item) => onSome(item)
| .None => onNone
);

--- Core operation for `fold`.
export let fold [T, U] (
  target : Option[T],
  onNone : U,
  onSome : T -> U
) : U := optionFold[T, U](target, onNone, onSome);

--- Returns true when the option contains a value.
export let (self : Option[T]).isSome [T] () : Bool := optionIsSome[T](self);

--- Returns true when the option contains no value.
export let (self : Option[T]).isNone [T] () : Bool := optionIsNone[T](self);

--- Return the contained value or the fallback.
export let (self : Option[T]).unwrapOr [T] (fallback : T) : T :=
  optionUnwrapOr[T](self, fallback);

--- Return the contained value or compute the fallback.
export let (self : Option[T]).unwrapOrElse [T] (fallback : T) : T :=
  optionUnwrapOrElse[T](self, fallback);

--- Transform a present value.
export let (self : Option[T]).map [T, U] (f : T -> U) : Option[U] :=
  optionMap[T, U](self, f);

--- Transform a present value or return the fallback.
export let (self : Option[T]).mapOr [T, U] (fallback : U, f : T -> U) : U :=
  optionMapOr[T, U](self, fallback, f);

--- Transform a present value or compute the fallback.
export let (self : Option[T]).mapOrElse [T, U] (fallback : U, f : T -> U) : U :=
  optionMapOrElse[T, U](self, fallback, f);

--- Chain an operation that may return none.
export let (self : Option[T]).andThen [T, U] (f : T -> Option[U]) : Option[U] :=
  optionAndThen[T, U](self, f);

--- Return this option or a fallback option.
export let (self : Option[T]).orElse [T] (fallback : Option[T]) : Option[T] :=
  optionOrElse[T](self, fallback);

--- Keep a present value only when the predicate passes.
export let (self : Option[T]).filter [T] (predicate : T -> Bool) : Option[T] :=
  optionFilter[T](self, predicate);

--- Fold both option cases into one value.
export let (self : Option[T]).fold [T, U] (onNone : U, onSome : T -> U) : U :=
  optionFold[T, U](self, onNone, onSome);
