--! Maybe value helpers for Some or None.

--- Maybe value with either Some value or None.
export hidden let Maybe [T] := data {
  | Some(T)
  | None
};

--- Build Some maybe value.
export let Some [T] (value : T) : Maybe[T] := .Some(value);

--- Build None maybe value.
export let None [T] () : Maybe[T] := .None;

--- Function for `isSome`.
let maybeIsSome [T] (target : Maybe[T]) :=
  match target (
  | .Some(_) => 0 = 0
  | .None => 0 = 1
  );

--- Returns true when target contains a value.
export let isSome [T] (target : Maybe[T]) := maybeIsSome[T](target);

--- Function for `isNone`.
let maybeIsNone [T] (target : Maybe[T]) :=
  match target (
  | .Some(_) => 0 = 1
  | .None => 0 = 0
  );

--- Returns true when target is None.
export let isNone [T] (target : Maybe[T]) := maybeIsNone[T](target);

--- Function for `unwrapOr`.
let maybeUnwrapOr [T] (target : Maybe[T], fallback : T) :=
  match target (
  | .Some(item) => item
  | .None => fallback
  );

--- Return contained Some value, or fallback when target is None.
export let unwrapOr [T] (target : Maybe[T], fallback : T) :=
  maybeUnwrapOr[T](target, fallback);

--- Function for `unwrapOrElse`.
let maybeUnwrapOrElse [T] (target : Maybe[T], fallback : () -> T) :=
  match target (
  | .Some(item) => item
  | .None => fallback()
  );

--- Return contained Some value, or compute fallback when target is None.
export let unwrapOrElse [T] (target : Maybe[T], fallback : () -> T) :=
  maybeUnwrapOrElse[T](target, fallback);

--- Function for `map`.
let maybeMap [T, U] (target : Maybe[T], f : T -> U) :=
  match target (
  | .Some(item) => Some[U](f(item))
  | .None => None[U]()
  );

--- Transform a Some value.
export let map [T, U] (target : Maybe[T], f : T -> U) :=
  maybeMap[T, U](target, f);

--- Function for `mapOr`.
let maybeMapOr [T, U] (target : Maybe[T], fallback : U, f : T -> U) :=
  match target (
  | .Some(item) => f(item)
  | .None => fallback
  );

--- Transform a Some value, or return fallback when target is None.
export let mapOr [T, U] (target : Maybe[T], fallback : U, f : T -> U) :=
  maybeMapOr[T, U](target, fallback, f);

--- Function for `mapOrElse`.
let maybeMapOrElse [T, U] (target : Maybe[T], fallback : () -> U, f : T -> U) :=
  match target (
  | .Some(item) => f(item)
  | .None => fallback()
  );

--- Transform a Some value, or compute fallback when target is None.
export let mapOrElse [T, U] (
  target : Maybe[T],
  fallback : () -> U,
  f : T -> U
) := maybeMapOrElse[T, U](target, fallback, f);

--- Function for `andThen`.
let maybeAndThen [T, U] (target : Maybe[T], f : T -> Maybe[U]) :=
  maybeMapOrElse[T, Maybe[U]](target, \() => None[U](), f);

--- Chain an operation that may return None.
export let andThen [T, U] (target : Maybe[T], f : T -> Maybe[U]) :=
  maybeAndThen[T, U](target, f);

--- Function for `orElse`.
let maybeOrElse [T] (target : Maybe[T], fallback : Maybe[T]) :=
  match target (
  | .Some(_) => target
  | .None => fallback
  );

--- Return target when it is Some, or fallback when target is None.
export let orElse [T] (target : Maybe[T], fallback : Maybe[T]) :=
  maybeOrElse[T](target, fallback);

--- Function for `filter`.
let maybeFilter [T] (target : Maybe[T], predicate : T -> Bit) :=
  match target (
  | .Some(item)where predicate(item) => target
  | .Some(_) => None[T]()
  | .None => None[T]()
  );

--- Keep a Some value only when predicate passes.
export let filter [T] (target : Maybe[T], predicate : T -> Bit) :=
  maybeFilter[T](target, predicate);

--- Function for `fold`.
let maybeFold [T, U] (target : Maybe[T], onNone : U, onSome : T -> U) :=
  maybeMapOrElse[T, U](target, \() => onNone, onSome);

--- Transform Some or None into one value.
export let fold [T, U] (target : Maybe[T], onNone : U, onSome : T -> U) :=
  maybeFold[T, U](target, onNone, onSome);

--- Returns true when the maybe contains a value.
export let (self : Maybe[T]).isSome [T] () := maybeIsSome[T](self);

--- Returns true when the maybe contains None value.
export let (self : Maybe[T]).isNone [T] () := maybeIsNone[T](self);

--- Return the contained value or the fallback.
export let (self : Maybe[T]).unwrapOr [T] (fallback : T) :=
  maybeUnwrapOr[T](self, fallback);

--- Return the contained value or compute the fallback.
export let (self : Maybe[T]).unwrapOrElse [T] (fallback : () -> T) :=
  maybeUnwrapOrElse[T](self, fallback);

--- Transform a Some value.
export let (self : Maybe[T]).map [T, U] (f : T -> U) := maybeMap[T, U](self, f);

--- Transform a Some value or return the fallback.
export let (self : Maybe[T]).mapOr [T, U] (fallback : U, f : T -> U) :=
  maybeMapOr[T, U](self, fallback, f);

--- Transform a Some value or compute the fallback.
export let (self : Maybe[T]).mapOrElse [T, U] (
  fallback : () -> U,
  f : T -> U
) := maybeMapOrElse[T, U](self, fallback, f);

--- Chain an operation that may return None.
export let (self : Maybe[T]).andThen [T, U] (f : T -> Maybe[U]) :=
  maybeAndThen[T, U](self, f);

--- Return this maybe or a fallback maybe.
export let (self : Maybe[T]).orElse [T] (fallback : Maybe[T]) :=
  maybeOrElse[T](self, fallback);

--- Keep a Some value only when the predicate passes.
export let (self : Maybe[T]).filter [T] (predicate : T -> Bit) :=
  maybeFilter[T](self, predicate);

--- Fold both maybe cases into one value.
export let (self : Maybe[T]).fold [T, U] (onNone : U, onSome : T -> U) :=
  maybeFold[T, U](self, onNone, onSome);
