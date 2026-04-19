--- Optional value with either a present value or no value.
export opaque let Option [T] := data {
  | Some(T)
  | None
};

--- Build a present optional value.
export let makeSome [T] (value : T) : Option[T] := .Some(value);

--- Build an absent optional value.
export let makeNone [T] () : Option[T] := .None;

--- Internal implementation for `isSome`.
export let isSome [T] (target : Option[T]) : Bool :=
  match target (
  | .Some(_) => 0 = 0
  | .None => 0 = 1
  );

--- Internal implementation for `isNone`.
export let isNone [T] (target : Option[T]) : Bool :=
  match target (
  | .Some(_) => 0 = 1
  | .None => 0 = 0
  );

--- Internal implementation for `unwrapOr`.
export let unwrapOr [T] (target : Option[T], fallback : T) : T :=
  match target (
  | .Some(item) => item
  | .None => fallback
  );

--- Internal implementation for `unwrapOrElse`.
export let unwrapOrElse [T] (target : Option[T], fallback : T) : T :=
  match target (
  | .Some(item) => item
  | .None => fallback
  );

--- Internal implementation for `map`.
export let map [T, U] (target : Option[T], f : T -> U) : Option[U] :=
  match target (
  | .Some(item) => makeSome[U](f(item))
  | .None => makeNone[U]()
  );

--- Internal implementation for `mapOr`.
export let mapOr [T, U] (target : Option[T], fallback : U, f : T -> U) : U
  :=
  match target (
  | .Some(item) => f(item)
  | .None => fallback
  );

--- Internal implementation for `mapOrElse`.
export let mapOrElse [T, U] (target : Option[T], fallback : U, f : T -> U) : U
  :=
  match target (
  | .Some(item) => f(item)
  | .None => fallback
  );

--- Internal implementation for `andThen`.
export let andThen [T, U] (target : Option[T], f : T -> Option[U]) : Option[U]
  :=
  match target (
  | .Some(item) => f(item)
  | .None => makeNone[U]()
  );

--- Internal implementation for `orElse`.
export let orElse [T] (target : Option[T], fallback : Option[T]) : Option[T]
  :=
  match target (
  | .Some(_) => target
  | .None => fallback
  );

--- Internal implementation for `filter`.
export let filter [T] (target : Option[T], predicate : T -> Bool) : Option[T]
  :=
  match target (
  | .Some(item)if predicate(item) => target
  | .Some(_) => makeNone[T]()
  | .None => makeNone[T]()
  );

--- Internal implementation for `fold`.
export let fold [T, U] (
  target : Option[T],
  onNone : U,
  onSome : T -> U
) : U := match target (
| .Some(item) => onSome(item)
| .None => onNone
);
