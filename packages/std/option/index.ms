let core := import "./_core.ms";

--- Optional value with either a present value or no value.
export let Option := core.Option;

--- Build a present optional value.
export let makeSome := core.makeSome;

--- Build an absent optional value.
export let makeNone := core.makeNone;

--- Returns true when the option contains a value.
export let(self : Option[T]).isSome [T] () : Bool := core.isSome[T](self);

--- Returns true when the option contains no value.
export let(self : Option[T]).isNone [T] () : Bool := core.isNone[T](self);

--- Return the contained value or the fallback.
export let(self : Option[T]).unwrapOr [T] (fallback : T) : T :=
    core.unwrapOr[T](self, fallback);

--- Return the contained value or compute the fallback.
export let(self : Option[T]).unwrapOrElse [T] (fallback : T) : T :=
    core.unwrapOrElse[T](self, fallback);

--- Transform a present value.
export let(self : Option[T]).map [T, U] (f : T -> U) : Option[U] :=
    core.map[T, U](self, f);

--- Transform a present value or return the fallback.
export let(self : Option[T]).mapOr [T, U] (fallback : U, f : T -> U) : U :=
    core.mapOr[T, U](self, fallback, f);

--- Transform a present value or compute the fallback.
export let(self : Option[T]).mapOrElse [T, U] (fallback : U, f : T -> U) : U
    :=
    core.mapOrElse[T, U](self, fallback, f);

--- Chain an operation that may return none.
export let(self : Option[T]).andThen [T, U] (f : T -> Option[U]) : Option[U]
    :=
    core.andThen[T, U](self, f);

--- Return this option or a fallback option.
export let(self : Option[T]).orElse [T] (fallback : Option[T]) : Option[T]
    :=
    core.orElse[T](self, fallback);

--- Keep a present value only when the predicate passes.
export let(self : Option[T]).filter [T] (predicate : T -> Bool) : Option[T]
    :=
    core.filter[T](self, predicate);

--- Fold both option cases into one value.
export let(self : Option[T]).fold [T, U] (onNone : U, onSome : T -> U) : U
    :=
    core.fold[T, U](self, onNone, onSome);
