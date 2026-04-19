let core := import "./_core.ms";

--- Ordered list backed by an array.
export let List := core.List;

--- Empty list.
export let empty := core.empty;

--- Single-value list.
export let of := core.of;

--- Two-value list.
export let pair := core.pair;

--- Build a list from an array copy.
export let fromArray := core.fromArray;

--- Return a copy of list values.
export let(self : List[T]).toArray [T] () : []T := core.toArray[T](self);

--- Copy a list.
export let(self : List[T]).copy [T] () : List[T] := core.copy[T](self);

--- Concatenate two lists.
export let(self : List[T]).concat [T] (other : List[T]) : List[T] :=
    core.concat[T](self, other);

--- Append one value.
export let(self : List[T]).append [T] (value : T) : List[T] :=
    core.append[T](self, value);

--- Prepend one value.
export let(self : List[T]).prepend [T] (value : T) : List[T] :=
    core.prepend[T](self, value);

--- Compare integer lists.
export let(self : List[Int]).equalsInt (other : List[Int]) : Bool :=
    core.equalsInt(self, other);

--- Returns true when list is empty.
export let(self : List[T]).isEmpty [T] () : Bool := core.isEmpty[T](self);

--- Returns true when list has values.
export let(self : List[T]).nonEmpty [T] () : Bool := core.nonEmpty[T](self);
