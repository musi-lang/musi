--! List collection helpers.

--- Ordered list backed by an array.
export hidden let List [T] := data {
  | ListValues(values : []T)
};

--- Empty list.
export let empty [T] () := .ListValues(values := []);

--- Single-value list.
export let of [T] (value : T) := .ListValues(values := [value]);

--- Two-value list.
export let pair [T] (first : T, second : T) :=
  .ListValues(values := [first, second]);

--- Build a list from an array copy.
export let fromArray [T] (values : []T) := .ListValues(values := [...values]);

--- Function for `toArray`.
let listToArray [T] (target : List[T]) :=
  match target (
  | .ListValues(values := values) => [...values]
  );

--- Return a copy of list values.
export let toArray [T] (target : List[T]) := listToArray[T](target);

--- Function for `copy`.
let copyList [T] (target : List[T]) := fromArray[T](listToArray[T](target));

--- Copy a list.
export let copy [T] (target : List[T]) := copyList[T](target);

--- Function for `concat`.
let concatList [T] (target : List[T], other : List[T]) :=
  fromArray[T]([...listToArray[T](target), ...listToArray[T](other)]);

--- Concatenate two lists.
export let concat [T] (target : List[T], other : List[T]) :=
  concatList[T](target, other);

--- Function for `append`.
let appendList [T] (target : List[T], value : T) :=
  fromArray[T]([...listToArray[T](target), value]);

--- Append one value.
export let append [T] (target : List[T], value : T) :=
  appendList[T](target, value);

--- Function for `prepend`.
let prependList [T] (target : List[T], value : T) :=
  fromArray[T]([value, ...listToArray[T](target)]);

--- Prepend one value.
export let prepend [T] (target : List[T], value : T) :=
  prependList[T](target, value);

--- Function for `equalsInt`.
let listsEqualInt (target : List[Int], other : List[Int]) :=
  listToArray[Int](target) = listToArray[Int](other);

--- Compare integer lists.
export let equalsInt (target : List[Int], other : List[Int]) :=
  listsEqualInt(target, other);

--- Function for `isEmpty`.
let listIsEmpty [T] (target : List[T]) :=
  if listToArray[T](target) = [] then 0 = 0 else 0 = 1;

--- Function for `nonEmpty`.
let listNonEmpty [T] (target : List[T]) :=
  if listToArray[T](target) = [] then 0 = 1 else 0 = 0;

--- Return a copy of list values.
export let (self : List[T]).toArray [T] () := listToArray[T](self);

--- Copy a list.
export let (self : List[T]).copy [T] () := copyList[T](self);

--- Concatenate two lists.
export let (self : List[T]).concat [T] (other : List[T]) :=
  concatList[T](self, other);

--- Append one value.
export let (self : List[T]).append [T] (value : T) :=
  appendList[T](self, value);

--- Prepend one value.
export let (self : List[T]).prepend [T] (value : T) :=
  prependList[T](self, value);

--- Compare integer lists.
export let (self : List[Int]).equalsInt (other : List[Int]) :=
  listsEqualInt(self, other);

--- Returns true when list is empty.
export let isEmpty [T] (target : List[T]) := listIsEmpty[T](target);

--- Returns true when list is empty.
export let (self : List[T]).isEmpty [T] () := listIsEmpty[T](self);

--- Returns true when list has values.
export let nonEmpty [T] (target : List[T]) := listNonEmpty[T](target);

--- Returns true when list has values.
export let (self : List[T]).nonEmpty [T] () := listNonEmpty[T](self);
