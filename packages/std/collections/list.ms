--- Ordered list backed by an array.
export opaque let List [T] := data {
  | ListValues(values : []T)
};

--- Empty list.
export let empty [T] () : List[T] := .ListValues(values := []);

--- Single-value list.
export let of [T] (value : T) : List[T] := .ListValues(values := [value]);

--- Two-value list.
export let pair [T] (first : T, second : T) : List[T] :=
  .ListValues(values := [first, second]);

--- Build a list from an array copy.
export let fromArray [T] (values : []T) : List[T] :=
  .ListValues(values := [...values]);

--- Core operation for `toArray`.
let listToArray [T] (target : List[T]) : []T :=
  match target (
  | .ListValues(values := values) =>[...values]
  );

--- Core operation for `toArray`.
export let toArray [T] (target : List[T]) : []T := listToArray[T](target);

--- Core operation for `copy`.
let copyList [T] (target : List[T]) : List[T] :=
  fromArray[T](listToArray[T](target));

--- Copy a list.
export let copy [T] (target : List[T]) : List[T] := copyList[T](target);

--- Core operation for `concat`.
let concatList [T] (target : List[T], other : List[T]) : List[T] :=
  fromArray[T]([...listToArray[T](target), ...listToArray[T](other)]);

--- Concatenate two lists.
export let concat [T] (target : List[T], other : List[T]) : List[T] :=
  concatList[T](target, other);

--- Core operation for `append`.
let appendList [T] (target : List[T], value : T) : List[T] :=
  fromArray[T]([...listToArray[T](target), value]);

--- Append one value.
export let append [T] (target : List[T], value : T) : List[T] :=
  appendList[T](target, value);

--- Core operation for `prepend`.
let prependList [T] (target : List[T], value : T) : List[T] :=
  fromArray[T]([value, ...listToArray[T](target)]);

--- Prepend one value.
export let prepend [T] (target : List[T], value : T) : List[T] :=
  prependList[T](target, value);

--- Core operation for `equalsInt`.
let listsEqualInt (target : List[Int], other : List[Int]) : Bool :=
  listToArray[Int](target) = listToArray[Int](other);

--- Compare integer lists.
export let equalsInt (target : List[Int], other : List[Int]) : Bool :=
  listsEqualInt(target, other);

--- Core operation for `isEmpty`.
let listIsEmpty [T] (target : List[T]) : Bool :=
  match listToArray[T](target) (
  | [] => 0 = 0
  | _ => 0 = 1
  );

--- Core operation for `nonEmpty`.
let listNonEmpty [T] (target : List[T]) : Bool :=
  match listToArray[T](target) (
  | [] => 0 = 1
  | _ => 0 = 0
  );

--- Return a copy of list values.
export let (self : List[T]).toArray [T] () : []T := listToArray[T](self);

--- Copy a list.
export let (self : List[T]).copy [T] () : List[T] := copyList[T](self);

--- Concatenate two lists.
export let (self : List[T]).concat [T] (other : List[T]) : List[T] :=
  concatList[T](self, other);

--- Append one value.
export let (self : List[T]).append [T] (value : T) : List[T] :=
  appendList[T](self, value);

--- Prepend one value.
export let (self : List[T]).prepend [T] (value : T) : List[T] :=
  prependList[T](self, value);

--- Compare integer lists.
export let (self : List[Int]).equalsInt (other : List[Int]) : Bool :=
  listsEqualInt(self, other);

--- Returns true when list is empty.
export let isEmpty [T] (target : List[T]) : Bool := listIsEmpty[T](target);

--- Returns true when list is empty.
export let (self : List[T]).isEmpty [T] () : Bool := listIsEmpty[T](self);

--- Returns true when list has values.
export let nonEmpty [T] (target : List[T]) : Bool := listNonEmpty[T](target);

--- Returns true when list has values.
export let (self : List[T]).nonEmpty [T] () : Bool := listNonEmpty[T](self);
