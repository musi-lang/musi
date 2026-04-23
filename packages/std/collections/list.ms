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
export let toArray [T] (target : List[T]) : []T :=
  match target (
  | .ListValues(values := values) =>[...values]
  );

--- Core operation for `copy`.
export let copy [T] (target : List[T]) : List[T] :=
  fromArray[T](toArray[T](target));

--- Core operation for `concat`.
export let concat [T] (target : List[T], other : List[T]) : List[T] :=
  fromArray[T]([...toArray[T](target), ...toArray[T](other)]);

--- Core operation for `append`.
export let append [T] (target : List[T], value : T) : List[T] :=
  fromArray[T]([...toArray[T](target), value]);

--- Core operation for `prepend`.
export let prepend [T] (target : List[T], value : T) : List[T] :=
  fromArray[T]([value, ...toArray[T](target)]);

--- Core operation for `equalsInt`.
export let equalsInt (target : List[Int], other : List[Int]) : Bool :=
  toArray[Int](target) = toArray[Int](other);

--- Core operation for `isEmpty`.
export let isEmpty [T] (target : List[T]) : Bool :=
  match toArray[T](target) (
  | [] => 0 = 0
  | _ => 0 = 1
  );

--- Core operation for `nonEmpty`.
export let nonEmpty [T] (target : List[T]) : Bool :=
  match toArray[T](target) (
  | [] => 0 = 1
  | _ => 0 = 0
  );

--- Return a copy of list values.
export let (self : List[T]).toArray [T] () : []T := toArray[T](self);

--- Copy a list.
export let (self : List[T]).copy [T] () : List[T] := copy[T](self);

--- Concatenate two lists.
export let (self : List[T]).concat [T] (other : List[T]) : List[T] :=
  concat[T](self, other);

--- Append one value.
export let (self : List[T]).append [T] (value : T) : List[T] :=
  append[T](self, value);

--- Prepend one value.
export let (self : List[T]).prepend [T] (value : T) : List[T] :=
  prepend[T](self, value);

--- Compare integer lists.
export let (self : List[Int]).equalsInt (other : List[Int]) : Bool :=
  equalsInt(self, other);

--- Returns true when list is empty.
export let (self : List[T]).isEmpty [T] () : Bool := isEmpty[T](self);

--- Returns true when list has values.
export let (self : List[T]).nonEmpty [T] () : Bool := nonEmpty[T](self);
