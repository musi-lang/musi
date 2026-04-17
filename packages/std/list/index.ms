/// Ordered list backed by an array.
export opaque let List[T] := data {
  | List(values : []T)
};

/// Empty list.
export let empty[T] () : List[T] := .List(values := []);

/// Single-value list.
export let of[T] (value : T) : List[T] := .List(values := [value]);

/// Two-value list.
export let pair[T] (first : T, second : T) : List[T] := .List(values := [first, second]);

/// Build a list from an array copy.
export let fromArray[T] (values : []T) : List[T] := .List(values := [...values]);

/// Return a copy of list values.
export let (self : List[T]).toArray[T] () : []T :=
  match self (
  | .List(values := values) => [...values]
  );

/// Copy a list.
export let (self : List[T]).copy[T] () : List[T] := fromArray[T](toArray[T](self));

/// Concatenate two lists.
export let (self : List[T]).concat[T] (other : List[T]) : List[T] := fromArray[T]([...toArray[T](self), ...toArray[T](other)]);

/// Append one value.
export let (self : List[T]).append[T] (value : T) : List[T] := fromArray[T]([...toArray[T](self), value]);

/// Prepend one value.
export let (self : List[T]).prepend[T] (value : T) : List[T] := fromArray[T]([value, ...toArray[T](self)]);

/// Compare integer lists.
export let (self : List[Int]).equalsInt (other : List[Int]) : Bool := toArray[Int](self) = toArray[Int](other);

/// Returns true when list is empty.
export let (self : List[T]).isEmpty[T] () : Bool :=
  match toArray[T](self) (
  | [] => 0 = 0
  | _ => 0 = 1
  );

/// Returns true when list has values.
export let (self : List[T]).nonEmpty[T] () : Bool :=
  match toArray[T](self) (
  | [] => 0 = 1
  | _ => 0 = 0
  );
