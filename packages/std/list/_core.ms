--- Ordered list backed by an array.
export opaque let List [T] := data {
  | List(values : []T)
};

--- Empty list.
export let empty [T] () : List[T] := .List(values := []);

--- Single-value list.
export let of [T] (value : T) : List[T] := .List(values := [value]);

--- Two-value list.
export let pair [T] (first : T, second : T) : List[T] := .List(values := [first, second]);

--- Build a list from an array copy.
export let fromArray [T] (values : []T) : List[T] := .List(values := [...values]);

--- Internal implementation for `toArray`.
export let toArray [T] (target : List[T]) : []T := match target (
  | .List(values := values) =>[...values]
);

--- Internal implementation for `copy`.
export let copy [T] (target : List[T]) : List[T] := fromArray[T](toArray[T](target));

--- Internal implementation for `concat`.
export let concat [T] (target : List[T], other : List[T]) : List[T] := fromArray[T]([...toArray[T](target), ...toArray[T](other)]);

--- Internal implementation for `append`.
export let append [T] (target : List[T], value : T) : List[T] := fromArray[T]([...toArray[T](target), value]);

--- Internal implementation for `prepend`.
export let prepend [T] (target : List[T], value : T) : List[T] := fromArray[T]([value, ...toArray[T](target)]);

--- Internal implementation for `equalsInt`.
export let equalsInt (target : List[Int], other : List[Int]) : Bool := toArray[Int](target) = toArray[Int](other);

--- Internal implementation for `isEmpty`.
export let isEmpty [T] (target : List[T]) : Bool := match toArray[T](target) (
  |[] => 0 = 0
  | _ => 0 = 1
);

--- Internal implementation for `nonEmpty`.
export let nonEmpty [T] (target : List[T]) : Bool := match toArray[T](target) (
  |[] => 0 = 1
  | _ => 0 = 0
);
