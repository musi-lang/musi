export let Option[T] := data {
    | Some : T
    | None
};

export let some [T] (value : T) : Option[T] := .Some(value);

export let none [T] () : Option[T] := .None;

export let is_some [T] (value : Option[T]) : Bool :=
    case value of (
        | .Some(item) => 0 = 0
        | .None => 0 = 1
    );

export let is_none [T] (value : Option[T]) : Bool :=
    case value of (
        | .Some(item) => 0 = 1
        | .None => 0 = 0
    );

export let unwrap_or [T] (value : Option[T], fallback : T) : T :=
    case value of (
        | .Some(item) => item
        | .None => fallback
    );
