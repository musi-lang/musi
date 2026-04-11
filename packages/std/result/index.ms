export let Result[T, E] := data {
    | Ok : T
    | Err : E
};

export let ok [T, E] (value : T) : Result[T, E] := .Ok(value);

export let err [T, E] (value : E) : Result[T, E] := .Err(value);

export let is_ok [T, E] (value : Result[T, E]) : Bool :=
    case value of (
        | .Ok(item) => 0 = 0
        | .Err(error) => 0 = 1
    );

export let is_err [T, E] (value : Result[T, E]) : Bool :=
    case value of (
        | .Ok(item) => 0 = 1
        | .Err(error) => 0 = 0
    );

export let unwrap_or [T, E] (value : Result[T, E], fallback : T) : T :=
    case value of (
        | .Ok(item) => item
        | .Err(error) => fallback
    );
