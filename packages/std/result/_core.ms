--- Recoverable success or failure value.
export opaque let Result [T, E] := data {
  | Ok(T)
  | Err(E)
};

--- Build a successful result.
export let ok [T, E] (value : T) : Result[T, E] := .Ok(value);

--- Build a failed result.
export let err [T, E] (value : E) : Result[T, E] := .Err(value);

--- Internal implementation for `isOk`.
export let isOk [T, E] (target : Result[T, E]) : Bool := match target (
  | .Ok(_) => 0 = 0
  | .Err(_) => 0 = 1
);

--- Internal implementation for `isErr`.
export let isErr [T, E] (target : Result[T, E]) : Bool := match target (
  | .Ok(_) => 0 = 1
  | .Err(_) => 0 = 0
);

--- Internal implementation for `unwrapOr`.
export let unwrapOr [T, E] (target : Result[T, E], fallback : T) : T := match target (
  | .Ok(item) => item
  | .Err(_) => fallback
);

--- Internal implementation for `unwrapOrElse`.
export let unwrapOrElse [T, E] (target : Result[T, E], fallback : E -> T) : T := match target (
  | .Ok(item) => item
  | .Err(error) => fallback(error)
);

--- Internal implementation for `map`.
export let map [T, E, U] (target : Result[T, E], f : T -> U) : Result[U, E] := match target (
  | .Ok(item) => ok[U, E](f(item))
  | .Err(error) => err[U, E](error)
);

--- Internal implementation for `mapErr`.
export let mapErr [T, E, F] (target : Result[T, E], f : E -> F) : Result[T, F] := match target (
  | .Ok(item) => ok[T, F](item)
  | .Err(error) => err[T, F](f(error))
);

--- Internal implementation for `mapOr`.
export let mapOr [T, E, U] (target : Result[T, E], fallback : U, f : T -> U) : U := match target (
  | .Ok(item) => f(item)
  | .Err(_) => fallback
);

--- Internal implementation for `mapOrElse`.
export let mapOrElse [T, E, U] (target : Result[T, E], onErr : E -> U,
  onOk : T -> U
) : U := match target (
  | .Ok(item) => onOk(item)
  | .Err(error) => onErr(error)
);

--- Internal implementation for `andThen`.
export let andThen [T, E, U] (target : Result[T, E], f : T -> Result[U, E]) : Result[U, E] := match target (
  | .Ok(item) => f(item)
  | .Err(error) => err[U, E](error)
);

--- Internal implementation for `orElse`.
export let orElse [T, E, F] (target : Result[T, E], f : E -> Result[T, F]) : Result[T, F] := match target (
  | .Ok(item) => ok[T, F](item)
  | .Err(error) => f(error)
);

--- Internal implementation for `fold`.
export let fold [T, E, U] (target : Result[T, E], onOk : T -> U, onErr : E -> U) : U := match target (
  | .Ok(item) => onOk(item)
  | .Err(error) => onErr(error)
);
