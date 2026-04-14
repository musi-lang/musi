export opaque let Result[T, E] := data {
  | Ok : T
  | Err : E
};

export let ok[T, E] (value : T) : Result[T, E] := .Ok(value);

export let err[T, E] (value : E) : Result[T, E] := .Err(value);

export let isOk[T, E] (value : Result[T, E]) : Bool :=
  match value (
  | .Ok(item) => 0 = 0
  | .Err(error) => 0 = 1
  );

export let isErr[T, E] (value : Result[T, E]) : Bool :=
  match value (
  | .Ok(item) => 0 = 1
  | .Err(error) => 0 = 0
  );

export let unwrapOr[T, E] (value : Result[T, E], fallback : T) : T :=
  match value (
  | .Ok(item) => item
  | .Err(error) => fallback
  );

export let unwrapOrElse[T, E] (value : Result[T, E], fallback : E -> T) : T :=
  match value (
  | .Ok(item) => item
  | .Err(error) => fallback(error)
  );

export let map[T, E, U] (value : Result[T, E], f : T -> U) : Result[U, E] :=
  match value (
  | .Ok(item) => ok[U, E](f(item))
  | .Err(error) => err[U, E](error)
  );

export let mapErr[T, E, F] (value : Result[T, E], f : E -> F) : Result[T, F] :=
  match value (
  | .Ok(item) => ok[T, F](item)
  | .Err(error) => err[T, F](f(error))
  );

export let andThen[T, E, U] (value : Result[T, E], f : T -> Result[U, E]) : Result[U, E] :=
  match value (
  | .Ok(item) => f(item)
  | .Err(error) => err[U, E](error)
  );

export let orElse[T, E, F] (value : Result[T, E], f : E -> Result[T, F]) : Result[T, F] :=
  match value (
  | .Ok(item) => ok[T, F](item)
  | .Err(error) => f(error)
  );

export let matchResult[T, E, U] (
  value : Result[T, E],
  onOk : T -> U,
  onErr : E -> U,
) : U :=
  match value (
  | .Ok(item) => onOk(item)
  | .Err(error) => onErr(error)
  );
