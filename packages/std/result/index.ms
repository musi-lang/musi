/// Recoverable success or failure value.
export opaque let Result[T, E] := data {
  | Ok(T)
  | Err(E)
};

/// Build a successful result.
export let ok[T, E] (value : T) : Result[T, E] := .Ok(value);

/// Build a failed result.
export let err[T, E] (value : E) : Result[T, E] := .Err(value);

/// Returns true for successful results.
export let (self : Result[T, E]).isOk[T, E] () : Bool :=
  match self (
  | .Ok(_) => 0 = 0
  | .Err(_) => 0 = 1
  );

/// Returns true for failed results.
export let (self : Result[T, E]).isErr[T, E] () : Bool :=
  match self (
  | .Ok(_) => 0 = 1
  | .Err(_) => 0 = 0
  );

/// Return the success value or the fallback.
export let (self : Result[T, E]).unwrapOr[T, E] (fallback : T) : T :=
  match self (
  | .Ok(item) => item
  | .Err(_) => fallback
  );

/// Return the success value or compute from the error.
export let (self : Result[T, E]).unwrapOrElse[T, E] (fallback : E -> T) : T :=
  match self (
  | .Ok(item) => item
  | .Err(error) => fallback(error)
  );

/// Transform a success value.
export let (self : Result[T, E]).map[T, E, U] (f : T -> U) : Result[U, E] :=
  match self (
  | .Ok(item) => ok[U, E](f(item))
  | .Err(error) => err[U, E](error)
  );

/// Transform an error value.
export let (self : Result[T, E]).mapErr[T, E, F] (f : E -> F) : Result[T, F] :=
  match self (
  | .Ok(item) => ok[T, F](item)
  | .Err(error) => err[T, F](f(error))
  );

/// Transform a success value or return the fallback.
export let (self : Result[T, E]).mapOr[T, E, U] (fallback : U, f : T -> U) : U :=
  match self (
  | .Ok(item) => f(item)
  | .Err(_) => fallback
  );

/// Transform either result case into one value.
export let (self : Result[T, E]).mapOrElse[T, E, U] (onErr : E -> U, onOk : T -> U) : U :=
  match self (
  | .Ok(item) => onOk(item)
  | .Err(error) => onErr(error)
  );

/// Chain an operation that may fail.
export let (self : Result[T, E]).andThen[T, E, U] (f : T -> Result[U, E]) : Result[U, E] :=
  match self (
  | .Ok(item) => f(item)
  | .Err(error) => err[U, E](error)
  );

/// Recover from an error with another result.
export let (self : Result[T, E]).orElse[T, E, F] (f : E -> Result[T, F]) : Result[T, F] :=
  match self (
  | .Ok(item) => ok[T, F](item)
  | .Err(error) => f(error)
  );

/// Fold both result cases into one value.
export let (self : Result[T, E]).fold[T, E, U] (onOk : T -> U, onErr : E -> U) : U :=
  match self (
  | .Ok(item) => onOk(item)
  | .Err(error) => onErr(error)
  );
