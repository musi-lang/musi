--- Recoverable success or failure value.
export opaque let Result [T, E] := data {
  | Ok(T)
  | Err(E)
};

--- Build a successful result.
export let ok [T, E] (value : T) : Result[T, E] := .Ok(value);

--- Build a failed result.
export let err [T, E] (value : E) : Result[T, E] := .Err(value);

--- Core operation for `isOk`.
export let isOk [T, E] (target : Result[T, E]) : Bool :=
  match target (
  | .Ok(_) => 0 = 0
  | .Err(_) => 0 = 1
  );

--- Core operation for `isErr`.
export let isErr [T, E] (target : Result[T, E]) : Bool :=
  match target (
  | .Ok(_) => 0 = 1
  | .Err(_) => 0 = 0
  );

--- Core operation for `unwrapOr`.
export let unwrapOr [T, E] (target : Result[T, E], fallback : T) : T :=
  match target (
  | .Ok(item) => item
  | .Err(_) => fallback
  );

--- Core operation for `unwrapOrElse`.
export let unwrapOrElse [T, E] (target : Result[T, E], fallback : E -> T) : T :=
  match target (
  | .Ok(item) => item
  | .Err(error) => fallback(error)
  );

--- Core operation for `map`.
export let map [T, E, U] (target : Result[T, E], f : T -> U) : Result[U, E] :=
  match target (
  | .Ok(item) => ok[U, E](f(item))
  | .Err(error) => err[U, E](error)
  );

--- Core operation for `mapErr`.
export let mapErr [T, E, F] (
  target : Result[T, E],
  f : E -> F
) : Result[T, F] :=
  match target (
  | .Ok(item) => ok[T, F](item)
  | .Err(error) => err[T, F](f(error))
  );

--- Core operation for `mapOr`.
export let mapOr [T, E, U] (
  target : Result[T, E],
  fallback : U,
  f : T -> U
) : U := match target (
| .Ok(item) => f(item)
| .Err(_) => fallback
);

--- Core operation for `mapOrElse`.
export let mapOrElse [T, E, U] (
  target : Result[T, E],
  onErr : E -> U,
  onOk : T -> U
) : U := match target (
| .Ok(item) => onOk(item)
| .Err(error) => onErr(error)
);

--- Core operation for `andThen`.
export let andThen [T, E, U] (
  target : Result[T, E],
  f : T -> Result[U, E]
) : Result[U, E] :=
  match target (
  | .Ok(item) => f(item)
  | .Err(error) => err[U, E](error)
  );

--- Core operation for `orElse`.
export let orElse [T, E, F] (
  target : Result[T, E],
  f : E -> Result[T, F]
) : Result[T, F] :=
  match target (
  | .Ok(item) => ok[T, F](item)
  | .Err(error) => f(error)
  );

--- Core operation for `fold`.
export let fold [T, E, U] (
  target : Result[T, E],
  onOk : T -> U,
  onErr : E -> U
) : U := match target (
| .Ok(item) => onOk(item)
| .Err(error) => onErr(error)
);

--- Returns true for successful results.
export let (self : Result[T, E]).isOk [T, E] () : Bool := isOk[T, E](self);

--- Returns true for failed results.
export let (self : Result[T, E]).isErr [T, E] () : Bool := isErr[T, E](self);

--- Return the success value or the fallback.
export let (self : Result[T, E]).unwrapOr [T, E] (fallback : T) : T :=
  unwrapOr[T, E](self, fallback);

--- Return the success value or compute from the error.
export let (self : Result[T, E]).unwrapOrElse [T, E] (fallback : E -> T) : T :=
  unwrapOrElse[T, E](self, fallback);

--- Transform a success value.
export let (self : Result[T, E]).map [T, E, U] (f : T -> U) : Result[U, E] :=
  map[T, E, U](self, f);

--- Transform an error value.
export let (self : Result[T, E]).mapErr [T, E, F] (f : E -> F) : Result[T, F] :=
  mapErr[T, E, F](self, f);

--- Transform a success value or return the fallback.
export let (self : Result[T, E]).mapOr [T, E, U] (
  fallback : U,
  f : T -> U
) : U := mapOr[T, E, U](self, fallback, f);

--- Transform either result case into one value.
export let (self : Result[T, E]).mapOrElse [T, E, U] (
  onErr : E -> U,
  onOk : T -> U
) : U := mapOrElse[T, E, U](self, onErr, onOk);

--- Chain an operation that may fail.
export let (self : Result[T, E]).andThen [T, E, U] (
  f : T -> Result[U, E]
) : Result[U, E] := andThen[T, E, U](self, f);

--- Recover from an error with another result.
export let (self : Result[T, E]).orElse [T, E, F] (
  f : E -> Result[T, F]
) : Result[T, F] := orElse[T, E, F](self, f);

--- Fold both result cases into one value.
export let (self : Result[T, E]).fold [T, E, U] (
  onOk : T -> U,
  onErr : E -> U
) : U := fold[T, E, U](self, onOk, onErr);
