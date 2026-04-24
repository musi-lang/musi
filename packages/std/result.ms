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
let resultIsOk [T, E] (target : Result[T, E]) : Bool :=
  match target (
  | .Ok(_) => 0 = 0
  | .Err(_) => 0 = 1
  );

--- Core operation for `isErr`.
let resultIsErr [T, E] (target : Result[T, E]) : Bool :=
  match target (
  | .Ok(_) => 0 = 1
  | .Err(_) => 0 = 0
  );

--- Core operation for `unwrapOr`.
let resultUnwrapOr [T, E] (target : Result[T, E], fallback : T) : T :=
  match target (
  | .Ok(item) => item
  | .Err(_) => fallback
  );

--- Core operation for `unwrapOrElse`.
let resultUnwrapOrElse [T, E] (target : Result[T, E], fallback : E -> T) : T :=
  match target (
  | .Ok(item) => item
  | .Err(error) => fallback(error)
  );

--- Core operation for `map`.
let resultMap [T, E, U] (target : Result[T, E], f : T -> U) : Result[U, E] :=
  match target (
  | .Ok(item) => ok[U, E](f(item))
  | .Err(error) => err[U, E](error)
  );

--- Core operation for `mapErr`.
let resultMapErr [T, E, F] (target : Result[T, E], f : E -> F) : Result[T, F] :=
  match target (
  | .Ok(item) => ok[T, F](item)
  | .Err(error) => err[T, F](f(error))
  );

--- Core operation for `mapOr`.
let resultMapOr [T, E, U] (
  target : Result[T, E],
  fallback : U,
  f : T -> U
) : U := match target (
| .Ok(item) => f(item)
| .Err(_) => fallback
);

--- Core operation for `mapOrElse`.
let resultMapOrElse [T, E, U] (
  target : Result[T, E],
  onErr : E -> U,
  onOk : T -> U
) : U := match target (
| .Ok(item) => onOk(item)
| .Err(error) => onErr(error)
);

--- Core operation for `andThen`.
let resultAndThen [T, E, U] (
  target : Result[T, E],
  f : T -> Result[U, E]
) : Result[U, E] :=
  match target (
  | .Ok(item) => f(item)
  | .Err(error) => err[U, E](error)
  );

--- Core operation for `orElse`.
let resultOrElse [T, E, F] (
  target : Result[T, E],
  f : E -> Result[T, F]
) : Result[T, F] :=
  match target (
  | .Ok(item) => ok[T, F](item)
  | .Err(error) => f(error)
  );

--- Core operation for `fold`.
let resultFold [T, E, U] (
  target : Result[T, E],
  onOk : T -> U,
  onErr : E -> U
) : U := match target (
| .Ok(item) => onOk(item)
| .Err(error) => onErr(error)
);

--- Core operation for `isOk`.
export let isOk [T, E] (target : Result[T, E]) : Bool :=
  resultIsOk[T, E](target);

--- Core operation for `isErr`.
export let isErr [T, E] (target : Result[T, E]) : Bool :=
  resultIsErr[T, E](target);

--- Core operation for `unwrapOr`.
export let unwrapOr [T, E] (target : Result[T, E], fallback : T) : T :=
  resultUnwrapOr[T, E](target, fallback);

--- Core operation for `unwrapOrElse`.
export let unwrapOrElse [T, E] (target : Result[T, E], fallback : E -> T) : T :=
  resultUnwrapOrElse[T, E](target, fallback);

--- Core operation for `map`.
export let map [T, E, U] (target : Result[T, E], f : T -> U) : Result[U, E] :=
  resultMap[T, E, U](target, f);

--- Core operation for `mapErr`.
export let mapErr [T, E, F] (
  target : Result[T, E],
  f : E -> F
) : Result[T, F] := resultMapErr[T, E, F](target, f);

--- Core operation for `mapOr`.
export let mapOr [T, E, U] (
  target : Result[T, E],
  fallback : U,
  f : T -> U
) : U := resultMapOr[T, E, U](target, fallback, f);

--- Core operation for `mapOrElse`.
export let mapOrElse [T, E, U] (
  target : Result[T, E],
  onErr : E -> U,
  onOk : T -> U
) : U := resultMapOrElse[T, E, U](target, onErr, onOk);

--- Core operation for `andThen`.
export let andThen [T, E, U] (
  target : Result[T, E],
  f : T -> Result[U, E]
) : Result[U, E] := resultAndThen[T, E, U](target, f);

--- Core operation for `orElse`.
export let orElse [T, E, F] (
  target : Result[T, E],
  f : E -> Result[T, F]
) : Result[T, F] := resultOrElse[T, E, F](target, f);

--- Core operation for `fold`.
export let fold [T, E, U] (
  target : Result[T, E],
  onOk : T -> U,
  onErr : E -> U
) : U := resultFold[T, E, U](target, onOk, onErr);

--- Returns true for successful results.
export let (self : Result[T, E]).isOk [T, E] () : Bool :=
  resultIsOk[T, E](self);

--- Returns true for failed results.
export let (self : Result[T, E]).isErr [T, E] () : Bool :=
  resultIsErr[T, E](self);

--- Return the success value or the fallback.
export let (self : Result[T, E]).unwrapOr [T, E] (fallback : T) : T :=
  resultUnwrapOr[T, E](self, fallback);

--- Return the success value or compute from the error.
export let (self : Result[T, E]).unwrapOrElse [T, E] (fallback : E -> T) : T :=
  resultUnwrapOrElse[T, E](self, fallback);

--- Transform a success value.
export let (self : Result[T, E]).map [T, E, U] (f : T -> U) : Result[U, E] :=
  resultMap[T, E, U](self, f);

--- Transform an error value.
export let (self : Result[T, E]).mapErr [T, E, F] (f : E -> F) : Result[T, F] :=
  resultMapErr[T, E, F](self, f);

--- Transform a success value or return the fallback.
export let (self : Result[T, E]).mapOr [T, E, U] (
  fallback : U,
  f : T -> U
) : U := resultMapOr[T, E, U](self, fallback, f);

--- Transform either result case into one value.
export let (self : Result[T, E]).mapOrElse [T, E, U] (
  onErr : E -> U,
  onOk : T -> U
) : U := resultMapOrElse[T, E, U](self, onErr, onOk);

--- Chain an operation that may fail.
export let (self : Result[T, E]).andThen [T, E, U] (
  f : T -> Result[U, E]
) : Result[U, E] := resultAndThen[T, E, U](self, f);

--- Recover from an error with another result.
export let (self : Result[T, E]).orElse [T, E, F] (
  f : E -> Result[T, F]
) : Result[T, F] := resultOrElse[T, E, F](self, f);

--- Fold both result cases into one value.
export let (self : Result[T, E]).fold [T, E, U] (
  onOk : T -> U,
  onErr : E -> U
) : U := resultFold[T, E, U](self, onOk, onErr);
