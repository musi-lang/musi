--! Expect success or failure result helpers.

--- Recoverable success or failure value.
export hidden let Expect [T, E] := data {
  | Success(T)
  | Failure(E)
};

--- Build successful expected value.
export let Success [T, E] (value : T) : Expect[T, E] := .Success(value);

--- Build failed expected value.
export let Failure [T, E] (value : E) : Expect[T, E] := .Failure(value);

--- Function for `isSuccess`.
let expectIsSuccess [T, E] (target : Expect[T, E]) :=
  match target (
  | .Success(_) => 0 = 0
  | .Failure(_) => 0 = 1
  );

--- Function for `isFailure`.
let expectIsFailure [T, E] (target : Expect[T, E]) :=
  match target (
  | .Success(_) => 0 = 1
  | .Failure(_) => 0 = 0
  );

--- Function for `unwrapOr`.
let expectUnwrapOr [T, E] (target : Expect[T, E], fallback : T) :=
  match target (
  | .Success(item) => item
  | .Failure(_) => fallback
  );

--- Function for `unwrapOrElse`.
let expectUnwrapOrElse [T, E] (target : Expect[T, E], fallback : E -> T) :=
  match target (
  | .Success(item) => item
  | .Failure(error) => fallback(error)
  );

--- Function for `map`.
let expectMap [T, E, U] (target : Expect[T, E], f : T -> U) :=
  match target (
  | .Success(item) => Success[U, E](f(item))
  | .Failure(error) => Failure[U, E](error)
  );

--- Function for `mapFail`.
let expectMapFail [T, E, F] (target : Expect[T, E], f : E -> F) :=
  match target (
  | .Success(item) => Success[T, F](item)
  | .Failure(error) => Failure[T, F](f(error))
  );

--- Function for `mapOr`.
let expectMapOr [T, E, U] (target : Expect[T, E], fallback : U, f : T -> U) :=
  match target (
  | .Success(item) => f(item)
  | .Failure(_) => fallback
  );

--- Function for `mapOrElse`.
let expectMapOrElse [T, E, U] (
  target : Expect[T, E],
  onFailure : E -> U,
  onSuccess : T -> U
) :=
  match target (
  | .Success(item) => onSuccess(item)
  | .Failure(error) => onFailure(error)
  );

--- Function for `andThen`.
let expectAndThen [T, E, U] (target : Expect[T, E], f : T -> Expect[U, E]) :=
  match target (
  | .Success(item) => f(item)
  | .Failure(error) => Failure[U, E](error)
  );

--- Function for `orElse`.
let expectOrElse [T, E, F] (target : Expect[T, E], f : E -> Expect[T, F]) :=
  match target (
  | .Success(item) => Success[T, F](item)
  | .Failure(error) => f(error)
  );

--- Function for `fold`.
let expectFold [T, E, U] (
  target : Expect[T, E],
  onSuccess : T -> U,
  onFailure : E -> U
) :=
  match target (
  | .Success(item) => onSuccess(item)
  | .Failure(error) => onFailure(error)
  );

--- Returns true when target is Success.
export let isSuccess [T, E] (target : Expect[T, E]) :=
  expectIsSuccess[T, E](target);

--- Returns true when target is Failure.
export let isFailure [T, E] (target : Expect[T, E]) :=
  expectIsFailure[T, E](target);

--- Return success value, or fallback when target is Failure.
export let unwrapOr [T, E] (target : Expect[T, E], fallback : T) :=
  expectUnwrapOr[T, E](target, fallback);

--- Return success value, or compute fallback from failure value.
export let unwrapOrElse [T, E] (target : Expect[T, E], fallback : E -> T) :=
  expectUnwrapOrElse[T, E](target, fallback);

--- Transform success value and keep failure unchanged.
export let map [T, E, U] (target : Expect[T, E], f : T -> U) :=
  expectMap[T, E, U](target, f);

--- Transform failure value and keep success unchanged.
export let mapFail [T, E, F] (target : Expect[T, E], f : E -> F) :=
  expectMapFail[T, E, F](target, f);

--- Transform success value, or return fallback when target is Failure.
export let mapOr [T, E, U] (target : Expect[T, E], fallback : U, f : T -> U) :=
  expectMapOr[T, E, U](target, fallback, f);

--- Transform success, or compute fallback from failure value.
export let mapOrElse [T, E, U] (
  target : Expect[T, E],
  onFailure : E -> U,
  onSuccess : T -> U
) := expectMapOrElse[T, E, U](target, onFailure, onSuccess);

--- Chain an operation that may fail.
export let andThen [T, E, U] (target : Expect[T, E], f : T -> Expect[U, E]) :=
  expectAndThen[T, E, U](target, f);

--- Recover from failure with another expected value.
export let orElse [T, E, F] (target : Expect[T, E], f : E -> Expect[T, F]) :=
  expectOrElse[T, E, F](target, f);

--- Transform success or failure into one value.
export let fold [T, E, U] (
  target : Expect[T, E],
  onSuccess : T -> U,
  onFailure : E -> U
) := expectFold[T, E, U](target, onSuccess, onFailure);

--- Returns true for successful results.
export let (self : Expect[T, E]).isSuccess [T, E] () :=
  expectIsSuccess[T, E](self);

--- Returns true for failed results.
export let (self : Expect[T, E]).isFailure [T, E] () :=
  expectIsFailure[T, E](self);

--- Return the success value or the fallback.
export let (self : Expect[T, E]).unwrapOr [T, E] (fallback : T) :=
  expectUnwrapOr[T, E](self, fallback);

--- Return the success value or compute from the error.
export let (self : Expect[T, E]).unwrapOrElse [T, E] (fallback : E -> T) :=
  expectUnwrapOrElse[T, E](self, fallback);

--- Transform a success value.
export let (self : Expect[T, E]).map [T, E, U] (f : T -> U) :=
  expectMap[T, E, U](self, f);

--- Transform an error value.
export let (self : Expect[T, E]).mapFail [T, E, F] (f : E -> F) :=
  expectMapFail[T, E, F](self, f);

--- Transform a success value or return the fallback.
export let (self : Expect[T, E]).mapOr [T, E, U] (fallback : U, f : T -> U) :=
  expectMapOr[T, E, U](self, fallback, f);

--- Transform either expect case into one value.
export let (
  self : Expect[T, E]
).mapOrElse [T, E, U] (onFailure : E -> U, onSuccess : T -> U)
  :=
  expectMapOrElse[T, E, U](self, onFailure, onSuccess);

--- Chain an operation that may fail.
export let (self : Expect[T, E]).andThen [T, E, U] (f : T -> Expect[U, E]) :=
  expectAndThen[T, E, U](self, f);

--- Recover from an error with another expect.
export let (self : Expect[T, E]).orElse [T, E, F] (f : E -> Expect[T, F]) :=
  expectOrElse[T, E, F](self, f);

--- Fold both expect cases into one value.
export let (self : Expect[T, E]).fold [T, E, U] (
  onSuccess : T -> U,
  onFailure : E -> U
) := expectFold[T, E, U](self, onSuccess, onFailure);
