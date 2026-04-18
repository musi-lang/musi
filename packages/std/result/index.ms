let core := import "./_core.ms";

--- Recoverable success or failure value.
export let Result := core.Result;

--- Build a successful result.
export let ok := core.ok;

--- Build a failed result.
export let err := core.err;

--- Returns true for successful results.
export let(self : Result[T, E]).isOk [T, E] () : Bool := core.isOk[T, E](self);

--- Returns true for failed results.
export let(self : Result[T, E]).isErr [T, E] () : Bool := core.isErr[T, E](self);

--- Return the success value or the fallback.
export let(self : Result[T, E]).unwrapOr [T, E] (fallback : T) : T := core.unwrapOr[T, E](self,
    fallback
);

--- Return the success value or compute from the error.
export let(self : Result[T, E]).unwrapOrElse [T, E] (fallback : E -> T) : T := core.unwrapOrElse[T, E](self,
    fallback
);

--- Transform a success value.
export let(self : Result[T, E]).map [T, E, U] (f : T -> U) : Result[U, E] := core.map[T, E, U](self,
    f
);

--- Transform an error value.
export let(self : Result[T, E]).mapErr [T, E, F] (f : E -> F) : Result[T, F] := core.mapErr[T, E, F](self,
    f
);

--- Transform a success value or return the fallback.
export let(self : Result[T, E]).mapOr [T, E, U] (fallback : U, f : T -> U) : U := core.mapOr[T, E, U](self,
    fallback, f
);

--- Transform either result case into one value.
export let(self : Result[T, E]).mapOrElse [T, E, U] (onErr : E -> U,
    onOk : T -> U
) : U := core.mapOrElse[T, E, U](self, onErr, onOk);

--- Chain an operation that may fail.
export let(self : Result[T, E]).andThen [T, E, U] (f : T -> Result[U, E]) : Result[U, E] := core.andThen[T, E, U](self,
    f
);

--- Recover from an error with another result.
export let(self : Result[T, E]).orElse [T, E, F] (f : E -> Result[T, F]) : Result[T, F] := core.orElse[T, E, F](self,
    f
);

--- Fold both result cases into one value.
export let(self : Result[T, E]).fold [T, E, U] (onOk : T -> U, onErr : E -> U) : U := core.fold[T, E, U](self,
    onOk, onErr
);
