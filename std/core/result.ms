/// A value that is either a success (`Ok`) or a failure (`Err`).
///
/// Example:
///   let x := .Ok(42);
///   let y := map(x, (n) -> n + 1)  -- .Ok(43)
#[lang := "result"]
export let Result := forall 'T, 'E -> Ok of 'T + Err of 'E;

/// Apply a function to the success value.
export let map : forall 'T, 'U, 'E -> (Result of ('T, 'E), ('T) -> 'U) -> Result of ('U, 'E) :=
    forall 'T, 'U, 'E -> (res, f) -> match res (.Ok(x) => .Ok(f(x)) | .Err(e) => .Err(e));

/// Apply a function to the error value.
export let map_err : forall 'T, 'E, 'F -> (Result of ('T, 'E), ('E) -> 'F) -> Result of ('T, 'F) :=
    forall 'T, 'E, 'F -> (res, f) -> match res (.Ok(x) => .Ok(x) | .Err(e) => .Err(f(e)));

/// Chain a computation that may fail.
export let flat_map : forall 'T, 'U, 'E -> (Result of ('T, 'E), ('T) -> Result of ('U, 'E)) -> Result of ('U, 'E) :=
    forall 'T, 'U, 'E -> (res, f) -> match res (.Ok(x) => f(x) | .Err(e) => .Err(e));

/// Return the success value or a default.
export let unwrap_or : forall 'T, 'E -> (Result of ('T, 'E), 'T) -> 'T :=
    forall 'T, 'E -> (res, default) -> match res (.Ok(x) => x | .Err(_) => default);

/// True if the result is `Ok`.
export let is_ok : forall 'T, 'E -> (Result of ('T, 'E)) -> Bool :=
    forall 'T, 'E -> (res) -> match res (.Ok(_) => .True | .Err(_) => .False);

/// True if the result is `Err`.
export let is_err : forall 'T, 'E -> (Result of ('T, 'E)) -> Bool :=
    forall 'T, 'E -> (res) -> match res (.Ok(_) => .False | .Err(_) => .True);

/// Convert to an option, discarding the error.
export let ok : forall 'T, 'E -> (Result of ('T, 'E)) -> ?'T :=
    forall 'T, 'E -> (res) -> match res (.Ok(x) => .Some(x) | .Err(_) => .None);

/// Convert to an option of the error, discarding the success.
export let err : forall 'T, 'E -> (Result of ('T, 'E)) -> ?'E :=
    forall 'T, 'E -> (res) -> match res (.Ok(_) => .None | .Err(e) => .Some(e));

given Eq of (Result of ('T, 'E)) where 'T <: Eq, 'E <: Eq {
    let (=)(a: Result of ('T, 'E), b: Result of ('T, 'E)) : Bool :=
        match (a, b) (
            (.Ok(x), .Ok(y)) => x = y
          | (.Err(x), .Err(y)) => x = y
          | _ => .False
        )
};

given Show of (Result of ('T, 'E)) where 'T <: Show, 'E <: Show {
    let show(value: Result of ('T, 'E)) : String :=
        match value (.Ok(x) => "Ok(" + show(x) + ")" | .Err(e) => "Err(" + show(e) + ")")
};
