/// A value that is either a success (`Ok`) or a failure (`Err`).
///
/// `Result` is used for computations that may fail. The `Ok` variant holds
/// a success value, while `Err` holds an error. Use `map`, `flat_map`, and
/// `unwrap_or` to transform results without explicit pattern matching.
///
/// # Examples
///
/// ```musi
/// let x := .Ok(42);
/// let y := map(x, (n) -> n + 1);  -- .Ok(43)
/// let z := unwrap_or(x, 0);        -- 42
/// ```
#[lang := "result"]
export let Result ['T, 'E] := choice { Ok of 'T + Err of 'E };

/// Apply a function to the success value, or propagate the error.
///
/// # Examples
///
/// ```musi
/// let x := map(.Ok(3), (n) -> n * 2);      -- .Ok(6)
/// let y := map(.Err("fail"), (n) -> n * 2); -- .Err("fail")
/// ```
export let map ['T, 'U, 'E] : (Result of ('T, 'E), ('T) -> 'U) -> Result of ('U, 'E) :=
    (res, f) => match res (.Ok(x) => .Ok(f(x)) | .Err(e) => .Err(e));

/// Apply a function to the error value, leaving `Ok` unchanged.
///
/// # Examples
///
/// ```musi
/// let x := map_err(.Err(3), (e) -> e + 1);  -- .Err(4)
/// let y := map_err(.Ok(42), (e) -> e + 1);   -- .Ok(42)
/// ```
export let map_err ['T, 'E, 'F] : (Result of ('T, 'E), ('E) -> 'F) -> Result of ('T, 'F) :=
    (res, f) => match res (.Ok(x) => .Ok(x) | .Err(e) => .Err(f(e)));

/// Chain a computation that may fail, flattening the result.
///
/// # Examples
///
/// ```musi
/// let half := (n) -> (.Ok(n / 2) if n mod 2 = 0 | .Err("odd") if _);
/// let x := flat_map(.Ok(4), half);  -- .Ok(2)
/// let y := flat_map(.Ok(3), half);  -- .Err("odd")
/// ```
export let flat_map ['T, 'U, 'E] : (Result of ('T, 'E), ('T) -> Result of ('U, 'E)) -> Result of ('U, 'E) :=
    (res, f) => match res (.Ok(x) => f(x) | .Err(e) => .Err(e));

/// Return the success value or a default.
///
/// # Examples
///
/// ```musi
/// let x := unwrap_or(.Ok(7), 0);       -- 7
/// let y := unwrap_or(.Err("fail"), 0); -- 0
/// ```
export let unwrap_or ['T, 'E] : (Result of ('T, 'E), 'T) -> 'T :=
    (res, default) => match res (.Ok(x) => x | .Err(_) => default);

/// True if the result is `Ok`.
export let is_ok ['T, 'E] : (Result of ('T, 'E)) -> Bool :=
    (res) => match res (.Ok(_) => .True | .Err(_) => .False);

/// True if the result is `Err`.
export let is_err ['T, 'E] : (Result of ('T, 'E)) -> Bool :=
    (res) => match res (.Ok(_) => .False | .Err(_) => .True);

/// Convert to an option, discarding the error.
///
/// # Examples
///
/// ```musi
/// let x := ok(.Ok(42));       -- .Some(42)
/// let y := ok(.Err("fail"));  -- .None
/// ```
export let ok ['T, 'E] : (Result of ('T, 'E)) -> ?'T :=
    (res) => match res (.Ok(x) => .Some(x) | .Err(_) => .None);

/// Convert to an option of the error, discarding the success.
///
/// # Examples
///
/// ```musi
/// let x := err(.Err("fail"));  -- .Some("fail")
/// let y := err(.Ok(42));        -- .None
/// ```
export let err ['T, 'E] : (Result of ('T, 'E)) -> ?'E :=
    (res) => match res (.Ok(_) => .None | .Err(e) => .Some(e));

given Eq of (Result of ('T, 'E)) where 'T <: Eq, 'E <: Eq {
    let (=)(a: Result of ('T, 'E), b: Result of ('T, 'E)) : Bool :=
        match (a, b) (
            (.Ok(x), .Ok(y)) => x = y
          | (.Err(x), .Err(y)) => x = y
          | _ => .False
        );
};

given Show of (Result of ('T, 'E)) where 'T <: Show, 'E <: Show {
    let show(value: Result of ('T, 'E)) : String :=
        match value (.Ok(x) => "Ok(" + show(x) + ")" | .Err(e) => "Err(" + show(e) + ")");
};
