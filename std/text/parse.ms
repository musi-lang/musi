/// Parse a string as an integer, or `None` on failure.
///
/// # Examples
///
/// ```musi
/// parse_int("42")     -- .Some(42)
/// parse_int("hello")  -- .None
/// ```
export let parse_int : String -> ?Int;

/// Parse a string as a float, or `None` on failure.
export let parse_float : String -> ?Float64;

/// Parse a string as a boolean, or `None` on failure.
///
/// Accepts `"True"` and `"False"` only.
///
/// # Examples
///
/// ```musi
/// parse_bool("True")   -- .Some(.True)
/// parse_bool("yes")    -- .None
/// ```
export let parse_bool : String -> ?Bool :=
    (s) => (.Some(.True) if s = "True" | .Some(.False) if s = "False" | .None if _);

/// Parse a string as an integer in the given radix (2..36), or `None`.
///
/// # Examples
///
/// ```musi
/// parse_int_radix("ff", 16)  -- .Some(255)
/// ```
export let parse_int_radix : (String, Int) -> ?Int;
