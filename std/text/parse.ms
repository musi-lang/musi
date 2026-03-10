/// Parse a string as an integer, or `None` on failure.
export let parse_int : String -> ?Int;

/// Parse a string as a float, or `None` on failure.
export let parse_float : String -> ?Float64;

/// Parse a string as a boolean ("True" or "False"), or `None`.
export let parse_bool : String -> ?Bool :=
    (s) => (.Some(.True) if s = "True" | .Some(.False) if s = "False" | .None if _);

/// Parse a string as an integer in the given radix (2..36), or `None`.
export let parse_int_radix : (String, Int) -> ?Int;
