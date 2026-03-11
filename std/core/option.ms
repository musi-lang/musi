/// An optional value: either `Some(x)` or `None`.
///
/// `Option` represents a value that may or may not be present. Use `map`,
/// `flat_map`, and `unwrap_or` to work with optional values without explicit
/// pattern matching.
///
/// # Examples
///
/// ```musi
/// let x := .Some(42);
/// let y := map(x, (n) -> n + 1);  -- .Some(43)
/// let z := unwrap_or(x, 0);        -- 42
/// ```
#[lang := "option"]
export let Option ['T] := choice { Some of 'T + None };

/// Apply a function to the contained value, or propagate `None`.
///
/// # Examples
///
/// ```musi
/// let x := map(.Some(3), (n) -> n * 2);  -- .Some(6)
/// let y := map(.None, (n) -> n * 2);      -- .None
/// ```
export let map ['T, 'U] : (?'T, ('T) -> 'U) -> ?'U :=
    (opt, f) => match opt (.Some(x) => .Some(f(x)) | .None => .None);

/// Apply a function that returns an option, flattening the result.
///
/// # Examples
///
/// ```musi
/// let parse := (s) -> (.Some(42) if s = "42" | .None if _);
/// let x := flat_map(.Some("42"), parse);  -- .Some(42)
/// let y := flat_map(.None, parse);         -- .None
/// ```
export let flat_map ['T, 'U] : (?'T, ('T) -> ?'U) -> ?'U :=
    (opt, f) => match opt (.Some(x) => f(x) | .None => .None);

/// Return the contained value or a default.
///
/// # Examples
///
/// ```musi
/// let x := unwrap_or(.Some(7), 0);  -- 7
/// let y := unwrap_or(.None, 0);     -- 0
/// ```
export let unwrap_or ['T] : (?'T, 'T) -> 'T :=
    (opt, default) => match opt (.Some(x) => x | .None => default);

/// Return the contained value or compute a default.
///
/// # Examples
///
/// ```musi
/// let x := unwrap_or_else(.Some(7), () -> 0);  -- 7
/// let y := unwrap_or_else(.None, () -> 0);      -- 0
/// ```
export let unwrap_or_else ['T] : (?'T, () -> 'T) -> 'T :=
    (opt, f) => match opt (.Some(x) => x | .None => f());

/// True if the option contains a value.
export let is_some ['T] : (?'T) -> Bool :=
    (opt) => match opt (.Some(_) => .True | .None => .False);

/// True if the option is `None`.
export let is_none ['T] : (?'T) -> Bool :=
    (opt) => match opt (.Some(_) => .False | .None => .True);

/// Return `None` if either is `None`, otherwise return the second.
///
/// # Examples
///
/// ```musi
/// let x := and(.Some(1), .Some("hi"));  -- .Some("hi")
/// let y := and(.None, .Some("hi"));     -- .None
/// ```
export let `and` ['T, 'U] : (?'T, ?'U) -> ?'U :=
    (a, b) => match a (.Some(_) => b | .None => .None);

/// Return the first `Some`, or `None` if both are `None`.
///
/// # Examples
///
/// ```musi
/// let x := or(.Some(1), .Some(2));  -- .Some(1)
/// let y := or(.None, .Some(2));     -- .Some(2)
/// ```
export let `or` ['T] : (?'T, ?'T) -> ?'T :=
    (a, b) => match a (.Some(_) => a | .None => b);

/// Keep the value only if it satisfies the predicate.
///
/// # Examples
///
/// ```musi
/// let x := filter(.Some(4), (n) -> n > 3);  -- .Some(4)
/// let y := filter(.Some(2), (n) -> n > 3);  -- .None
/// ```
export let filter ['T] : (?'T, ('T) -> Bool) -> ?'T :=
    (opt, p) => match opt (
        .Some(x) => (.Some(x) if p(x) | .None if _)
      | .None => .None
    );

/// Flatten a nested option.
///
/// # Examples
///
/// ```musi
/// let x := flatten(.Some(.Some(42)));  -- .Some(42)
/// let y := flatten(.Some(.None));       -- .None
/// ```
export let flatten ['T] : (?(?'T)) -> ?'T :=
    (opt) => match opt (.Some(inner) => inner | .None => .None);

/// Combine two options into an option of a pair.
///
/// # Examples
///
/// ```musi
/// let x := zip(.Some(1), .Some("a"));  -- .Some((1, "a"))
/// let y := zip(.Some(1), .None);        -- .None
/// ```
export let zip ['T, 'U] : (?'T, ?'U) -> ?(('T, 'U)) :=
    (a, b) => match (a, b) (
        (.Some(x), .Some(y)) => .Some((x, y))
      | _ => .None
    );

given Eq of (Option of 'T) where 'T <: Eq {
    let (=)(a: ?'T, b: ?'T) : Bool :=
        match (a, b) (
            (.Some(x), .Some(y)) => x = y
          | (.None, .None) => .True
          | _ => .False
        );
};

given Show of (Option of 'T) where 'T <: Show {
    let show(value: ?'T) : String :=
        match value (.Some(x) => "Some(" + show(x) + ")" | .None => "None");
};

given Ord of (Option of 'T) where 'T <: Ord {
    let compare(a: ?'T, b: ?'T) : Ordering :=
        match (a, b) (
            (.None, .None) => .Eq
          | (.None, .Some(_)) => .Lt
          | (.Some(_), .None) => .Gt
          | (.Some(x), .Some(y)) => compare(x, y)
        );
};
