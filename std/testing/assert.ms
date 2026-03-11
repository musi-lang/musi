/// Assert that two values are equal.
///
/// Panics with a message showing both values on failure.
///
/// # Examples
///
/// ```musi
/// assert_eq(1 + 1, 2)
/// ```
export let assert_eq ['T] where 'T <: Eq, 'T <: Show : ('T, 'T) -> ();

/// Assert that two values are not equal.
///
/// Panics with a message showing both values on failure.
export let assert_ne ['T] where 'T <: Eq, 'T <: Show : ('T, 'T) -> ();

/// Assert that a condition is true.
///
/// Panics if the value is false.
export let assert_true : Bool -> ();

/// Assert that a condition is false.
///
/// Panics if the value is true.
export let assert_false : Bool -> ();

/// Assert that an option is `Some`.
///
/// Panics if the value is `None`.
export let assert_some ['T] : (?'T) -> ();

/// Assert that an option is `None`.
///
/// Panics if the value is `Some`.
export let assert_none ['T] : (?'T) -> ();

/// Assert that a result is `Ok`.
///
/// Panics if the value is `Err`.
export let assert_ok ['T, 'E] : (Result of ('T, 'E)) -> ();

/// Assert that a result is `Err`.
///
/// Panics if the value is `Ok`.
export let assert_err ['T, 'E] : (Result of ('T, 'E)) -> ();
