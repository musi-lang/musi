/// Assert that two values are equal. Panics with a message showing both values on failure.
export let assert_eq : forall 'T where 'T <: Eq, 'T <: Show -> ('T, 'T) -> ();

/// Assert that two values are not equal.
export let assert_ne : forall 'T where 'T <: Eq, 'T <: Show -> ('T, 'T) -> ();

/// Assert that a condition is true.
export let assert_true : Bool -> ();

/// Assert that a condition is false.
export let assert_false : Bool -> ();

/// Assert that an option is `Some`.
export let assert_some : forall 'T -> (?'T) -> ();

/// Assert that an option is `None`.
export let assert_none : forall 'T -> (?'T) -> ();

/// Assert that a result is `Ok`.
export let assert_ok : forall 'T, 'E -> (Result of ('T, 'E)) -> ();

/// Assert that a result is `Err`.
export let assert_err : forall 'T, 'E -> (Result of ('T, 'E)) -> ();
