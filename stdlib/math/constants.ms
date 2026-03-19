//! Numeric constants.

import "@std/rt" as rt;

/// The ratio of a circle's circumference to its diameter.
export let PI       := rt.float_pi();

/// Euler's number, the base of the natural logarithm.
export let E        := rt.float_e();

/// Positive floating-point infinity.
export let INFINITY := rt.float_infinity();

/// Not-a-Number sentinel value.
export let NAN      := rt.float_nan();

/// Minimum representable integer value.
export let INT_MIN  := rt.int_min_val();

/// Maximum representable integer value.
export let INT_MAX  := rt.int_max_val();
