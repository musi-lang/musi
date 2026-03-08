// std/core/ordering.ms — Ordering type and comparison utilities
// import { Ordering, is_lt, is_eq, is_gt, invert, then_with, compare_int } from "std/core/ordering"

/// Three-way comparison result: less-than, equal, or greater-than.
export choice Ordering { Lt | Eq | Gt };

/// Return `.True` if the ordering is `.Lt`.
export fn is_lt(o: Ordering): Bool => match o with ( .Lt => .True | _ => .False );
/// Return `.True` if the ordering is `.Eq`.
export fn is_eq(o: Ordering): Bool => match o with ( .Eq => .True | _ => .False );
/// Return `.True` if the ordering is `.Gt`.
export fn is_gt(o: Ordering): Bool => match o with ( .Gt => .True | _ => .False );

/// Flip an ordering: `.Lt` ↔ `.Gt`, `.Eq` stays `.Eq`.
export fn invert(o: Ordering): Ordering => match o with (
    .Lt => .Gt
  | .Eq => .Eq
  | .Gt => .Lt
);

/// Combine two orderings: use `secondary` when `primary` is `.Eq`.
/// Useful for lexicographic comparison chains.
export fn then_with(primary: Ordering, secondary: Ordering): Ordering => match primary with (
    .Eq   => secondary
  | other => other
);

/// Three-way comparison of two integers.
export fn compare_int(a: Int, b: Int): Ordering =>
    if a < b then .Lt else if a > b then .Gt else .Eq;

/// Three-way comparison of two floats.
export fn compare_float(a: Float, b: Float): Ordering =>
    if a < b then .Lt else if a > b then .Gt else .Eq;
