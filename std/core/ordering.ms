// std/core/ordering.ms — Ordering type and comparison utilities
// import { Ordering, is_lt, is_eq, is_gt, invert, then_with, compare_int } from "std/core/ordering"

export choice Ordering { Lt | Eq | Gt };

export fn is_lt(o: Ordering): Bool => match o with ( .Lt => .True | _ => .False );
export fn is_eq(o: Ordering): Bool => match o with ( .Eq => .True | _ => .False );
export fn is_gt(o: Ordering): Bool => match o with ( .Gt => .True | _ => .False );

export fn invert(o: Ordering): Ordering => match o with (
    .Lt => .Gt
  | .Eq => .Eq
  | .Gt => .Lt
);

// Combine two orderings: use second if first is Eq.
export fn then_with(primary: Ordering, secondary: Ordering): Ordering => match primary with (
    .Eq   => secondary
  | other => other
);

export fn compare_int(a: Int, b: Int): Ordering =>
    if a < b then .Lt else if a > b then .Gt else .Eq;

export fn compare_float(a: Float, b: Float): Ordering =>
    if a < b then .Lt else if a > b then .Gt else .Eq;
