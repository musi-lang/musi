//! Comparison utilities for Ordering values and comparator functions.

import "./ord";

/// Return whether an Ordering is Less.
export let is_less := (ord) =>
  match ord (
    .Less => .True
    | _ => .False
  );

/// Return whether an Ordering is Equal.
export let is_equal := (ord) =>
  match ord (
    .Equal => .True
    | _ => .False
  );

/// Return whether an Ordering is Greater.
export let is_greater := (ord) =>
  match ord (
    .Greater => .True
    | _ => .False
  );

/// Flip Less to Greater and Greater to Less; leave Equal unchanged.
export let reverse_order := (ord) =>
  match ord (
    .Less => .Greater
    | .Greater => .Less
    | .Equal => .Equal
  );

/// Return ord2 if ord1 is Equal, otherwise return ord1 (tiebreak combinator).
export let then_compare := (ord1, ord2) =>
  match ord1 (
    .Equal => ord2
    | _ => ord1
  );

/// Return the smaller of two values according to a comparator.
export let min_by ['a] := (a, b, cmp) =>
  match cmp(a, b) (
    .Greater => b
    | _ => a
  );

/// Return the larger of two values according to a comparator.
export let max_by ['a] := (a, b, cmp) =>
  match cmp(a, b) (
    .Less => b
    | _ => a
  );

/// Clamp a value to the inclusive range [lo, hi] according to a comparator.
export let clamp_by ['a] := (val, lo, hi, cmp) =>
  match cmp(val, lo) (
    .Less => lo
    | _ => match cmp(val, hi) (
        .Greater => hi
        | _ => val
      )
  );

/// Build a comparator that compares values by a projection key.
export let comparing ['a, 'b] := (key, cmp) => (a, b) => cmp(key(a), key(b));

/// Return a comparator that reverses the order of an existing comparator.
export let reverse_cmp ['a] := (cmp) => (a, b) => reverse_order(cmp(a, b));
