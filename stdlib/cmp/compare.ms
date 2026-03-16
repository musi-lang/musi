//! Comparison utilities for Ordering values and comparator functions.

import "./ord";

export let is_less := (ord) =>
  match ord (
    .Less => .True
    | _ => .False
  );

export let is_equal := (ord) =>
  match ord (
    .Equal => .True
    | _ => .False
  );

export let is_greater := (ord) =>
  match ord (
    .Greater => .True
    | _ => .False
  );

export let reverse_order := (ord) =>
  match ord (
    .Less => .Greater
    | .Greater => .Less
    | .Equal => .Equal
  );

export let then_compare := (ord1, ord2) =>
  match ord1 (
    .Equal => ord2
    | _ => ord1
  );

export let min_by ['a] := (a, b, cmp) =>
  match cmp(a, b) (
    .Greater => b
    | _ => a
  );

export let max_by ['a] := (a, b, cmp) =>
  match cmp(a, b) (
    .Less => b
    | _ => a
  );

export let clamp_by ['a] := (val, lo, hi, cmp) =>
  match cmp(val, lo) (
    .Less => lo
    | _ => match cmp(val, hi) (
        .Greater => hi
        | _ => val
      )
  );

export let comparing ['a, 'b] := (key, cmp) => (a, b) => cmp(key(a), key(b));

export let reverse_cmp ['a] := (cmp) => (a, b) => reverse_order(cmp(a, b));
