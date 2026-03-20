//! Sampling operations using the RNG.

import "@std/rt" as rt;
import "./rng" as rng;

/// Returns a random element from arr as .Some, or .None if arr is empty.
export let choose ['a] := (r, arr) => (
  let len := rt.arr_len(arr);
  (.Some(arr.[rng.next_bounded(r, 0, len - 1)]) if len > 0
  | .None if _)
);

/// Returns a new array with the same elements in a random order.
export let shuffle ['a] := (r, arr) => (
  let len := rt.arr_len(arr);
  let mut result := rt.arr_slice(arr, 0, len);
  let step := (i) => (
    (
      let j := rng.next_bounded(r, i, len - 1);
      let tmp : 'a := result.[i];
      result.[i] <- result.[j];
      result.[j] <- tmp;
      step(i + 1)
    ) if i < len - 1
    | result if _
  );
  (step(0) if len > 1 | result if _)
);

/// Returns up to n randomly selected elements from arr without replacement.
export let sample ['a] := (r, arr, n) => (
  let len := rt.arr_len(arr);
  let count := (n if n < len | len if _);
  let shuffled := shuffle(r, arr);
  rt.arr_slice(shuffled, 0, count)
);

/// Returns a sample from a standard normal distribution using Box-Muller.
export let next_gaussian := (r) => (
  let u1 := rng.next_float(r);
  let u2 := rng.next_float(r);
  let safe_u1 := (u1 if u1 > 0.0 | 0.0001 if _);
  rt.float_sqrt(-2.0 * rt.float_log(safe_u1)) * rt.float_cos(2.0 * rt.float_pi() * u2)
);
