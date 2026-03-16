//! Tests for array aggregation operations.

import "@std/testing" as t;
import "@std/array/aggregate" as agg;

t.runner.report(t.runner.run_suite([
  ("min_of — finds minimum in array", () => (
    let result := agg.min_of([3, 1, 4, 1, 5, 9]);
    t.assertions.assert_eq(result, .Some(1))
  )),
  ("min_of — single element", () => (
    let result := agg.min_of([7]);
    t.assertions.assert_eq(result, .Some(7))
  )),
  ("min_of — empty array returns None", () => (
    let result := agg.min_of([]);
    t.assertions.assert_eq(result, .None)
  )),
  ("max_of — finds maximum in array", () => (
    let result := agg.max_of([3, 1, 4, 1, 5, 9]);
    t.assertions.assert_eq(result, .Some(9))
  )),
  ("max_of — single element", () => (
    let result := agg.max_of([42]);
    t.assertions.assert_eq(result, .Some(42))
  )),
  ("max_of — empty array returns None", () => (
    let result := agg.max_of([]);
    t.assertions.assert_eq(result, .None)
  )),
  ("count_by — counts matching elements", () => (
    let result := agg.count_by([1, 2, 3, 4, 5], (x) => x > 2);
    t.assertions.assert_eq(result, 3)
  )),
  ("count_by — no matches returns zero", () => (
    let result := agg.count_by([1, 2, 3], (x) => x > 10);
    t.assertions.assert_eq(result, 0)
  )),
  ("count_by — empty array returns zero", () => (
    let result := agg.count_by([], (x) => x > 0);
    t.assertions.assert_eq(result, 0)
  )),
  ("all_arr — all match returns true", () => (
    let result := agg.all_arr([2, 4, 6], (x) => x > 0);
    t.assertions.assert_true(result)
  )),
  ("all_arr — one fails returns false", () => (
    let result := agg.all_arr([2, 4, 5], (x) => x mod 2 = 0);
    t.assertions.assert_false(result)
  )),
  ("all_arr — empty array returns true", () => (
    let result := agg.all_arr([], (x) => x > 0);
    t.assertions.assert_true(result)
  )),
  ("any_arr — one matches returns true", () => (
    let result := agg.any_arr([1, 3, 5, 6], (x) => x mod 2 = 0);
    t.assertions.assert_true(result)
  )),
  ("any_arr — none match returns false", () => (
    let result := agg.any_arr([1, 3, 5], (x) => x mod 2 = 0);
    t.assertions.assert_false(result)
  )),
  ("any_arr — empty array returns false", () => (
    let result := agg.any_arr([], (x) => x > 0);
    t.assertions.assert_false(result)
  )),
  ("fold_arr — folds with initial value", () => (
    let result := agg.fold_arr([1, 2, 3, 4], 0, (acc, x) => acc + x);
    t.assertions.assert_eq(result, 10)
  )),
  ("fold_arr — empty array returns init", () => (
    let result := agg.fold_arr([], 99, (acc, x) => acc + x);
    t.assertions.assert_eq(result, 99)
  )),
  ("fold_arr — string concat fold", () => (
    let result := agg.fold_arr(["a", "b", "c"], "", (acc, x) => acc + x);
    t.assertions.assert_eq(result, "abc")
  )),
  ("reduce_arr — reduces non-empty array", () => (
    let result := agg.reduce_arr([1, 2, 3, 4], (acc, x) => acc + x);
    t.assertions.assert_eq(result, .Some(10))
  )),
  ("reduce_arr — single element returns Some", () => (
    let result := agg.reduce_arr([5], (acc, x) => acc + x);
    t.assertions.assert_eq(result, .Some(5))
  )),
  ("reduce_arr — empty array returns None", () => (
    let result := agg.reduce_arr([], (acc, x) => acc + x);
    t.assertions.assert_eq(result, .None)
  )),
  ("sum_arr — sums all elements", () => (
    let result := agg.sum_arr([1, 2, 3, 4, 5]);
    t.assertions.assert_eq(result, 15)
  )),
  ("sum_arr — empty array returns zero", () => (
    let result := agg.sum_arr([]);
    t.assertions.assert_eq(result, 0)
  )),
  ("sum_arr — single element", () => (
    let result := agg.sum_arr([7]);
    t.assertions.assert_eq(result, 7)
  )),
]));
