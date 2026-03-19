//! Tests for array aggregation operations.

import "@std/testing" as t;
import "@std/array/aggregate" as agg;

t.describe("Array.aggregate", () => (
  t.it("min_of - finds minimum in array", () => (
    let result := agg.min_of([3, 1, 4, 1, 5, 9]);
    t.expect(result).toBe(.Some(1))
  ));
  t.it("min_of - single element", () => (
    let result := agg.min_of([7]);
    t.expect(result).toBe(.Some(7))
  ));
  t.it("min_of - empty array returns None", () => (
    let result := agg.min_of([]);
    t.expect(result).toBe(.None)
  ));
  t.it("max_of - finds maximum in array", () => (
    let result := agg.max_of([3, 1, 4, 1, 5, 9]);
    t.expect(result).toBe(.Some(9))
  ));
  t.it("max_of - single element", () => (
    let result := agg.max_of([42]);
    t.expect(result).toBe(.Some(42))
  ));
  t.it("max_of - empty array returns None", () => (
    let result := agg.max_of([]);
    t.expect(result).toBe(.None)
  ));
  t.it("count_by - counts matching elements", () => (
    let result := agg.count_by([1, 2, 3, 4, 5], (x) => x > 2);
    t.expect(result).toBe(3)
  ));
  t.it("count_by - no matches returns zero", () => (
    let result := agg.count_by([1, 2, 3], (x) => x > 10);
    t.expect(result).toBe(0)
  ));
  t.it("count_by - empty array returns zero", () => (
    let result := agg.count_by([], (x) => x > 0);
    t.expect(result).toBe(0)
  ));
  t.it("all_arr - all match returns true", () => (
    let result := agg.all_arr([2, 4, 6], (x) => x > 0);
    t.expect(result).toBe(.True)
  ));
  t.it("all_arr - one fails returns false", () => (
    let result := agg.all_arr([2, 4, 5], (x) => x % 2 = 0);
    t.expect(result).toBe(.False)
  ));
  t.it("all_arr - empty array returns true", () => (
    let result := agg.all_arr([], (x) => x > 0);
    t.expect(result).toBe(.True)
  ));
  t.it("any_arr - one matches returns true", () => (
    let result := agg.any_arr([1, 3, 5, 6], (x) => x % 2 = 0);
    t.expect(result).toBe(.True)
  ));
  t.it("any_arr - none match returns false", () => (
    let result := agg.any_arr([1, 3, 5], (x) => x % 2 = 0);
    t.expect(result).toBe(.False)
  ));
  t.it("any_arr - empty array returns false", () => (
    let result := agg.any_arr([], (x) => x > 0);
    t.expect(result).toBe(.False)
  ));
  t.it("fold_arr - folds with initial value", () => (
    let result := agg.fold_arr([1, 2, 3, 4], 0, (acc, x) => acc + x);
    t.expect(result).toBe(10)
  ));
  t.it("fold_arr - empty array returns init", () => (
    let result := agg.fold_arr([], 99, (acc, x) => acc + x);
    t.expect(result).toBe(99)
  ));
  t.it("fold_arr - string concat fold", () => (
    let result := agg.fold_arr(["a", "b", "c"], "", (acc, x) => acc + x);
    t.expect(result).toBe("abc")
  ));
  t.it("reduce_arr - reduces non-empty array", () => (
    let result := agg.reduce_arr([1, 2, 3, 4], (acc, x) => acc + x);
    t.expect(result).toBe(.Some(10))
  ));
  t.it("reduce_arr - single element returns Some", () => (
    let result := agg.reduce_arr([5], (acc, x) => acc + x);
    t.expect(result).toBe(.Some(5))
  ));
  t.it("reduce_arr - empty array returns None", () => (
    let result := agg.reduce_arr([], (acc, x) => acc + x);
    t.expect(result).toBe(.None)
  ));
  t.it("sum_arr - sums all elements", () => (
    let result := agg.sum_arr([1, 2, 3, 4, 5]);
    t.expect(result).toBe(15)
  ));
  t.it("sum_arr - empty array returns zero", () => (
    let result := agg.sum_arr([]);
    t.expect(result).toBe(0)
  ));
  t.it("sum_arr - single element", () => (
    let result := agg.sum_arr([7]);
    t.expect(result).toBe(7)
  ))
));

t.run();
