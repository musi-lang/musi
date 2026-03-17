//! Tests for array transformation operations.

import "@std/testing" as t;
import "@std/array/transform" as tr;

t.runner.report(t.runner.run_suite([
  ("map_arr - applies function to each element", () => (
    let result := tr.map_arr([1, 2, 3], (x) => x * 2);
    t.assertions.assert_eq(result, [2, 4, 6])
  )),
  ("map_arr - empty array stays empty", () => (
    let result := tr.map_arr([], (x) => x * 2);
    t.assertions.assert_eq(result, [])
  )),
  ("filter_arr - keeps matching elements", () => (
    let result := tr.filter_arr([1, 2, 3, 4, 5], (x) => x > 2);
    t.assertions.assert_eq(result, [3, 4, 5])
  )),
  ("filter_arr - no matches returns empty", () => (
    let result := tr.filter_arr([1, 2, 3], (x) => x > 10);
    t.assertions.assert_eq(result, [])
  )),
  ("filter_arr - empty array stays empty", () => (
    let result := tr.filter_arr([], (x) => x > 0);
    t.assertions.assert_eq(result, [])
  )),
  ("flat_map_arr - flattens mapped subarrays", () => (
    let result := tr.flat_map_arr([1, 2, 3], (x) => [x, x * 10]);
    t.assertions.assert_eq(result, [1, 10, 2, 20, 3, 30])
  )),
  ("flat_map_arr - empty subarrays result in empty", () => (
    let result := tr.flat_map_arr([1, 2], (_) => []);
    t.assertions.assert_eq(result, [])
  )),
  ("flat_map_arr - empty input returns empty", () => (
    let result := tr.flat_map_arr([], (x) => [x]);
    t.assertions.assert_eq(result, [])
  )),
  ("take_arr - takes first n elements", () => (
    let result := tr.take_arr([1, 2, 3, 4, 5], 3);
    t.assertions.assert_eq(result, [1, 2, 3])
  )),
  ("take_arr - n larger than length takes all", () => (
    let result := tr.take_arr([1, 2], 10);
    t.assertions.assert_eq(result, [1, 2])
  )),
  ("take_arr - n is zero returns empty", () => (
    let result := tr.take_arr([1, 2, 3], 0);
    t.assertions.assert_eq(result, [])
  )),
  ("drop_arr - drops first n elements", () => (
    let result := tr.drop_arr([1, 2, 3, 4, 5], 2);
    t.assertions.assert_eq(result, [3, 4, 5])
  )),
  ("drop_arr - n larger than length returns empty", () => (
    let result := tr.drop_arr([1, 2], 10);
    t.assertions.assert_eq(result, [])
  )),
  ("drop_arr - n is zero returns all", () => (
    let result := tr.drop_arr([1, 2, 3], 0);
    t.assertions.assert_eq(result, [1, 2, 3])
  )),
  ("reverse_copy - reverses elements", () => (
    let result := tr.reverse_copy([1, 2, 3, 4]);
    t.assertions.assert_eq(result, [4, 3, 2, 1])
  )),
  ("reverse_copy - single element unchanged", () => (
    let result := tr.reverse_copy([5]);
    t.assertions.assert_eq(result, [5])
  )),
  ("reverse_copy - empty array stays empty", () => (
    let result := tr.reverse_copy([]);
    t.assertions.assert_eq(result, [])
  )),
  ("dedup - removes consecutive duplicates", () => (
    let result := tr.dedup([1, 1, 2, 3, 3, 3, 4]);
    t.assertions.assert_eq(result, [1, 2, 3, 4])
  )),
  ("dedup - no duplicates unchanged", () => (
    let result := tr.dedup([1, 2, 3]);
    t.assertions.assert_eq(result, [1, 2, 3])
  )),
  ("dedup - empty array stays empty", () => (
    let result := tr.dedup([]);
    t.assertions.assert_eq(result, [])
  )),
  ("intersperse - inserts separator between elements", () => (
    let result := tr.intersperse([1, 2, 3], 0);
    t.assertions.assert_eq(result, [1, 0, 2, 0, 3])
  )),
  ("intersperse - single element has no separator", () => (
    let result := tr.intersperse([9], 0);
    t.assertions.assert_eq(result, [9])
  )),
  ("scan - accumulates intermediate values", () => (
    let result := tr.scan([1, 2, 3, 4], 0, (acc, x) => acc + x);
    t.assertions.assert_eq(result, [1, 3, 6, 10])
  )),
  ("scan - empty array returns empty", () => (
    let result := tr.scan([], 0, (acc, x) => acc + x);
    t.assertions.assert_eq(result, [])
  )),
]));
