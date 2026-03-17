//! Tests for array combination operations.

import "@std/testing" as t;
import "@std/array/combine" as comb;

t.runner.report(t.runner.run_suite([
  ("zip_arr - pairs elements at same index", () => (
    let result := comb.zip_arr([1, 2, 3], ["a", "b", "c"]);
    t.assertions.assert_eq(result, [(1, "a"), (2, "b"), (3, "c")])
  )),
  ("zip_arr - stops at shorter array", () => (
    let result := comb.zip_arr([1, 2, 3], ["a", "b"]);
    t.assertions.assert_eq(result, [(1, "a"), (2, "b")])
  )),
  ("zip_arr - empty first array returns empty", () => (
    let result := comb.zip_arr([], [1, 2, 3]);
    t.assertions.assert_eq(result, [])
  )),
  ("zip_arr - empty second array returns empty", () => (
    let result := comb.zip_arr([1, 2, 3], []);
    t.assertions.assert_eq(result, [])
  )),
  ("unzip - splits array of pairs into two arrays", () => (
    let result := comb.unzip([(1, "a"), (2, "b"), (3, "c")]);
    t.assertions.assert_eq(result, ([1, 2, 3], ["a", "b", "c"]))
  )),
  ("unzip - single pair", () => (
    let result := comb.unzip([(5, "x")]);
    t.assertions.assert_eq(result, ([5], ["x"]))
  )),
  ("unzip - empty array produces two empty arrays", () => (
    let result := comb.unzip([]);
    t.assertions.assert_eq(result, ([], []))
  )),
  ("zip_arr then unzip - roundtrip", () => (
    let zipped := comb.zip_arr([1, 2, 3], [4, 5, 6]);
    let (lefts, _) := comb.unzip(zipped);
    t.assertions.assert_eq(lefts, [1, 2, 3])
  )),
]));
