//! Tests for array combination operations.

import "@std/testing" as t;
import "@std/array/combine" as comb;

t.describe("Array.combine", () => (
  t.it("zip_arr - pairs elements at same index", () => (
    let result := comb.zip_arr([1, 2, 3], ["a", "b", "c"]);
    t.expect(result).toBe([(1, "a"), (2, "b"), (3, "c")])
  ));
  t.it("zip_arr - stops at shorter array", () => (
    let result := comb.zip_arr([1, 2, 3], ["a", "b"]);
    t.expect(result).toBe([(1, "a"), (2, "b")])
  ));
  t.it("zip_arr - empty first array returns empty", () => (
    let result := comb.zip_arr([], [1, 2, 3]);
    t.expect(result).toBe([])
  ));
  t.it("zip_arr - empty second array returns empty", () => (
    let result := comb.zip_arr([1, 2, 3], []);
    t.expect(result).toBe([])
  ));
  t.it("unzip - splits array of pairs into two arrays", () => (
    let result := comb.unzip([(1, "a"), (2, "b"), (3, "c")]);
    t.expect(result).toBe(([1, 2, 3], ["a", "b", "c"]))
  ));
  t.it("unzip - single pair", () => (
    let result := comb.unzip([(5, "x")]);
    t.expect(result).toBe(([5], ["x"]))
  ));
  t.it("unzip - empty array produces two empty arrays", () => (
    let result := comb.unzip([]);
    t.expect(result).toBe(([], []))
  ));
  t.it("zip_arr then unzip - roundtrip", () => (
    let zipped := comb.zip_arr([1, 2, 3], [4, 5, 6]);
    let (lefts, _) := comb.unzip(zipped);
    t.expect(lefts).toBe([1, 2, 3])
  ))
));

t.run();
