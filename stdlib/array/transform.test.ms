//! Tests for array transformation operations.

import "@std/testing" as t;
import "@std/array/transform" as tr;

t.describe("Array.transform", () => (
  t.it("map_arr - applies function to each element", () => (
    let result := tr.map_arr([1, 2, 3], (x) => x * 2);
    t.expect(result).toBe([2, 4, 6])
  ));
  t.it("map_arr - empty array stays empty", () => (
    let result := tr.map_arr([], (x) => x * 2);
    t.expect(result).toBe([])
  ));
  t.it("filter_arr - keeps matching elements", () => (
    let result := tr.filter_arr([1, 2, 3, 4, 5], (x) => x > 2);
    t.expect(result).toBe([3, 4, 5])
  ));
  t.it("filter_arr - no matches returns empty", () => (
    let result := tr.filter_arr([1, 2, 3], (x) => x > 10);
    t.expect(result).toBe([])
  ));
  t.it("filter_arr - empty array stays empty", () => (
    let result := tr.filter_arr([], (x) => x > 0);
    t.expect(result).toBe([])
  ));
  t.it("flat_map_arr - flattens mapped subarrays", () => (
    let result := tr.flat_map_arr([1, 2, 3], (x) => [x, x * 10]);
    t.expect(result).toBe([1, 10, 2, 20, 3, 30])
  ));
  t.it("flat_map_arr - empty subarrays result in empty", () => (
    let result := tr.flat_map_arr([1, 2], (_) => []);
    t.expect(result).toBe([])
  ));
  t.it("flat_map_arr - empty input returns empty", () => (
    let result := tr.flat_map_arr([], (x) => [x]);
    t.expect(result).toBe([])
  ));
  t.it("take_arr - takes first n elements", () => (
    let result := tr.take_arr([1, 2, 3, 4, 5], 3);
    t.expect(result).toBe([1, 2, 3])
  ));
  t.it("take_arr - n larger than length takes all", () => (
    let result := tr.take_arr([1, 2], 10);
    t.expect(result).toBe([1, 2])
  ));
  t.it("take_arr - n is zero returns empty", () => (
    let result := tr.take_arr([1, 2, 3], 0);
    t.expect(result).toBe([])
  ));
  t.it("drop_arr - drops first n elements", () => (
    let result := tr.drop_arr([1, 2, 3, 4, 5], 2);
    t.expect(result).toBe([3, 4, 5])
  ));
  t.it("drop_arr - n larger than length returns empty", () => (
    let result := tr.drop_arr([1, 2], 10);
    t.expect(result).toBe([])
  ));
  t.it("drop_arr - n is zero returns all", () => (
    let result := tr.drop_arr([1, 2, 3], 0);
    t.expect(result).toBe([1, 2, 3])
  ));
  t.it("reverse_copy - reverses elements", () => (
    let result := tr.reverse_copy([1, 2, 3, 4]);
    t.expect(result).toBe([4, 3, 2, 1])
  ));
  t.it("reverse_copy - single element unchanged", () => (
    let result := tr.reverse_copy([5]);
    t.expect(result).toBe([5])
  ));
  t.it("reverse_copy - empty array stays empty", () => (
    let result := tr.reverse_copy([]);
    t.expect(result).toBe([])
  ));
  t.it("dedup - removes consecutive duplicates", () => (
    let result := tr.dedup([1, 1, 2, 3, 3, 3, 4]);
    t.expect(result).toBe([1, 2, 3, 4])
  ));
  t.it("dedup - no duplicates unchanged", () => (
    let result := tr.dedup([1, 2, 3]);
    t.expect(result).toBe([1, 2, 3])
  ));
  t.it("dedup - empty array stays empty", () => (
    let result := tr.dedup([]);
    t.expect(result).toBe([])
  ));
  t.it("intersperse - inserts separator between elements", () => (
    let result := tr.intersperse([1, 2, 3], 0);
    t.expect(result).toBe([1, 0, 2, 0, 3])
  ));
  t.it("intersperse - single element has no separator", () => (
    let result := tr.intersperse([9], 0);
    t.expect(result).toBe([9])
  ));
  t.it("scan - accumulates intermediate values", () => (
    let result := tr.scan([1, 2, 3, 4], 0, (acc, x) => acc + x);
    t.expect(result).toBe([1, 3, 6, 10])
  ));
  t.it("scan - empty array returns empty", () => (
    let result := tr.scan([], 0, (acc, x) => acc + x);
    t.expect(result).toBe([])
  ))
));

t.run();
