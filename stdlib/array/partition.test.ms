//! Tests for array partitioning and chunking operations.

import "@std/testing" as t;
import "@std/array/partition" as part;

t.describe("Array.partition", () => (
  t.it("partition - splits into matching and rest", () => (
    let result := part.partition([1, 2, 3, 4, 5], (x) => x % 2 = 0);
    t.expect(result).toBe(([2, 4], [1, 3, 5]))
  ));
  t.it("partition - all match", () => (
    let result := part.partition([2, 4, 6], (x) => x % 2 = 0);
    t.expect(result).toBe(([2, 4, 6], []))
  ));
  t.it("partition - none match", () => (
    let result := part.partition([1, 3, 5], (x) => x % 2 = 0);
    t.expect(result).toBe(([], [1, 3, 5]))
  ));
  t.it("partition - empty array", () => (
    let result := part.partition([], (x) => x > 0);
    t.expect(result).toBe(([], []))
  ));
  t.it("chunk - divides into chunks of given size", () => (
    let result := part.chunk([1, 2, 3, 4, 5, 6], 2);
    t.expect(result).toBe([[1, 2], [3, 4], [5, 6]])
  ));
  t.it("chunk - last chunk is smaller when not evenly divisible", () => (
    let result := part.chunk([1, 2, 3, 4, 5], 2);
    t.expect(result).toBe([[1, 2], [3, 4], [5]])
  ));
  t.it("chunk - chunk size larger than array", () => (
    let result := part.chunk([1, 2], 5);
    t.expect(result).toBe([[1, 2]])
  ));
  t.it("chunk - empty array returns empty", () => (
    let result := part.chunk([], 3);
    t.expect(result).toBe([])
  ));
  t.it("windows - produces sliding windows", () => (
    let result := part.windows([1, 2, 3, 4], 2);
    t.expect(result).toBe([[1, 2], [2, 3], [3, 4]])
  ));
  t.it("windows - window size equals length gives one window", () => (
    let result := part.windows([1, 2, 3], 3);
    t.expect(result).toBe([[1, 2, 3]])
  ));
  t.it("windows - window size larger than length returns empty", () => (
    let result := part.windows([1, 2], 5);
    t.expect(result).toBe([])
  ));
  t.it("windows - empty array returns empty", () => (
    let result := part.windows([], 2);
    t.expect(result).toBe([])
  ));
  t.it("split_at - splits at index", () => (
    let result := part.split_at([1, 2, 3, 4, 5], 3);
    t.expect(result).toBe(([1, 2, 3], [4, 5]))
  ));
  t.it("split_at - index zero gives empty first", () => (
    let result := part.split_at([1, 2, 3], 0);
    t.expect(result).toBe(([], [1, 2, 3]))
  ));
  t.it("split_at - index beyond length gives empty second", () => (
    let result := part.split_at([1, 2, 3], 10);
    t.expect(result).toBe(([1, 2, 3], []))
  ))
));

t.run();
