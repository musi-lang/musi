//! Tests for array partitioning and chunking operations.

import "@std/testing" as t;
import "@std/array/partition" as part;

t.runner.report(t.runner.run_suite([
  ("partition — splits into matching and rest", () => (
    let result := part.partition([1, 2, 3, 4, 5], (x) => x % 2 = 0);
    t.assertions.assert_eq(result, ([2, 4], [1, 3, 5]))
  )),
  ("partition — all match", () => (
    let result := part.partition([2, 4, 6], (x) => x % 2 = 0);
    t.assertions.assert_eq(result, ([2, 4, 6], []))
  )),
  ("partition — none match", () => (
    let result := part.partition([1, 3, 5], (x) => x % 2 = 0);
    t.assertions.assert_eq(result, ([], [1, 3, 5]))
  )),
  ("partition — empty array", () => (
    let result := part.partition([], (x) => x > 0);
    t.assertions.assert_eq(result, ([], []))
  )),
  ("chunk — divides into chunks of given size", () => (
    let result := part.chunk([1, 2, 3, 4, 5, 6], 2);
    t.assertions.assert_eq(result, [[1, 2], [3, 4], [5, 6]])
  )),
  ("chunk — last chunk is smaller when not evenly divisible", () => (
    let result := part.chunk([1, 2, 3, 4, 5], 2);
    t.assertions.assert_eq(result, [[1, 2], [3, 4], [5]])
  )),
  ("chunk — chunk size larger than array", () => (
    let result := part.chunk([1, 2], 5);
    t.assertions.assert_eq(result, [[1, 2]])
  )),
  ("chunk — empty array returns empty", () => (
    let result := part.chunk([], 3);
    t.assertions.assert_eq(result, [])
  )),
  ("windows — produces sliding windows", () => (
    let result := part.windows([1, 2, 3, 4], 2);
    t.assertions.assert_eq(result, [[1, 2], [2, 3], [3, 4]])
  )),
  ("windows — window size equals length gives one window", () => (
    let result := part.windows([1, 2, 3], 3);
    t.assertions.assert_eq(result, [[1, 2, 3]])
  )),
  ("windows — window size larger than length returns empty", () => (
    let result := part.windows([1, 2], 5);
    t.assertions.assert_eq(result, [])
  )),
  ("windows — empty array returns empty", () => (
    let result := part.windows([], 2);
    t.assertions.assert_eq(result, [])
  )),
  ("split_at — splits at index", () => (
    let result := part.split_at([1, 2, 3, 4, 5], 3);
    t.assertions.assert_eq(result, ([1, 2, 3], [4, 5]))
  )),
  ("split_at — index zero gives empty first", () => (
    let result := part.split_at([1, 2, 3], 0);
    t.assertions.assert_eq(result, ([], [1, 2, 3]))
  )),
  ("split_at — index beyond length gives empty second", () => (
    let result := part.split_at([1, 2, 3], 10);
    t.assertions.assert_eq(result, ([1, 2, 3], []))
  )),
]));
