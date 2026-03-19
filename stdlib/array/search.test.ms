//! Tests for array search operations.

import "@std/testing" as t;
import "@std/array/search" as srch;

t.describe("Array.search", () => (
  t.it("first - non-empty returns Some of first element", () => (
    let result := srch.first([10, 20, 30]);
    t.expect(result).toBe(.Some(10))
  ));
  t.it("first - empty returns None", () => (
    let result := srch.first([]);
    t.expect(result).toBe(.None)
  ));
  t.it("last - non-empty returns Some of last element", () => (
    let result := srch.last([10, 20, 30]);
    t.expect(result).toBe(.Some(30))
  ));
  t.it("last - single element", () => (
    let result := srch.last([42]);
    t.expect(result).toBe(.Some(42))
  ));
  t.it("last - empty returns None", () => (
    let result := srch.last([]);
    t.expect(result).toBe(.None)
  ));
  t.it("find_index - predicate matches first element", () => (
    let result := srch.find_index([5, 3, 8], (x) => x > 4);
    t.expect(result).toBe(.Some(0))
  ));
  t.it("find_index - predicate matches middle element", () => (
    let result := srch.find_index([1, 2, 7, 4], (x) => x > 5);
    t.expect(result).toBe(.Some(2))
  ));
  t.it("find_index - no match returns None", () => (
    let result := srch.find_index([1, 2, 3], (x) => x > 10);
    t.expect(result).toBe(.None)
  ));
  t.it("find_index - empty array returns None", () => (
    let result := srch.find_index([], (x) => x > 0);
    t.expect(result).toBe(.None)
  ));
  t.it("find_last_index - finds last matching element", () => (
    let result := srch.find_last_index([1, 5, 2, 6, 3], (x) => x > 4);
    t.expect(result).toBe(.Some(3))
  ));
  t.it("find_last_index - single match", () => (
    let result := srch.find_last_index([1, 2, 10], (x) => x > 5);
    t.expect(result).toBe(.Some(2))
  ));
  t.it("find_last_index - no match returns None", () => (
    let result := srch.find_last_index([1, 2, 3], (x) => x > 10);
    t.expect(result).toBe(.None)
  ));
  t.it("find_last_index - empty array returns None", () => (
    let result := srch.find_last_index([], (x) => x > 0);
    t.expect(result).toBe(.None)
  ))
));

t.run();
