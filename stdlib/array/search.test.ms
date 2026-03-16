//! Tests for array search operations.

import "@std/testing" as t;
import "@std/array/search" as srch;

t.runner.report(t.runner.run_suite([
  ("first — non-empty returns Some of first element", () => (
    let result := srch.first([10, 20, 30]);
    t.assertions.assert_eq(result, .Some(10))
  )),
  ("first — empty returns None", () => (
    let result := srch.first([]);
    t.assertions.assert_eq(result, .None)
  )),
  ("last — non-empty returns Some of last element", () => (
    let result := srch.last([10, 20, 30]);
    t.assertions.assert_eq(result, .Some(30))
  )),
  ("last — single element", () => (
    let result := srch.last([42]);
    t.assertions.assert_eq(result, .Some(42))
  )),
  ("last — empty returns None", () => (
    let result := srch.last([]);
    t.assertions.assert_eq(result, .None)
  )),
  ("find_index — predicate matches first element", () => (
    let result := srch.find_index([5, 3, 8], (x) => x > 4);
    t.assertions.assert_eq(result, .Some(0))
  )),
  ("find_index — predicate matches middle element", () => (
    let result := srch.find_index([1, 2, 7, 4], (x) => x > 5);
    t.assertions.assert_eq(result, .Some(2))
  )),
  ("find_index — no match returns None", () => (
    let result := srch.find_index([1, 2, 3], (x) => x > 10);
    t.assertions.assert_eq(result, .None)
  )),
  ("find_index — empty array returns None", () => (
    let result := srch.find_index([], (x) => x > 0);
    t.assertions.assert_eq(result, .None)
  )),
  ("find_last_index — finds last matching element", () => (
    let result := srch.find_last_index([1, 5, 2, 6, 3], (x) => x > 4);
    t.assertions.assert_eq(result, .Some(3))
  )),
  ("find_last_index — single match", () => (
    let result := srch.find_last_index([1, 2, 10], (x) => x > 5);
    t.assertions.assert_eq(result, .Some(2))
  )),
  ("find_last_index — no match returns None", () => (
    let result := srch.find_last_index([1, 2, 3], (x) => x > 10);
    t.assertions.assert_eq(result, .None)
  )),
  ("find_last_index — empty array returns None", () => (
    let result := srch.find_last_index([], (x) => x > 0);
    t.assertions.assert_eq(result, .None)
  )),
]));
