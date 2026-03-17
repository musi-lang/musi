//! Tests for iterator consumer (terminal) operations.

import "@std/testing" as t;
import "@std/iter/consume" as m;
import "@std/iter/sources" as src;

t.runner.report(t.runner.run_suite([
  ("fold - accumulates values", () => (
    let iter := src.range(1, 5);
    t.assertions.assert_eq(m.fold(iter, 0, (acc, x) => acc + x), 10)
  )),
  ("fold - empty iterator returns init", () => (
    t.assertions.assert_eq(m.fold(src.empty(), 42, (acc, x) => acc + x), 42)
  )),
  ("collect_array - gathers all elements", () => (
    let result := m.collect_array(src.range(0, 3));
    t.assertions.assert_eq(result.[2], 2)
  )),
  ("collect_array - empty produces empty array", () => (
    let result := m.collect_array(src.empty());
    t.assertions.assert_true(.True)
  )),
  ("count - counts all elements", () => (
    t.assertions.assert_eq(m.count(src.range(0, 7)), 7)
  )),
  ("count - empty is zero", () => (
    t.assertions.assert_eq(m.count(src.empty()), 0)
  )),
  ("any - true when predicate matches", () => (
    t.assertions.assert_true(m.any(src.range(0, 5), (x) => x = 3))
  )),
  ("any - false when nothing matches", () => (
    t.assertions.assert_false(m.any(src.range(0, 5), (x) => x > 100))
  )),
  ("any - false on empty", () => (
    t.assertions.assert_false(m.any(src.empty(), (x) => .True))
  )),
  ("all - true when all match", () => (
    t.assertions.assert_true(m.all(src.range(0, 5), (x) => x < 10))
  )),
  ("all - false when one doesn't match", () => (
    t.assertions.assert_false(m.all(src.range(0, 5), (x) => x < 3))
  )),
  ("all - true on empty", () => (
    t.assertions.assert_true(m.all(src.empty(), (x) => .False))
  )),
  ("find - returns first match", () => (
    t.assertions.assert_eq(m.find(src.range(0, 5), (x) => x > 2), .Some(3))
  )),
  ("find - None when nothing matches", () => (
    t.assertions.assert_eq(m.find(src.range(0, 5), (x) => x > 100), .None)
  )),
  ("sum - sums all elements", () => (
    t.assertions.assert_eq(m.sum(src.range(1, 6)), 15)
  )),
  ("sum - empty is zero", () => (
    t.assertions.assert_eq(m.sum(src.empty()), 0)
  )),
  ("for_each - executes for every element", () => (
    let mut n := 0;
    m.for_each(src.range(0, 5), (x) => (n <- n + 1));
    t.assertions.assert_eq(n, 5)
  )),
]));
