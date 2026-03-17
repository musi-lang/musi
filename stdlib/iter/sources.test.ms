//! Tests for iterator source constructors.

import "@std/testing" as t;
import "@std/iter/sources" as m;
import "@std/iter/consume" as c;

t.runner.report(t.runner.run_suite([
  ("from_array - collects all elements", () => (
    let iter := m.from_array([1, 2, 3]);
    let result := c.collect_array(iter);
    t.assertions.assert_eq(result.[0], 1)
  )),
  ("from_array - empty array produces no elements", () => (
    let iter := m.from_array([]);
    t.assertions.assert_eq(c.count(iter), 0)
  )),
  ("from_array - count matches input length", () => (
    let iter := m.from_array([10, 20, 30]);
    t.assertions.assert_eq(c.count(iter), 3)
  )),
  ("range - produces values from start to end exclusive", () => (
    let iter := m.range(0, 3);
    let result := c.collect_array(iter);
    t.assertions.assert_eq(result.[0], 0)
  )),
  ("range - correct count", () => (
    let iter := m.range(5, 10);
    t.assertions.assert_eq(c.count(iter), 5)
  )),
  ("range - empty when start equals end", () => (
    let iter := m.range(3, 3);
    t.assertions.assert_eq(c.count(iter), 0)
  )),
  ("repeat - always produces value", () => (
    let iter := m.repeat(42);
    t.assertions.assert_eq(iter.next(), .Some(42))
  )),
  ("repeat - second call still produces value", () => (
    let iter := m.repeat(7);
    iter.next();
    t.assertions.assert_eq(iter.next(), .Some(7))
  )),
  ("empty - produces nothing", () => (
    let iter := m.empty();
    t.assertions.assert_eq(iter.next(), .None)
  )),
  ("empty - count is zero", () => (
    let iter := m.empty();
    t.assertions.assert_eq(c.count(iter), 0)
  )),
]));
