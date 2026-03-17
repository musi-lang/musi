//! Tests for iterator transformation combinators.

import "@std/testing" as t;
import "@std/iter/transform" as m;
import "@std/iter/sources" as src;
import "@std/iter/consume" as c;

t.runner.report(t.runner.run_suite([
  ("map - transforms each element", () => (
    let iter := m.map(src.range(0, 3), (x) => x * 2);
    let result := c.collect_array(iter);
    t.assertions.assert_eq(result.[0], 0)
  )),
  ("map - correct count", () => (
    let iter := m.map(src.range(0, 5), (x) => x + 1);
    t.assertions.assert_eq(c.count(iter), 5)
  )),
  ("map - empty source", () => (
    let iter := m.map(src.empty(), (x) => x);
    t.assertions.assert_eq(c.count(iter), 0)
  )),
  ("filter - keeps matching elements", () => (
    let iter := m.filter(src.range(0, 6), (x) => x % 2 = 0);
    t.assertions.assert_eq(c.count(iter), 3)
  )),
  ("filter - empty when none match", () => (
    let iter := m.filter(src.range(0, 5), (x) => x > 100);
    t.assertions.assert_eq(c.count(iter), 0)
  )),
  ("take - limits element count", () => (
    let iter := m.take(src.range(0, 100), 3);
    t.assertions.assert_eq(c.count(iter), 3)
  )),
  ("take - zero produces nothing", () => (
    let iter := m.take(src.range(0, 10), 0);
    t.assertions.assert_eq(c.count(iter), 0)
  )),
  ("take - more than available", () => (
    let iter := m.take(src.range(0, 3), 10);
    t.assertions.assert_eq(c.count(iter), 3)
  )),
  ("skip - skips first n elements", () => (
    let iter := m.skip(src.range(0, 5), 2);
    let result := c.collect_array(iter);
    t.assertions.assert_eq(result.[0], 2)
  )),
  ("skip - count reduced by n", () => (
    let iter := m.skip(src.range(0, 5), 3);
    t.assertions.assert_eq(c.count(iter), 2)
  )),
  ("zip - pairs elements", () => (
    let iter := m.zip(src.range(0, 3), src.range(10, 13));
    let result := c.collect_array(iter);
    t.assertions.assert_eq(result.[0].0, 0)
  )),
  ("zip - stops at shorter", () => (
    let iter := m.zip(src.range(0, 2), src.range(0, 5));
    t.assertions.assert_eq(c.count(iter), 2)
  )),
  ("chain - concatenates iterators", () => (
    let iter := m.chain(src.range(0, 2), src.range(5, 7));
    t.assertions.assert_eq(c.count(iter), 4)
  )),
  ("enumerate - pairs index and value", () => (
    let iter := m.enumerate(src.from_array(["a", "b", "c"]));
    let result := c.collect_array(iter);
    t.assertions.assert_eq(result.[0].0, 0)
  )),
  ("flat_map - flattens mapped iterators", () => (
    let iter := m.flat_map(src.range(0, 3), (x) => src.range(0, x));
    t.assertions.assert_eq(c.count(iter), 3)
  )),
  ("take_while - stops at first false", () => (
    let iter := m.take_while(src.range(0, 10), (x) => x < 5);
    t.assertions.assert_eq(c.count(iter), 5)
  )),
  ("skip_while - skips leading matches", () => (
    let iter := m.skip_while(src.range(0, 5), (x) => x < 3);
    let result := c.collect_array(iter);
    t.assertions.assert_eq(result.[0], 3)
  )),
]));
