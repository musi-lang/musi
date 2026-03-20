//! Tests for iterator transformation combinators.

import "@std/testing" as t;
import "@std/iter/transform" as m;
import "@std/iter/sources" as src;
import "@std/iter/consume" as c;

t.describe("Iter.transform", () => (
  t.it("map - transforms each element", () => (
    let iter := m.map(src.range(0, 3), (x) => x * 2);
    let result := c.collect_array(iter);
    t.expect(result.[0]).toBe(0)
  ));
  t.it("map - correct count", () => (
    let iter := m.map(src.range(0, 5), (x) => x + 1);
    t.expect(c.count(iter)).toBe(5)
  ));
  t.it("map - empty source", () => (
    let iter := m.map(src.empty(), (x : Int) => x);
    t.expect(c.count(iter)).toBe(0)
  ));
  t.it("filter - keeps matching elements", () => (
    let iter := m.filter(src.range(0, 6), (x) => x % 2 = 0);
    t.expect(c.count(iter)).toBe(3)
  ));
  t.it("filter - empty when none match", () => (
    let iter := m.filter(src.range(0, 5), (x) => x > 100);
    t.expect(c.count(iter)).toBe(0)
  ));
  t.it("take - limits element count", () => (
    let iter := m.take(src.range(0, 100), 3);
    t.expect(c.count(iter)).toBe(3)
  ));
  t.it("take - zero produces nothing", () => (
    let iter := m.take(src.range(0, 10), 0);
    t.expect(c.count(iter)).toBe(0)
  ));
  t.it("take - more than available", () => (
    let iter := m.take(src.range(0, 3), 10);
    t.expect(c.count(iter)).toBe(3)
  ));
  t.it("skip - skips first n elements", () => (
    let iter := m.skip(src.range(0, 5), 2);
    let result := c.collect_array(iter);
    t.expect(result.[0]).toBe(2)
  ));
  t.it("skip - count reduced by n", () => (
    let iter := m.skip(src.range(0, 5), 3);
    t.expect(c.count(iter)).toBe(2)
  ));
  t.it("zip - pairs elements", () => (
    let iter := m.zip(src.range(0, 3), src.range(10, 13));
    let result := c.collect_array(iter);
    t.expect(result.[0].0).toBe(0)
  ));
  t.it("zip - stops at shorter", () => (
    let iter := m.zip(src.range(0, 2), src.range(0, 5));
    t.expect(c.count(iter)).toBe(2)
  ));
  t.it("chain - concatenates iterators", () => (
    let iter := m.chain(src.range(0, 2), src.range(5, 7));
    t.expect(c.count(iter)).toBe(4)
  ));
  t.it("enumerate - pairs index and value", () => (
    let iter := m.enumerate(src.from_array(["a", "b", "c"]));
    let result := c.collect_array(iter);
    t.expect(result.[0].0).toBe(0)
  ));
  t.it("flat_map - flattens mapped iterators", () => (
    let iter := m.flat_map(src.range(0, 3), (x) => src.range(0, x));
    t.expect(c.count(iter)).toBe(3)
  ));
  t.it("take_while - stops at first false", () => (
    let iter := m.take_while(src.range(0, 10), (x) => x < 5);
    t.expect(c.count(iter)).toBe(5)
  ));
  t.it("skip_while - skips leading matches", () => (
    let iter := m.skip_while(src.range(0, 5), (x) => x < 3);
    let result := c.collect_array(iter);
    t.expect(result.[0]).toBe(3)
  ))
));

t.run();
