//! Tests for iterator source constructors.

import "@std/testing" as t;
import "@std/iter/sources" as m;
import "@std/iter/consume" as c;

t.describe("Iter.sources", () => (
  t.it("from_array - collects all elements", () => (
    let iter := m.from_array([1, 2, 3]);
    let result := c.collect_array(iter);
    t.expect(result.[0]).toBe(1)
  ));
  t.it("from_array - empty array produces no elements", () => (
    let iter := m.from_array([]);
    t.expect(c.count(iter)).toBe(0)
  ));
  t.it("from_array - count matches input length", () => (
    let iter := m.from_array([10, 20, 30]);
    t.expect(c.count(iter)).toBe(3)
  ));
  t.it("range - produces values from start to end exclusive", () => (
    let iter := m.range(0, 3);
    let result := c.collect_array(iter);
    t.expect(result.[0]).toBe(0)
  ));
  t.it("range - correct count", () => (
    let iter := m.range(5, 10);
    t.expect(c.count(iter)).toBe(5)
  ));
  t.it("range - empty when start equals end", () => (
    let iter := m.range(3, 3);
    t.expect(c.count(iter)).toBe(0)
  ));
  t.it("repeat - always produces value", () => (
    let iter := m.repeat(42);
    t.expect(iter.next()).toBe(.Some(42))
  ));
  t.it("repeat - second call still produces value", () => (
    let iter := m.repeat(7);
    iter.next();
    t.expect(iter.next()).toBe(.Some(7))
  ));
  t.it("empty - produces nothing", () => (
    let iter := m.empty();
    t.expect(iter.next()).toBe(.None)
  ));
  t.it("empty - count is zero", () => (
    let iter := m.empty();
    t.expect(c.count(iter)).toBe(0)
  ))
));

t.run();
