//! Tests for iterator consumer (terminal) operations.

import "@std/testing" as t;
import "@std/iter/consume" as m;
import "@std/iter/sources" as src;

t.describe("Iter.consume", () => (
  t.it("fold - accumulates values", () => (
    let iter := src.range(1, 5);
    t.expect(m.fold(iter, 0, (acc, x) => acc + x)).toBe(10)
  ));
  t.it("fold - empty iterator returns init", () => (
    t.expect(m.fold(src.empty(), 42, (acc, x) => acc + x)).toBe(42)
  ));
  t.it("collect_array - gathers all elements", () => (
    let result := m.collect_array(src.range(0, 3));
    t.expect(result.[2]).toBe(2)
  ));
  t.it("collect_array - empty produces empty array", () => (
    let _result := m.collect_array(src.empty());
    t.expect(.True).toBe(.True)
  ));
  t.it("count - counts all elements", () => (
    t.expect(m.count(src.range(0, 7))).toBe(7)
  ));
  t.it("count - empty is zero", () => (
    t.expect(m.count(src.empty())).toBe(0)
  ));
  t.it("any - true when predicate matches", () => (
    t.expect(m.any(src.range(0, 5), (x) => x = 3)).toBe(.True)
  ));
  t.it("any - false when nothing matches", () => (
    t.expect(m.any(src.range(0, 5), (x) => x > 100)).toBe(.False)
  ));
  t.it("any - false on empty", () => (
    t.expect(m.any(src.empty(), (_) => .True)).toBe(.False)
  ));
  t.it("all - true when all match", () => (
    t.expect(m.all(src.range(0, 5), (x) => x < 10)).toBe(.True)
  ));
  t.it("all - false when one doesn't match", () => (
    t.expect(m.all(src.range(0, 5), (x) => x < 3)).toBe(.False)
  ));
  t.it("all - true on empty", () => (
    t.expect(m.all(src.empty(), (_) => .False)).toBe(.True)
  ));
  t.it("find - returns first match", () => (
    t.expect(m.find(src.range(0, 5), (x) => x > 2)).toBe(.Some(3))
  ));
  t.it("find - None when nothing matches", () => (
    t.expect(m.find(src.range(0, 5), (x) => x > 100)).toBe(.None)
  ));
  t.it("sum - sums all elements", () => (
    t.expect(m.sum(src.range(1, 6))).toBe(15)
  ));
  t.it("sum - empty is zero", () => (
    t.expect(m.sum(src.empty())).toBe(0)
  ));
  t.it("for_each - executes for every element", () => (
    let mut n := 0;
    m.for_each(src.range(0, 5), (_) => (n <- n + 1));
    t.expect(n).toBe(5)
  ))
));

t.run();
