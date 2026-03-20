//! Tests for dense bitset operations.

import "@std/testing" as t;
import "@std/collections/bitset" as bitset;

t.describe("Bitset", () => (
  t.it("new - all bits clear initially", () => (
    let bs := bitset.new(64);
    t.expect(bitset.test(bs, 0)).toBe(.False)
  ));
  t.it("new - count is zero", () => (
    let bs := bitset.new(64);
    t.expect(bitset.count(bs)).toBe(0)
  ));
  t.it("set/test - bit is set after set", () => (
    let bs := bitset.new(64);
    bitset.set(bs, 5);
    t.expect(bitset.test(bs, 5)).toBe(.True)
  ));
  t.it("set - other bits unaffected", () => (
    let bs := bitset.new(64);
    bitset.set(bs, 5);
    t.expect(bitset.test(bs, 6)).toBe(.False)
  ));
  t.it("clear - clears a set bit", () => (
    let bs := bitset.new(64);
    bitset.set(bs, 10);
    bitset.clear(bs, 10);
    t.expect(bitset.test(bs, 10)).toBe(.False)
  ));
  t.it("clear - no-op on already-clear bit", () => (
    let bs := bitset.new(64);
    bitset.clear(bs, 3);
    t.expect(bitset.test(bs, 3)).toBe(.False)
  ));
  t.it("count - reflects number of set bits", () => (
    let bs := bitset.new(64);
    bitset.set(bs, 0);
    bitset.set(bs, 7);
    bitset.set(bs, 63);
    t.expect(bitset.count(bs)).toBe(3)
  ));
  t.it("count - decrements after clear", () => (
    let bs := bitset.new(64);
    bitset.set(bs, 1);
    bitset.set(bs, 2);
    bitset.clear(bs, 1);
    t.expect(bitset.count(bs)).toBe(1)
  ));
  t.it("union - result has bits from both", () => (
    let a := bitset.new(64);
    let b := bitset.new(64);
    bitset.set(a, 1);
    bitset.set(b, 2);
    let u := bitset.union(a, b);
    t.expect(bitset.test(u, 1) and bitset.test(u, 2)).toBe(.True)
  ));
  t.it("intersect - result has only shared bits", () => (
    let a := bitset.new(64);
    let b := bitset.new(64);
    bitset.set(a, 5);
    bitset.set(a, 10);
    bitset.set(b, 5);
    let i := bitset.intersect(a, b);
    t.expect(bitset.test(i, 5) and not bitset.test(i, 10)).toBe(.True)
  ));
  t.it("difference - result has bits in a but not b", () => (
    let a := bitset.new(64);
    let b := bitset.new(64);
    bitset.set(a, 3);
    bitset.set(a, 7);
    bitset.set(b, 7);
    let d := bitset.difference(a, b);
    t.expect(bitset.test(d, 3) and not bitset.test(d, 7)).toBe(.True)
  ));
  t.it("set - bit at word boundary (index 63)", () => (
    let bs := bitset.new(128);
    bitset.set(bs, 62);
    t.expect(bitset.test(bs, 62)).toBe(.True)
  ));
  t.it("set - bit in second word (index 63+)", () => (
    let bs := bitset.new(128);
    bitset.set(bs, 63);
    t.expect(bitset.test(bs, 63)).toBe(.True)
  ))
));

t.run();
