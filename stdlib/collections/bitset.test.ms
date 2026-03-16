//! Tests for dense bitset operations.

import "@std/testing" as t;
import "@std/collections/bitset" as bitset;

t.runner.report(t.runner.run_suite([
  ("new — all bits clear initially", () => (
    let bs := bitset.new(64);
    t.assertions.assert_false(bitset.test(bs, 0))
  )),
  ("new — count is zero", () => (
    let bs := bitset.new(64);
    t.assertions.assert_eq(bitset.count(bs), 0)
  )),
  ("set/test — bit is set after set", () => (
    let bs := bitset.new(64);
    bitset.set(bs, 5);
    t.assertions.assert_true(bitset.test(bs, 5))
  )),
  ("set — other bits unaffected", () => (
    let bs := bitset.new(64);
    bitset.set(bs, 5);
    t.assertions.assert_false(bitset.test(bs, 6))
  )),
  ("clear — clears a set bit", () => (
    let bs := bitset.new(64);
    bitset.set(bs, 10);
    bitset.clear(bs, 10);
    t.assertions.assert_false(bitset.test(bs, 10))
  )),
  ("clear — no-op on already-clear bit", () => (
    let bs := bitset.new(64);
    bitset.clear(bs, 3);
    t.assertions.assert_false(bitset.test(bs, 3))
  )),
  ("count — reflects number of set bits", () => (
    let bs := bitset.new(64);
    bitset.set(bs, 0);
    bitset.set(bs, 7);
    bitset.set(bs, 63);
    t.assertions.assert_eq(bitset.count(bs), 3)
  )),
  ("count — decrements after clear", () => (
    let bs := bitset.new(64);
    bitset.set(bs, 1);
    bitset.set(bs, 2);
    bitset.clear(bs, 1);
    t.assertions.assert_eq(bitset.count(bs), 1)
  )),
  ("union — result has bits from both", () => (
    let a := bitset.new(64);
    let b := bitset.new(64);
    bitset.set(a, 1);
    bitset.set(b, 2);
    let u := bitset.union(a, b);
    t.assertions.assert_true(bitset.test(u, 1) and bitset.test(u, 2))
  )),
  ("intersect — result has only shared bits", () => (
    let a := bitset.new(64);
    let b := bitset.new(64);
    bitset.set(a, 5);
    bitset.set(a, 10);
    bitset.set(b, 5);
    let i := bitset.intersect(a, b);
    t.assertions.assert_true(bitset.test(i, 5) and not bitset.test(i, 10))
  )),
  ("difference — result has bits in a but not b", () => (
    let a := bitset.new(64);
    let b := bitset.new(64);
    bitset.set(a, 3);
    bitset.set(a, 7);
    bitset.set(b, 7);
    let d := bitset.difference(a, b);
    t.assertions.assert_true(bitset.test(d, 3) and not bitset.test(d, 7))
  )),
  ("set — bit at word boundary (index 63)", () => (
    let bs := bitset.new(128);
    bitset.set(bs, 62);
    t.assertions.assert_true(bitset.test(bs, 62))
  )),
  ("set — bit in second word (index 63+)", () => (
    let bs := bitset.new(128);
    bitset.set(bs, 63);
    t.assertions.assert_true(bitset.test(bs, 63))
  )),
]));
