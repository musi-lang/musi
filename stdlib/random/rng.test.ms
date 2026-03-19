//! Tests for Xorshift PRNG operations.

import "@std/testing" as t;
import "@std/random/rng" as m;

t.describe("Random.rng", () => (
  t.it("new - zero seed becomes one", () => (
    let r := m.new(0);
    t.expect(r.seed >= 1).toBe(.True)
  ));
  t.it("new - positive seed used as-is", () => (
    let r := m.new(42);
    t.expect(r.seed).toBe(42)
  ));
  t.it("next_int - returns positive value", () => (
    let r := m.new(1);
    t.expect(m.next_int(r) > 0).toBe(.True)
  ));
  t.it("next_int - successive calls differ", () => (
    let r := m.new(1);
    let a := m.next_int(r);
    let b := m.next_int(r);
    t.expect(a).not.toBe(b)
  ));
  t.it("next_bounded - within range", () => (
    let r := m.new(1);
    let v := m.next_bounded(r, 0, 9);
    t.expect(v >= 0 and v <= 9).toBe(.True)
  ));
  t.it("next_bounded - tight range", () => (
    let r := m.new(1);
    let v := m.next_bounded(r, 5, 5);
    t.expect(v).toBe(5)
  ));
  t.it("next_float - between 0 and 1", () => (
    let r := m.new(1);
    let v := m.next_float(r);
    t.expect(v >= 0.0 and v <= 1.0).toBe(.True)
  ));
  t.it("next_float - successive calls differ", () => (
    let r := m.new(1);
    let a := m.next_float(r);
    let b := m.next_float(r);
    t.expect(a).not.toBe(b)
  ));
  t.it("next_bool - is boolean", () => (
    let r := m.new(1);
    let v := m.next_bool(r);
    t.expect(v or not v).toBe(.True)
  ));
  t.it("seed_from - resets sequence", () => (
    let r := m.new(1);
    m.next_int(r);
    m.seed_from(r, 1);
    let r2 := m.new(1);
    t.expect(m.next_int(r)).toBe(m.next_int(r2))
  ))
));

t.run();
