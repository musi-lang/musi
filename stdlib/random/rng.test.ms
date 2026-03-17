//! Tests for Xorshift PRNG operations.

import "@std/testing" as t;
import "@std/random/rng" as m;

t.runner.report(t.runner.run_suite([
  ("new - zero seed becomes one", () => (
    let r := m.new(0);
    t.assertions.assert_true(r.seed >= 1)
  )),
  ("new - positive seed used as-is", () => (
    let r := m.new(42);
    t.assertions.assert_eq(r.seed, 42)
  )),
  ("next_int - returns positive value", () => (
    let r := m.new(1);
    t.assertions.assert_true(m.next_int(r) > 0)
  )),
  ("next_int - successive calls differ", () => (
    let r := m.new(1);
    let a := m.next_int(r);
    let b := m.next_int(r);
    t.assertions.assert_ne(a, b)
  )),
  ("next_bounded - within range", () => (
    let r := m.new(1);
    let v := m.next_bounded(r, 0, 9);
    t.assertions.assert_true(v >= 0 and v <= 9)
  )),
  ("next_bounded - tight range", () => (
    let r := m.new(1);
    let v := m.next_bounded(r, 5, 5);
    t.assertions.assert_eq(v, 5)
  )),
  ("next_float - between 0 and 1", () => (
    let r := m.new(1);
    let v := m.next_float(r);
    t.assertions.assert_true(v >= 0.0 and v <= 1.0)
  )),
  ("next_float - successive calls differ", () => (
    let r := m.new(1);
    let a := m.next_float(r);
    let b := m.next_float(r);
    t.assertions.assert_ne(a, b)
  )),
  ("next_bool - is boolean", () => (
    let r := m.new(1);
    let v := m.next_bool(r);
    t.assertions.assert_true(v or not v)
  )),
  ("seed_from - resets sequence", () => (
    let r := m.new(1);
    m.next_int(r);
    m.seed_from(r, 1);
    let r2 := m.new(1);
    t.assertions.assert_eq(m.next_int(r), m.next_int(r2))
  )),
]));
