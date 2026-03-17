//! Tests for function combinators.

import "@std/testing" as t;
import "@std/func/index" as m;

t.runner.report(t.runner.run_suite([
  ("identity — returns input unchanged", () => (
    t.assertions.assert_eq(m.identity(42), 42)
  )),
  ("identity — string value", () => (
    t.assertions.assert_eq(m.identity("hello"), "hello")
  )),
  ("constant — ignores second argument", () => (
    let always5 := m.constant(5);
    t.assertions.assert_eq(always5("ignored"), 5)
  )),
  ("constant — different ignored values", () => (
    let alwaysHi := m.constant("hi");
    t.assertions.assert_eq(alwaysHi(99), "hi")
  )),
  ("compose — applies g then f", () => (
    let add1 := (x) => x + 1;
    let double := (x) => x * 2;
    let double_then_add1 := m.compose(add1, double);
    t.assertions.assert_eq(double_then_add1(3), 7)
  )),
  ("compose — order matters", () => (
    let add1 := (x) => x + 1;
    let double := (x) => x * 2;
    let add1_then_double := m.compose(double, add1);
    t.assertions.assert_eq(add1_then_double(3), 8)
  )),
  ("flip — reverses argument order", () => (
    let subtract := (a, b) => a - b;
    let flipped := m.flip(subtract);
    t.assertions.assert_eq(flipped(3, 10), 7)
  )),
  ("apply — calls function with argument", () => (
    let double := (x) => x * 2;
    t.assertions.assert_eq(m.apply(double, 5), 10)
  )),
  ("on — applies g to both args then f", () => (
    let strlen := (s) => 3;
    let add := (a, b) => a + b;
    let add_lengths := m.on(add, strlen);
    t.assertions.assert_eq(add_lengths("abc", "de"), 6)
  )),
  ("negate — inverts predicate result", () => (
    let is_positive := (x) => x > 0;
    let is_not_positive := m.negate(is_positive);
    t.assertions.assert_true(is_not_positive(-1))
  )),
  ("negate — true becomes false", () => (
    let always_true := (x) => .True;
    let always_false := m.negate(always_true);
    t.assertions.assert_false(always_false(0))
  )),
  ("pipe2 — applies f then g", () => (
    let add1 := (x) => x + 1;
    let double := (x) => x * 2;
    t.assertions.assert_eq(m.pipe2(3, add1, double), 8)
  )),
  ("pipe3 — applies f, g, h in order", () => (
    let add1 := (x) => x + 1;
    let double := (x) => x * 2;
    let negate := (x) => 0 - x;
    t.assertions.assert_eq(m.pipe3(3, add1, double, negate), -8)
  )),
]));
