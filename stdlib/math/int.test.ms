//! Tests for integer math operations.

import "@std/testing" as t;
import "@std/math/int" as m;

t.runner.report(t.runner.run_suite([
  ("abs — positive stays positive", () => (
    t.assertions.assert_eq(m.abs(5), 5)
  )),
  ("abs — negative becomes positive", () => (
    t.assertions.assert_eq(m.abs(-7), 7)
  )),
  ("abs — zero stays zero", () => (
    t.assertions.assert_eq(m.abs(0), 0)
  )),
  ("min_int — returns smaller", () => (
    t.assertions.assert_eq(m.min_int(3, 8), 3)
  )),
  ("min_int — equal values", () => (
    t.assertions.assert_eq(m.min_int(4, 4), 4)
  )),
  ("max_int — returns larger", () => (
    t.assertions.assert_eq(m.max_int(3, 8), 8)
  )),
  ("max_int — equal values", () => (
    t.assertions.assert_eq(m.max_int(4, 4), 4)
  )),
  ("clamp_int — value below range", () => (
    t.assertions.assert_eq(m.clamp_int(1, 5, 10), 5)
  )),
  ("clamp_int — value above range", () => (
    t.assertions.assert_eq(m.clamp_int(15, 5, 10), 10)
  )),
  ("clamp_int — value within range", () => (
    t.assertions.assert_eq(m.clamp_int(7, 5, 10), 7)
  )),
  ("pow — base to positive exponent", () => (
    t.assertions.assert_eq(m.pow(2, 10), 1024)
  )),
  ("pow — any base to zero", () => (
    t.assertions.assert_eq(m.pow(5, 0), 1)
  )),
  ("pow — base one", () => (
    t.assertions.assert_eq(m.pow(1, 100), 1)
  )),
  ("sign_int — negative", () => (
    t.assertions.assert_eq(m.sign_int(-42), -1)
  )),
  ("sign_int — zero", () => (
    t.assertions.assert_eq(m.sign_int(0), 0)
  )),
  ("sign_int — positive", () => (
    t.assertions.assert_eq(m.sign_int(99), 1)
  )),
  ("is_even — even number", () => (
    t.assertions.assert_true(m.is_even(4))
  )),
  ("is_even — odd number", () => (
    t.assertions.assert_false(m.is_even(7))
  )),
  ("is_odd — odd number", () => (
    t.assertions.assert_true(m.is_odd(3))
  )),
  ("is_odd — even number", () => (
    t.assertions.assert_false(m.is_odd(6))
  )),
  ("gcd — two positive numbers", () => (
    t.assertions.assert_eq(m.gcd(12, 8), 4)
  )),
  ("gcd — coprime numbers", () => (
    t.assertions.assert_eq(m.gcd(7, 13), 1)
  )),
  ("gcd — one is zero", () => (
    t.assertions.assert_eq(m.gcd(0, 5), 5)
  )),
  ("gcd — negatives handled", () => (
    t.assertions.assert_eq(m.gcd(-12, 8), 4)
  )),
  ("lcm — two positive numbers", () => (
    t.assertions.assert_eq(m.lcm(4, 6), 12)
  )),
  ("lcm — one is zero", () => (
    t.assertions.assert_eq(m.lcm(0, 5), 0)
  )),
  ("lcm — coprime numbers", () => (
    t.assertions.assert_eq(m.lcm(3, 7), 21)
  )),
]));
