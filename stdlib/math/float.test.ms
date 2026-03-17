//! Tests for floating-point math operations.

import "@std/testing" as t;
import "@std/math/float" as m;

t.runner.report(t.runner.run_suite([
  ("floor - positive fractional", () => (
    t.assertions.assert_eq(m.floor(3.7), 3.0)
  )),
  ("floor - negative fractional", () => (
    t.assertions.assert_eq(m.floor(-2.3), -3.0)
  )),
  ("ceil - positive fractional", () => (
    t.assertions.assert_eq(m.ceil(3.2), 4.0)
  )),
  ("ceil - negative fractional", () => (
    t.assertions.assert_eq(m.ceil(-2.7), -2.0)
  )),
  ("round - rounds half up", () => (
    t.assertions.assert_eq(m.round(2.5), 3.0)
  )),
  ("round - rounds down", () => (
    t.assertions.assert_eq(m.round(2.3), 2.0)
  )),
  ("sqrt - perfect square", () => (
    t.assertions.assert_eq(m.sqrt(4.0), 2.0)
  )),
  ("sqrt - zero", () => (
    t.assertions.assert_eq(m.sqrt(0.0), 0.0)
  )),
  ("sqrt - one", () => (
    t.assertions.assert_eq(m.sqrt(1.0), 1.0)
  )),
  ("sin - zero", () => (
    t.assertions.assert_eq(m.sin(0.0), 0.0)
  )),
  ("cos - zero", () => (
    t.assertions.assert_eq(m.cos(0.0), 1.0)
  )),
  ("log - one gives zero", () => (
    t.assertions.assert_eq(m.log(1.0), 0.0)
  )),
  ("exp - zero gives one", () => (
    t.assertions.assert_eq(m.exp(0.0), 1.0)
  )),
  ("is_nan - NaN value", () => (
    t.assertions.assert_true(m.is_nan(m.sqrt(-1.0)))
  )),
  ("is_nan - normal value", () => (
    t.assertions.assert_false(m.is_nan(1.0))
  )),
  ("is_infinite - normal value", () => (
    t.assertions.assert_false(m.is_infinite(1.0))
  )),
  ("abs_float - negative", () => (
    t.assertions.assert_eq(m.abs_float(-3.5), 3.5)
  )),
  ("abs_float - positive", () => (
    t.assertions.assert_eq(m.abs_float(3.5), 3.5)
  )),
  ("abs_float - zero", () => (
    t.assertions.assert_eq(m.abs_float(0.0), 0.0)
  )),
  ("clamp_float - below range", () => (
    t.assertions.assert_eq(m.clamp_float(0.5, 1.0, 3.0), 1.0)
  )),
  ("clamp_float - above range", () => (
    t.assertions.assert_eq(m.clamp_float(5.0, 1.0, 3.0), 3.0)
  )),
  ("clamp_float - within range", () => (
    t.assertions.assert_eq(m.clamp_float(2.0, 1.0, 3.0), 2.0)
  )),
  ("min_float - returns smaller", () => (
    t.assertions.assert_eq(m.min_float(1.5, 2.5), 1.5)
  )),
  ("max_float - returns larger", () => (
    t.assertions.assert_eq(m.max_float(1.5, 2.5), 2.5)
  )),
  ("sign_float - negative", () => (
    t.assertions.assert_eq(m.sign_float(-3.0), -1.0)
  )),
  ("sign_float - zero", () => (
    t.assertions.assert_eq(m.sign_float(0.0), 0.0)
  )),
  ("sign_float - positive", () => (
    t.assertions.assert_eq(m.sign_float(5.0), 1.0)
  )),
  ("lerp - at t=0 gives a", () => (
    t.assertions.assert_eq(m.lerp(0.0, 10.0, 0.0), 0.0)
  )),
  ("lerp - at t=1 gives b", () => (
    t.assertions.assert_eq(m.lerp(0.0, 10.0, 1.0), 10.0)
  )),
  ("lerp - at t=0.5 gives midpoint", () => (
    t.assertions.assert_eq(m.lerp(0.0, 10.0, 0.5), 5.0)
  )),
]));
