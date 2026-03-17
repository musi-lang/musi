//! Tests for numeric constants.

import "@std/testing" as t;
import "@std/math/constants" as m;
import "@std/math/float" as f;

t.runner.report(t.runner.run_suite([
  ("PI - greater than 3.14", () => (
    t.assertions.assert_true(m.PI > 3.14)
  )),
  ("PI - less than 3.15", () => (
    t.assertions.assert_true(m.PI < 3.15)
  )),
  ("E - greater than 2.71", () => (
    t.assertions.assert_true(m.E > 2.71)
  )),
  ("E - less than 2.72", () => (
    t.assertions.assert_true(m.E < 2.72)
  )),
  ("INFINITY - is infinite", () => (
    t.assertions.assert_true(f.is_infinite(m.INFINITY))
  )),
  ("INFINITY - greater than large number", () => (
    t.assertions.assert_true(m.INFINITY > 1000000.0)
  )),
  ("NAN - is NaN", () => (
    t.assertions.assert_true(f.is_nan(m.NAN))
  )),
  ("INT_MIN - less than zero", () => (
    t.assertions.assert_true(m.INT_MIN < 0)
  )),
  ("INT_MAX - greater than zero", () => (
    t.assertions.assert_true(m.INT_MAX > 0)
  )),
  ("INT_MIN and INT_MAX - min is less than max", () => (
    t.assertions.assert_true(m.INT_MIN < m.INT_MAX)
  )),
]));
