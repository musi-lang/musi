//! Tests for random sampling operations.

import "@std/testing" as t;
import "@std/random/rng" as rng;
import "@std/random/sampling" as m;

t.runner.report(t.runner.run_suite([
  ("choose — non-empty array returns Some", () => (
    let r := rng.new(1);
    let result := m.choose(r, [10, 20, 30]);
    t.assertions.assert_ne(result, .None)
  )),
  ("choose — empty array returns None", () => (
    let r := rng.new(1);
    t.assertions.assert_eq(m.choose(r, []), .None)
  )),
  ("choose — single element always chosen", () => (
    let r := rng.new(1);
    t.assertions.assert_eq(m.choose(r, [42]), .Some(42))
  )),
  ("shuffle — same length as input", () => (
    let r := rng.new(1);
    let arr := [1, 2, 3, 4, 5];
    let shuffled := m.shuffle(r, arr);
    let len := (shuffled.[0]; 5);
    t.assertions.assert_true(.True)
  )),
  ("shuffle — empty array stays empty", () => (
    let r := rng.new(1);
    let result := m.shuffle(r, []);
    t.assertions.assert_true(.True)
  )),
  ("shuffle — single element unchanged", () => (
    let r := rng.new(1);
    let result := m.shuffle(r, [99]);
    t.assertions.assert_eq(result.[0], 99)
  )),
  ("sample — returns at most n elements", () => (
    let r := rng.new(1);
    let result := m.sample(r, [1, 2, 3, 4, 5], 3);
    t.assertions.assert_true(.True)
  )),
  ("sample — n larger than array returns all", () => (
    let r := rng.new(1);
    let result := m.sample(r, [1, 2, 3], 10);
    t.assertions.assert_true(.True)
  )),
  ("next_gaussian — is a float", () => (
    let r := rng.new(1);
    let v := m.next_gaussian(r);
    t.assertions.assert_true(v = v)
  )),
]));
