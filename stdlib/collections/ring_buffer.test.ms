//! Tests for fixed-capacity ring buffer operations.

import "@std/testing" as t;
import "@std/collections/ring_buffer" as rb;

t.runner.report(t.runner.run_suite([
  ("new — starts empty", () => (
    let r := rb.new(4);
    t.assertions.assert_true(rb.is_empty(r))
  )),
  ("new — len is zero", () => (
    let r := rb.new(4);
    t.assertions.assert_eq(rb.len(r), 0)
  )),
  ("new — not full initially", () => (
    let r := rb.new(4);
    t.assertions.assert_false(rb.is_full(r))
  )),
  ("peek — empty returns None", () => (
    let r := rb.new(4);
    t.assertions.assert_eq(rb.peek(r), .None)
  )),
  ("pop — empty returns None", () => (
    let r := rb.new(4);
    t.assertions.assert_eq(rb.pop(r), .None)
  )),
  ("push/peek — single element", () => (
    let r := rb.new(4);
    rb.push(r, 10);
    t.assertions.assert_eq(rb.peek(r), .Some(10))
  )),
  ("push/pop — single element roundtrip", () => (
    let r := rb.new(4);
    rb.push(r, 42);
    t.assertions.assert_eq(rb.pop(r), .Some(42))
  )),
  ("push — len increments", () => (
    let r := rb.new(4);
    rb.push(r, 1);
    rb.push(r, 2);
    t.assertions.assert_eq(rb.len(r), 2)
  )),
  ("is_full — true when at capacity", () => (
    let r := rb.new(3);
    rb.push(r, 1);
    rb.push(r, 2);
    rb.push(r, 3);
    t.assertions.assert_true(rb.is_full(r))
  )),
  ("push — overwrites oldest when full", () => (
    let r := rb.new(3);
    rb.push(r, 1);
    rb.push(r, 2);
    rb.push(r, 3);
    rb.push(r, 4);
    // oldest (1) was overwritten; head should now be 2
    t.assertions.assert_eq(rb.pop(r), .Some(2))
  )),
  ("pop — FIFO order", () => (
    let r := rb.new(4);
    rb.push(r, 10);
    rb.push(r, 20);
    rb.push(r, 30);
    let a := rb.pop(r);
    let b := rb.pop(r);
    t.assertions.assert_true(a = .Some(10) and b = .Some(20))
  )),
  ("pop — len decrements", () => (
    let r := rb.new(4);
    rb.push(r, 1);
    rb.push(r, 2);
    rb.pop(r);
    t.assertions.assert_eq(rb.len(r), 1)
  )),
  ("push/pop — wrap-around maintains order", () => (
    let r := rb.new(3);
    rb.push(r, 1);
    rb.push(r, 2);
    rb.push(r, 3);
    rb.pop(r);
    rb.push(r, 4);
    let a := rb.pop(r);
    let b := rb.pop(r);
    let c := rb.pop(r);
    t.assertions.assert_true(a = .Some(2) and b = .Some(3) and c = .Some(4))
  )),
  ("is_empty — false when non-empty", () => (
    let r := rb.new(4);
    rb.push(r, 5);
    t.assertions.assert_false(rb.is_empty(r))
  )),
]));
