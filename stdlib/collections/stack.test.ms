//! Tests for LIFO stack operations.

import "@std/testing" as t;
import "@std/collections/stack" as stack;

t.runner.report(t.runner.run_suite([
  ("new — starts empty", () => (
    let s := stack.new();
    t.assertions.assert_true(stack.is_empty(s))
  )),
  ("new — len is zero", () => (
    let s := stack.new();
    t.assertions.assert_eq(stack.len(s), 0)
  )),
  ("peek — empty returns None", () => (
    let s := stack.new();
    t.assertions.assert_eq(stack.peek(s), .None)
  )),
  ("push — len increments", () => (
    let s := stack.new();
    stack.push(s, 1);
    stack.push(s, 2);
    t.assertions.assert_eq(stack.len(s), 2)
  )),
  ("push/peek — returns top element", () => (
    let s := stack.new();
    stack.push(s, 10);
    stack.push(s, 20);
    t.assertions.assert_eq(stack.peek(s), .Some(20))
  )),
  ("push/pop — single element roundtrip", () => (
    let s := stack.new();
    stack.push(s, 42);
    t.assertions.assert_eq(stack.pop(s), 42)
  )),
  ("pop — LIFO order", () => (
    let s := stack.new();
    stack.push(s, 1);
    stack.push(s, 2);
    stack.push(s, 3);
    let a := stack.pop(s);
    let b := stack.pop(s);
    let c := stack.pop(s);
    t.assertions.assert_true(a = 3 and b = 2 and c = 1)
  )),
  ("pop — len decrements", () => (
    let s := stack.new();
    stack.push(s, 1);
    stack.push(s, 2);
    stack.pop(s);
    t.assertions.assert_eq(stack.len(s), 1)
  )),
  ("is_empty — false when non-empty", () => (
    let s := stack.new();
    stack.push(s, 5);
    t.assertions.assert_false(stack.is_empty(s))
  )),
  ("peek — does not remove element", () => (
    let s := stack.new();
    stack.push(s, 99);
    stack.peek(s);
    t.assertions.assert_eq(stack.len(s), 1)
  )),
  ("push/pop — interleaved maintains LIFO", () => (
    let s := stack.new();
    stack.push(s, 1);
    stack.push(s, 2);
    stack.pop(s);
    stack.push(s, 3);
    t.assertions.assert_eq(stack.peek(s), .Some(3))
  )),
]));
