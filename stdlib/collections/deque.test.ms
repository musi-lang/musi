//! Tests for double-ended queue operations.

import "@std/testing" as t;
import "@std/collections/deque" as deque;

t.runner.report(t.runner.run_suite([
  ("new — starts empty", () => (
    let d := deque.new();
    t.assertions.assert_true(deque.is_empty(d))
  )),
  ("new — len is zero", () => (
    let d := deque.new();
    t.assertions.assert_eq(deque.len(d), 0)
  )),
  ("peek_front — empty returns None", () => (
    let d := deque.new();
    t.assertions.assert_eq(deque.peek_front(d), .None)
  )),
  ("peek_back — empty returns None", () => (
    let d := deque.new();
    t.assertions.assert_eq(deque.peek_back(d), .None)
  )),
  ("pop_front — empty returns None", () => (
    let d := deque.new();
    t.assertions.assert_eq(deque.pop_front(d), .None)
  )),
  ("pop_back — empty returns None", () => (
    let d := deque.new();
    t.assertions.assert_eq(deque.pop_back(d), .None)
  )),
  ("push_back/pop_front — FIFO order", () => (
    let d := deque.new();
    deque.push_back(d, 1);
    deque.push_back(d, 2);
    deque.push_back(d, 3);
    let a := deque.pop_front(d);
    let b := deque.pop_front(d);
    t.assertions.assert_true(a = .Some(1) and b = .Some(2))
  )),
  ("push_front/pop_front — stack order", () => (
    let d := deque.new();
    deque.push_front(d, 1);
    deque.push_front(d, 2);
    deque.push_front(d, 3);
    let a := deque.pop_front(d);
    let b := deque.pop_front(d);
    t.assertions.assert_true(a = .Some(3) and b = .Some(2))
  )),
  ("push_back/pop_back — stack order", () => (
    let d := deque.new();
    deque.push_back(d, 10);
    deque.push_back(d, 20);
    t.assertions.assert_eq(deque.pop_back(d), .Some(20))
  )),
  ("push_front — peek_front returns correct value", () => (
    let d := deque.new();
    deque.push_front(d, 5);
    t.assertions.assert_eq(deque.peek_front(d), .Some(5))
  )),
  ("push_back — peek_back returns correct value", () => (
    let d := deque.new();
    deque.push_back(d, 7);
    t.assertions.assert_eq(deque.peek_back(d), .Some(7))
  )),
  ("len — counts both sides", () => (
    let d := deque.new();
    deque.push_front(d, 1);
    deque.push_back(d, 2);
    deque.push_back(d, 3);
    t.assertions.assert_eq(deque.len(d), 3)
  )),
  ("pop_front — drains back when front empty", () => (
    let d := deque.new();
    deque.push_back(d, 10);
    deque.push_back(d, 20);
    t.assertions.assert_eq(deque.pop_front(d), .Some(10))
  )),
  ("pop_back — drains front when back empty", () => (
    let d := deque.new();
    deque.push_front(d, 1);
    deque.push_front(d, 2);
    t.assertions.assert_eq(deque.pop_back(d), .Some(1))
  )),
  ("is_empty — false when non-empty", () => (
    let d := deque.new();
    deque.push_back(d, 99);
    t.assertions.assert_false(deque.is_empty(d))
  )),
]));
