//! Tests for FIFO queue operations.

import "@std/testing" as t;
import "@std/collections/queue" as queue;

t.runner.report(t.runner.run_suite([
  ("new - starts empty", () => (
    let q := queue.new();
    t.assertions.assert_true(queue.is_empty(q))
  )),
  ("new - len is zero", () => (
    let q := queue.new();
    t.assertions.assert_eq(queue.len(q), 0)
  )),
  ("dequeue - empty returns None", () => (
    let q := queue.new();
    t.assertions.assert_eq(queue.dequeue(q), .None)
  )),
  ("enqueue - len increments", () => (
    let q := queue.new();
    queue.enqueue(q, 1);
    queue.enqueue(q, 2);
    t.assertions.assert_eq(queue.len(q), 2)
  )),
  ("enqueue/dequeue - single element roundtrip", () => (
    let q := queue.new();
    queue.enqueue(q, 99);
    t.assertions.assert_eq(queue.dequeue(q), .Some(99))
  )),
  ("dequeue - FIFO order preserved", () => (
    let q := queue.new();
    queue.enqueue(q, 1);
    queue.enqueue(q, 2);
    queue.enqueue(q, 3);
    let a := queue.dequeue(q);
    let b := queue.dequeue(q);
    let c := queue.dequeue(q);
    t.assertions.assert_true(a = .Some(1) and b = .Some(2) and c = .Some(3))
  )),
  ("dequeue - len decrements", () => (
    let q := queue.new();
    queue.enqueue(q, 10);
    queue.enqueue(q, 20);
    queue.dequeue(q);
    t.assertions.assert_eq(queue.len(q), 1)
  )),
  ("dequeue - empty after draining", () => (
    let q := queue.new();
    queue.enqueue(q, 5);
    queue.dequeue(q);
    t.assertions.assert_true(queue.is_empty(q))
  )),
  ("dequeue - returns None after draining", () => (
    let q := queue.new();
    queue.enqueue(q, 1);
    queue.dequeue(q);
    t.assertions.assert_eq(queue.dequeue(q), .None)
  )),
  ("enqueue/dequeue - interleaved operations maintain order", () => (
    let q := queue.new();
    queue.enqueue(q, 1);
    queue.enqueue(q, 2);
    queue.dequeue(q);
    queue.enqueue(q, 3);
    let a := queue.dequeue(q);
    let b := queue.dequeue(q);
    t.assertions.assert_true(a = .Some(2) and b = .Some(3))
  )),
  ("is_empty - false when non-empty", () => (
    let q := queue.new();
    queue.enqueue(q, 7);
    t.assertions.assert_false(queue.is_empty(q))
  )),
]));
