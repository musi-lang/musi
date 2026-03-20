//! Tests for FIFO queue operations.

import "@std/testing" as t;
import "@std/collections/queue" as queue;

t.describe("Queue", () => (
  t.it("new - starts empty", () => (
    let q := queue.new();
    t.expect(queue.is_empty(q)).toBe(.True)
  ));
  t.it("new - len is zero", () => (
    let q := queue.new();
    t.expect(queue.len(q)).toBe(0)
  ));
  t.it("dequeue - empty returns None", () => (
    let q := queue.new();
    t.expect(queue.dequeue(q)).toBe(.None)
  ));
  t.it("enqueue - len increments", () => (
    let q := queue.new();
    queue.enqueue(q, 1);
    queue.enqueue(q, 2);
    t.expect(queue.len(q)).toBe(2)
  ));
  t.it("enqueue/dequeue - single element roundtrip", () => (
    let q := queue.new();
    queue.enqueue(q, 99);
    t.expect(queue.dequeue(q)).toBe(.Some(99))
  ));
  t.it("dequeue - FIFO order preserved", () => (
    let q := queue.new();
    queue.enqueue(q, 1);
    queue.enqueue(q, 2);
    queue.enqueue(q, 3);
    let a : ?Int := queue.dequeue(q);
    let b : ?Int := queue.dequeue(q);
    let c : ?Int := queue.dequeue(q);
    t.expect(a = .Some(1) and b = .Some(2) and c = .Some(3)).toBe(.True)
  ));
  t.it("dequeue - len decrements", () => (
    let q := queue.new();
    queue.enqueue(q, 10);
    queue.enqueue(q, 20);
    queue.dequeue(q);
    t.expect(queue.len(q)).toBe(1)
  ));
  t.it("dequeue - empty after draining", () => (
    let q := queue.new();
    queue.enqueue(q, 5);
    queue.dequeue(q);
    t.expect(queue.is_empty(q)).toBe(.True)
  ));
  t.it("dequeue - returns None after draining", () => (
    let q := queue.new();
    queue.enqueue(q, 1);
    queue.dequeue(q);
    t.expect(queue.dequeue(q)).toBe(.None)
  ));
  t.it("enqueue/dequeue - interleaved operations maintain order", () => (
    let q := queue.new();
    queue.enqueue(q, 1);
    queue.enqueue(q, 2);
    queue.dequeue(q);
    queue.enqueue(q, 3);
    let a : ?Int := queue.dequeue(q);
    let b : ?Int := queue.dequeue(q);
    t.expect(a = .Some(2) and b = .Some(3)).toBe(.True)
  ));
  t.it("is_empty - false when non-empty", () => (
    let q := queue.new();
    queue.enqueue(q, 7);
    t.expect(queue.is_empty(q)).toBe(.False)
  ))
));

t.run();
