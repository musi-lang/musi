//! Tests for double-ended queue operations.

import "@std/testing" as t;
import "@std/collections/deque" as deque;

t.describe("Deque", () => (
  t.it("new - starts empty", () => (
    let d := deque.new();
    t.expect(deque.is_empty(d)).toBe(.True)
  ));
  t.it("new - len is zero", () => (
    let d := deque.new();
    t.expect(deque.len(d)).toBe(0)
  ));
  t.it("peek_front - empty returns None", () => (
    let d := deque.new();
    t.expect(deque.peek_front(d)).toBe(.None)
  ));
  t.it("peek_back - empty returns None", () => (
    let d := deque.new();
    t.expect(deque.peek_back(d)).toBe(.None)
  ));
  t.it("pop_front - empty returns None", () => (
    let d := deque.new();
    t.expect(deque.pop_front(d)).toBe(.None)
  ));
  t.it("pop_back - empty returns None", () => (
    let d := deque.new();
    t.expect(deque.pop_back(d)).toBe(.None)
  ));
  t.it("push_back/pop_front - FIFO order", () => (
    let d := deque.new();
    deque.push_back(d, 1);
    deque.push_back(d, 2);
    deque.push_back(d, 3);
    let a := deque.pop_front(d);
    let b := deque.pop_front(d);
    t.expect(a = .Some(1) and b = .Some(2)).toBe(.True)
  ));
  t.it("push_front/pop_front - stack order", () => (
    let d := deque.new();
    deque.push_front(d, 1);
    deque.push_front(d, 2);
    deque.push_front(d, 3);
    let a := deque.pop_front(d);
    let b := deque.pop_front(d);
    t.expect(a = .Some(3) and b = .Some(2)).toBe(.True)
  ));
  t.it("push_back/pop_back - stack order", () => (
    let d := deque.new();
    deque.push_back(d, 10);
    deque.push_back(d, 20);
    t.expect(deque.pop_back(d)).toBe(.Some(20))
  ));
  t.it("push_front - peek_front returns correct value", () => (
    let d := deque.new();
    deque.push_front(d, 5);
    t.expect(deque.peek_front(d)).toBe(.Some(5))
  ));
  t.it("push_back - peek_back returns correct value", () => (
    let d := deque.new();
    deque.push_back(d, 7);
    t.expect(deque.peek_back(d)).toBe(.Some(7))
  ));
  t.it("len - counts both sides", () => (
    let d := deque.new();
    deque.push_front(d, 1);
    deque.push_back(d, 2);
    deque.push_back(d, 3);
    t.expect(deque.len(d)).toBe(3)
  ));
  t.it("pop_front - drains back when front empty", () => (
    let d := deque.new();
    deque.push_back(d, 10);
    deque.push_back(d, 20);
    t.expect(deque.pop_front(d)).toBe(.Some(10))
  ));
  t.it("pop_back - drains front when back empty", () => (
    let d := deque.new();
    deque.push_front(d, 1);
    deque.push_front(d, 2);
    t.expect(deque.pop_back(d)).toBe(.Some(1))
  ));
  t.it("is_empty - false when non-empty", () => (
    let d := deque.new();
    deque.push_back(d, 99);
    t.expect(deque.is_empty(d)).toBe(.False)
  ))
));

t.run();
