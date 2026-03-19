//! Tests for LIFO stack operations.

import "@std/testing" as t;
import "@std/collections/stack" as stack;

t.describe("Stack", () => (
  t.it("new - starts empty", () => (
    let s := stack.new();
    t.expect(stack.is_empty(s)).toBe(.True)
  ));
  t.it("new - len is zero", () => (
    let s := stack.new();
    t.expect(stack.len(s)).toBe(0)
  ));
  t.it("peek - empty returns None", () => (
    let s := stack.new();
    t.expect(stack.peek(s)).toBe(.None)
  ));
  t.it("push - len increments", () => (
    let s := stack.new();
    stack.push(s, 1);
    stack.push(s, 2);
    t.expect(stack.len(s)).toBe(2)
  ));
  t.it("push/peek - returns top element", () => (
    let s := stack.new();
    stack.push(s, 10);
    stack.push(s, 20);
    t.expect(stack.peek(s)).toBe(.Some(20))
  ));
  t.it("push/pop - single element roundtrip", () => (
    let s := stack.new();
    stack.push(s, 42);
    t.expect(stack.pop(s)).toBe(42)
  ));
  t.it("pop - LIFO order", () => (
    let s := stack.new();
    stack.push(s, 1);
    stack.push(s, 2);
    stack.push(s, 3);
    let a := stack.pop(s);
    let b := stack.pop(s);
    let c := stack.pop(s);
    t.expect(a = 3 and b = 2 and c = 1).toBe(.True)
  ));
  t.it("pop - len decrements", () => (
    let s := stack.new();
    stack.push(s, 1);
    stack.push(s, 2);
    stack.pop(s);
    t.expect(stack.len(s)).toBe(1)
  ));
  t.it("is_empty - false when non-empty", () => (
    let s := stack.new();
    stack.push(s, 5);
    t.expect(stack.is_empty(s)).toBe(.False)
  ));
  t.it("peek - does not remove element", () => (
    let s := stack.new();
    stack.push(s, 99);
    stack.peek(s);
    t.expect(stack.len(s)).toBe(1)
  ));
  t.it("push/pop - interleaved maintains LIFO", () => (
    let s := stack.new();
    stack.push(s, 1);
    stack.push(s, 2);
    stack.pop(s);
    stack.push(s, 3);
    t.expect(stack.peek(s)).toBe(.Some(3))
  ))
));

t.run();
