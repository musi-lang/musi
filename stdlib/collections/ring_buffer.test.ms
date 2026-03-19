//! Tests for fixed-capacity ring buffer operations.

import "@std/testing" as t;
import "@std/collections/ring_buffer" as rb;

t.describe("RingBuffer", () => (
  t.it("new - starts empty", () => (
    let r := rb.new(4);
    t.expect(rb.is_empty(r)).toBe(.True)
  ));
  t.it("new - len is zero", () => (
    let r := rb.new(4);
    t.expect(rb.len(r)).toBe(0)
  ));
  t.it("new - not full initially", () => (
    let r := rb.new(4);
    t.expect(rb.is_full(r)).toBe(.False)
  ));
  t.it("peek - empty returns None", () => (
    let r := rb.new(4);
    t.expect(rb.peek(r)).toBe(.None)
  ));
  t.it("pop - empty returns None", () => (
    let r := rb.new(4);
    t.expect(rb.pop(r)).toBe(.None)
  ));
  t.it("push/peek - single element", () => (
    let r := rb.new(4);
    rb.push(r, 10);
    t.expect(rb.peek(r)).toBe(.Some(10))
  ));
  t.it("push/pop - single element roundtrip", () => (
    let r := rb.new(4);
    rb.push(r, 42);
    t.expect(rb.pop(r)).toBe(.Some(42))
  ));
  t.it("push - len increments", () => (
    let r := rb.new(4);
    rb.push(r, 1);
    rb.push(r, 2);
    t.expect(rb.len(r)).toBe(2)
  ));
  t.it("is_full - true when at capacity", () => (
    let r := rb.new(3);
    rb.push(r, 1);
    rb.push(r, 2);
    rb.push(r, 3);
    t.expect(rb.is_full(r)).toBe(.True)
  ));
  t.it("push - overwrites oldest when full", () => (
    let r := rb.new(3);
    rb.push(r, 1);
    rb.push(r, 2);
    rb.push(r, 3);
    rb.push(r, 4);
    // oldest (1) was overwritten; head should now be 2
    t.expect(rb.pop(r)).toBe(.Some(2))
  ));
  t.it("pop - FIFO order", () => (
    let r := rb.new(4);
    rb.push(r, 10);
    rb.push(r, 20);
    rb.push(r, 30);
    let a := rb.pop(r);
    let b := rb.pop(r);
    t.expect(a = .Some(10) and b = .Some(20)).toBe(.True)
  ));
  t.it("pop - len decrements", () => (
    let r := rb.new(4);
    rb.push(r, 1);
    rb.push(r, 2);
    rb.pop(r);
    t.expect(rb.len(r)).toBe(1)
  ));
  t.it("push/pop - wrap-around maintains order", () => (
    let r := rb.new(3);
    rb.push(r, 1);
    rb.push(r, 2);
    rb.push(r, 3);
    rb.pop(r);
    rb.push(r, 4);
    let a := rb.pop(r);
    let b := rb.pop(r);
    let c := rb.pop(r);
    t.expect(a = .Some(2) and b = .Some(3) and c = .Some(4)).toBe(.True)
  ));
  t.it("is_empty - false when non-empty", () => (
    let r := rb.new(4);
    rb.push(r, 5);
    t.expect(rb.is_empty(r)).toBe(.False)
  ))
));

t.run();
