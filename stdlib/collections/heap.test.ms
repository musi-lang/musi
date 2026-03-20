//! Tests for binary min-heap operations.

import "@std/testing" as t;
import "@std/collections/heap" as heap;

let min_cmp := (a : Int, b : Int) => (
  .Less if a < b
  | .Greater if a > b
  | .Equal if _
);

let max_cmp := (a : Int, b : Int) => (
  .Less if a > b
  | .Greater if a < b
  | .Equal if _
);

t.describe("Heap", () => (
  t.it("new - starts empty", () => (
    let h := heap.new(min_cmp);
    t.expect(heap.is_empty(h)).toBe(.True)
  ));
  t.it("new - len is zero", () => (
    let h := heap.new(min_cmp);
    t.expect(heap.len(h)).toBe(0)
  ));
  t.it("peek - empty returns None", () => (
    let h := heap.new(min_cmp);
    t.expect(heap.peek(h)).toBe(.None)
  ));
  t.it("pop - empty returns None", () => (
    let h := heap.new(min_cmp);
    t.expect(heap.pop(h)).toBe(.None)
  ));
  t.it("push/peek - single element", () => (
    let h := heap.new(min_cmp);
    heap.push(h, 42);
    t.expect(heap.peek(h)).toBe(.Some(42))
  ));
  t.it("push/pop - single element returns value", () => (
    let h := heap.new(min_cmp);
    heap.push(h, 7);
    t.expect(heap.pop(h)).toBe(.Some(7))
  ));
  t.it("push - len increments", () => (
    let h := heap.new(min_cmp);
    heap.push(h, 1);
    heap.push(h, 2);
    heap.push(h, 3);
    t.expect(heap.len(h)).toBe(3)
  ));
  t.it("pop - min is always at top", () => (
    let h := heap.new(min_cmp);
    heap.push(h, 5);
    heap.push(h, 1);
    heap.push(h, 3);
    t.expect(heap.pop(h)).toBe(.Some(1))
  ));
  t.it("pop - ordering maintained across multiple pops", () => (
    let h := heap.new(min_cmp);
    heap.push(h, 9);
    heap.push(h, 2);
    heap.push(h, 7);
    heap.push(h, 1);
    heap.push(h, 5);
    let a : ?Int := heap.pop(h);
    let b : ?Int := heap.pop(h);
    let c : ?Int := heap.pop(h);
    t.expect(a = .Some(1) and b = .Some(2) and c = .Some(5)).toBe(.True)
  ));
  t.it("push/pop - duplicates handled correctly", () => (
    let h := heap.new(min_cmp);
    heap.push(h, 3);
    heap.push(h, 3);
    heap.push(h, 1);
    let a : ?Int := heap.pop(h);
    let b : ?Int := heap.pop(h);
    let c : ?Int := heap.pop(h);
    t.expect(a = .Some(1) and b = .Some(3) and c = .Some(3)).toBe(.True)
  ));
  t.it("max_cmp - max-heap pops largest first", () => (
    let h := heap.new(max_cmp);
    heap.push(h, 3);
    heap.push(h, 10);
    heap.push(h, 1);
    t.expect(heap.pop(h)).toBe(.Some(10))
  ));
  t.it("from_array - builds valid heap", () => (
    let h := heap.from_array([4, 1, 7, 2, 9], min_cmp);
    t.expect(heap.pop(h)).toBe(.Some(1))
  ));
  t.it("from_array - empty array", () => (
    let h := heap.from_array([], min_cmp);
    t.expect(heap.is_empty(h)).toBe(.True)
  ));
  t.it("to_sorted_array - returns elements in ascending order", () => (
    let h := heap.from_array([3, 1, 4, 1, 5, 9, 2, 6], min_cmp);
    let sorted := heap.to_sorted_array(h);
    t.expect(
      sorted.[0] = 1 and sorted.[1] = 1 and sorted.[2] = 2 and sorted.[3] = 3
    ).toBe(.True)
  ));
  t.it("pop - empty after draining", () => (
    let h := heap.new(min_cmp);
    heap.push(h, 1);
    heap.pop(h);
    t.expect(heap.is_empty(h)).toBe(.True)
  ))
));

t.run();
