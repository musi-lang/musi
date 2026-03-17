//! Tests for binary min-heap operations.

import "@std/testing" as t;
import "@std/collections/heap" as heap;

let min_cmp := (a, b) => (
  .Less if a < b
  | .Greater if a > b
  | .Equal if _
);

let max_cmp := (a, b) => (
  .Less if a > b
  | .Greater if a < b
  | .Equal if _
);

t.runner.report(t.runner.run_suite([
  ("new - starts empty", () => (
    let h := heap.new(min_cmp);
    t.assertions.assert_true(heap.is_empty(h))
  )),
  ("new - len is zero", () => (
    let h := heap.new(min_cmp);
    t.assertions.assert_eq(heap.len(h), 0)
  )),
  ("peek - empty returns None", () => (
    let h := heap.new(min_cmp);
    t.assertions.assert_eq(heap.peek(h), .None)
  )),
  ("pop - empty returns None", () => (
    let h := heap.new(min_cmp);
    t.assertions.assert_eq(heap.pop(h), .None)
  )),
  ("push/peek - single element", () => (
    let h := heap.new(min_cmp);
    heap.push(h, 42);
    t.assertions.assert_eq(heap.peek(h), .Some(42))
  )),
  ("push/pop - single element returns value", () => (
    let h := heap.new(min_cmp);
    heap.push(h, 7);
    t.assertions.assert_eq(heap.pop(h), .Some(7))
  )),
  ("push - len increments", () => (
    let h := heap.new(min_cmp);
    heap.push(h, 1);
    heap.push(h, 2);
    heap.push(h, 3);
    t.assertions.assert_eq(heap.len(h), 3)
  )),
  ("pop - min is always at top", () => (
    let h := heap.new(min_cmp);
    heap.push(h, 5);
    heap.push(h, 1);
    heap.push(h, 3);
    t.assertions.assert_eq(heap.pop(h), .Some(1))
  )),
  ("pop - ordering maintained across multiple pops", () => (
    let h := heap.new(min_cmp);
    heap.push(h, 9);
    heap.push(h, 2);
    heap.push(h, 7);
    heap.push(h, 1);
    heap.push(h, 5);
    let a := heap.pop(h);
    let b := heap.pop(h);
    let c := heap.pop(h);
    t.assertions.assert_true(a = .Some(1) and b = .Some(2) and c = .Some(5))
  )),
  ("push/pop - duplicates handled correctly", () => (
    let h := heap.new(min_cmp);
    heap.push(h, 3);
    heap.push(h, 3);
    heap.push(h, 1);
    let a := heap.pop(h);
    let b := heap.pop(h);
    let c := heap.pop(h);
    t.assertions.assert_true(a = .Some(1) and b = .Some(3) and c = .Some(3))
  )),
  ("max_cmp - max-heap pops largest first", () => (
    let h := heap.new(max_cmp);
    heap.push(h, 3);
    heap.push(h, 10);
    heap.push(h, 1);
    t.assertions.assert_eq(heap.pop(h), .Some(10))
  )),
  ("from_array - builds valid heap", () => (
    let h := heap.from_array([4, 1, 7, 2, 9], min_cmp);
    t.assertions.assert_eq(heap.pop(h), .Some(1))
  )),
  ("from_array - empty array", () => (
    let h := heap.from_array([], min_cmp);
    t.assertions.assert_true(heap.is_empty(h))
  )),
  ("to_sorted_array - returns elements in ascending order", () => (
    let h := heap.from_array([3, 1, 4, 1, 5, 9, 2, 6], min_cmp);
    let sorted := heap.to_sorted_array(h);
    t.assertions.assert_true(
      sorted.[0] = 1 and sorted.[1] = 1 and sorted.[2] = 2 and sorted.[3] = 3
    )
  )),
  ("pop - empty after draining", () => (
    let h := heap.new(min_cmp);
    heap.push(h, 1);
    heap.pop(h);
    t.assertions.assert_true(heap.is_empty(h))
  )),
]));
