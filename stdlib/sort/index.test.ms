//! Tests for sorting algorithms.

import "@std/testing" as t;
import "@std/sort/index" as sort;

let num_cmp := (a, b) => a - b;

t.runner.report(t.runner.run_suite([
  ("is_sorted - sorted array returns true", () => (
    let result := sort.is_sorted([1, 2, 3, 4, 5], num_cmp);
    t.assertions.assert_true(result)
  )),
  ("is_sorted - unsorted array returns false", () => (
    let result := sort.is_sorted([1, 3, 2, 4], num_cmp);
    t.assertions.assert_false(result)
  )),
  ("is_sorted - single element returns true", () => (
    let result := sort.is_sorted([7], num_cmp);
    t.assertions.assert_true(result)
  )),
  ("is_sorted - empty array returns true", () => (
    let result := sort.is_sorted([], num_cmp);
    t.assertions.assert_true(result)
  )),
  ("is_sorted - equal elements returns true", () => (
    let result := sort.is_sorted([3, 3, 3], num_cmp);
    t.assertions.assert_true(result)
  )),
  ("merge_sort - sorts in ascending order", () => (
    let result := sort.merge_sort([3, 1, 4, 1, 5, 9, 2, 6], num_cmp);
    t.assertions.assert_eq(result, [1, 1, 2, 3, 4, 5, 6, 9])
  )),
  ("merge_sort - already sorted stays sorted", () => (
    let result := sort.merge_sort([1, 2, 3, 4], num_cmp);
    t.assertions.assert_eq(result, [1, 2, 3, 4])
  )),
  ("merge_sort - reverse sorted", () => (
    let result := sort.merge_sort([5, 4, 3, 2, 1], num_cmp);
    t.assertions.assert_eq(result, [1, 2, 3, 4, 5])
  )),
  ("merge_sort - single element", () => (
    let result := sort.merge_sort([42], num_cmp);
    t.assertions.assert_eq(result, [42])
  )),
  ("merge_sort - empty array", () => (
    let result := sort.merge_sort([], num_cmp);
    t.assertions.assert_eq(result, [])
  )),
  ("merge_sort - duplicates preserved", () => (
    let result := sort.merge_sort([2, 1, 2, 1], num_cmp);
    t.assertions.assert_eq(result, [1, 1, 2, 2])
  )),
  ("sort_by - sorts by comparator", () => (
    let result := sort.sort_by([5, 3, 8, 1], num_cmp);
    t.assertions.assert_eq(result, [1, 3, 5, 8])
  )),
  ("sort_by - descending comparator", () => (
    let result := sort.sort_by([5, 3, 8, 1], (a, b) => b - a);
    t.assertions.assert_eq(result, [8, 5, 3, 1])
  )),
  ("insertion_sort - sorts correctly", () => (
    let result := sort.insertion_sort([4, 2, 7, 1, 3], num_cmp);
    t.assertions.assert_eq(result, [1, 2, 3, 4, 7])
  )),
  ("insertion_sort - empty array", () => (
    let result := sort.insertion_sort([], num_cmp);
    t.assertions.assert_eq(result, [])
  )),
  ("insertion_sort - single element", () => (
    let result := sort.insertion_sort([9], num_cmp);
    t.assertions.assert_eq(result, [9])
  )),
]));
