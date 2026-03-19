//! Binary min-heap (priority queue) - array-backed.
//!
//! The comparator `cmp` determines priority: `cmp(a, b)` returning `.Less`
//! means `a` has higher priority than `b`. The element with highest priority
//! sits at index 0 and is returned by `peek`/`pop`.

import "@std/rt" as rt;
import "@std/cmp/ordering";

/// Array-backed binary heap with a caller-supplied comparator.
export let Heap ['a] := record { data : []'a; cmp : ('a, 'a) -> Ordering };

/// Create an empty heap using `cmp` to order elements.
export let new ['a] := (cmp) => .{ data := [], cmp := cmp };

let swap ['a] := (mut data, i, j) => (
  let tmp := data.[i];
  data.[i] <- data.[j];
  data.[j] <- tmp
);

let sift_up ['a] := (data, idx, cmp) => (
  (
    let parent := (idx - 1) / 2;
    (
      swap(data, idx, parent);
      sift_up(data, parent, cmp)
    ) if cmp(data.[idx], data.[parent]) = .Less
    | () if _
  ) if idx > 0
  | () if _
);

let sift_down ['a] := (data, idx, len, cmp) => (
  let left := 2 * idx + 1;
  let right := 2 * idx + 2;
  (
    let best := (
      (right if cmp(data.[right], data.[left]) = .Less | left if _) if right < len
      | left if _
    );
    (
      swap(data, idx, best);
      sift_down(data, best, len, cmp)
    ) if cmp(data.[best], data.[idx]) = .Less
    | () if _
  ) if left < len
  | () if _
);

/// Insert a value into the heap.
export let push ['a] := (h, val) => (
  rt.arr_push(h.data, val);
  sift_up(h.data, rt.arr_len(h.data) - 1, h.cmp)
);

/// Remove and return the highest-priority element, or `.None` if empty.
export let pop ['a] := (h) => (
  let n := rt.arr_len(h.data);
  (
    (
      let top := h.data.[0];
      swap(h.data, 0, n - 1);
      rt.arr_pop(h.data);
      sift_down(h.data, 0, n - 1, h.cmp);
      .Some(top)
    ) if n > 0
    | .None if _
  )
);

/// Return the highest-priority element without removing it, or `.None` if empty.
export let peek ['a] := (h) => (
  let n := rt.arr_len(h.data);
  (.Some(h.data.[0]) if n > 0
  | .None if _)
);

/// Return the number of elements in the heap.
export let len ['a] := (h) => rt.arr_len(h.data);

/// Return `.True` if the heap contains no elements.
export let is_empty ['a] := (h) => rt.arr_len(h.data) = 0;

/// Build a heap from an existing array in O(n) time.
export let from_array ['a] := (arr, cmp) => (
  let data := rt.arr_slice(arr, 0, rt.arr_len(arr));
  let h := .{ data := data, cmp := cmp };
  let n := rt.arr_len(data);
  let heapify := (i) => (
    (sift_down(data, i, n, cmp); heapify(i - 1)) if i >= 0
    | () if _
  );
  heapify((n / 2) - 1);
  h
);

/// Return a copy of the heap's backing array in unspecified order.
export let to_array ['a] := (h) => rt.arr_slice(h.data, 0, rt.arr_len(h.data));

/// Drain the heap and return its elements in priority order.
export let to_sorted_array ['a] := (h) => (
  let copy := from_array(h.data, h.cmp);
  let mut out := [];
  let step := () =>
    match pop(copy) (
      .Some(v) => (rt.arr_push(out, v); step())
      | .None => ()
    );
  step();
  out
);
