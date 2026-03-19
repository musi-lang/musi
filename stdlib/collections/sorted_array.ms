//! Sorted array with binary search.
//!
//! Elements are kept in ascending order. Lookup, insertion, and removal
//! use binary search (O(log n) comparisons, O(n) shifts).

import "@std/rt" as rt;

/// Array that maintains elements in ascending order.
export let SortedArray ['a] := record { data : []'a };

/// Create an empty sorted array.
export let new ['a] := () => .{ data := [] };

let binary_search ['a] := (arr, val) => (
  let mut lo := 0;
  let mut hi := rt.arr_len(arr);
  let step := () => (
    (
      let mid := lo + (hi - lo) / 2;
      let elem := arr.[mid];
      (
        (lo <- mid + 1; step()) if elem < val
        | (hi <- mid; step()) if elem > val
        | mid if _
      )
    ) if lo < hi
    | lo if _
  );
  step()
);

/// Insert `val` in sorted position.
export let insert ['a] := (sa, val) => (
  let mut arr := sa.data;
  let pos := binary_search(arr, val);
  let len := rt.arr_len(arr);
  rt.arr_push(arr, val);
  let shift := (i) => (
    (arr.[i] <- arr.[i - 1]; shift(i - 1)) if i > pos
    | () if _
  );
  shift(len);
  arr.[pos] <- val
);

/// Return `.True` if `val` is present in the sorted array.
export let contains ['a] := (sa, val) => (
  let arr := sa.data;
  let len := rt.arr_len(arr);
  let pos := binary_search(arr, val);
  pos < len and arr.[pos] = val
);

/// Remove the first occurrence of `val` and return `.True`, or `.False` if not found.
export let remove ['a] := (sa, val) => (
  let mut arr := sa.data;
  let len := rt.arr_len(arr);
  let pos := binary_search(arr, val);
  (
    (
      let shift := (i) => (
        (arr.[i] <- arr.[i + 1]; shift(i + 1)) if i < len - 1
        | () if _
      );
      shift(pos);
      rt.arr_pop(arr);
      .True
    ) if pos < len and arr.[pos] = val
    | .False if _
  )
);

/// Return the number of elements in the sorted array.
export let len ['a] := (sa) => rt.arr_len(sa.data);

/// Return a copy of the underlying array in ascending order.
export let to_array ['a] := (sa) => rt.arr_slice(sa.data, 0, rt.arr_len(sa.data));
