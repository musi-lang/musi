//! Sorted array with binary search.
//!
//! Elements are kept in ascending order. Lookup, insertion, and removal
//! use binary search (O(log n) comparisons, O(n) shifts).

foreign "musi:core" (
  let arr_len : [] 'a -> Int;
  let arr_push : [] 'a -> 'a -> Unit;
  let arr_pop : [] 'a -> ?'a;
);

export let SortedArray ['a] := record { data : [] 'a };

export let new ['a] := () => .{ data := [] };

/// Binary search: returns the index where `val` is or should be inserted.
let binary_search ['a] := (arr, val) => (
  let mut lo := 0;
  let mut hi := arr_len(arr);
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

export let insert ['a] := (sa, val) => (
  let arr := sa.data;
  let pos := binary_search(arr, val);
  let len := arr_len(arr);
  arr_push(arr, val);
  let shift := (i) => (
    (arr.[i] <- arr.[i - 1]; shift(i - 1)) if i > pos
    | () if _
  );
  shift(len);
  arr.[pos] <- val
);

export let contains ['a] := (sa, val) => (
  let arr := sa.data;
  let len := arr_len(arr);
  let pos := binary_search(arr, val);
  pos < len and arr.[pos] = val
);

export let remove ['a] := (sa, val) => (
  let arr := sa.data;
  let len := arr_len(arr);
  let pos := binary_search(arr, val);
  (
    (
      let shift := (i) => (
        (arr.[i] <- arr.[i + 1]; shift(i + 1)) if i < len - 1
        | () if _
      );
      shift(pos);
      arr_pop(arr);
      .True
    ) if pos < len and arr.[pos] = val
    | .False if _
  )
);

export let len ['a] := (sa) => arr_len(sa.data);

export let to_array ['a] := (sa) => sa.data;
