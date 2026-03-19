//! Dense bitset backed by an integer array.
//!
//! Each word holds 63 usable bits (Int is 64-bit signed; bit 63 is avoided
//! to stay within safe positive range). `bit_count` is the logical size.

import "@std/rt" as rt;

/// Dense fixed-size bitset.
export let Bitset := record { words : []Int; bit_count : Int };

let word_size := 63;

/// Create a bitset of `size` bits, all cleared.
export let new := (size) => (
  let word_count := (size + word_size - 1) / word_size;
  let mut arr := [];
  let fill := (i) => (
    () if i >= word_count
    | (rt.arr_push(arr, 0); fill(i + 1)) if _
  );
  fill(0);
  .{ words := arr, bit_count := size }
);

let bit_mask := (bit_idx) => rt.int_pow(2, bit_idx % word_size);

/// Set bit `idx` to 1.
export let set := (bs, idx) => (
  let w := idx / word_size;
  let mask := bit_mask(idx);
  bs.words.[w] <- bs.words.[w] or mask
);

/// Clear bit `idx` to 0.
export let clear := (bs, idx) => (
  let w := idx / word_size;
  let mask := bit_mask(idx);
  let all_ones := rt.int_pow(2, word_size) - 1;
  bs.words.[w] <- bs.words.[w] and (all_ones xor mask)
);

/// Return `.True` if bit `idx` is set.
export let test := (bs, idx) => (
  let w := idx / word_size;
  let mask := bit_mask(idx);
  (bs.words.[w] and mask) /= 0
);

/// Return a new bitset with all bits set in either `a` or `b`.
export let union := (a, b) => (
  let n := rt.arr_len(a.words);
  let result := new(a.bit_count);
  let step := (i) => (
    () if i >= n
    | (
      result.words.[i] <- a.words.[i] or b.words.[i];
      step(i + 1)
    ) if _
  );
  step(0);
  result
);

/// Return a new bitset with only bits set in both `a` and `b`.
export let intersect := (a, b) => (
  let n := rt.arr_len(a.words);
  let result := new(a.bit_count);
  let step := (i) => (
    () if i >= n
    | (
      result.words.[i] <- a.words.[i] and b.words.[i];
      step(i + 1)
    ) if _
  );
  step(0);
  result
);

/// Return a new bitset with bits set in `a` but not in `b`.
export let difference := (a, b) => (
  let n := rt.arr_len(a.words);
  let all_ones := rt.int_pow(2, word_size) - 1;
  let result := new(a.bit_count);
  let step := (i) => (
    () if i >= n
    | (
      let not_b := all_ones xor b.words.[i];
      result.words.[i] <- a.words.[i] and not_b;
      step(i + 1)
    ) if _
  );
  step(0);
  result
);

let popcount_word := (n) => (
  let step := (v, acc) => (
    acc if v = 0
    | step(v and (v - 1), acc + 1) if _
  );
  step(n, 0)
);

/// Return the number of bits that are set to 1.
export let count := (bs) => (
  let n := rt.arr_len(bs.words);
  let step := (i, acc) => (
    acc if i >= n
    | step(i + 1, acc + popcount_word(bs.words.[i])) if _
  );
  step(0, 0)
);
