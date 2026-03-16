//! Dense bitset backed by an integer array.
//!
//! Each word holds 63 usable bits (Int is 64-bit signed; bit 63 is avoided
//! to stay within safe positive range). `bit_count` is the logical size.

import "../rt" as rt;

export let Bitset := record { words : []Int; bit_count : Int };

let word_size := 63;

export let new := (size) => (
  let word_count := (size + word_size - 1) / word_size;
  let mut arr := [];
  let fill := (i) => (
    () if i >= word_count
    | (rt.arr_push(arr, 0); fill(i + 1)) if _
  );
  fill(0);
  .{ words := arr; bit_count := size }
);

let bit_mask := (bit_idx) => rt.int_pow(2, bit_idx mod word_size);

export let set := (bs, idx) => (
  let w := idx / word_size;
  let mask := bit_mask(idx);
  bs.words.[w] <- bs.words.[w] or mask
);

export let clear := (bs, idx) => (
  let w := idx / word_size;
  let mask := bit_mask(idx);
  let all_ones := rt.int_pow(2, word_size) - 1;
  bs.words.[w] <- bs.words.[w] and (all_ones xor mask)
);

export let test := (bs, idx) => (
  let w := idx / word_size;
  let mask := bit_mask(idx);
  (bs.words.[w] and mask) /= 0
);

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
    | step(v / 2, acc + (v and 1)) if _
  );
  step(n, 0)
);

export let count := (bs) => (
  let n := rt.arr_len(bs.words);
  let step := (i, acc) => (
    acc if i >= n
    | step(i + 1, acc + popcount_word(bs.words.[i])) if _
  );
  step(0, 0)
);
