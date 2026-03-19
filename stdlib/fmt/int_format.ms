//! Integer formatting utilities.

import "@std/rt" as rt;

let hex_char := (d) => (
  rt.rune_from_int(d + rt.rune_to_int('0')) if d < 10
  | rt.rune_from_int(d - 10 + rt.rune_to_int('a')) if _
);

/// Converts an integer to its lowercase hexadecimal string representation.
export let int_to_hex := (n) => (
  let abs_n := (n if n >= 0 | 0 - n if _);
  (
    let mut chars := [];
    let step := (v) => (
      (rt.arr_push(chars, hex_char(v and 0xF)); step(v / 16)) if v > 0
      | () if _
    );
    step(abs_n);
    rt.arr_reverse(chars);
    let digits := rt.str_from_chars(chars);
    ("-" + digits if n < 0 | digits if _)
  ) if abs_n > 0
  | "0" if _
);

/// Converts an integer to its binary string representation.
export let int_to_bin := (n) => (
  let abs_n := (n if n >= 0 | 0 - n if _);
  (
    let mut chars := [];
    let step := (v) => (
      (
        let bit := ('1' if (v and 1) = 1 | '0' if _);
        rt.arr_push(chars, bit);
        step(v / 2)
      ) if v > 0
      | () if _
    );
    step(abs_n);
    rt.arr_reverse(chars);
    let digits := rt.str_from_chars(chars);
    ("-" + digits if n < 0 | digits if _)
  ) if abs_n > 0
  | "0" if _
);
