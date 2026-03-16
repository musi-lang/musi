//! String formatting utilities.

import "./rt" as rt;

export let pad_left := (s, width, ch) => (
  let len := rt.str_len(s);
  (rt.str_repeat(ch, width - len) + s if len < width | s if _)
);

export let pad_right := (s, width, ch) => (
  let len := rt.str_len(s);
  (s + rt.str_repeat(ch, width - len) if len < width | s if _)
);

export let center := (s, width, ch) => (
  let len := rt.str_len(s);
  (
    let total := width - len;
    let left := total / 2;
    let right := total - left;
    rt.str_repeat(ch, left) + s + rt.str_repeat(ch, right)
  ) if len < width
  | s if _
);

export let join := (arr, sep) => (
  let len := rt.arr_len(arr);
  "" if len = 0
  | (
    let step := (acc, i) => (
      acc if i >= len
      | step(acc + sep + arr.[i], i + 1) if _
    );
    step(arr.[0], 1)
  ) if _
);

let hex_char := (d) => (
  rt.rune_from_int(d + rt.rune_to_int('0')) if d < 10
  | rt.rune_from_int(d - 10 + rt.rune_to_int('a')) if _
);

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

export let int_to_bin := (n) => (
  let abs_n := (n if n >= 0 | 0 - n if _);
  (
    let mut chars := [];
    let step := (v) => (
      (
        let bit := (rt.rune_from_int(rt.rune_to_int('1')) if (v and 1) = 1 | rt.rune_from_int(rt.rune_to_int('0')) if _);
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

export let truncate := (s, max_len, suffix) => (
  let len := rt.str_len(s);
  let suf_len := rt.str_len(suffix);
  (rt.str_slice(s, 0, max_len - suf_len) + suffix if len > max_len | s if _)
);
