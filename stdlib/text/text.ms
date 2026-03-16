//! String utilities.

import "../rt" as rt;

export let pad_start := (s, target_len, pad_char) => (
  let step := (acc, remaining) => (
    acc if remaining <= 0
    | step(pad_char + acc, remaining - 1) if _
  );
  step(s, target_len - rt.str_len(s))
);

export let pad_end := (s, target_len, pad_char) => (
  let step := (acc, remaining) => (
    acc if remaining <= 0
    | step(acc + pad_char, remaining - 1) if _
  );
  step(s, target_len - rt.str_len(s))
);

export let center := (s, target_len, pad_char) => (
  let deficit := target_len - rt.str_len(s);
  let left_pad := deficit / 2;
  let right_pad := deficit - left_pad;
  let add_left := (acc, remaining) => (
    acc if remaining <= 0
    | add_left(pad_char + acc, remaining - 1) if _
  );
  let add_right := (acc, remaining) => (
    acc if remaining <= 0
    | add_right(acc + pad_char, remaining - 1) if _
  );
  add_right(add_left(s, left_pad), right_pad)
);

export let capitalize := (s) => (
  "" if rt.str_len(s) = 0
  | rt.str_to_upper(rt.str_slice(s, 0, 1)) + rt.str_slice(s, 1, rt.str_len(s)) if _
);

export let uncapitalize := (s) => (
  "" if rt.str_len(s) = 0
  | rt.str_to_lower(rt.str_slice(s, 0, 1)) + rt.str_slice(s, 1, rt.str_len(s)) if _
);

export let is_blank := (s) => (
  let chars := rt.str_chars(s);
  let len := rt.arr_len(chars);
  let step := (i) => (
    .True if i >= len
    | (step(i + 1) if rt.rune_is_whitespace(chars.[i]) | .False if _) if _
  );
  step(0)
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

export let lines := (s) => rt.str_split(s, "\n");

export let words := (s) => (
  let raw := rt.str_split(rt.str_trim(s), " ");
  let len := rt.arr_len(raw);
  let mut result := [];
  let step := (i) => (
    () if i >= len
    | (
      (rt.arr_push(result, raw.[i]); step(i + 1)) if rt.str_len(raw.[i]) > 0
      | step(i + 1) if _
    ) if _
  );
  step(0);
  result
);

export let count_occurrences := (s, sub) => (
  let sub_len := rt.str_len(sub);
  0 if sub_len = 0
  | (
    let str_len := rt.str_len(s);
    let step := (pos, acc) => (
      acc if pos > str_len - sub_len
      | (
        let idx := rt.str_index_of(rt.str_slice(s, pos, str_len), sub);
        acc if idx = -1
        | step(pos + idx + sub_len, acc + 1) if _
      ) if _
    );
    step(0, 0)
  ) if _
);

export let last_index_of := (s, sub) => (
  let sub_len := rt.str_len(sub);
  -1 if sub_len = 0
  | (
    let str_len := rt.str_len(s);
    let step := (pos, last) => (
      last if pos > str_len - sub_len
      | (
        let idx := rt.str_index_of(rt.str_slice(s, pos, str_len), sub);
        last if idx = -1
        | step(pos + idx + sub_len, pos + idx) if _
      ) if _
    );
    step(0, -1)
  ) if _
);

export let reverse_str := (s) => (
  rt.str_from_chars(rt.arr_reverse(rt.str_chars(s)))
);

export let truncate := (s, max_len) => (
  rt.str_slice(s, 0, max_len) if rt.str_len(s) > max_len
  | s if _
);

export let remove_prefix := (s, pre) => (
  rt.str_slice(s, rt.str_len(pre), rt.str_len(s)) if rt.str_starts_with(s, pre)
  | s if _
);

export let remove_suffix := (s, suf) => (
  rt.str_slice(s, 0, rt.str_len(s) - rt.str_len(suf)) if rt.str_ends_with(s, suf)
  | s if _
);

export let is_alpha := (s) => (
  let chars := rt.str_chars(s);
  let len := rt.arr_len(chars);
  .False if len = 0
  | (
    let step := (i) => (
      .True if i >= len
      | (step(i + 1) if rt.rune_is_alpha(chars.[i]) | .False if _) if _
    );
    step(0)
  ) if _
);

export let is_digit := (s) => (
  let chars := rt.str_chars(s);
  let len := rt.arr_len(chars);
  .False if len = 0
  | (
    let step := (i) => (
      .True if i >= len
      | (step(i + 1) if rt.rune_is_digit(chars.[i]) | .False if _) if _
    );
    step(0)
  ) if _
);

export let is_alphanumeric := (s) => (
  let chars := rt.str_chars(s);
  let len := rt.arr_len(chars);
  .False if len = 0
  | (
    let step := (i) => (
      .True if i >= len
      | (
        step(i + 1) if rt.rune_is_alpha(chars.[i])
        | step(i + 1) if rt.rune_is_digit(chars.[i])
        | .False if _
      ) if _
    );
    step(0)
  ) if _
);
