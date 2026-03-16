//! String inspection predicates.

import "@std/rt" as rt;

export let is_blank := (s) => (
  let chars := rt.str_chars(s);
  let len := rt.arr_len(chars);
  let step := (i) => (
    .True if i >= len
    | (step(i + 1) if rt.rune_is_whitespace(chars.[i]) | .False if _) if _
  );
  step(0)
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
