//! String inspection predicates.

import "@std/rt" as rt;

/// Return whether the string contains only whitespace characters (or is empty).
export let is_blank := (s) => (
  let chars := rt.str_chars(s);
  let len := rt.arr_len(chars);
  let step := (i) => (
    .True if i >= len
    | (step(i + 1) if rt.rune_is_whitespace(chars.[i]) | .False if _) if _
  );
  step(0)
);

/// Return whether the string is non-empty and every character is alphabetic.
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

/// Return whether the string is non-empty and every character is a decimal digit.
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

/// Return whether the string is non-empty and every character is alphabetic or a digit.
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
