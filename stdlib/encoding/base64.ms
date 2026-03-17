//! Base64 encoding and decoding (RFC 4648).

import "@std/rt" as rt;

let STANDARD_ALPHABET := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
let URL_SAFE_ALPHABET  := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

export let DecodeError := choice {
  InvalidChar of Int
  + InvalidLength
  + InvalidPadding
};

let char_to_value := (code, is_url_safe) => (
  // 'A'-'Z' → 0-25
  (code - 65) if code >= 65 and code <= 90
  // 'a'-'z' → 26-51
  | (code - 71) if code >= 97 and code <= 122
  // '0'-'9' → 52-61
  | (code + 4) if code >= 48 and code <= 57
  // '+' → 62 (standard), '-' → 62 (url-safe)
  | 62 if code = 43
  | 62 if code = 45 and is_url_safe
  // '/' → 63 (standard), '_' → 63 (url-safe)
  | 63 if code = 47
  | 63 if code = 95 and is_url_safe
  | -1 if _
);

let encode_with := (bytes, alphabet) => (
  let len := rt.arr_len(bytes);
  let full_groups := len / 3;
  let remainder := len - full_groups * 3;
  let mut out := [];

  let push_char := (idx) => (
    rt.arr_push(out, rt.str_at(alphabet, idx))
  );

  let encode_group := (g) => (
    () if g >= full_groups
    | (
      let base := g * 3;
      let b0 := bytes.[base];
      let b1 := bytes.[base + 1];
      let b2 := bytes.[base + 2];
      push_char(b0 / 4);
      push_char((b0 - (b0 / 4) * 4) * 16 + b1 / 16);
      push_char((b1 - (b1 / 16) * 16) * 4 + b2 / 64);
      push_char(b2 - (b2 / 64) * 64);
      encode_group(g + 1)
    ) if _
  );

  encode_group(0);

  let rem_base := full_groups * 3;
  (
    // 1 remaining byte → 2 chars + "=="
    (
      let b0 := bytes.[rem_base];
      push_char(b0 / 4);
      push_char((b0 - (b0 / 4) * 4) * 16);
      rt.arr_push(out, rt.rune_from_int(61));
      rt.arr_push(out, rt.rune_from_int(61))
    ) if remainder = 1
    // 2 remaining bytes → 3 chars + "="
    | (
      let b0 := bytes.[rem_base];
      let b1 := bytes.[rem_base + 1];
      push_char(b0 / 4);
      push_char((b0 - (b0 / 4) * 4) * 16 + b1 / 16);
      push_char((b1 - (b1 / 16) * 16) * 4);
      rt.arr_push(out, rt.rune_from_int(61))
    ) if remainder = 2
    | () if _
  );

  rt.str_from_chars(out)
);

export let encode := (bytes) => encode_with(bytes, STANDARD_ALPHABET);

export let encode_url_safe := (bytes) => encode_with(bytes, URL_SAFE_ALPHABET);

let decode_with := (s, is_url_safe) => (
  let chars := rt.str_chars(s);
  let total := rt.arr_len(chars);

  // Base64 strings must be a multiple of 4 characters
  .Err(.InvalidLength) if total = 0 or (total - (total / 4) * 4) /= 0
  | (
    let full_groups := total / 4;
    let mut out := [];
    let mut has_error := .False;
    let mut error := .None;

    let decode_group := (g) => (
      () if g >= full_groups or has_error
      | (
        let base := g * 4;
        let r0 := rt.rune_to_int(chars.[base]);
        let r1 := rt.rune_to_int(chars.[base + 1]);
        let r2 := rt.rune_to_int(chars.[base + 2]);
        let r3 := rt.rune_to_int(chars.[base + 3]);
        let is_last := g = full_groups - 1;
        // '=' is codepoint 61
        let pad2 := r2 = 61 and r3 = 61 and is_last;
        let pad1 := not (r2 = 61) and r3 = 61 and is_last;

        let v0 := char_to_value(r0, is_url_safe);
        let v1 := char_to_value(r1, is_url_safe);

        (
          // Validate v0 and v1 - always required
          (has_error <- .True; error <- .Some(.Err(.InvalidChar(r0)))) if v0 = -1
          | (has_error <- .True; error <- .Some(.Err(.InvalidChar(r1)))) if v1 = -1
          | (
            // "==" padding: 2 pad chars, produce 1 byte
            (
              rt.arr_push(out, v0 * 4 + v1 / 16)
            ) if pad2
            // "=" padding: 1 pad char, produce 2 bytes
            | (
              let v2 := char_to_value(r2, is_url_safe);
              (has_error <- .True; error <- .Some(.Err(.InvalidChar(r2)))) if v2 = -1
              | (
                rt.arr_push(out, v0 * 4 + v1 / 16);
                rt.arr_push(out, (v1 - (v1 / 16) * 16) * 16 + v2 / 4)
              ) if _
            ) if pad1
            // Non-padding group: produce 3 bytes
            | (
              let v2 := char_to_value(r2, is_url_safe);
              let v3 := char_to_value(r3, is_url_safe);
              (has_error <- .True; error <- .Some(.Err(.InvalidChar(r2)))) if v2 = -1
              | (has_error <- .True; error <- .Some(.Err(.InvalidChar(r3)))) if v3 = -1
              | (
                rt.arr_push(out, v0 * 4 + v1 / 16);
                rt.arr_push(out, (v1 - (v1 / 16) * 16) * 16 + v2 / 4);
                rt.arr_push(out, (v2 - (v2 / 4) * 4) * 64 + v3)
              ) if _
            ) if _
          ) if _
        );

        decode_group(g + 1)
      ) if _
    );

    decode_group(0);

    match error (
      .Some(e) => e
      | .None => .Ok(out)
    )
  ) if _
);

export let decode := (s) => decode_with(s, .False);

export let decode_url_safe := (s) => decode_with(s, .True);
