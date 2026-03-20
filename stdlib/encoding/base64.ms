//! Base64 encoding and decoding (RFC 4648).

import "@std/rt" as rt;

let STANDARD_ALPHABET := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
let URL_SAFE_ALPHABET  := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

/// Error returned by `decode` and `decode_url_safe` when input is malformed.
export let DecodeError := choice {
  InvalidChar of Int
  + InvalidLength
  + InvalidPadding
};

let char_to_value := (code, is_url_safe) => (
  (code - 65) if code >= 65 and code <= 90
  | (code - 71) if code >= 97 and code <= 122
  | (code + 4)  if code >= 48 and code <= 57
  | 62 if code = 43
  | 62 if code = 45 and is_url_safe
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
    (
      let b0 := bytes.[rem_base];
      push_char(b0 / 4);
      push_char((b0 - (b0 / 4) * 4) * 16);
      rt.arr_push(out, rt.rune_from_int(61));
      rt.arr_push(out, rt.rune_from_int(61))
    ) if remainder = 1
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

/// Encodes a byte array as standard Base64 (with `+` and `/`).
export let encode := (bytes) => encode_with(bytes, STANDARD_ALPHABET);

/// Encodes a byte array as URL-safe Base64 (with `-` and `_`).
export let encode_url_safe := (bytes) => encode_with(bytes, URL_SAFE_ALPHABET);

let decode_with := (s, is_url_safe) => (
  let chars := rt.str_chars(s);
  let total := rt.arr_len(chars);

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
        // codepoint 61 is '='
        let pad2 := r2 = 61 and r3 = 61 and is_last;
        let pad1 := not (r2 = 61) and r3 = 61 and is_last;

        let v0 := char_to_value(r0, is_url_safe);
        let v1 := char_to_value(r1, is_url_safe);

        (
          (has_error <- .True; error <- .Some(.Err(.InvalidChar(r0)))) if v0 = -1
          | (has_error <- .True; error <- .Some(.Err(.InvalidChar(r1)))) if v1 = -1
          | (
            (
              rt.arr_push(out, v0 * 4 + v1 / 16)
            ) if pad2
            | (
              let v2 := char_to_value(r2, is_url_safe);
              (has_error <- .True; error <- .Some(.Err(.InvalidChar(r2)))) if v2 = -1
              | (
                rt.arr_push(out, v0 * 4 + v1 / 16);
                rt.arr_push(out, (v1 - (v1 / 16) * 16) * 16 + v2 / 4)
              ) if _
            ) if pad1
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

/// Decodes a standard Base64 string into a byte array, returning .Err on invalid input.
export let decode := (s) => decode_with(s, .False);

/// Decodes a URL-safe Base64 string into a byte array, returning .Err on invalid input.
export let decode_url_safe := (s) => decode_with(s, .True);
