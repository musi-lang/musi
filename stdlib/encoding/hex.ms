//! Hex encoding and decoding for byte arrays.

import "@std/rt" as rt;

/// Error returned by `decode` when input is invalid hex.
export let DecodeError := choice { InvalidChar of Int + OddLength };

let nibble_to_rune_lower := (nibble) => (
  rt.rune_from_int(48 + nibble) if nibble < 10
  | rt.rune_from_int(97 + nibble - 10) if _
);

let nibble_to_rune_upper := (nibble) => (
  rt.rune_from_int(48 + nibble) if nibble < 10
  | rt.rune_from_int(65 + nibble - 10) if _
);

let rune_to_nibble := (r : Rune, pos : Int) => (
  let cp := rt.rune_to_int(r);
  .Ok(cp - 48) if (cp >= 48 and cp <= 57)
  | .Ok(cp - 97 + 10) if (cp >= 97 and cp <= 102)
  | .Ok(cp - 65 + 10) if (cp >= 65 and cp <= 70)
  | .Err(.InvalidChar(pos)) if _
);

/// Encodes a byte array as a lowercase hexadecimal string.
export let encode := (bytes) => (
  let len := rt.arr_len(bytes);
  let mut chars := [];
  let step := (i) => (
    () if i >= len
    | (
      let b := bytes.[i];
      rt.arr_push(chars, nibble_to_rune_lower(b / 16));
      rt.arr_push(chars, nibble_to_rune_lower(b % 16));
      step(i + 1)
    ) if _
  );
  step(0);
  rt.str_from_chars(chars)
);

/// Encodes a byte array as an uppercase hexadecimal string.
export let encode_upper := (bytes) => (
  let len := rt.arr_len(bytes);
  let mut chars := [];
  let step := (i) => (
    () if i >= len
    | (
      let b := bytes.[i];
      rt.arr_push(chars, nibble_to_rune_upper(b / 16));
      rt.arr_push(chars, nibble_to_rune_upper(b % 16));
      step(i + 1)
    ) if _
  );
  step(0);
  rt.str_from_chars(chars)
);

/// Decodes a hexadecimal string into a byte array, returning .Err on invalid input.
export let decode := (s) => (
  let len := rt.str_len(s);
  .Err(.OddLength) if (len % 2 = 1)
  | (
    let mut bytes := [];
    let step := (i) => (
      .Ok(bytes) if i >= len
      | (
        match rune_to_nibble(rt.str_at(s, i), i) (
          .Err(e) => .Err(e)
          | .Ok(hi) => match rune_to_nibble(rt.str_at(s, i + 1), i + 1) (
            .Err(e) => .Err(e)
            | .Ok(lo) => (
              rt.arr_push(bytes, hi * 16 + lo);
              step(i + 2)
            )
          )
        )
      ) if _
    );
    step(0)
  ) if _
);
