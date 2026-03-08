// std/encoding/hex.ms — hexadecimal encoding/decoding
// import { hex_encode, hex_decode, hex_encode_upper } from "std/encoding/hex"

const HEX_CHARS_LOWER: String := "0123456789abcdef";
const HEX_CHARS_UPPER: String := "0123456789ABCDEF";

/// Encode an array of bytes as a lowercase hex string.
export fn hex_encode(bytes: []Int): String => (
    var result := "";
    for b in bytes loop (
        const byte := b % 256;
        const hi := byte / 16;
        const lo := byte % 16;
        result <- string_concat(result, string_concat(
            string_slice(HEX_CHARS_LOWER, hi, hi + 1),
            string_slice(HEX_CHARS_LOWER, lo, lo + 1)
        ))
    );
    result
);

/// Encode an array of bytes as an uppercase hex string.
export fn hex_encode_upper(bytes: []Int): String => (
    var result := "";
    for b in bytes loop (
        const byte := b % 256;
        const hi := byte / 16;
        const lo := byte % 16;
        result <- string_concat(result, string_concat(
            string_slice(HEX_CHARS_UPPER, hi, hi + 1),
            string_slice(HEX_CHARS_UPPER, lo, lo + 1)
        ))
    );
    result
);

fn hex_char_value(c: String): Option[Int] => (
    match string_index_of(string_to_lower(HEX_CHARS_LOWER), c) with (
        .Some(i) => .Some(i)
      | .None => .None
    )
);

/// Decode a hex string to an array of bytes. Returns `.None` on invalid input.
export fn hex_decode(s: String): Option[[]Int] => (
    const len := string_length(s);
    if len % 2 /= 0 then .None
    else (
        var result := [];
        var i := 0;
        var valid := .True;
        while i < len and valid loop (
            const hi_char := string_to_lower(string_slice(s, i, i + 1));
            const lo_char := string_to_lower(string_slice(s, i + 1, i + 2));
            match hex_char_value(hi_char) with (
                .None => (valid <- .False)
              | .Some(hi) => match hex_char_value(lo_char) with (
                    .None => (valid <- .False)
                  | .Some(lo) => array_push(result, hi * 16 + lo)
                )
            );
            i <- i + 2
        );
        if valid then .Some(result) else .None
    )
);

/// Encode a single integer as a hex string (zero-padded to at least 2 chars).
export fn int_to_hex(n: Int): String => (
    if n = 0 then "00"
    else (
        var result := "";
        var val := if n < 0 then 0 - n else n;
        while val > 0 loop (
            const digit := val % 16;
            result <- string_concat(string_slice(HEX_CHARS_LOWER, digit, digit + 1), result);
            val <- val / 16
        );
        if string_length(result) % 2 = 1 then result <- string_concat("0", result);
        if n < 0 then string_concat("-", result) else result
    )
);

/// Parse a hex string to an integer. Returns `.None` on invalid input.
export fn hex_to_int(s: String): Option[Int] => (
    const input := string_trim(s);
    const len := string_length(input);
    if len = 0 then .None
    else (
        var result := 0;
        var i := 0;
        var valid := .True;
        while i < len and valid loop (
            const c := string_to_lower(string_slice(input, i, i + 1));
            match hex_char_value(c) with (
                .None => (valid <- .False)
              | .Some(v) => (result <- result * 16 + v)
            );
            i <- i + 1
        );
        if valid then .Some(result) else .None
    )
);
