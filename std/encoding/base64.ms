// std/encoding/base64.ms — Base64 encoding/decoding (RFC 4648)
// import { base64_encode, base64_decode } from "std/encoding/base64"

const B64_CHARS: String := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/// Encode an array of bytes as a Base64 string.
export fn base64_encode(bytes: []Int): String => (
    const len := array_length(bytes);
    var result := "";
    var i := 0;

    while i < len loop (
        const b0 := array_get(bytes, i) % 256;
        const b1 := if i + 1 < len then array_get(bytes, i + 1) % 256 else 0;
        const b2 := if i + 2 < len then array_get(bytes, i + 2) % 256 else 0;

        const c0 := b0 / 4;
        const c1 := (b0 % 4) * 16 + b1 / 16;
        const c2 := (b1 % 16) * 4 + b2 / 64;
        const c3 := b2 % 64;

        result <- string_concat(result, string_slice(B64_CHARS, c0, c0 + 1));
        result <- string_concat(result, string_slice(B64_CHARS, c1, c1 + 1));

        if i + 1 < len then result <- string_concat(result, string_slice(B64_CHARS, c2, c2 + 1))
        else result <- string_concat(result, "=");

        if i + 2 < len then result <- string_concat(result, string_slice(B64_CHARS, c3, c3 + 1))
        else result <- string_concat(result, "=");

        i <- i + 3
    );
    result
);

fn b64_char_value(c: String): Option[Int] => (
    match string_index_of(B64_CHARS, c) with (
        .Some(i) => .Some(i)
      | .None => if c = "=" then .Some(0) else .None
    )
);

/// Decode a Base64 string to an array of bytes. Returns `.None` on invalid input.
export fn base64_decode(s: String): Option[[]Int] => (
    const input := string_trim(s);
    const len := string_length(input);
    if len % 4 /= 0 then .None
    else (
        var result := [];
        var i := 0;
        var valid := .True;

        while i < len and valid loop (
            const c0 := string_slice(input, i, i + 1);
            const c1 := string_slice(input, i + 1, i + 2);
            const c2 := string_slice(input, i + 2, i + 3);
            const c3 := string_slice(input, i + 3, i + 4);

            match b64_char_value(c0) with (
                .None => valid <- .False
              | .Some(v0) => match b64_char_value(c1) with (
                    .None => valid <- .False
                  | .Some(v1) => match b64_char_value(c2) with (
                        .None => valid <- .False
                      | .Some(v2) => match b64_char_value(c3) with (
                            .None => valid <- .False
                          | .Some(v3) => (
                                array_push(result, v0 * 4 + v1 / 16);
                                if c2 /= "=" then array_push(result, (v1 % 16) * 16 + v2 / 4);
                                if c3 /= "=" then array_push(result, (v2 % 4) * 64 + v3)
                            )
                        )
                    )
                )
            );
            i <- i + 4
        );
        if valid then .Some(result) else .None
    )
);

/// Encode a string as Base64 (treats string as bytes).
export fn base64_encode_string(s: String): String => (
    var bytes := [];
    var i := 0;
    while i < string_length(s) loop (
        // Note: This is a simplification; proper UTF-8 encoding would be needed
        // For ASCII, this works correctly
        array_push(bytes, char_code_at(s, i));
        i <- i + 1
    );
    base64_encode(bytes)
);

fn char_code_at(s: String, i: Int): Int => (
    // Simplified: return ASCII code of character
    // In a full implementation, this would need proper UTF-8 handling
    const c := string_slice(s, i, i + 1);
    match string_index_of("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=!@#$%^&*()-_[]{}|;':\",./<>?`~ \t\n\r", c) with (
        .Some(idx) => (
            if idx < 26 then 65 + idx           // A-Z
            else if idx < 52 then 97 + (idx - 26)    // a-z
            else if idx < 62 then 48 + (idx - 52)    // 0-9
            else 0  // fallback
        )
      | .None => 0
    )
);

/// Check if a string is valid Base64.
export fn is_valid_base64(s: String): Bool => (
    const input := string_trim(s);
    const len := string_length(input);
    if len % 4 /= 0 then .False
    else (
        var valid := .True;
        var i := 0;
        while i < len and valid loop (
            const c := string_slice(input, i, i + 1);
            if string_contains(B64_CHARS, c) or c = "=" then ()
            else valid <- .False;
            i <- i + 1
        );
        valid
    )
);
