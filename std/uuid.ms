// std/uuid.ms — UUID utilities (validation and formatting)
// import { UUID, uuid_parse, uuid_to_string, is_valid_uuid } from "std/uuid"
// Note: Generation requires random intrinsics (not yet available)

/// A parsed UUID (128 bits as 4 x 32-bit integers).
export record UUID {
    a: Int,  // bits 96-127
    b: Int,  // bits 64-95
    c: Int,  // bits 32-63
    d: Int   // bits 0-31
};

/// The nil UUID (all zeros).
export fn uuid_nil(): UUID => UUID.{ a := 0, b := 0, c := 0, d := 0 };

/// Check if a UUID is the nil UUID.
export fn uuid_is_nil(u: UUID): Bool => u.a = 0 and u.b = 0 and u.c = 0 and u.d = 0;

/// Format a UUID as the standard string representation (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx).
export fn uuid_to_string(u: UUID): String => (
    const hex_a := pad_hex(u.a, 8);
    const hex_b := pad_hex(u.b, 8);
    const hex_c := pad_hex(u.c, 8);
    const hex_d := pad_hex(u.d, 8);

    string_concat(
        string_concat(string_slice(hex_a, 0, 8), "-"),
        string_concat(
            string_concat(string_slice(hex_b, 0, 4), "-"),
            string_concat(
                string_concat(string_slice(hex_b, 4, 8), "-"),
                string_concat(
                    string_concat(string_slice(hex_c, 0, 4), "-"),
                    string_concat(string_slice(hex_c, 4, 8), hex_d)
                )
            )
        )
    )
);

fn pad_hex(n: Int, width: Int): String => (
    const HEX := "0123456789abcdef";
    var result := "";
    var val := n;
    var i := 0;
    while i < width loop (
        const digit := val % 16;
        result <- string_concat(string_slice(HEX, digit, digit + 1), result);
        val <- val / 16;
        i <- i + 1
    );
    result
);

fn hex_digit_value(c: String): Option[Int] => (
    const HEX := "0123456789abcdef";
    string_index_of(HEX, string_to_lower(c))
);

/// Parse a UUID string. Returns `.None` on invalid input.
/// Accepts formats: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx or xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
export fn uuid_parse(s: String): Option[UUID] => (
    const input := string_to_lower(string_trim(s));
    // Remove hyphens
    const clean := if string_contains(input, "-")
        then string_concat(
            string_concat(
                string_concat(
                    string_concat(
                        string_slice(input, 0, 8),
                        string_slice(input, 9, 13)
                    ),
                    string_slice(input, 14, 18)
                ),
                string_slice(input, 19, 23)
            ),
            string_slice(input, 24, 36)
        )
        else input;

    if string_length(clean) /= 32 then .None
    else (
        match parse_hex_segment(clean, 0, 8) with (
            .None => .None
          | .Some(a) => match parse_hex_segment(clean, 8, 16) with (
                .None => .None
              | .Some(b) => match parse_hex_segment(clean, 16, 24) with (
                    .None => .None
                  | .Some(c) => match parse_hex_segment(clean, 24, 32) with (
                        .None => .None
                      | .Some(d) => .Some(UUID.{ a := a, b := b, c := c, d := d })
                    )
                )
            )
        )
    )
);

fn parse_hex_segment(s: String, start: Int, end: Int): Option[Int] => (
    var result := 0;
    var i := start;
    var valid := .True;
    while i < end and valid loop (
        const c := string_slice(s, i, i + 1);
        match hex_digit_value(c) with (
            .None => (valid <- .False)
          | .Some(v) => (result <- result * 16 + v)
        );
        i <- i + 1
    );
    if valid then .Some(result) else .None
);

/// Check if a string is a valid UUID format.
export fn is_valid_uuid(s: String): Bool => (
    match uuid_parse(s) with (
        .Some(_) => .True
      | .None => .False
    )
);

/// Get the version number from a UUID (bits 12-15 of time_hi_and_version).
export fn uuid_version(u: UUID): Int => (u.b / 4096) % 16;

/// Get the variant from a UUID (bits 0-1 of clock_seq_hi_and_reserved).
export fn uuid_variant(u: UUID): Int => (u.c / 16384) % 4;

/// Compare two UUIDs for equality.
export fn uuid_eq(a: UUID, b: UUID): Bool =>
    a.a = b.a and a.b = b.b and a.c = b.c and a.d = b.d;
