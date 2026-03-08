// std/rune.ms — Rune (Unicode code point) utilities
// import { rune_to_string, string_to_runes, rune_is_alpha, rune_is_digit, ... } from "std/rune"

import { rune_to_int, int_to_rune } from "musi:string";
export { rune_to_string, string_to_runes, rune_to_int, int_to_rune } from "musi:string";

/// Return `.True` if the rune is an ASCII letter (a-z or A-Z).
export fn rune_is_alpha(r: Rune): Bool =>
    (r >= 'a' and r <= 'z') or (r >= 'A' and r <= 'Z');

/// Return `.True` if the rune is an ASCII decimal digit (0-9).
export fn rune_is_digit(r: Rune): Bool => r >= '0' and r <= '9';

/// Return `.True` if the rune is an ASCII letter or digit.
export fn rune_is_alnum(r: Rune): Bool => rune_is_alpha(r) or rune_is_digit(r);

/// Return `.True` if the rune is an ASCII lowercase letter.
export fn rune_is_lower(r: Rune): Bool => r >= 'a' and r <= 'z';

/// Return `.True` if the rune is an ASCII uppercase letter.
export fn rune_is_upper(r: Rune): Bool => r >= 'A' and r <= 'Z';

/// Return `.True` if the rune is ASCII whitespace (space, tab, newline, carriage return).
export fn rune_is_whitespace(r: Rune): Bool =>
    r = ' ' or r = '\t' or r = '\n' or r = '\r';

/// Return `.True` if the rune is a printable ASCII character (space through tilde).
export fn rune_is_printable(r: Rune): Bool => r >= ' ' and r <= '~';

/// Return `.True` if the rune is in the ASCII range (code point 0–127).
export fn rune_is_ascii(r: Rune): Bool => r >= '\0' and r <= '~';

/// Return `.True` if the rune is a hex digit (0-9, a-f, A-F).
export fn rune_is_hex_digit(r: Rune): Bool =>
    rune_is_digit(r) or (r >= 'a' and r <= 'f') or (r >= 'A' and r <= 'F');

/// Convert an ASCII uppercase rune to lowercase. Other runes are returned unchanged.
export fn rune_to_lower(r: Rune): Rune =>
    if rune_is_upper(r) then int_to_rune(rune_to_int(r) + 32) else r;

/// Convert an ASCII lowercase rune to uppercase. Other runes are returned unchanged.
export fn rune_to_upper(r: Rune): Rune =>
    if rune_is_lower(r) then int_to_rune(rune_to_int(r) - 32) else r;

/// Return the numeric value of a decimal digit rune (0-9), or `.None` if not a digit.
export fn rune_digit_value(r: Rune): Option[Int] =>
    if rune_is_digit(r) then .Some(rune_to_int(r) - rune_to_int('0')) else .None;

/// Return the numeric value of a hex digit rune (0-9, a-f, A-F), or `.None`.
export fn rune_hex_value(r: Rune): Option[Int] =>
    if r >= '0' and r <= '9' then .Some(rune_to_int(r) - rune_to_int('0'))
    else if r >= 'a' and r <= 'f' then .Some(rune_to_int(r) - rune_to_int('a') + 10)
    else if r >= 'A' and r <= 'F' then .Some(rune_to_int(r) - rune_to_int('A') + 10)
    else .None;
