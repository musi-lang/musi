//! String literal decoding.
//!
//! Inputs use the token slice form, including surrounding quotes.

use core::str;

/// Decode a Musi string literal token slice, including surrounding quotes.
///
/// When the input is not a quoted string token, it is returned verbatim.
#[must_use]
pub fn decode(raw: &str) -> String {
    let Some(inner) = raw.strip_prefix('"').and_then(|s| s.strip_suffix('"')) else {
        return raw.to_owned();
    };

    if !inner.contains('\\') {
        return inner.to_owned();
    }

    let mut out = String::with_capacity(inner.len());
    let mut chars = inner.chars();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            out.push(ch);
            continue;
        }

        let Some(esc) = chars.next() else {
            break;
        };

        match esc {
            'n' => out.push('\n'),
            't' => out.push('\t'),
            'r' => out.push('\r'),
            '\\' => out.push('\\'),
            '"' => out.push('"'),
            '0' => out.push('\0'),
            'x' => {
                if let Some((hi, lo)) = chars.next().zip(chars.next())
                    && let Ok(hi) = u8::try_from(u32::from(hi))
                    && let Ok(lo) = u8::try_from(u32::from(lo))
                {
                    let buf = [hi, lo];
                    if let Ok(hex) = str::from_utf8(&buf) {
                        if let Ok(byte) = u8::from_str_radix(hex, 16) {
                            out.push(char::from(byte));
                        }
                    }
                }
            }
            'u' => {
                if chars.next() != Some('{') {
                    continue;
                }
                let mut digits = String::new();
                for next in chars.by_ref() {
                    if next == '}' {
                        break;
                    }
                    digits.push(next);
                }
                if let Ok(code) = u32::from_str_radix(&digits, 16) {
                    if let Some(ch) = char::from_u32(code) {
                        out.push(ch);
                    }
                }
            }
            other => out.push(other),
        }
    }

    out
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
