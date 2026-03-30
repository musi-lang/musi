//! String literal decoding.
//!
//! Inputs use the token slice form, including surrounding quotes.

/// Decode a Musi string literal token slice, including surrounding quotes.
///
/// When the input is not a quoted string token, it is returned verbatim.
#[must_use]
pub fn decode(raw: &str) -> String {
    let Some(inner) = raw.strip_prefix('"').and_then(|s| s.strip_suffix('"')) else {
        return raw.to_string();
    };

    if !inner.contains('\\') {
        return inner.to_string();
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
                let hi = chars.next();
                let lo = chars.next();
                let Some((hi, lo)) = hi.zip(lo) else {
                    break;
                };
                let mut buf = [0u8; 2];
                buf[0] = hi as u8;
                buf[1] = lo as u8;
                let Ok(hex) = core::str::from_utf8(&buf) else {
                    continue;
                };
                if let Ok(byte) = u8::from_str_radix(hex, 16) {
                    out.push(byte as char);
                }
            }
            'u' => {
                if chars.next() != Some('{') {
                    continue;
                }
                let mut digits = String::new();
                while let Some(next) = chars.next() {
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
