//! Integer literal parsing shared across compiler crates.
//!
//! Grammar reference: `grammar.abnf` (`lit-int` and digit separator `_` rules).

/// Parses an integer literal into `u64`.
///
/// Supported forms:
/// - decimal: `42`, `10_000`
/// - hex: `0xFF`, `0xDEAD_BEEF`
/// - octal: `0o755`, `0o7_55`
/// - binary: `0b1010`, `0b1010_0110`
#[must_use]
pub fn parse_u64(text: &str) -> Option<u64> {
    let text = text.trim();
    let (base, digits) = match text.get(0..2) {
        Some("0x" | "0X") => (16, text.get(2..).unwrap_or("")),
        Some("0o" | "0O") => (8, text.get(2..).unwrap_or("")),
        Some("0b" | "0B") => (2, text.get(2..).unwrap_or("")),
        _ => (10, text),
    };
    if digits.is_empty() {
        return None;
    }
    if !digits.as_bytes().contains(&b'_') {
        return u64::from_str_radix(digits, base).ok();
    }

    let mut buf = String::with_capacity(digits.len());
    for ch in digits.chars() {
        if ch != '_' {
            buf.push(ch);
        }
    }
    u64::from_str_radix(&buf, base).ok()
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
