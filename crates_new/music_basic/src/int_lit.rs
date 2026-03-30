/// Integer literal parsing shared across compiler crates.
///
/// Grammar reference: `grammar.abnf` (`lit-int` and digit separator `_` rules).

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
    let (base, digits) = if let Some(hex) = text.strip_prefix("0x").or(text.strip_prefix("0X")) {
        (16, hex)
    } else if let Some(oct) = text.strip_prefix("0o").or(text.strip_prefix("0O")) {
        (8, oct)
    } else if let Some(bin) = text.strip_prefix("0b").or(text.strip_prefix("0B")) {
        (2, bin)
    } else {
        (10, text)
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

