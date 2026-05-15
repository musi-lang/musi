#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericSuffixClass {
    Z,
    N,
    F,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NumericSuffix {
    pub class: NumericSuffixClass,
    pub width: Option<u16>,
}

#[must_use]
pub fn split_numeric_suffix(raw: &str) -> (&str, Option<NumericSuffix>) {
    let bytes = raw.as_bytes();
    if bytes.is_empty() {
        return (raw, None);
    }

    let mut width_start = bytes.len();
    while width_start > 0 && bytes[width_start - 1].is_ascii_digit() {
        width_start -= 1;
    }
    if width_start == 0 {
        return (raw, None);
    }

    let class_index = width_start - 1;
    let class = match bytes[class_index] {
        b'z' => NumericSuffixClass::Z,
        b'n' => NumericSuffixClass::N,
        b'f' => NumericSuffixClass::F,
        _ => return (raw, None),
    };

    let has_separator = class_index > 0 && bytes[class_index - 1] == b'_';
    let suffix_start = if has_separator {
        class_index - 1
    } else {
        class_index
    };
    if suffix_start == 0 {
        return (raw, None);
    }
    if !has_separator {
        let prev = bytes[class_index - 1];
        if !prev.is_ascii_digit() && prev != b'.' {
            return (raw, None);
        }
        let Some(raw_prefix) = raw.get(..class_index) else {
            return (raw, None);
        };
        if matches!(class, NumericSuffixClass::F) && has_non_decimal_prefix(raw_prefix) {
            return (raw, None);
        }
    }

    let width = if width_start < bytes.len() {
        raw.get(width_start..)
            .and_then(|suffix| suffix.parse::<u16>().ok())
    } else {
        None
    };
    let Some(number_part) = raw.get(..suffix_start) else {
        return (raw, None);
    };
    (number_part, Some(NumericSuffix { class, width }))
}

#[must_use]
pub fn parse_i64_literal(raw: &str) -> Option<i64> {
    let (raw, _) = split_numeric_suffix(raw);
    let compact = raw.replace('_', "");
    let (sign, digits) = compact
        .strip_prefix('-')
        .map_or((1_i64, compact.as_str()), |rest| (-1_i64, rest));
    let (radix, digits) = digits
        .strip_prefix("0x")
        .or_else(|| digits.strip_prefix("0X"))
        .map_or_else(
            || {
                digits
                    .strip_prefix("0o")
                    .or_else(|| digits.strip_prefix("0O"))
                    .map_or_else(
                        || {
                            digits
                                .strip_prefix("0b")
                                .or_else(|| digits.strip_prefix("0B"))
                                .map_or((10, digits), |rest| (2, rest))
                        },
                        |rest| (8, rest),
                    )
            },
            |rest| (16, rest),
        );
    i64::from_str_radix(digits, radix)
        .ok()
        .and_then(|value| value.checked_mul(sign))
}

#[must_use]
pub fn parse_u64_literal(raw: &str) -> Option<u64> {
    let (raw, _) = split_numeric_suffix(raw);
    let compact = raw.replace('_', "");
    if compact.starts_with('-') {
        return None;
    }
    let (radix, digits) = compact
        .strip_prefix("0x")
        .or_else(|| compact.strip_prefix("0X"))
        .map_or_else(
            || {
                compact
                    .strip_prefix("0o")
                    .or_else(|| compact.strip_prefix("0O"))
                    .map_or_else(
                        || {
                            compact
                                .strip_prefix("0b")
                                .or_else(|| compact.strip_prefix("0B"))
                                .map_or((10, compact.as_str()), |rest| (2, rest))
                        },
                        |rest| (8, rest),
                    )
            },
            |rest| (16, rest),
        );
    u64::from_str_radix(digits, radix).ok()
}

#[must_use]
pub fn parse_u32_literal(raw: &str) -> Option<u32> {
    parse_u64_literal(raw).and_then(|value| u32::try_from(value).ok())
}

fn has_non_decimal_prefix(raw: &str) -> bool {
    let raw = raw.strip_prefix('-').unwrap_or(raw);
    raw.starts_with("0x")
        || raw.starts_with("0X")
        || raw.starts_with("0o")
        || raw.starts_with("0O")
        || raw.starts_with("0b")
        || raw.starts_with("0B")
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
