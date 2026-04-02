use core::fmt;
use core::str;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringLitErrorKind {
    Unterminated,
    InvalidEscape,
    MissingHexDigits,
    InvalidHexDigit,
    InvalidUnicodeScalar,
    RuneCharCount,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLitError {
    pub kind: StringLitErrorKind,
    pub offset: usize,
}

impl fmt::Display for StringLitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} at byte {}", self.kind, self.offset)
    }
}

pub fn decode_string_lit(raw: &str) -> Result<String, StringLitError> {
    decode_delimited(raw, b'"')
}

pub fn decode_rune_lit(raw: &str) -> Result<u32, StringLitError> {
    let s = decode_delimited(raw, b'\'')?;
    let mut chars = s.chars();
    let Some(ch) = chars.next() else {
        return Err(StringLitError {
            kind: StringLitErrorKind::RuneCharCount,
            offset: 0,
        });
    };
    if chars.next().is_some() {
        return Err(StringLitError {
            kind: StringLitErrorKind::RuneCharCount,
            offset: 0,
        });
    }
    Ok(u32::from(ch))
}

pub fn decode_template_lit(raw: &str) -> Result<String, StringLitError> {
    decode_template_no_subst(raw)
}

pub fn decode_template_no_subst(raw: &str) -> Result<String, StringLitError> {
    decode_delimited(raw, b'`')
}

pub fn decode_template_head(raw: &str) -> Result<String, StringLitError> {
    decode_template_chunk(raw, TemplateChunkStyle::Head)
}

pub fn decode_template_middle(raw: &str) -> Result<String, StringLitError> {
    decode_template_chunk(raw, TemplateChunkStyle::Middle)
}

pub fn decode_template_tail(raw: &str) -> Result<String, StringLitError> {
    decode_template_chunk(raw, TemplateChunkStyle::Tail)
}

#[derive(Debug, Clone, Copy)]
enum TemplateChunkStyle {
    Head,
    Middle,
    Tail,
}

fn decode_template_chunk(raw: &str, style: TemplateChunkStyle) -> Result<String, StringLitError> {
    let b = raw.as_bytes();
    match style {
        TemplateChunkStyle::Head => {
            if b.len() < 3 || b[0] != b'`' || !raw.ends_with("${") {
                return Err(StringLitError {
                    kind: StringLitErrorKind::Unterminated,
                    offset: 0,
                });
            }
            decode_inner(b, 1, b.len() - 2)
        }
        TemplateChunkStyle::Middle => {
            if b.len() < 3 || b[0] != b'}' || !raw.ends_with("${") {
                return Err(StringLitError {
                    kind: StringLitErrorKind::Unterminated,
                    offset: 0,
                });
            }
            decode_inner(b, 1, b.len() - 2)
        }
        TemplateChunkStyle::Tail => {
            if b.len() < 2 || b[0] != b'}' || *b.last().unwrap_or(&0) != b'`' {
                return Err(StringLitError {
                    kind: StringLitErrorKind::Unterminated,
                    offset: 0,
                });
            }
            decode_inner(b, 1, b.len().saturating_sub(1))
        }
    }
}

fn decode_delimited(raw: &str, delim: u8) -> Result<String, StringLitError> {
    let raw_bytes = raw.as_bytes();
    if raw_bytes.len() < 2 || raw_bytes[0] != delim {
        return Err(StringLitError {
            kind: StringLitErrorKind::Unterminated,
            offset: 0,
        });
    }
    if *raw_bytes.last().unwrap_or(&0) != delim {
        return Err(StringLitError {
            kind: StringLitErrorKind::Unterminated,
            offset: raw_bytes.len(),
        });
    }
    decode_inner(raw_bytes, 1, raw_bytes.len().saturating_sub(1))
}

fn decode_inner(raw_bytes: &[u8], start: usize, end: usize) -> Result<String, StringLitError> {
    let mut out = String::new();
    let mut i: usize = start;
    while i < end {
        if raw_bytes.get(i).copied() == Some(b'\\') {
            i += 1;
            if i >= end {
                return Err(StringLitError {
                    kind: StringLitErrorKind::InvalidEscape,
                    offset: i.saturating_sub(1),
                });
            }
            let esc = raw_bytes.get(i).copied().unwrap_or(0);
            match esc {
                b'\\' => out.push('\\'),
                b'"' => out.push('"'),
                b'\'' => out.push('\''),
                b'`' => out.push('`'),
                b'$' => out.push('$'),
                b'n' => out.push('\n'),
                b'r' => out.push('\r'),
                b't' => out.push('\t'),
                b'0' => out.push('\0'),
                b'x' => {
                    let (b, next_i) = decode_hex_u8(raw_bytes, i + 1, end)?;
                    out.push(char::from(b));
                    i = next_i - 1;
                }
                b'u' => {
                    let (ch, next_i) = decode_unicode_scalar(raw_bytes, i + 1, end)?;
                    out.push(ch);
                    i = next_i - 1;
                }
                _ => {
                    return Err(StringLitError {
                        kind: StringLitErrorKind::InvalidEscape,
                        offset: i.saturating_sub(1),
                    });
                }
            }
            i += 1;
            continue;
        }

        let Some(slice) = raw_bytes.get(i..end) else {
            break;
        };
        let Ok(s) = str::from_utf8(slice) else {
            break;
        };
        let Some((ch, size)) = s.chars().next().map(|ch| (ch, ch.len_utf8())) else {
            break;
        };
        out.push(ch);
        i += size;
    }
    Ok(out)
}

fn decode_hex_u8(bytes: &[u8], start: usize, end: usize) -> Result<(u8, usize), StringLitError> {
    if start + 2 > end {
        return Err(StringLitError {
            kind: StringLitErrorKind::MissingHexDigits,
            offset: start.saturating_sub(2),
        });
    }
    let hi = bytes.get(start).copied().unwrap_or(0);
    let lo = bytes.get(start + 1).copied().unwrap_or(0);
    let Some(hi) = hex_value(hi) else {
        return Err(StringLitError {
            kind: StringLitErrorKind::InvalidHexDigit,
            offset: start,
        });
    };
    let Some(lo) = hex_value(lo) else {
        return Err(StringLitError {
            kind: StringLitErrorKind::InvalidHexDigit,
            offset: start + 1,
        });
    };
    Ok(((hi << 4) | lo, start + 2))
}

fn decode_unicode_scalar(
    bytes: &[u8],
    start: usize,
    end: usize,
) -> Result<(char, usize), StringLitError> {
    let (mut value, mut i) = decode_hex_u32(bytes, start, end, 4)?;
    match (bytes.get(i).copied(), bytes.get(i + 1).copied()) {
        (Some(a), Some(b)) if a.is_ascii_hexdigit() && b.is_ascii_hexdigit() => {
            let Some(a) = hex_value(a) else {
                return Err(StringLitError {
                    kind: StringLitErrorKind::InvalidHexDigit,
                    offset: i,
                });
            };
            let Some(b) = hex_value(b) else {
                return Err(StringLitError {
                    kind: StringLitErrorKind::InvalidHexDigit,
                    offset: i + 1,
                });
            };
            value = (value << 8) | u32::from((a << 4) | b);
            i += 2;
        }
        (Some(a), _) if a.is_ascii_hexdigit() => {
            return Err(StringLitError {
                kind: StringLitErrorKind::MissingHexDigits,
                offset: i,
            });
        }
        _ => {}
    }

    let Some(ch) = char::from_u32(value) else {
        return Err(StringLitError {
            kind: StringLitErrorKind::InvalidUnicodeScalar,
            offset: start.saturating_sub(2),
        });
    };
    Ok((ch, i))
}

fn decode_hex_u32(
    bytes: &[u8],
    start: usize,
    end: usize,
    digits: usize,
) -> Result<(u32, usize), StringLitError> {
    if start + digits > end {
        return Err(StringLitError {
            kind: StringLitErrorKind::MissingHexDigits,
            offset: start.saturating_sub(2),
        });
    }
    let mut value: u32 = 0;
    for j in 0..digits {
        let b = bytes.get(start + j).copied().unwrap_or(0);
        let Some(v) = hex_value(b) else {
            return Err(StringLitError {
                kind: StringLitErrorKind::InvalidHexDigit,
                offset: start + j,
            });
        };
        value = (value << 4) | u32::from(v);
    }
    Ok((value, start + digits))
}

const fn hex_value(b: u8) -> Option<u8> {
    Some(match b {
        b'0'..=b'9' => b - b'0',
        b'a'..=b'f' => b - b'a' + 10,
        b'A'..=b'F' => b - b'A' + 10,
        _ => return None,
    })
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
