use crate::basic::diagnostic::{DiagnosticBag, report};
use crate::basic::errors::{Error, LexErrorKind};
use crate::basic::span::Span;

const ESCAPES: &[(char, char)] = &[
    ('0', '\0'),
    ('"', '\"'),
    ('\'', '\''),
    ('\\', '\\'),
    ('a', '\x07'),
    ('b', '\x08'),
    ('e', '\x1b'),
    ('f', '\x0c'),
    ('n', '\n'),
    ('r', '\r'),
    ('t', '\t'),
    ('v', '\x0b'),
];

pub fn unescape(s: &str, start_pos: usize, errors: &mut DiagnosticBag) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    let mut offset = 0;

    while let Some(c) = chars.next() {
        if c == '\\' {
            match scan_escape(&mut chars) {
                Ok((esc, len)) => {
                    out.push(esc);
                    offset += 1 + len;
                }
                Err((esc_err, len)) => {
                    let err_span = Span::new(
                        (start_pos + offset) as u32,
                        (start_pos + offset + 1 + len) as u32,
                    );
                    errors.add(report(Error::new(
                        LexErrorKind::UnknownEscape(esc_err).into(),
                        err_span,
                    )));
                    offset += 1 + len;
                }
            }
        } else {
            out.push(c);
            offset += c.len_utf8();
        }
    }
    out
}

pub fn scan_escape(chars: &mut std::str::Chars<'_>) -> Result<(char, usize), (String, usize)> {
    let c = match chars.next() {
        Some(c) => c,
        None => return Ok(('\0', 0)),
    };

    if let Ok(i) = ESCAPES.binary_search_by_key(&c, |e| e.0) {
        return Ok((ESCAPES[i].1, 1));
    }

    match c {
        'x' => {
            let s: String = chars.clone().take(2).collect();
            if s.len() < 2 {
                return Err((format!("x{s}"), 1 + s.len()));
            }

            let _ = chars.next();
            let _ = chars.next();

            match u32::from_str_radix(&s, 16).ok().and_then(char::from_u32) {
                Some(c) => Ok((c, 3)),
                None => Err((format!("x{s}"), 3)),
            }
        }
        'u' => {
            if chars.clone().next() == Some('{') {
                let _ = chars.next();
                let mut s = String::new();
                let mut len = 0;
                while let Some(c) = chars.next() {
                    if c == '}' {
                        match u32::from_str_radix(&s, 16).ok().and_then(char::from_u32) {
                            Some(c) => return Ok((c, s.len() + 3)),
                            None => return Err((format!("u{{{s}}}"), s.len() + 3)),
                        }
                    }
                    s.push(c);
                    len += c.len_utf8();
                    if s.len() > 6 {
                        break;
                    }
                }
                // len used here for error span calcing
                Err((format!("u{{{s}}}"), len + 2))
            } else {
                Err(("u".to_string(), 1))
            }
        }
        _ => Err((c.to_string(), 1)),
    }
}
