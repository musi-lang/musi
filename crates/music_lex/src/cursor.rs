use music_shared::Span;

/// Length of a UTF-8 character from its first byte, without decoding.
const fn char_len_utf8(first: u8) -> usize {
    match first.leading_ones() {
        2 => 2,
        3 => 3,
        4 => 4,
        _ => 1, // ASCII (0 leading ones) or invalid lead byte
    }
}

pub struct Cursor<'src> {
    src: &'src str,
    pos: usize,
}

impl<'src> Cursor<'src> {
    pub const fn new(src: &'src str) -> Self {
        Self { src, pos: 0 }
    }

    pub fn pos(&self) -> u32 {
        u32::try_from(self.pos).expect("source exceeds u32::MAX bytes")
    }

    pub const fn src_len(&self) -> usize {
        self.src.len()
    }

    pub const fn is_eof(&self) -> bool {
        self.pos >= self.src.len()
    }

    pub fn remaining(&self) -> &[u8] {
        self.src.as_bytes().get(self.pos..).unwrap_or_default()
    }

    pub fn peek_byte(&self) -> Option<u8> {
        self.src.as_bytes().get(self.pos).copied()
    }

    pub fn peek(&self) -> Option<char> {
        let b = *self.src.as_bytes().get(self.pos)?;
        if b.is_ascii() {
            Some(char::from(b))
        } else {
            self.src.get(self.pos..)?.chars().next()
        }
    }

    pub fn peek_next(&self) -> Option<char> {
        let bytes = self.src.as_bytes();
        let first = *bytes.get(self.pos)?;
        let next_pos = if first.is_ascii() {
            self.pos + 1
        } else {
            self.pos + char_len_utf8(first)
        };
        let second = *bytes.get(next_pos)?;
        if second.is_ascii() {
            Some(char::from(second))
        } else {
            self.src.get(next_pos..)?.chars().next()
        }
    }

    pub fn advance(&mut self) -> Option<char> {
        let bytes = self.src.as_bytes();
        let b = *bytes.get(self.pos)?;
        if b.is_ascii() {
            self.pos += 1;
            Some(char::from(b))
        } else {
            let ch = self.src.get(self.pos..)?.chars().next()?;
            self.pos += ch.len_utf8();
            Some(ch)
        }
    }

    pub const fn advance_by(&mut self, n: usize) {
        self.pos += n;
    }

    pub fn eat(&mut self, ch: char) -> bool {
        if self.peek() == Some(ch) {
            let _ = self.advance();
            true
        } else {
            false
        }
    }

    pub fn eat_while(&mut self, pred: impl Fn(char) -> bool) -> &'src str {
        let start = self.pos;
        while let Some(ch) = self.peek() {
            if !pred(ch) {
                break;
            }
            let _ = self.advance();
        }
        self.src
            .get(start..self.pos)
            .expect("eat_while boundaries are valid UTF-8")
    }

    /// Fast byte-level scanning for ASCII-only predicates.
    /// Stops at non-ASCII bytes or when the predicate returns false.
    /// Returns the number of bytes consumed.
    pub fn eat_while_ascii(&mut self, predicate: impl Fn(u8) -> bool) -> usize {
        let start = self.pos;
        let bytes = self.src.as_bytes();
        while self.pos < bytes.len() && bytes[self.pos].is_ascii() && predicate(bytes[self.pos]) {
            self.pos += 1;
        }
        self.pos - start
    }

    pub fn slice(&self, start: u32) -> &'src str {
        let s = usize::try_from(start).expect("start fits in usize");
        self.src
            .get(s..self.pos)
            .expect("slice boundaries are valid UTF-8")
    }

    pub fn span_from(&self, start: u32) -> Span {
        Span::new(start, self.pos())
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
