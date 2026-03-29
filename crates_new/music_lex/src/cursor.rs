use music_basic::Span;

const fn char_len_utf8(first: u8) -> usize {
    match first.leading_ones() {
        2 => 2,
        3 => 3,
        4 => 4,
        _ => 1,
    }
}

pub struct Cursor<'src> {
    source: &'src str,
    position: usize,
}

impl<'src> Cursor<'src> {
    #[must_use]
    pub const fn new(source: &'src str) -> Self {
        Self {
            source,
            position: 0,
        }
    }

    #[must_use]
    ///
    /// # Panics
    ///
    /// Panics if the source offset exceeds `u32::MAX`.
    pub fn pos(&self) -> u32 {
        u32::try_from(self.position).expect("source exceeds u32::MAX bytes")
    }

    #[must_use]
    pub const fn source_len(&self) -> usize {
        self.source.len()
    }

    #[must_use]
    pub const fn is_eof(&self) -> bool {
        self.position >= self.source.len()
    }

    #[must_use]
    pub fn remaining_bytes(&self) -> &[u8] {
        self.source
            .as_bytes()
            .get(self.position..)
            .unwrap_or_default()
    }

    #[must_use]
    pub fn peek_byte(&self) -> Option<u8> {
        self.source.as_bytes().get(self.position).copied()
    }

    #[must_use]
    pub fn peek(&self) -> Option<char> {
        let byte = *self.source.as_bytes().get(self.position)?;
        if byte.is_ascii() {
            return Some(char::from(byte));
        }
        self.source.get(self.position..)?.chars().next()
    }

    #[must_use]
    pub fn peek_next(&self) -> Option<char> {
        let bytes = self.source.as_bytes();
        let first = *bytes.get(self.position)?;
        let next_position = if first.is_ascii() {
            self.position + 1
        } else {
            self.position + char_len_utf8(first)
        };
        let second = *bytes.get(next_position)?;
        if second.is_ascii() {
            return Some(char::from(second));
        }
        self.source.get(next_position..)?.chars().next()
    }

    pub fn advance(&mut self) -> Option<char> {
        let byte = *self.source.as_bytes().get(self.position)?;
        if byte.is_ascii() {
            self.position += 1;
            return Some(char::from(byte));
        }

        let ch = self.source.get(self.position..)?.chars().next()?;
        self.position += ch.len_utf8();
        Some(ch)
    }

    pub const fn advance_by(&mut self, count: usize) {
        self.position += count;
    }

    pub fn eat(&mut self, ch: char) -> bool {
        if self.peek() == Some(ch) {
            let _ = self.advance();
            return true;
        }
        false
    }

    /// Consume characters while `predicate` holds and return the consumed text.
    ///
    /// # Panics
    ///
    /// Panics if the internal cursor leaves UTF-8 character boundaries.
    pub fn eat_while(&mut self, predicate: impl Fn(char) -> bool) -> &'src str {
        let start = self.position;
        while let Some(ch) = self.peek() {
            if !predicate(ch) {
                break;
            }
            let _ = self.advance();
        }
        self.source
            .get(start..self.position)
            .expect("cursor boundaries stay on UTF-8 edges")
    }

    pub fn eat_while_ascii(&mut self, predicate: impl Fn(u8) -> bool) -> usize {
        let start = self.position;
        let bytes = self.source.as_bytes();
        while self.position < bytes.len()
            && bytes[self.position].is_ascii()
            && predicate(bytes[self.position])
        {
            self.position += 1;
        }
        self.position - start
    }

    #[must_use]
    ///
    /// # Panics
    ///
    /// Panics if `start` does not fit in `usize` or does not lie on a valid
    /// UTF-8 boundary within the source.
    pub fn slice(&self, start: u32) -> &'src str {
        let start = usize::try_from(start).expect("span offset fits in usize");
        self.source
            .get(start..self.position)
            .expect("cursor boundaries stay on UTF-8 edges")
    }

    #[must_use]
    ///
    /// # Panics
    ///
    /// Panics if `span` does not fit in `usize` or does not lie on valid
    /// UTF-8 boundaries within the source.
    pub fn slice_span(&self, span: Span) -> &'src str {
        let start = usize::try_from(span.start).expect("span start fits in usize");
        let end = usize::try_from(span.end).expect("span end fits in usize");
        self.source
            .get(start..end)
            .expect("span boundaries stay on UTF-8 edges")
    }

    #[must_use]
    pub fn span_from(&self, start: u32) -> Span {
        Span::new(start, self.pos())
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
