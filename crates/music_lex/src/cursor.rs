use music_found::Span;

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

    pub const fn is_eof(&self) -> bool {
        self.pos >= self.src.len()
    }

    pub fn remaining(&self) -> &[u8] {
        self.src.as_bytes().get(self.pos..).unwrap_or_default()
    }

    pub fn peek(&self) -> Option<char> {
        self.src.get(self.pos..)?.chars().next()
    }

    pub fn peek_next(&self) -> Option<char> {
        let mut chars = self.src.get(self.pos..)?.chars();
        let _ = chars.next()?;
        chars.next()
    }

    pub fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += ch.len_utf8();
        Some(ch)
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
