use music_found::Span;

pub struct Cursor<'src> {
    src: &'src str,
    pos: u32,
}

impl<'src> Cursor<'src> {
    pub const fn new(src: &'src str) -> Self {
        Self { src, pos: 0 }
    }

    pub const fn pos(&self) -> u32 {
        self.pos
    }

    pub fn is_eof(&self) -> bool {
        usize::try_from(self.pos).expect("pos fits in usize") >= self.src.len()
    }

    pub fn remaining(&self) -> &[u8] {
        let pos = usize::try_from(self.pos).expect("pos fits in usize");
        self.src.as_bytes().get(pos..).unwrap_or_default()
    }

    pub fn peek(&self) -> Option<char> {
        self.src
            .get(usize::try_from(self.pos).expect("pos fits in usize")..)
            .and_then(|s| s.chars().next())
    }

    pub fn peek_next(&self) -> Option<char> {
        let pos = usize::try_from(self.pos).expect("pos fits in usize");
        let mut chars = self.src.get(pos..)?.chars();
        let _ = chars.next()?;
        chars.next()
    }

    pub fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += u32::try_from(ch.len_utf8()).expect("char len fits in u32");
        Some(ch)
    }

    pub fn advance_by(&mut self, n: usize) {
        self.pos += u32::try_from(n).expect("advance_by amount fits in u32");
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
        self.slice(start)
    }

    pub fn slice(&self, start: u32) -> &'src str {
        let s = usize::try_from(start).expect("start fits in usize");
        let e = usize::try_from(self.pos).expect("pos fits in usize");
        self.src
            .get(s..e)
            .expect("slice boundaries are valid UTF-8")
    }

    pub fn span_from(&self, start: u32) -> Span {
        Span::new(start, self.pos)
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
