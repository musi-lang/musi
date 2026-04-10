#[derive(Debug, Clone, Copy)]
pub struct Cursor<'src> {
    text: &'src str,
    pos: usize,
}

impl<'src> Cursor<'src> {
    #[must_use]
    pub const fn new(text: &'src str) -> Self {
        Self { text, pos: 0 }
    }

    #[must_use]
    pub const fn text(&self) -> &'src str {
        self.text
    }

    #[must_use]
    pub const fn pos(&self) -> usize {
        self.pos
    }

    #[must_use]
    pub const fn checkpoint(&self) -> usize {
        self.pos
    }

    pub fn reset(&mut self, pos: usize) {
        self.pos = pos.min(self.text.len());
        debug_assert!(self.text.is_char_boundary(self.pos));
    }

    #[must_use]
    pub const fn is_eof(&self) -> bool {
        self.pos >= self.text.len()
    }

    #[must_use]
    pub fn slice(&self) -> &'src str {
        debug_assert!(self.text.is_char_boundary(self.pos));
        self.text.get(self.pos..).unwrap_or("")
    }

    #[must_use]
    pub fn peek_char(&self) -> Option<char> {
        self.slice().chars().next()
    }

    #[must_use]
    pub fn peek_byte(&self) -> Option<u8> {
        self.text.as_bytes().get(self.pos).copied()
    }

    #[must_use]
    pub fn peek_byte_n(&self, n: usize) -> Option<u8> {
        self.text.as_bytes().get(self.pos + n).copied()
    }

    pub fn bump_bytes(&mut self, n: usize) {
        self.pos = (self.pos + n).min(self.text.len());
    }

    pub fn bump_char(&mut self) -> Option<char> {
        let c = self.peek_char()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    pub fn bump(&mut self) {
        let _ = self.bump_char();
    }

    #[must_use]
    pub fn starts_with_bytes(&self, bytes: &[u8]) -> bool {
        let remaining = self.text.len().saturating_sub(self.pos);
        if remaining < bytes.len() {
            return false;
        }
        &self.text.as_bytes()[self.pos..self.pos + bytes.len()] == bytes
    }

    pub fn consume_while(&mut self, mut keep_going: impl FnMut(char) -> bool) {
        while let Some(c) = self.peek_char() {
            if !keep_going(c) {
                break;
            }
            self.bump();
        }
    }

    pub fn consume_while_byte(&mut self, mut keep_going: impl FnMut(u8) -> bool) {
        while let Some(b) = self.peek_byte() {
            if !keep_going(b) {
                break;
            }
            self.bump_bytes(1);
        }
    }
}
