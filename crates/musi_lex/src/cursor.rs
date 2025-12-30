use std::str::Chars;

#[derive(Debug, Clone)]
/// Iterator-like cursor over string slice input.
pub struct Cursor<'a> {
    chars: Chars<'a>,
    len_remaining: usize,
    initial_len: usize,
}

impl<'a> Cursor<'a> {
    #[must_use]
    /// Creates new cursor from input string.
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars(),
            len_remaining: input.len(),
            initial_len: input.len(),
        }
    }

    #[must_use]
    /// Returns current position.
    pub const fn pos(&self) -> usize {
        self.initial_len - self.len_remaining
    }

    #[must_use]
    /// Returns next character without advancing.
    pub fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    #[must_use]
    /// Returns nth character ahead.
    pub fn peek_nth(&self, n: usize) -> Option<char> {
        self.chars.clone().nth(n)
    }

    #[must_use]
    /// Checks if next character matches expected.
    pub fn is_next(&self, c: char) -> bool {
        self.chars.clone().next() == Some(c)
    }

    /// Consumes and returns next character.
    pub fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.len_remaining -= c.len_utf8();
        Some(c)
    }

    /// Consumes n characters.
    pub fn bump_n(&mut self, n: usize) {
        for _ in 0..n {
            let _: Option<char> = self.bump();
        }
    }

    /// Consumes characters matching predicate.
    pub fn eat_while(&mut self, mut pred: impl FnMut(char) -> bool) {
        while let Some(c) = self.peek() {
            if pred(c) {
                let _: Option<char> = self.bump();
            } else {
                break;
            }
        }
    }

    #[must_use]
    /// Returns remaining input as string slice.
    pub fn rest(&self) -> &'a str {
        self.chars.as_str()
    }
}
