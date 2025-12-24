use std::str::Chars;

#[derive(Debug, Clone)]
pub struct Cursor<'a> {
    chars: Chars<'a>,
    len_remaining: usize,
    initial_len: usize,
}

impl<'a> Cursor<'a> {
    #[must_use]
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars(),
            len_remaining: input.len(),
            initial_len: input.len(),
        }
    }

    #[must_use]
    pub const fn pos(&self) -> usize {
        self.initial_len - self.len_remaining
    }

    #[must_use]
    pub fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    #[must_use]
    pub fn peek_nth(&self, n: usize) -> Option<char> {
        self.chars.clone().nth(n)
    }

    #[must_use]
    pub fn is_next(&self, c: char) -> bool {
        self.chars.clone().next() == Some(c)
    }

    pub fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.len_remaining -= c.len_utf8();
        Some(c)
    }

    pub fn bump_n(&mut self, n: usize) {
        for _ in 0..n {
            let _: Option<char> = self.bump();
        }
    }

    pub fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while let Some(c) = self.peek() {
            if predicate(c) {
                let _: Option<char> = self.bump();
            } else {
                break;
            }
        }
    }

    #[must_use]
    pub fn rest(&self) -> &'a str {
        self.chars.as_str()
    }
}
