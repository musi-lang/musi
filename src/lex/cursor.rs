use std::str::Chars;

#[derive(Debug, Clone)]
pub struct Cursor<'a> {
    chars: Chars<'a>,
    len_remaining: usize,
    initial_len: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars(),
            len_remaining: input.len(),
            initial_len: input.len(),
        }
    }

    pub const fn pos(&self) -> usize {
        self.initial_len - self.len_remaining
    }

    pub fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn peek_nth(&self, n: usize) -> Option<char> {
        self.chars.clone().nth(n)
    }

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
            let _ = self.bump();
        }
    }

    pub fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while let Some(c) = self.peek() {
            if predicate(c) {
                let _ = self.bump();
            } else {
                break;
            }
        }
    }

    pub fn rest(&self) -> &'a str {
        self.chars.as_str()
    }
}
