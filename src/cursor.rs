use crate::location::Location;

pub struct Cursor {
    pub source: Vec<u8>,
    pub position: usize,
    pub location: Location,
}

impl Cursor {
    pub fn new(input: &[u8]) -> Self {
        Self {
            source: Vec::from(input),
            position: 0,
            location: Location::new(),
        }
    }

    #[inline]
    pub fn peek(&self) -> Option<u8> {
        self.source.get(self.position).copied()
    }

    #[inline]
    pub fn peek_next(&self) -> Option<u8> {
        self.source.get(self.position + 1).copied()
    }

    #[inline]
    pub fn advance(&mut self) -> Option<u8> {
        if self.position >= self.source.len() {
            return None;
        }

        let current = self.source[self.position];
        self.position += 1;

        if current != b'\n' {
            self.location.column += 1;
        }
        self.location.offset += 1;

        Some(current)
    }

    #[inline]
    pub fn advance_by(&mut self, offset: usize) -> Option<u8> {
        let mut previous = None;
        for _ in 0..offset {
            previous = self.advance();
        }
        previous
    }

    #[inline]
    pub fn match_2byte(&self, first: u8, second: u8) -> bool {
        self.source.get(self.position) == Some(&first)
            && self.source.get(self.position + 1) == Some(&second)
    }

    #[inline]
    pub fn match_3byte(&self, first: u8, second: u8, third: u8) -> bool {
        self.source.get(self.position) == Some(&first)
            && self.source.get(self.position + 1) == Some(&second)
            && self.source.get(self.position + 2) == Some(&third)
    }
}
