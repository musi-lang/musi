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

    #[inline(always)]
    pub fn peek(&self) -> Option<u8> {
        self.source.get(self.position).copied()
    }

    #[inline(always)]
    pub fn peek_next(&self) -> Option<u8> {
        self.source.get(self.position + 1).copied()
    }

    #[inline(always)]
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

    #[inline(always)]
    pub fn advance_by(&mut self, offset: usize) -> Option<u8> {
        let mut previous = None;
        for _ in 0..offset {
            previous = self.advance();
        }
        previous
    }

    #[inline(always)]
    pub fn match_sequence(&mut self, bytes: &[u8]) -> bool {
        let mut start_position = self.position;
        for &expected in bytes {
            match self.source.get(start_position) {
                Some(&current) if current == expected => start_position += 1,
                _ => return false,
            }
        }
        start_position > self.position
    }
}
