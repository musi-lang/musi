use crate::common::location::Location;

pub(super) struct Cursor {
    pub(super) source: [u8; 8192],
    pub(super) position: usize,
    pub(super) location: Location,
}

impl Cursor {
    pub(super) fn new(input: &[u8]) -> Self {
        let mut source = [0; 8192];
        source[..input.len()].copy_from_slice(input);

        Self {
            source,
            position: 0,
            location: Location::new(),
        }
    }

    #[inline]
    pub(super) fn peek(&self) -> Option<u8> {
        if self.position < self.source_end() {
            self.source.get(self.position).copied()
        } else {
            None
        }
    }

    #[inline]
    pub(super) fn peek_next(&self) -> Option<u8> {
        if self.position + 1 < self.source_end() {
            self.source.get(self.position + 1).copied()
        } else {
            None
        }
    }

    #[inline]
    pub(super) fn advance(&mut self) -> Option<u8> {
        if self.position >= self.source_end() {
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
    pub(super) fn advance_by(&mut self, offset: usize) -> Option<u8> {
        let mut previous = None;
        for _ in 0..offset {
            previous = self.advance();
            if previous.is_none() {
                break;
            }
        }
        previous
    }

    #[inline]
    pub(super) fn match_2byte(&self, first: u8, second: u8) -> bool {
        self.position < self.source_end() - 1
            && self.source.get(self.position) == Some(&first)
            && self.source.get(self.position + 1) == Some(&second)
    }

    #[inline]
    pub(super) fn match_3byte(&self, first: u8, second: u8, third: u8) -> bool {
        self.position < self.source_end() - 2
            && self.source.get(self.position) == Some(&first)
            && self.source.get(self.position + 1) == Some(&second)
            && self.source.get(self.position + 2) == Some(&third)
    }

    #[inline]
    fn source_end(&self) -> usize {
        self.source[..]
            .iter()
            .position(|&x| x == 0)
            .unwrap_or(self.source.len())
    }
}
