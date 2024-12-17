#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct SourceFile {
    pub name: Box<str>,
    pub content: Vec<u8>,
    pub line_starts: Vec<usize>,
}

#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct SourcePosition {
    pub absolute_offset: usize,
    pub line: u32,
    pub column: u32,
}

impl SourceFile {
    #[inline]
    #[must_use]
    pub fn new(name: &str, bytes: Vec<u8>) -> Self {
        let mut line_starts = vec![0];
        for (index, &byte) in bytes.iter().enumerate() {
            if byte == b'\n' {
                line_starts.push(index.saturating_add(1));
            }
        }

        Self {
            name: name.into(),
            content: bytes,
            line_starts,
        }
    }

    #[inline]
    #[must_use]
    pub fn position(&self, offset: usize) -> SourcePosition {
        let mut line = 1_u32;
        let mut last_line_start = 0;
        for (index, byte) in self.content.get(..offset).unwrap_or(&[]).iter().enumerate() {
            if *byte == b'\n' {
                line = line.saturating_add(1);
                last_line_start = index.saturating_add(1);
            }
        }

        SourcePosition {
            absolute_offset: offset,
            line,
            column: u32::try_from(offset.saturating_sub(last_line_start).saturating_add(1))
                .unwrap_or(u32::MAX),
        }
    }
}
