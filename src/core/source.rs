#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct NamedSource {
    pub file_path: Box<str>,
    pub bytes: Vec<u8>,
    pub line_offsets: Vec<usize>,
}

#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct Position {
    pub offset: usize,
    pub line: u32,
    pub column: u32,
}

impl NamedSource {
    #[inline]
    #[must_use]
    pub fn new(file_name: &str, bytes: Vec<u8>) -> Self {
        let mut line_offsets = vec![0];
        for (offset, &current_byte) in bytes.iter().enumerate() {
            if current_byte == b'\n' {
                line_offsets.push(offset.saturating_add(1));
            }
        }

        Self {
            file_path: format!("{file_name}.musi").into(),
            bytes,
            line_offsets,
        }
    }

    #[inline]
    #[must_use]
    pub fn position(&self, offset: usize) -> Position {
        let mut line = 1_u32;
        let mut last_line_start = 0;
        for (index, byte) in self.bytes.get(..offset).unwrap_or(&[]).iter().enumerate() {
            if *byte == b'\n' {
                line = line.saturating_add(1);
                last_line_start = index.saturating_add(1);
            }
        }

        Position {
            offset,
            line,
            column: u32::try_from(offset.saturating_sub(last_line_start).saturating_add(1))
                .unwrap_or(u32::MAX),
        }
    }
}
