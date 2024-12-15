#[derive(Clone, Debug)]
pub struct Source {
    pub name: Box<str>,
    pub content: Vec<u8>,
    pub line_starts: Vec<usize>,
}

#[derive(Clone, Copy, Debug)]
pub struct Position {
    pub offset: usize,
    pub line: u32,
    pub column: u32,
}

impl Source {
    #[must_use]
    pub fn new(name: &str, content: Vec<u8>) -> Self {
        let mut line_starts = vec![0];
        for (offset, &current) in content.iter().enumerate() {
            if current == b'\n' {
                line_starts.push(offset + 1);
            }
        }

        Self {
            name: format!("{name}.musi").into(),
            content,
            line_starts,
        }
    }

    #[must_use]
    pub fn position(&self, offset: usize) -> Position {
        let mut line = 1;
        let mut last_line_start = 0;

        for (position, current) in self.content[..offset].iter().enumerate() {
            if *current == b'\n' {
                line += 1;
                last_line_start = position + 1;
            }
        }

        Position {
            offset,
            line,
            column: u32::try_from(offset - last_line_start + 1).unwrap_or(u32::MAX),
        }
    }
}
