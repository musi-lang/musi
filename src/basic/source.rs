use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub name: String,
    pub input: String,
    pub start: u32,
    pub line_starts: Vec<u32>,
}

impl SourceFile {
    pub fn new(name: String, input: String, start: u32) -> Self {
        let mut line_starts = vec![0];
        for (i, c) in input.char_indices() {
            if c == '\n' {
                line_starts.push((i + 1) as u32);
            }
        }
        Self {
            name,
            input,
            start,
            line_starts,
        }
    }

    pub const fn end_pos(&self) -> u32 {
        self.start + self.input.len() as u32
    }

    pub const fn contains(&self, offset: u32) -> bool {
        offset >= self.start && offset < self.end_pos()
    }

    pub fn line_index(&self, offset: u32) -> usize {
        let rel = offset - self.start;
        match self.line_starts.binary_search(&rel) {
            Ok(idx) => idx,
            Err(idx) => idx - 1,
        }
    }

    pub fn location_at(&self, offset: u32) -> (usize, usize) {
        let rel = offset - self.start;
        let line_idx = self.line_index(offset);
        let line_start = self.line_starts[line_idx];
        let col = rel - line_start;
        (line_idx + 1, col as usize + 1)
    }

    pub fn line_at(&self, line_idx: usize) -> Option<&str> {
        if line_idx >= self.line_starts.len() {
            return None;
        }

        let start = self.line_starts[line_idx] as usize;
        let end = if line_idx + 1 < self.line_starts.len() {
            self.line_starts[line_idx + 1] as usize
        } else {
            self.input.len()
        };
        if end <= start {
            return Some("");
        }

        let end_excl_newline = end - 1;
        self.input.get(start..end_excl_newline)
    }
}

#[derive(Debug, Default)]
pub struct SourceMap {
    files: Vec<Arc<SourceFile>>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_file(&mut self, name: String, src: String) -> u32 {
        let start_pos = self.files.last().map(|f| f.end_pos()).unwrap_or(0);
        let file = Arc::new(SourceFile::new(name, src, start_pos));
        let id = self.files.len() as u32;
        self.files.push(file);
        id
    }

    pub fn lookup_source(&self, offset: u32) -> Option<&Arc<SourceFile>> {
        let idx = self.files.binary_search_by(|f| {
            if offset < f.start {
                std::cmp::Ordering::Greater
            } else if offset >= f.end_pos() {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Equal
            }
        });

        match idx {
            Ok(i) => Some(&self.files[i]),
            Err(_) => None,
        }
    }

    pub fn get(&self, id: u32) -> Option<&Arc<SourceFile>> {
        self.files.get(id as usize)
    }
}
