use std::cmp::Ordering;
use std::sync::Arc;

#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct SourceFile {
    pub name: String,
    pub input: String,
    pub start: u32,
    pub line_starts: Vec<u32>,
}

impl SourceFile {
    /// Creates new source file with computed line start positions.
    ///
    /// # Panics
    ///
    /// Panics if input file is larger than `u32::MAX` bytes.
    #[must_use]
    pub fn new(name: String, input: String, start: u32) -> Self {
        let mut line_starts = vec![0];
        for (i, c) in input.char_indices() {
            if c == '\n' {
                let pos = u32::try_from(i + 1).expect("file too large");
                line_starts.push(pos);
            }
        }
        Self {
            name,
            input,
            start,
            line_starts,
        }
    }

    /// Returns byte offset just past end of this file.
    ///
    /// # Panics
    ///
    /// Panics if file length exceeds `u32::MAX`.
    #[must_use]
    pub fn end_pos(&self) -> u32 {
        self.start + u32::try_from(self.input.len()).expect("file too large")
    }

    #[must_use]
    pub fn contains(&self, offset: u32) -> bool {
        offset >= self.start && offset < self.end_pos()
    }

    #[must_use]
    pub fn line_index(&self, offset: u32) -> usize {
        let rel = offset - self.start;
        match self.line_starts.binary_search(&rel) {
            Ok(idx) => idx,
            Err(idx) => idx - 1,
        }
    }

    #[must_use]
    pub fn location_at(&self, offset: u32) -> (usize, usize) {
        let rel = offset - self.start;
        let line_idx = self.line_index(offset);
        let line_start = self.line_starts[line_idx];
        let col = rel - line_start;
        (line_idx + 1, usize::try_from(col).unwrap_or(0) + 1)
    }

    #[must_use]
    pub fn line_at(&self, line_idx: usize) -> Option<&str> {
        if line_idx >= self.line_starts.len() {
            return None;
        }

        let start = usize::try_from(self.line_starts[line_idx]).ok()?;
        let end = if line_idx + 1 < self.line_starts.len() {
            usize::try_from(self.line_starts[line_idx + 1]).ok()?
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
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds source file and returns its ID.
    ///
    /// # Panics
    ///
    /// Panics if more than `u32::MAX` files are added.
    pub fn add_file(&mut self, name: String, src: String) -> u32 {
        let start_pos = self.files.last().map_or(0, |f| f.end_pos());
        let file = Arc::new(SourceFile::new(name, src, start_pos));
        let id = u32::try_from(self.files.len()).expect("too many source files");
        self.files.push(file);
        id
    }

    #[must_use]
    pub fn lookup_source(&self, offset: u32) -> Option<&Arc<SourceFile>> {
        let idx = self.files.binary_search_by(|f| {
            if offset < f.start {
                Ordering::Greater
            } else if offset >= f.end_pos() {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        });
        idx.ok().map(|i| &self.files[i])
    }

    #[must_use]
    pub fn get(&self, id: u32) -> Option<&Arc<SourceFile>> {
        self.files.get(usize::try_from(id).ok()?)
    }
}
