use std::path::Path;

#[derive(Debug, Clone)]
pub struct Source {
    pub filename: String,
    pub lines: Vec<String>,
}

impl Source {
    pub fn new(filename: impl Into<String>, content: &str) -> Self {
        Self {
            filename: filename.into(),
            lines: content.split('\n').map(str::to_string).collect(),
        }
    }

    pub fn path(&self) -> &Path {
        Path::new(&self.filename)
    }

    pub fn line_col(&self, pos: u32) -> (usize, usize) {
        let pos = pos as usize;
        let mut start = 0;

        for (i, line) in self.lines.iter().enumerate() {
            let len = line.len() + 1; // +1 for stripped '\n'
            if start + len > pos {
                return (i + 1, pos - start + 1);
            }
            start += len;
        }

        (self.lines.len(), 1)
    }

    pub fn line_text(&self, line: usize) -> Option<&str> {
        if line > 0 && line <= self.lines.len() {
            Some(&self.lines[line - 1])
        } else {
            None
        }
    }
}
