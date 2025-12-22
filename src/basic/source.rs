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

    pub fn location_at(&self, target: u32) -> (usize, usize) {
        let mut offset = 0;
        let target = target as usize;

        for (idx, line) in self.lines.iter().enumerate() {
            let len = line.len() + 1; // for stripped '\n'
            if offset + len > target {
                return (idx + 1, target - offset + 1);
            }
            offset += len;
        }

        (self.lines.len(), 1)
    }

    pub fn line_at_opt(&self, idx: usize) -> Option<&str> {
        if idx > 0 && idx <= self.lines.len() {
            Some(&self.lines[idx - 1])
        } else {
            None
        }
    }

    pub fn text(&self) -> String {
        self.lines.join("\n")
    }
}
