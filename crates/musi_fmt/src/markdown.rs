use crate::{FormatOptions, FormatResult, FormatResultOf, format_source};

const MUSI_FENCE_TAGS: &[&str] = &["musi", "ms", "music"];

/// Formats Musi code fences in Markdown while preserving prose and non-Musi fences.
///
/// # Errors
///
/// Returns formatter errors from Musi source blocks.
pub fn format_markdown(source: &str, options: &FormatOptions) -> FormatResultOf {
    if has_markdown_ignore_file(source) {
        let text = ensure_final_newline(source);
        return Ok(FormatResult {
            changed: text != source,
            text,
        });
    }

    let mut formatter = MarkdownFormatter::new(source, options);
    formatter.format()?;
    Ok(formatter.finish())
}

struct MarkdownFormatter<'a> {
    source: &'a str,
    options: &'a FormatOptions,
    out: String,
    offset: usize,
    ignore_range: bool,
    ignore_next_fence: bool,
}

impl<'a> MarkdownFormatter<'a> {
    const fn new(source: &'a str, options: &'a FormatOptions) -> Self {
        Self {
            source,
            options,
            out: String::new(),
            offset: 0,
            ignore_range: false,
            ignore_next_fence: false,
        }
    }

    fn format(&mut self) -> FormatResultOf<()> {
        while self.offset < self.source.len() {
            let line_start = self.offset;
            let line = self.next_line();
            if line.contains("musi-fmt-ignore-start") {
                self.ignore_range = true;
            }
            if line.contains("musi-fmt-ignore")
                && !line.contains("musi-fmt-ignore-file")
                && !line.contains("musi-fmt-ignore-start")
                && !line.contains("musi-fmt-ignore-end")
            {
                self.ignore_next_fence = true;
            }
            let Some(fence) = Fence::parse(line) else {
                self.out.push_str(line);
                if line.contains("musi-fmt-ignore-end") {
                    self.ignore_range = false;
                }
                continue;
            };
            if self.ignore_range || !fence.is_musi() {
                self.copy_fence_block(line_start, &fence);
                continue;
            }
            if self.ignore_next_fence {
                self.ignore_next_fence = false;
                self.copy_fence_block(line_start, &fence);
                continue;
            }
            self.format_fence_block(line, &fence)?;
        }
        Ok(())
    }

    fn finish(mut self) -> FormatResult {
        if !self.out.ends_with('\n') {
            self.out.push('\n');
        }
        FormatResult {
            changed: self.out != self.source,
            text: self.out,
        }
    }

    fn next_line(&mut self) -> &'a str {
        let Some(rest) = self.source.get(self.offset..) else {
            return "";
        };
        let end = rest
            .find('\n')
            .map_or(self.source.len(), |index| self.offset + index + 1);
        let start = self.offset;
        self.offset = end;
        self.source.get(start..end).unwrap_or_default()
    }

    fn copy_fence_block(&mut self, line_start: usize, fence: &Fence) {
        while self.offset < self.source.len() {
            let line = self.next_line();
            if fence.is_closing(line) {
                break;
            }
            if line.contains("musi-fmt-ignore-end") {
                self.ignore_range = false;
            }
        }
        if let Some(block) = self.source.get(line_start..self.offset) {
            self.out.push_str(block);
        }
    }

    fn format_fence_block(&mut self, opening_line: &str, fence: &Fence) -> FormatResultOf<()> {
        self.out.push_str(opening_line);
        let mut body = String::new();
        while self.offset < self.source.len() {
            let line = self.next_line();
            if fence.is_closing(line) {
                let formatted = format_source(&body, self.options)?;
                self.out.push_str(&formatted.text);
                self.out.push_str(line);
                return Ok(());
            }
            body.push_str(line);
        }
        self.out.push_str(&body);
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
struct Fence<'a> {
    marker: char,
    marker_len: usize,
    tag: &'a str,
}

impl<'a> Fence<'a> {
    fn parse(line: &'a str) -> Option<Self> {
        let trimmed = line.trim_start();
        let marker = trimmed.chars().next()?;
        if marker != '`' && marker != '~' {
            return None;
        }
        let marker_len = trimmed.chars().take_while(|char| *char == marker).count();
        if marker_len < 3 {
            return None;
        }
        let tag = fence_tag(trimmed.trim_start_matches(marker).trim());
        Some(Self {
            marker,
            marker_len,
            tag,
        })
    }

    fn is_closing(self, line: &str) -> bool {
        let trimmed = line.trim_start();
        let marker_len = trimmed
            .chars()
            .take_while(|char| *char == self.marker)
            .count();
        if marker_len < self.marker_len {
            return false;
        }
        trimmed
            .get(marker_len..)
            .is_some_and(|rest| rest.trim().is_empty())
    }

    fn is_musi(self) -> bool {
        MUSI_FENCE_TAGS
            .iter()
            .any(|tag| self.tag.eq_ignore_ascii_case(tag))
    }
}

fn fence_tag(info: &str) -> &str {
    if let Some(attributes) = info.strip_prefix('{') {
        return attributes
            .trim_end_matches('}')
            .split(|char: char| char.is_whitespace())
            .find_map(|attribute| attribute.strip_prefix('.'))
            .unwrap_or_default();
    }
    info.split_whitespace().next().unwrap_or_default()
}

fn has_markdown_ignore_file(source: &str) -> bool {
    source
        .lines()
        .take(5)
        .any(|line| line.contains("musi-fmt-ignore-file"))
}

fn ensure_final_newline(source: &str) -> String {
    let mut text = source.trim_end_matches(['\r', '\n']).to_owned();
    text.push('\n');
    text
}
