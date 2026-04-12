use std::collections::BTreeMap;
use std::path::PathBuf;

use music_base::Span;
use music_base::diag::DiagCode;

use crate::errors::{ProjectError, ProjectSourceDiagnostic, ProjectSourceLabel};

type ManifestSpanMap = BTreeMap<String, Span>;

#[derive(Debug, Clone)]
pub struct ManifestSource {
    path: PathBuf,
    text: String,
    root_span: Span,
    key_spans: ManifestSpanMap,
    value_spans: ManifestSpanMap,
}

impl ManifestSource {
    pub fn from_text(path: impl Into<PathBuf>, text: String) -> Self {
        let path = path.into();
        let fallback_end = u32::try_from(text.len()).unwrap_or(u32::MAX);
        let fallback_span = Span::new(0, fallback_end);
        let Some(index) = JsonSpanIndexer::build(&text) else {
            return Self {
                path,
                text,
                root_span: fallback_span,
                key_spans: BTreeMap::new(),
                value_spans: BTreeMap::new(),
            };
        };
        Self {
            path,
            text,
            root_span: index.root_span,
            key_spans: index.key_spans,
            value_spans: index.value_spans,
        }
    }

    #[must_use]
    pub fn key_span(&self, pointer: &str) -> Option<Span> {
        self.key_spans.get(pointer).copied()
    }

    #[must_use]
    pub fn value_span(&self, pointer: &str) -> Option<Span> {
        self.value_spans.get(pointer).copied()
    }

    #[must_use]
    pub fn insertion_span(&self) -> Span {
        let insert = self
            .root_span
            .start
            .saturating_add(1)
            .min(self.root_span.end);
        Span::new(insert, insert)
    }

    pub fn parse_error(&self, error: &serde_json::Error) -> ProjectError {
        let offset = offset_for_error(&self.text, error.line(), error.column());
        let span = span_at_offset(&self.text, offset);
        self.error(
            DiagCode::new(3604),
            "manifest JSON invalid",
            span,
            "JSON parse error",
        )
    }

    pub fn error(
        &self,
        code: DiagCode,
        message: impl Into<String>,
        span: Span,
        label: impl Into<String>,
    ) -> ProjectError {
        ProjectError::ManifestSourceDiagnostic(Box::new(ProjectSourceDiagnostic::new(
            self.path.clone(),
            self.text.clone(),
            code,
            message,
            ProjectSourceLabel::new(span, label),
        )))
    }

    pub fn error_with_hint(
        &self,
        code: DiagCode,
        message: impl Into<String>,
        span: Span,
        label: impl Into<String>,
        hint: impl Into<String>,
    ) -> ProjectError {
        let mut diag = ProjectSourceDiagnostic::new(
            self.path.clone(),
            self.text.clone(),
            code,
            message,
            ProjectSourceLabel::new(span, label),
        );
        diag.set_hint(hint);
        ProjectError::ManifestSourceDiagnostic(Box::new(diag))
    }
}

#[derive(Debug)]
struct ManifestSpanIndex {
    root_span: Span,
    key_spans: ManifestSpanMap,
    value_spans: ManifestSpanMap,
}

struct JsonSpanIndexer<'a> {
    text: &'a str,
    bytes: &'a [u8],
    pos: usize,
    key_spans: ManifestSpanMap,
    value_spans: ManifestSpanMap,
}

impl<'a> JsonSpanIndexer<'a> {
    fn build(text: &'a str) -> Option<ManifestSpanIndex> {
        let mut indexer = Self {
            text,
            bytes: text.as_bytes(),
            pos: 0,
            key_spans: BTreeMap::new(),
            value_spans: BTreeMap::new(),
        };
        let root_span = indexer.parse_value("")?;
        indexer.skip_ws();
        if indexer.pos != indexer.bytes.len() {
            return None;
        }
        Some(ManifestSpanIndex {
            root_span,
            key_spans: indexer.key_spans,
            value_spans: indexer.value_spans,
        })
    }

    fn parse_value(&mut self, pointer: &str) -> Option<Span> {
        self.skip_ws();
        let start = self.pos;
        let span = match self.bytes.get(self.pos)? {
            b'{' => self.parse_object(pointer, start)?,
            b'[' => self.parse_array(pointer, start)?,
            b'"' => self.parse_string_span()?,
            b'-' | b'0'..=b'9' => self.parse_number_span(),
            b't' => self.parse_literal(b"true")?,
            b'f' => self.parse_literal(b"false")?,
            b'n' => self.parse_literal(b"null")?,
            _ => return None,
        };
        if !pointer.is_empty() {
            let _ = self.value_spans.insert(pointer.to_owned(), span);
        }
        Some(span)
    }

    fn parse_object(&mut self, pointer: &str, start: usize) -> Option<Span> {
        self.pos += 1;
        self.skip_ws();
        if self.bytes.get(self.pos) == Some(&b'}') {
            self.pos += 1;
            return span_from_range(start, self.pos);
        }
        loop {
            self.skip_ws();
            let (key, key_span) = self.parse_string_literal()?;
            let key_pointer = join_pointer(pointer, &key);
            let _ = self.key_spans.insert(key_pointer.clone(), key_span);
            self.skip_ws();
            self.expect_byte(b':')?;
            let _ = self.parse_value(&key_pointer)?;
            self.skip_ws();
            match self.bytes.get(self.pos)? {
                b',' => self.pos += 1,
                b'}' => {
                    self.pos += 1;
                    return span_from_range(start, self.pos);
                }
                _ => return None,
            }
        }
    }

    fn parse_array(&mut self, pointer: &str, start: usize) -> Option<Span> {
        self.pos += 1;
        self.skip_ws();
        if self.bytes.get(self.pos) == Some(&b']') {
            self.pos += 1;
            return span_from_range(start, self.pos);
        }
        let mut index = 0usize;
        loop {
            let item_pointer = join_pointer(pointer, &index.to_string());
            let _ = self.parse_value(&item_pointer)?;
            index = index.saturating_add(1);
            self.skip_ws();
            match self.bytes.get(self.pos)? {
                b',' => self.pos += 1,
                b']' => {
                    self.pos += 1;
                    return span_from_range(start, self.pos);
                }
                _ => return None,
            }
        }
    }

    fn parse_string_span(&mut self) -> Option<Span> {
        let (_, span) = self.parse_string_literal()?;
        Some(span)
    }

    fn parse_string_literal(&mut self) -> Option<(String, Span)> {
        let start = self.pos;
        self.expect_byte(b'"')?;
        let mut escaped = false;
        while let Some(byte) = self.bytes.get(self.pos) {
            self.pos += 1;
            if escaped {
                escaped = false;
                continue;
            }
            match byte {
                b'\\' => escaped = true,
                b'"' => {
                    let span = span_from_range(start, self.pos)?;
                    let raw = self.text.get(start..self.pos)?;
                    let decoded = serde_json::from_str(raw).ok()?;
                    return Some((decoded, span));
                }
                _ => {}
            }
        }
        None
    }

    fn parse_number_span(&mut self) -> Span {
        let start = self.pos;
        if self.bytes.get(self.pos) == Some(&b'-') {
            self.pos += 1;
        }
        self.consume_digits();
        if self.bytes.get(self.pos) == Some(&b'.') {
            self.pos += 1;
            self.consume_digits();
        }
        if matches!(self.bytes.get(self.pos), Some(b'e' | b'E')) {
            self.pos += 1;
            if matches!(self.bytes.get(self.pos), Some(b'+' | b'-')) {
                self.pos += 1;
            }
            self.consume_digits();
        }
        span_from_range(start, self.pos).unwrap_or(Span::DUMMY)
    }

    fn parse_literal(&mut self, literal: &[u8]) -> Option<Span> {
        let start = self.pos;
        for expected in literal {
            if self.bytes.get(self.pos)? != expected {
                return None;
            }
            self.pos += 1;
        }
        span_from_range(start, self.pos)
    }

    fn consume_digits(&mut self) {
        while matches!(self.bytes.get(self.pos), Some(b'0'..=b'9')) {
            self.pos += 1;
        }
    }

    fn expect_byte(&mut self, expected: u8) -> Option<()> {
        if self.bytes.get(self.pos) == Some(&expected) {
            self.pos += 1;
            Some(())
        } else {
            None
        }
    }

    fn skip_ws(&mut self) {
        while matches!(self.bytes.get(self.pos), Some(b' ' | b'\n' | b'\r' | b'\t')) {
            self.pos += 1;
        }
    }
}

fn join_pointer(parent: &str, segment: &str) -> String {
    if parent.is_empty() {
        format!("/{}", escape_pointer_segment(segment))
    } else {
        format!("{parent}/{}", escape_pointer_segment(segment))
    }
}

fn escape_pointer_segment(segment: &str) -> String {
    segment.replace('~', "~0").replace('/', "~1")
}

fn span_from_range(start: usize, end: usize) -> Option<Span> {
    let start = u32::try_from(start).ok()?;
    let end = u32::try_from(end).ok()?;
    Some(Span::new(start, end))
}

fn offset_for_error(text: &str, line: usize, column: usize) -> usize {
    if line <= 1 {
        return column.saturating_sub(1).min(text.len());
    }
    let mut current_line = 1usize;
    let mut offset = 0usize;
    for chunk in text.split_inclusive('\n') {
        if current_line == line {
            return offset + column.saturating_sub(1).min(chunk.len());
        }
        offset += chunk.len();
        current_line += 1;
    }
    text.len()
}

fn span_at_offset(text: &str, offset: usize) -> Span {
    let start = u32::try_from(offset.min(text.len())).unwrap_or(u32::MAX);
    let mut end = start.saturating_add(1);
    let text_len = u32::try_from(text.len()).unwrap_or(u32::MAX);
    if start >= text_len {
        end = start;
    }
    Span::new(start, end.min(text_len))
}
