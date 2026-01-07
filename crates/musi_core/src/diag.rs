use crate::error::ErrorCode;
use crate::source::{SourceFile, SourceMap};
use crate::span::Span;
use std::io::{self, IsTerminal, Write};
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
/// Severity level of diagnostic.
pub enum Level {
    /// Critical error preventing compilation.
    Error,
    /// Warning about potential issue.
    Warning,
    /// Informational note.
    Note,
}

#[derive(Debug, Clone, Error)]
#[error("{message}")]
#[non_exhaustive]
/// Represents compiler diagnostic/message.
pub struct Diagnostic {
    pub code: Option<ErrorCode>,
    pub level: Level,
    pub message: String,
    pub notes: Vec<(String, Span)>,
    pub span: Span,
}

impl Diagnostic {
    /// Creates new diagnostic with specified level.
    pub fn new(level: Level, message: impl Into<String>, span: Span) -> Self {
        Self {
            code: None,
            level,
            message: message.into(),
            notes: vec![],
            span,
        }
    }

    /// Creates new error diagnostic.
    pub fn error(message: impl Into<String>, span: Span) -> Self {
        Self::new(Level::Error, message, span)
    }

    #[must_use]
    /// Attaches error code to diagnostic.
    pub const fn with_code(mut self, code: ErrorCode) -> Self {
        self.code = Some(code);
        self
    }

    #[must_use]
    /// Appends note to diagnostic.
    pub fn with_note(mut self, message: impl Into<String>, span: Span) -> Self {
        self.notes.push((message.into(), span));
        self
    }
}

#[derive(Default, Debug, Clone)]
#[non_exhaustive]
/// Collection of compiler diagnostics.
pub struct DiagnosticBag {
    /// List of accumulated diagnostics.
    pub diagnostics: Vec<Diagnostic>,
    /// Count of error-level diagnostics.
    pub errors: usize,
    /// Count of warning-level diagnostics.
    pub warnings: usize,
}

impl DiagnosticBag {
    /// Adds diagnostic to bag.
    pub fn add(&mut self, diag: Diagnostic) {
        match diag.level {
            Level::Error => self.errors += 1,
            Level::Warning => self.warnings += 1,
            Level::Note => {}
        }
        self.diagnostics.push(diag);
    }

    #[must_use]
    /// Checks if bag contains no diagnostics.
    pub const fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    /// Merges other diagnostic bag into this one.
    pub fn merge(&mut self, other: Self) {
        self.errors += other.errors;
        self.warnings += other.warnings;
        self.diagnostics.extend(other.diagnostics);
    }

    /// Emits all diagnostics to `stderr`.
    pub fn emit_all(&self, source_map: &SourceMap) {
        for diag in &self.diagnostics {
            emit(diag, source_map);
        }
    }
}

/// Emits diagnostic to `stderr`.
pub fn emit(diag: &Diagnostic, source_map: &SourceMap) {
    let mut writer = io::stderr();
    let use_color = writer.is_terminal();
    drop(render(&mut writer, diag, source_map, use_color));
}

fn render(
    writer: &mut impl Write,
    diag: &Diagnostic,
    source_map: &SourceMap,
    colour: bool,
) -> io::Result<()> {
    let (style, header) = match diag.level {
        Level::Error => ("\x1b[31m", "error"),
        Level::Warning => ("\x1b[33m", "warning"),
        Level::Note => ("\x1b[36m", "note"),
    };

    let source_file = source_map.lookup_source(diag.span.lo);
    if let Some(source) = source_file {
        render_location(writer, source, diag.span.lo, colour)?;
    }

    render_message(writer, header, &diag.message, style, colour)?;

    if let Some(source) = source_file {
        render_snippet(writer, source, diag.span, style, colour)?;
    }

    for (message, span) in &diag.notes {
        render_note(writer, message, *span, source_map, colour)?;
    }

    Ok(())
}

fn render_location(
    writer: &mut impl Write,
    source_file: &SourceFile,
    offset: u32,
    colour: bool,
) -> io::Result<()> {
    let (ln, col) = source_file.location_at(offset);
    if colour {
        write!(writer, "\x1b[1m")?;
    }
    write!(writer, "{}:{ln}:{col}: ", source_file.name)?;
    if colour {
        write!(writer, "\x1b[0m")?;
    }
    Ok(())
}

fn render_message(
    writer: &mut impl Write,
    header: &str,
    message: &str,
    style: &str,
    colour: bool,
) -> io::Result<()> {
    if colour {
        writeln!(writer, "\x1b[1m{style}{header}:\x1b[0m {message}")
    } else {
        writeln!(writer, "{header}: {message}")
    }
}

fn render_snippet(
    writer: &mut impl Write,
    source_file: &SourceFile,
    span: Span,
    style: &str,
    colour: bool,
) -> io::Result<()> {
    let (line_index, col) = source_file.location_at(span.lo);
    if let Some(line) = source_file.line_at(line_index - 1) {
        let line_index_str = line_index.to_string();
        if colour {
            writeln!(writer, " \x1b[1m{line_index_str} |\x1b[0m {line}")?;
        } else {
            writeln!(writer, " {line_index_str} | {line}")?;
        }

        let (_, end_col) = source_file.location_at(span.hi);
        let highlight_len = if source_file.line_index(span.lo) == source_file.line_index(span.hi) {
            (end_col - col).max(1)
        } else {
            line.len() - col + 1
        };

        let padding = " ".repeat(line_index_str.len());
        let indent = " ".repeat(col - 1);
        let carets = "^".repeat(highlight_len);

        if colour {
            writeln!(writer, " {padding} | {indent}{style}{carets}\x1b[0m")?;
        } else {
            writeln!(writer, " {padding} | {indent}{carets}")?;
        }
    }
    Ok(())
}

fn render_note(
    writer: &mut impl Write,
    message: &str,
    span: Span,
    source_map: &SourceMap,
    colour: bool,
) -> io::Result<()> {
    let (note_style, note_header) = ("\x1b[36m", "note");
    let source_file = source_map.lookup_source(span.lo);
    if let Some(source) = source_file {
        render_location(writer, source, span.lo, colour)?;
    }
    if colour {
        writeln!(writer, "\x1b[1m{note_style}{note_header}:\x1b[0m {message}")
    } else {
        writeln!(writer, "{note_header}: {message}")
    }
}
