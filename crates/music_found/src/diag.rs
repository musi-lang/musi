use std::io::{self, IsTerminal, Write};

use crate::Span;
use crate::source::{SourceId, SourceMap};

/// Severity level for a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagLevel {
    Fatal,
    Error,
    Warning,
    Note,
}

impl DiagLevel {
    /// Human-readable label without trailing colon.
    #[must_use]
    pub const fn label(self) -> &'static str {
        match self {
            Self::Fatal => "fatal error",
            Self::Error => "error",
            Self::Warning => "warning",
            Self::Note => "note",
        }
    }

    /// ANSI color associated with this severity.
    #[must_use]
    pub const fn color(self) -> Color {
        match self {
            Self::Fatal => Color::Purple,
            Self::Error => Color::Red,
            Self::Warning => Color::Yellow,
            Self::Note => Color::Cyan,
        }
    }
}

/// Terminal colors used for diagnostic rendering.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Color {
    Red,
    Yellow,
    Cyan,
    Purple,
    Bold,
    Reset,
}

impl Color {
    /// ANSI escape sequence for this color.
    #[must_use]
    pub const fn ansi_code(self) -> &'static str {
        match self {
            Self::Red => "\x1b[1;31m",
            Self::Yellow => "\x1b[1;33m",
            Self::Cyan => "\x1b[1;36m",
            Self::Purple => "\x1b[1;35m",
            Self::Bold => "\x1b[1m",
            Self::Reset => "\x1b[0m",
        }
    }
}

/// A source annotation pointing to a span with a message.
#[derive(Debug)]
pub struct Label {
    pub span: Span,
    pub source_id: SourceId,
    pub message: String,
}

/// A compiler diagnostic with severity, message, labels, and notes.
#[derive(Debug)]
pub struct Diag {
    pub level: DiagLevel,
    pub message: String,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}

impl Diag {
    fn with_level(level: DiagLevel, message: impl Into<String>) -> Self {
        Self {
            level,
            message: message.into(),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Create a fatal-level diagnostic.
    #[must_use]
    pub fn fatal(message: impl Into<String>) -> Self {
        Self::with_level(DiagLevel::Fatal, message)
    }

    /// Create an error-level diagnostic.
    #[must_use]
    pub fn error(message: impl Into<String>) -> Self {
        Self::with_level(DiagLevel::Error, message)
    }

    /// Create a warning-level diagnostic.
    #[must_use]
    pub fn warning(message: impl Into<String>) -> Self {
        Self::with_level(DiagLevel::Warning, message)
    }

    /// Create a note-level diagnostic.
    #[must_use]
    pub fn note(message: impl Into<String>) -> Self {
        Self::with_level(DiagLevel::Note, message)
    }

    /// Attach a source label to this diagnostic.
    #[must_use]
    pub fn with_label(
        mut self,
        span: Span,
        source_id: SourceId,
        message: impl Into<String>,
    ) -> Self {
        self.labels.push(Label {
            span,
            source_id,
            message: message.into(),
        });
        self
    }

    /// Attach a note to this diagnostic.
    #[must_use]
    pub fn with_note(mut self, message: impl Into<String>) -> Self {
        self.notes.push(message.into());
        self
    }
}

/// Render a diagnostic to the given writer.
///
/// # Errors
///
/// Returns `io::Error` if writing to `writer` fails.
///
/// Output format (Clang-style):
/// ```text
/// src/main.ms:5:23: error: expected ';' after expression
///     5 | let x := 42 + y
///       |                  ^
/// ```
pub fn emit<W: Write>(
    writer: &mut W,
    diag: &Diag,
    sources: &SourceMap,
    use_color: bool,
) -> io::Result<()> {
    let paint = |color: Color, text: &str| -> String {
        if use_color {
            format!("{}{text}{}", color.ansi_code(), Color::Reset.ansi_code())
        } else {
            String::from(text)
        }
    };

    for label in &diag.labels {
        if let Some(source) = sources.get(label.source_id) {
            let (line, col) = source.line_col(label.span.start);
            let path_display = source.path().display();

            // Header: path:line:col: level: message
            let loc = format!("{path_display}:{line}:{col}:");
            writeln!(
                writer,
                "{} {}: {}",
                paint(Color::Bold, &loc),
                paint(diag.level.color(), diag.level.label()),
                paint(Color::Bold, &diag.message),
            )?;

            // Source line
            if let Some(line_text) = source.line_text(line) {
                let line_num = format!("{line}");
                let padding = " ".repeat(line_num.len());

                writeln!(writer, "{padding} |")?;
                writeln!(writer, "{line_num} | {line_text}")?;

                // Count chars (not bytes) before the caret for UTF-8 alignment.
                let byte_col = col.saturating_sub(1).min(line_text.len());
                let caret_offset = line_text
                    .char_indices()
                    .take_while(|&(i, _)| i < byte_col)
                    .count();
                let total_chars = line_text.chars().count();
                let remaining = total_chars.saturating_sub(caret_offset);
                let span_len = usize::try_from(label.span.len()).unwrap_or(1).max(1);
                let caret_count = span_len.min(remaining.max(1)).max(1);
                let caret_padding = " ".repeat(caret_offset);
                let carets = "^".repeat(caret_count);

                let caret_text = if label.message.is_empty() {
                    carets
                } else {
                    format!("{carets} {}", label.message)
                };
                writeln!(
                    writer,
                    "{padding} | {caret_padding}{}",
                    paint(diag.level.color(), &caret_text),
                )?;
            }
        }
    }

    // If no labels, just print the header without source context.
    if diag.labels.is_empty() {
        writeln!(
            writer,
            "{}: {}",
            paint(diag.level.color(), diag.level.label()),
            paint(Color::Bold, &diag.message),
        )?;
    }

    for note_msg in &diag.notes {
        writeln!(
            writer,
            "{}: {}",
            paint(Color::Cyan, "note"),
            paint(Color::Bold, note_msg),
        )?;
    }

    Ok(())
}

/// Emit a diagnostic to stderr, auto-detecting color support.
pub fn emit_to_stderr(diag: &Diag, sources: &SourceMap) {
    let use_color = supports_color();
    let mut stderr = io::stderr();
    // Best-effort write to stderr; nothing useful to do if it fails.
    let _result = emit(&mut stderr, diag, sources, use_color);
}

/// Whether stderr is a terminal that likely supports ANSI colors.
#[must_use]
pub fn supports_color() -> bool {
    io::stderr().is_terminal()
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
