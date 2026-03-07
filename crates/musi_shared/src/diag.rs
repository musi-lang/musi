//! Structured, accumulating error reporting.

use std::sync::Arc;

use crate::{FileId, SourceDb, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

impl Severity {
    #[must_use]
    pub const fn label(self) -> &'static str {
        match self {
            Self::Error => "error",
            Self::Warning => "warning",
            Self::Note => "note",
        }
    }

    #[must_use]
    const fn ansi_color(self) -> &'static str {
        match self {
            Self::Error => "\x1b[31m",   // red
            Self::Warning => "\x1b[33m", // yellow
            Self::Note => "\x1b[36m",    // cyan
        }
    }
}

#[derive(Debug, Clone)]
pub struct Label {
    pub span: Span,
    pub file_id: FileId,
    pub message: Arc<str>,
}

impl Label {
    #[must_use]
    pub fn new(span: Span, file_id: FileId, message: impl Into<Arc<str>>) -> Self {
        Self {
            span,
            file_id,
            message: message.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: Arc<str>,
    pub primary: Label,
    pub secondary: Vec<Label>,
}

impl Diagnostic {
    pub fn add_secondary(
        &mut self,
        span: Span,
        file_id: FileId,
        message: impl Into<Arc<str>>,
    ) -> &mut Self {
        self.secondary.push(Label::new(span, file_id, message));
        self
    }

    /// Renders `"{file}:{line}:{col}: {severity}: {message}"`.
    #[must_use]
    pub fn render_simple(&self, source_db: &SourceDb) -> String {
        let (line, col) = source_db.lookup(self.primary.file_id, self.primary.span.start);
        let file = source_db.name(self.primary.file_id);
        let severity = self.severity.label();
        format!("{file}:{line}:{col}: {severity}: {}", self.message)
    }

    /// Renders a rich, rustc-style diagnostic with source context and underlines.
    ///
    /// Uses ANSI colors when `use_color` is true.
    #[must_use]
    pub fn render_rich(&self, source_db: &SourceDb, use_color: bool) -> String {
        use std::fmt::Write;

        let (line, col) = source_db.lookup(self.primary.file_id, self.primary.span.start);
        let file = source_db.name(self.primary.file_id);
        let severity = self.severity.label();

        let (sev_start, reset) = if use_color {
            (self.severity.ansi_color(), "\x1b[0m")
        } else {
            ("", "")
        };

        let mut out = String::new();

        // Header: "error: message"
        let _ = writeln!(out, "{sev_start}{severity}{reset}: {}", self.message);

        let line_str = line.to_string();
        let gutter_width = line_str.len();

        // Location: " --> file:line:col"
        let _ = writeln!(out, "{:gutter_width$}--> {file}:{line}:{col}", " ");

        // Blank gutter line
        let _ = writeln!(out, "{:gutter_width$} |", " ");

        // Source line
        let source_line = source_db.get_line(self.primary.file_id, line);
        let _ = writeln!(out, "{line_str} | {source_line}");

        // Underline
        let underline_col = (col as usize).saturating_sub(1);
        let caret_len = (self.primary.span.length as usize).max(1);
        let _ = write!(
            out,
            "{:gutter_width$} | {:underline_col$}{sev_start}{:^>caret_len$}{reset}",
            " ", "", ""
        );

        // Secondary labels
        for label in &self.secondary {
            let (s_line, s_col) = source_db.lookup(label.file_id, label.span.start);
            let s_file = source_db.name(label.file_id);

            let (note_start, note_reset) = if use_color {
                (Severity::Note.ansi_color(), "\x1b[0m")
            } else {
                ("", "")
            };

            let _ = write!(out, "\n{note_start}note{note_reset}: {}", label.message);
            let _ = write!(out, "\n{:gutter_width$}--> {s_file}:{s_line}:{s_col}", " ");

            let s_line_str = s_line.to_string();
            let s_gutter = s_line_str.len().max(gutter_width);
            let s_source = source_db.get_line(label.file_id, s_line);
            let s_underline_col = (s_col as usize).saturating_sub(1);
            let s_caret_len = (label.span.length as usize).max(1);

            let _ = write!(out, "\n{:s_gutter$} |", " ");
            let _ = write!(out, "\n{s_line_str:>s_gutter$} | {s_source}");
            let _ = write!(
                out,
                "\n{:s_gutter$} | {:s_underline_col$}{note_start}{:^>s_caret_len$}{note_reset}",
                " ", "", ""
            );
        }

        out
    }
}

const MAX_ERRORS: usize = 200;

#[derive(Debug, Default)]
pub struct DiagnosticBag {
    diagnostics: Vec<Diagnostic>,
}

macro_rules! severity_builder {
    ($fn:ident, $sev:expr) => {
        pub fn $fn(
            &mut self,
            message: impl Into<Arc<str>>,
            span: Span,
            file_id: FileId,
        ) -> &mut Diagnostic {
            self.push_with_severity($sev, message, span, file_id)
        }
    };
}

impl DiagnosticBag {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    pub fn push(&mut self, diagnostic: Diagnostic) {
        if self.diagnostics.len() < MAX_ERRORS {
            self.diagnostics.push(diagnostic);
        }
    }

    severity_builder!(error, Severity::Error);
    severity_builder!(warning, Severity::Warning);
    severity_builder!(note, Severity::Note);

    #[must_use]
    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics.iter()
    }

    fn push_with_severity(
        &mut self,
        severity: Severity,
        message: impl Into<Arc<str>>,
        span: Span,
        file_id: FileId,
    ) -> &mut Diagnostic {
        if self.diagnostics.len() >= MAX_ERRORS {
            return self.diagnostics.last_mut().expect("at capacity");
        }
        let diagnostic = if self.diagnostics.len() == MAX_ERRORS - 1 {
            let text = "too many errors; further diagnostics suppressed";
            Diagnostic {
                severity: Severity::Error,
                message: Arc::from(text),
                primary: Label::new(span, file_id, text),
                secondary: Vec::new(),
            }
        } else {
            let msg: Arc<str> = message.into();
            Diagnostic {
                severity,
                message: Arc::clone(&msg),
                primary: Label::new(span, file_id, msg),
                secondary: Vec::new(),
            }
        };
        self.diagnostics.push(diagnostic);
        self.diagnostics.last_mut().expect("just pushed")
    }
}

#[cfg(test)]
mod tests;
