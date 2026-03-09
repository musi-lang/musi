//! Structured, accumulating error reporting.

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
    pub message: Box<str>,
}

impl Label {
    #[must_use]
    pub fn new(span: Span, file_id: FileId, message: impl Into<Box<str>>) -> Self {
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
    pub message: Box<str>,
    pub primary: Label,
    pub secondary: Vec<Label>,
}

impl Diagnostic {
    pub fn add_secondary(
        &mut self,
        span: Span,
        file_id: FileId,
        message: impl Into<Box<str>>,
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

    /// Renders a rich, clang-style diagnostic with source context and underlines.
    ///
    /// Uses ANSI colors when `use_color` is true.
    #[must_use]
    pub fn render_rich(&self, source_db: &SourceDb, use_color: bool) -> String {
        let (sev_start, reset) = if use_color {
            (self.severity.ansi_color(), "\x1b[0m")
        } else {
            ("", "")
        };

        let mut out = String::new();

        self.render_primary_line(source_db, &mut out, sev_start, reset);
        self.render_primary_context(source_db, &mut out, sev_start, reset);
        self.render_secondary_labels(source_db, &mut out, use_color);

        out
    }

    fn render_primary_line(
        &self,
        source_db: &SourceDb,
        out: &mut String,
        sev_start: &str,
        reset: &str,
    ) {
        use std::fmt::Write;

        let (line, col) = source_db.lookup(self.primary.file_id, self.primary.span.start);
        let file = source_db.name(self.primary.file_id);
        let severity = self.severity.label();

        writeln!(
            out,
            "{file}:{line}:{col}: {sev_start}{severity}{reset}: {}",
            self.message
        )
        .expect("write to String");
    }

    fn render_primary_context(
        &self,
        source_db: &SourceDb,
        out: &mut String,
        sev_start: &str,
        reset: &str,
    ) {
        use std::fmt::Write;

        let (line, col) = source_db.lookup(self.primary.file_id, self.primary.span.start);
        let line_str = line.to_string();
        let gutter_width = line_str.len();

        let source_line = source_db.get_line(self.primary.file_id, line);
        writeln!(out, " {line_str} | {source_line}").expect("write to String");
        let underline_col = usize::try_from(col).unwrap_or(0).saturating_sub(1);
        let caret_len = usize::try_from(self.primary.span.length)
            .unwrap_or(1)
            .max(1);
        write!(
            out,
            " {:gutter_width$} | {:underline_col$}{sev_start}{:^>caret_len$}{reset}",
            "", "", ""
        )
        .expect("write to String");
    }

    fn render_secondary_labels(&self, source_db: &SourceDb, out: &mut String, use_color: bool) {
        use std::fmt::Write;

        let (primary_line, _) = source_db.lookup(self.primary.file_id, self.primary.span.start);
        let primary_gutter_width = primary_line.to_string().len();

        for label in &self.secondary {
            let (s_line, s_col) = source_db.lookup(label.file_id, label.span.start);
            let s_file = source_db.name(label.file_id);

            let (note_start, note_reset) = if use_color {
                (Severity::Note.ansi_color(), "\x1b[0m")
            } else {
                ("", "")
            };

            let s_line_str = s_line.to_string();
            let s_gutter = s_line_str.len().max(primary_gutter_width);
            let s_source = source_db.get_line(label.file_id, s_line);
            let s_underline_col = usize::try_from(s_col).unwrap_or(0).saturating_sub(1);
            let s_caret_len = usize::try_from(label.span.length).unwrap_or(1).max(1);

            write!(
                out,
                "\n{s_file}:{s_line}:{s_col}: {note_start}note{note_reset}: {}",
                label.message
            )
            .expect("write to String");
            write!(out, "\n {s_line_str:>s_gutter$} | {s_source}").expect("write to String");
            write!(
                out,
                "\n {:s_gutter$} | {:s_underline_col$}{note_start}{:^>s_caret_len$}{note_reset}",
                " ", "", ""
            )
            .expect("write to String");
        }
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
            message: impl Into<Box<str>>,
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
            diagnostics: vec![],
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
        message: impl Into<Box<str>>,
        span: Span,
        file_id: FileId,
    ) -> &mut Diagnostic {
        if self.diagnostics.len() >= MAX_ERRORS {
            return self.diagnostics.last_mut().expect("at capacity");
        }
        let diagnostic = if self.diagnostics.len() == MAX_ERRORS - 1 {
            let text: Box<str> = Box::from("too many errors; further diagnostics suppressed");
            Diagnostic {
                severity: Severity::Error,
                message: text.clone(),
                primary: Label::new(span, file_id, text),
                secondary: vec![],
            }
        } else {
            let msg: Box<str> = message.into();
            Diagnostic {
                severity,
                message: msg.clone(),
                primary: Label::new(span, file_id, msg),
                secondary: vec![],
            }
        };
        self.diagnostics.push(diagnostic);
        self.diagnostics.last_mut().expect("just pushed")
    }
}

#[cfg(test)]
mod tests;
