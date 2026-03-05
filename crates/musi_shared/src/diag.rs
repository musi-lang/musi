//! Structured, accumulating error reporting.

use std::sync::Arc;

use crate::{FileId, SourceDb, Span};

/// The severity level of a [`Diagnostic`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Severity {
    /// A fatal error that prevents compilation.
    Error,
    /// A non-fatal warning.
    Warning,
    /// An informational note.
    Note,
}

impl Severity {
    /// Returns a human-readable label for this severity.
    #[must_use]
    pub const fn label(self) -> &'static str {
        match self {
            Self::Error => "error",
            Self::Warning => "warning",
            Self::Note => "note",
        }
    }
}

/// A labeled span pointing into a source file.
#[derive(Debug, Clone)]
pub struct Label {
    /// The byte range this label covers.
    pub span: Span,
    /// The file this label refers to.
    pub file_id: FileId,
    /// A message describing what this label highlights.
    pub message: Arc<str>,
}

impl Label {
    /// Creates a new `Label`.
    #[must_use]
    pub fn new(span: Span, file_id: FileId, message: impl Into<Arc<str>>) -> Self {
        Self {
            span,
            file_id,
            message: message.into(),
        }
    }
}

/// A single compiler diagnostic with a primary label and optional secondary labels.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// How severe this diagnostic is.
    pub severity: Severity,
    /// The top-level message.
    pub message: Arc<str>,
    /// The primary source location.
    pub primary: Label,
    /// Additional source locations that provide context.
    pub secondary: Vec<Label>,
}

impl Diagnostic {
    /// Appends a secondary label to this diagnostic.
    pub fn add_secondary(
        &mut self,
        span: Span,
        file_id: FileId,
        message: impl Into<Arc<str>>,
    ) -> &mut Self {
        self.secondary.push(Label::new(span, file_id, message));
        self
    }

    /// Renders a single-line summary of this diagnostic.
    ///
    /// Format: `"{file}:{line}:{col}: {severity}: {message}"`.
    #[must_use]
    pub fn render_simple(&self, source_db: &SourceDb) -> String {
        let (line, col) = source_db.lookup(self.primary.file_id, self.primary.span.start);
        let file = source_db.name(self.primary.file_id);
        let severity = self.severity.label();
        format!("{file}:{line}:{col}: {severity}: {}", self.message)
    }
}

/// Maximum number of diagnostics stored before further pushes are suppressed.
///
/// When this limit is reached the last slot is replaced with a sentinel
/// "too many errors" message so callers can still iterate and print.
const MAX_ERRORS: usize = 200;

/// An accumulating collection of [`Diagnostic`]s emitted during compilation.
#[derive(Debug, Default)]
pub struct DiagnosticBag {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticBag {
    /// Creates an empty `DiagnosticBag`.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    /// Appends a pre-built [`Diagnostic`].
    ///
    /// Silently dropped once [`MAX_ERRORS`] diagnostics have accumulated.
    pub fn push(&mut self, diagnostic: Diagnostic) {
        if self.diagnostics.len() < MAX_ERRORS {
            self.diagnostics.push(diagnostic);
        }
    }

    /// Appends an error diagnostic and returns a mutable reference to it.
    pub fn error(
        &mut self,
        message: impl Into<Arc<str>>,
        span: Span,
        file_id: FileId,
    ) -> &mut Diagnostic {
        self.push_with_severity(Severity::Error, message, span, file_id)
    }

    /// Appends a warning diagnostic and returns a mutable reference to it.
    pub fn warning(
        &mut self,
        message: impl Into<Arc<str>>,
        span: Span,
        file_id: FileId,
    ) -> &mut Diagnostic {
        self.push_with_severity(Severity::Warning, message, span, file_id)
    }

    /// Appends a note diagnostic and returns a mutable reference to it.
    pub fn note(
        &mut self,
        message: impl Into<Arc<str>>,
        span: Span,
        file_id: FileId,
    ) -> &mut Diagnostic {
        self.push_with_severity(Severity::Note, message, span, file_id)
    }

    /// Returns `true` if any diagnostic has [`Severity::Error`].
    #[must_use]
    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error)
    }

    /// Returns an iterator over all diagnostics.
    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics.iter()
    }

    /// Builds a diagnostic, pushes it, and returns a mutable reference to it.
    ///
    /// When the cap is reached the final slot becomes a "too many errors"
    /// sentinel and all subsequent calls return a reference to that sentinel.
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
