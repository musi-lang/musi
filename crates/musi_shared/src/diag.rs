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
