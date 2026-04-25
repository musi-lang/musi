use std::fmt::{self, Display, Formatter};
use std::path::{Path, PathBuf};

use crate::{SourceId, Span};

use super::style;

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
}

/// Stable diagnostic code (`MSNNNN`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DiagCode(u16);

impl DiagCode {
    #[must_use]
    pub const fn new(value: u16) -> Self {
        Self(value)
    }

    #[must_use]
    pub const fn raw(self) -> u16 {
        self.0
    }

    #[must_use]
    pub fn parse(raw: &str) -> Option<Self> {
        let digits = raw.strip_prefix("ms")?;
        let numeric_code = digits.parse::<u16>().ok()?;
        Some(Self(numeric_code))
    }
}

impl Display for DiagCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "MS{:04}", self.0)
    }
}

/// A source annotation pointing to a span with a message.
#[derive(Debug, Clone)]
pub struct DiagLabel {
    kind: DiagLabelKind,
    span: Span,
    source_id: SourceId,
    message: String,
}

/// Source annotation role in diagnostic output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagLabelKind {
    Primary,
    Secondary,
}

/// Exact source replacement suggestion.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagFix {
    span: Span,
    source_id: SourceId,
    replacement: String,
}

/// A diagnostic paired with owned source-path and source-text data.
#[derive(Debug, Clone)]
pub struct OwnedSourceDiag {
    path: PathBuf,
    text: String,
    diag: Diag,
}

impl OwnedSourceDiag {
    #[must_use]
    pub const fn new(path: PathBuf, text: String, diag: Diag) -> Self {
        Self { path, text, diag }
    }

    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }

    #[must_use]
    pub fn text(&self) -> &str {
        &self.text
    }

    #[must_use]
    pub const fn diag(&self) -> &Diag {
        &self.diag
    }
}

impl Display for OwnedSourceDiag {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.diag.message())
    }
}

impl DiagLabel {
    #[must_use]
    pub fn new(span: Span, source_id: SourceId, message: impl Into<String>) -> Self {
        Self::primary(span, source_id, message)
    }

    #[must_use]
    pub fn primary(span: Span, source_id: SourceId, message: impl Into<String>) -> Self {
        let message = message.into();
        style::validate(message.as_str());
        Self {
            kind: DiagLabelKind::Primary,
            span,
            source_id,
            message,
        }
    }

    #[must_use]
    pub fn secondary(span: Span, source_id: SourceId, message: impl Into<String>) -> Self {
        let message = message.into();
        style::validate(message.as_str());
        Self {
            kind: DiagLabelKind::Secondary,
            span,
            source_id,
            message,
        }
    }

    #[must_use]
    pub const fn kind(&self) -> DiagLabelKind {
        self.kind
    }

    #[must_use]
    pub const fn span(&self) -> Span {
        self.span
    }

    #[must_use]
    pub const fn source_id(&self) -> SourceId {
        self.source_id
    }

    #[must_use]
    pub const fn message(&self) -> &str {
        self.message.as_str()
    }
}

impl DiagFix {
    #[must_use]
    pub fn new(span: Span, source_id: SourceId, replacement: impl Into<String>) -> Self {
        Self {
            span,
            source_id,
            replacement: replacement.into(),
        }
    }

    #[must_use]
    pub const fn span(&self) -> Span {
        self.span
    }

    #[must_use]
    pub const fn source_id(&self) -> SourceId {
        self.source_id
    }

    #[must_use]
    pub const fn replacement(&self) -> &str {
        self.replacement.as_str()
    }
}

/// A compiler diagnostic with severity, message, labels, and notes.
#[derive(Debug, Clone)]
pub struct Diag {
    level: DiagLevel,
    code: Option<DiagCode>,
    message: String,
    hint: Option<String>,
    labels: Vec<DiagLabel>,
    notes: Vec<String>,
    fixes: Vec<DiagFix>,
}

impl Diag {
    fn with_level(level: DiagLevel, message: impl Into<String>) -> Self {
        let message = message.into();
        style::validate(message.as_str());
        Self {
            level,
            code: None,
            message,
            hint: None,
            labels: Vec::new(),
            notes: Vec::new(),
            fixes: Vec::new(),
        }
    }

    #[must_use]
    pub fn fatal(message: impl Into<String>) -> Self {
        Self::with_level(DiagLevel::Fatal, message)
    }

    #[must_use]
    pub fn error(message: impl Into<String>) -> Self {
        Self::with_level(DiagLevel::Error, message)
    }

    #[must_use]
    pub fn warning(message: impl Into<String>) -> Self {
        Self::with_level(DiagLevel::Warning, message)
    }

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
        self.labels
            .push(DiagLabel::primary(span, source_id, message));
        self
    }

    /// Attach a primary source label to this diagnostic.
    #[must_use]
    pub fn with_primary_label(
        mut self,
        span: Span,
        source_id: SourceId,
        message: impl Into<String>,
    ) -> Self {
        self.labels
            .push(DiagLabel::primary(span, source_id, message));
        self
    }

    /// Attach a secondary source label to this diagnostic.
    #[must_use]
    pub fn with_secondary_label(
        mut self,
        span: Span,
        source_id: SourceId,
        message: impl Into<String>,
    ) -> Self {
        self.labels
            .push(DiagLabel::secondary(span, source_id, message));
        self
    }

    /// Attach a stable diagnostic code.
    #[must_use]
    pub const fn with_code(mut self, code: DiagCode) -> Self {
        self.code = Some(code);
        self
    }

    /// Attach a short fix-it hint.
    #[must_use]
    pub fn with_hint(mut self, hint: impl Into<String>) -> Self {
        let hint = hint.into();
        style::validate(hint.as_str());
        self.hint = Some(hint);
        self
    }

    /// Attach a note to this diagnostic.
    #[must_use]
    pub fn with_note(mut self, message: impl Into<String>) -> Self {
        let message = message.into();
        style::validate(message.as_str());
        self.notes.push(message);
        self
    }

    /// Attach exact source replacement.
    #[must_use]
    pub fn with_replacement(
        mut self,
        span: Span,
        source_id: SourceId,
        replacement: impl Into<String>,
    ) -> Self {
        self.fixes.push(DiagFix::new(span, source_id, replacement));
        self
    }

    #[must_use]
    pub const fn level(&self) -> DiagLevel {
        self.level
    }

    #[must_use]
    pub const fn code(&self) -> Option<DiagCode> {
        self.code
    }

    #[must_use]
    pub const fn message(&self) -> &str {
        self.message.as_str()
    }

    #[must_use]
    pub fn hint(&self) -> Option<&str> {
        self.hint.as_deref()
    }

    #[must_use]
    pub const fn labels(&self) -> &[DiagLabel] {
        self.labels.as_slice()
    }

    #[must_use]
    pub const fn notes(&self) -> &[String] {
        self.notes.as_slice()
    }

    #[must_use]
    pub const fn fixes(&self) -> &[DiagFix] {
        self.fixes.as_slice()
    }
}
