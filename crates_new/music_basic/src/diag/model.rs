use std::fmt::{self, Display, Formatter};

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
        let value = digits.parse::<u16>().ok()?;
        Some(Self(value))
    }
}

impl Display for DiagCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "ms{:04}", self.0)
    }
}

/// A source annotation pointing to a span with a message.
#[derive(Debug)]
pub struct DiagLabel {
    pub span: Span,
    pub source_id: SourceId,
    pub message: String,
}

/// A compiler diagnostic with severity, message, labels, and notes.
#[derive(Debug)]
pub struct Diag {
    pub level: DiagLevel,
    pub code: Option<DiagCode>,
    pub message: String,
    pub hint: Option<String>,
    pub labels: Vec<DiagLabel>,
    pub notes: Vec<String>,
}

impl Diag {
    fn with_level(level: DiagLevel, message: impl Into<String>) -> Self {
        let message = message.into();
        style::validate(&message);
        Self {
            level,
            code: None,
            message,
            hint: None,
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
        let message = message.into();
        style::validate(&message);
        self.labels.push(DiagLabel {
            span,
            source_id,
            message,
        });
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
        style::validate(&hint);
        self.hint = Some(hint);
        self
    }

    /// Attach a note to this diagnostic.
    #[must_use]
    pub fn with_note(mut self, message: impl Into<String>) -> Self {
        let message = message.into();
        style::validate(&message);
        self.notes.push(message);
        self
    }
}
