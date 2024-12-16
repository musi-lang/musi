use super::{source::NamedSource, span::Span};
use std::fmt::{self, Write as _};

#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    severity: Severity,
    primary_message: String,
    help_message: Option<String>,
    span: Span,
    source: Option<Box<NamedSource>>,
}

impl Diagnostic {
    #[inline]
    pub fn error<M: Into<String>>(message: M, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            primary_message: message.into(),
            help_message: None,
            span,
            source: None,
        }
    }

    #[inline]
    pub fn warning<M: Into<String>>(message: M, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            primary_message: message.into(),
            help_message: None,
            span,
            source: None,
        }
    }

    #[inline]
    #[must_use]
    pub fn with_help<M: Into<String>>(mut self, message: M) -> Self {
        self.help_message = Some(message.into());
        self
    }

    #[inline]
    #[must_use]
    pub fn with_source(mut self, source: &NamedSource) -> Self {
        self.source = Some(Box::new(source.clone()));
        self
    }

    fn format(&self) -> String {
        let mut formatted_message = String::new();

        if let Some(source) = self.source.as_ref() {
            let start = source.position(self.span.start);

            writeln!(
                formatted_message,
                " --> {}:{}:{}",
                source.file_path, start.line, start.column
            )
            .unwrap();

            writeln!(formatted_message, "error: {}", self.primary_message).unwrap();

            let line_start_offset = source
                .line_offsets
                .get(
                    usize::try_from(start.line)
                        .unwrap_or_default()
                        .saturating_sub(1),
                )
                .copied()
                .unwrap_or(0);

            let line_end_offset = source
                .line_offsets
                .get(usize::try_from(start.line).unwrap_or_default())
                .copied()
                .unwrap_or(source.bytes.len());

            let source_line = String::from_utf8_lossy(
                source
                    .bytes
                    .get(line_start_offset..line_end_offset)
                    .unwrap_or_default(),
            );
            writeln!(formatted_message, "{:>4} | {}", start.line, source_line).unwrap();

            let error_market_offset = usize::try_from(start.column).unwrap_or(1).saturating_sub(1);
            writeln!(
                formatted_message,
                "     | {}^",
                " ".repeat(error_market_offset)
            )
            .unwrap();

            if let Some(message) = self.help_message.as_ref() {
                writeln!(formatted_message, "help: {message}").unwrap();
            }
        }

        formatted_message
    }
}

impl fmt::Display for Diagnostic {
    #[expect(clippy::min_ident_chars, reason = "rustc forbids changing it")]
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format())
    }
}

pub struct ErrorReporter {
    diagnostics: Vec<Diagnostic>,
    error_count: usize,
    source: Option<&'static NamedSource>,
}

impl ErrorReporter {
    #[inline]
    #[must_use]
    pub const fn new(source: Option<&'static NamedSource>) -> Self {
        Self {
            diagnostics: vec![],
            error_count: 0,
            source,
        }
    }

    #[inline]
    pub fn error<M: Into<String>>(&mut self, message: M, span: Span) {
        self.error_count = self.error_count.saturating_add(1);

        let mut diagnostic = Diagnostic::error(message, span);
        if let Some(source) = self.source {
            diagnostic = diagnostic.with_source(source);
        };

        self.diagnostics.push(diagnostic);
    }

    #[inline]
    #[must_use]
    pub const fn has_errors(&self) -> bool {
        self.error_count > 0
    }
}
