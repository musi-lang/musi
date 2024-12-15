use std::fmt::{self, Write as _};

use super::{source::Source, span::Span};

#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Span,
    pub source: Option<Box<Source>>,
}

impl Diagnostic {
    #[inline]
    pub fn error<T: Into<String>>(message: T, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            span,
            source: None,
        }
    }

    #[inline]
    pub fn warning<T: Into<String>>(message: T, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            message: message.into(),
            span,
            source: None,
        }
    }

    #[must_use]
    #[inline]
    pub fn with_source(mut self, source: &Source) -> Self {
        self.source = Some(Box::new(source.clone()));
        self
    }

    fn format(&self) -> String {
        const RED: &str = "\x1b[31m";
        const YELLOW: &str = "\x1b[33m";
        const BOLD: &str = "\x1b[1m";
        const RESET: &str = "\x1b[0m";

        let severity_colour = match self.severity {
            Severity::Error => RED,
            Severity::Warning => YELLOW,
        };

        let mut output = String::new();

        if let Some(source) = self.source.as_ref() {
            let start_position = source.position(self.span.start);
            let end_position = source.position(self.span.end);

            write!(
                output,
                "{BOLD}{}:{}:{}:{RESET} ",
                source.name, start_position.line, start_position.column
            )
            .unwrap();
            writeln!(
                output,
                "{BOLD}{severity_colour}{}{RESET}: {}\n",
                match self.severity {
                    Severity::Error => "error",
                    Severity::Warning => "warning",
                },
                self.message
            )
            .unwrap();

            let line_start = source
                .line_starts
                .get(
                    usize::try_from(start_position.line)
                        .unwrap_or_default()
                        .saturating_sub(1),
                )
                .copied()
                .unwrap_or(0);
            let line_end = source
                .line_starts
                .get(usize::try_from(start_position.line).unwrap_or_default())
                .copied()
                .unwrap_or(source.content.len());

            let line = String::from_utf8_lossy(
                source.content.get(line_start..line_end).unwrap_or_default(),
            );
            writeln!(output, "\n{line}").unwrap();
            let pointer_indent = usize::try_from(start_position.column)
                .unwrap_or(1)
                .saturating_sub(1);
            let pointer_width = if start_position.line == end_position.line {
                (end_position.column.saturating_sub(start_position.column)).saturating_add(1)
            } else {
                line_end
                    .saturating_sub(line_start)
                    .saturating_sub(pointer_indent)
                    .saturating_add(1)
                    .try_into()
                    .unwrap_or(1)
            };

            writeln!(
                output,
                "{}{severity_colour}{}{RESET}",
                " ".repeat(pointer_indent),
                "^".repeat(usize::try_from(pointer_width).unwrap_or(1))
            )
            .unwrap();
        }

        output
    }
}

impl fmt::Display for Diagnostic {
    #[inline]
    #[expect(clippy::min_ident_chars, reason = "forbids renaming")]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format())
    }
}

#[non_exhaustive]
pub struct ErrorReporter {
    pub diagnostics: Vec<Diagnostic>,
    pub error_count: usize,
    pub source: Option<&'static Source>,
}

impl ErrorReporter {
    #[must_use]
    #[inline]
    pub const fn new(source: Option<&'static Source>) -> Self {
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
        }
        self.diagnostics.push(diagnostic);
    }

    #[inline]
    pub fn warning<M: Into<String>>(&mut self, message: M, span: Span) {
        self.error_count = self.error_count.saturating_add(1);

        let mut diagnostic = Diagnostic::warning(message, span);
        if let Some(source) = self.source {
            diagnostic = diagnostic.with_source(source);
        }
        self.diagnostics.push(diagnostic);
    }

    #[must_use]
    #[inline]
    pub const fn has_errors(&self) -> bool {
        self.error_count > 0
    }
}
