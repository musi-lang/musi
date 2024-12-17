use super::{source::SourceFile, span::Span};
use std::fmt::{self, Write as _};

const BOLD: &str = "\x1b[1m";
const RED: &str = "\x1b[31m";
const YELLOW: &str = "\x1b[33m";
const BLUE: &str = "\x1b[34m";
const RESET: &str = "\x1b[0m";

#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub enum Severity {
    Error,
    Warning,
}

impl fmt::Display for Severity {
    #[expect(clippy::min_ident_chars, reason = "rustc forbids changing it")]
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Error => write!(f, "{RED}error{RESET}"),
            Self::Warning => write!(f, "{YELLOW}warning{RESET}"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    severity: Severity,
    message: String,
    note: Option<String>,
    span: Span,
    source: Option<Box<SourceFile>>,
}

impl Diagnostic {
    #[inline]
    pub fn error<M: Into<String>>(message: M, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            note: None,
            span,
            source: None,
        }
    }

    #[inline]
    pub fn warning<M: Into<String>>(message: M, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            message: message.into(),
            note: None,
            span,
            source: None,
        }
    }

    #[inline]
    #[must_use]
    pub fn with_note<M: Into<String>>(mut self, message: M) -> Self {
        self.note = Some(message.into());
        self
    }

    #[inline]
    #[must_use]
    pub fn with_source(mut self, source: &SourceFile) -> Self {
        self.source = Some(Box::new(source.clone()));
        self
    }

    fn format(&self) -> String {
        let mut output = String::new();

        if let Some(source) = self.source.as_ref() {
            let start_position = source.position(self.span.start);
            let end_position = source.position(self.span.end);

            writeln!(
                output,
                "{}:{}: {BOLD}{}{BOLD}: {BOLD}{}{RESET}",
                source.name, start_position.line, self.severity, self.message
            )
            .unwrap();
            writeln!(output, "    {BLUE}{BOLD}|{RESET}").unwrap();

            let line = usize::try_from(start_position.line)
                .unwrap_or_default()
                .saturating_sub(1);
            let line_start = source.line_starts.get(line).copied().unwrap_or(0);
            let line_end = source
                .line_starts
                .get(line.saturating_add(1))
                .copied()
                .unwrap_or(source.content.len());
            let line_content = String::from_utf8_lossy(
                source.content.get(line_start..line_end).unwrap_or_default(),
            );

            writeln!(
                output,
                "{BLUE}{BOLD}{:3}{RESET} {BLUE}{BOLD}|{RESET} {line_content}",
                start_position.column
            )
            .unwrap();

            let start_column = start_position.column.saturating_sub(1);
            let end_column = end_position.column.saturating_sub(1);

            let marker = format!(
                "{}{}{}",
                match self.severity {
                    Severity::Error => RED,
                    Severity::Warning => YELLOW,
                },
                "^".repeat(
                    end_column
                        .saturating_sub(start_column)
                        .saturating_add(1)
                        .try_into()
                        .unwrap_or_default(),
                ),
                RESET
            );
            writeln!(
                output,
                "    {BLUE}{BOLD}|{RESET} {}{BOLD}{marker}{RESET}",
                " ".repeat(start_column.try_into().unwrap_or_default())
            )
            .unwrap();

            if let Some(note) = self.note.as_ref() {
                writeln!(output, "    {BLUE}{BOLD}={RESET} {BOLD}note:{RESET} {note}").unwrap();
            }
        }

        output
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
    error_total: usize,
    source: Option<&'static SourceFile>,
}

impl ErrorReporter {
    #[inline]
    #[must_use]
    pub const fn new(source: Option<&'static SourceFile>) -> Self {
        Self {
            diagnostics: vec![],
            error_total: 0,
            source,
        }
    }

    #[inline]
    pub fn error<M: Into<String>>(&mut self, message: M, span: Span) {
        self.error_total = self.error_total.saturating_add(1);

        let mut diagnostic = Diagnostic::error(message, span);
        if let Some(source) = self.source {
            diagnostic = diagnostic.with_source(source);
        };

        self.diagnostics.push(diagnostic);
    }

    #[inline]
    #[must_use]
    pub const fn has_errors(&self) -> bool {
        self.error_total > 0
    }
}
