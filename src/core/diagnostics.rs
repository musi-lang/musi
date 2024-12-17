use super::{source::NamedSource, span::Span};
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
    note_message: Option<String>,
    span: Span,
    source: Option<Box<NamedSource>>,
}

impl Diagnostic {
    #[inline]
    pub fn error<M: Into<String>>(message: M, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            note_message: None,
            span,
            source: None,
        }
    }

    #[inline]
    pub fn warning<M: Into<String>>(message: M, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            message: message.into(),
            note_message: None,
            span,
            source: None,
        }
    }

    #[inline]
    #[must_use]
    pub fn with_note<M: Into<String>>(mut self, message: M) -> Self {
        self.note_message = Some(message.into());
        self
    }

    #[inline]
    #[must_use]
    pub fn with_source(mut self, source: &NamedSource) -> Self {
        self.source = Some(Box::new(source.clone()));
        self
    }

    fn format(&self) -> String {
        let mut output = String::new();

        if let Some(source) = self.source.as_ref() {
            let (start_position, end_position) = (
                source.position(self.span.start),
                source.position(self.span.end),
            );

            writeln!(
                output,
                "{}:{}: {BOLD}{}{BOLD}: {BOLD}{}{RESET}",
                source.file_path, start_position.line, self.severity, self.message
            )
            .unwrap();

            writeln!(output, "    {BLUE}{BOLD}|{RESET}").unwrap();

            let line = usize::try_from(start_position.line)
                .unwrap_or_default()
                .saturating_sub(1);

            let (current_line_start_offset, current_line_end_offset) = (
                source.line_offsets.get(line).copied().unwrap_or(0),
                source
                    .line_offsets
                    .get(line.saturating_add(1))
                    .copied()
                    .unwrap_or(source.bytes.len()),
            );

            let source_line = String::from_utf8_lossy(
                source
                    .bytes
                    .get(current_line_start_offset..current_line_end_offset)
                    .unwrap_or_default(),
            );
            writeln!(
                output,
                "{BLUE}{BOLD}{:3}{RESET} {BLUE}{BOLD}|{RESET} {}",
                start_position.column, source_line
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

            if let Some(note) = self.note_message.as_ref() {
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
