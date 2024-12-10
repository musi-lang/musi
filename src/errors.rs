use crate::span::Span;

#[derive(Debug, Clone, Copy)]
pub enum ErrorSeverity {
    Error,
    Warning,
}

#[derive(Debug)]
pub struct ErrorDiagnostic {
    pub severity: ErrorSeverity,
    pub message: String,
    pub span: Span,
    pub source_file: Option<String>,
    pub source_line: Option<String>,
}

impl ErrorDiagnostic {
    pub fn error(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: ErrorSeverity::Error,
            message: message.into(),
            span,
            source_file: None,
            source_line: None,
        }
    }

    pub fn warning(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: ErrorSeverity::Warning,
            message: message.into(),
            span,
            source_file: None,
            source_line: None,
        }
    }

    pub fn with_file(mut self, file: impl Into<String>) -> Self {
        self.source_file = Some(file.into());
        self
    }

    pub fn with_source(mut self, line: impl Into<String>) -> Self {
        self.source_line = Some(line.into());
        self
    }

    fn report(&self) -> String {
        const RED: &str = "\x1b[31m";
        const YELLOW: &str = "\x1b[33m";
        const BOLD: &str = "\x1b[1m";
        const RESET: &str = "\x1b[0m";

        let severity_colour = match self.severity {
            ErrorSeverity::Error => RED,
            ErrorSeverity::Warning => YELLOW,
        };

        let mut output = format!(
            "{BOLD}{}:{}:{}:{RESET} {BOLD}{severity_colour}{}{RESET}: {}\n",
            self.source_file.as_deref().unwrap_or("<source>"),
            self.span.start.line,
            self.span.start.column,
            match self.severity {
                ErrorSeverity::Error => "error",
                ErrorSeverity::Warning => "warning",
            },
            self.message
        );

        if let Some(ref line) = self.source_line {
            output.push_str(&format!("\n{line}\n"));
            output.push_str(&format!(
                "{}{severity_colour}{}",
                " ".repeat((self.span.start.column - 1) as usize),
                "^".repeat((self.span.end.column - self.span.start.column + 1) as usize)
            ));
        }

        output
    }
}

impl std::error::Error for ErrorDiagnostic {}

impl std::fmt::Display for ErrorDiagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.report())
    }
}

pub struct ErrorReporter {
    diagnostics: Vec<ErrorDiagnostic>,
    error_count: usize,
}

impl ErrorReporter {
    pub const fn new() -> Self {
        Self {
            diagnostics: vec![],
            error_count: 0,
        }
    }

    pub fn error(&mut self, message: impl Into<String>, span: Span) {
        self.error_count += 1;
        self.diagnostics.push(ErrorDiagnostic {
            severity: ErrorSeverity::Error,
            message: message.into(),
            span,
            source_file: None,
            source_line: None,
        });
    }

    pub fn warning(&mut self, message: impl Into<String>, span: Span) {
        self.diagnostics.push(ErrorDiagnostic {
            severity: ErrorSeverity::Warning,
            message: message.into(),
            span,
            source_file: None,
            source_line: None,
        });
    }

    pub const fn has_errors(&self) -> bool {
        self.error_count > 0
    }

    pub fn take_diagnostics(&mut self) -> Vec<ErrorDiagnostic> {
        std::mem::take(&mut self.diagnostics)
    }
}

pub type MusiResult<T> = Result<T, ErrorDiagnostic>;
