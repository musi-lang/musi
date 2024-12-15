use super::{source::Source, span::Span};

#[derive(Debug, Clone, Copy)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Span,
    pub source: Option<Box<Source>>,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            span,
            source: None,
        }
    }

    pub fn warning(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            message: message.into(),
            span,
            source: None,
        }
    }

    #[must_use]
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

        if let Some(source) = &self.source {
            let start_position = source.position(self.span.start);
            let end_position = source.position(self.span.end);

            output.push_str(&format!(
                "{BOLD}{}:{}:{}:{RESET} ",
                source.name, start_position.line, start_position.column
            ));

            output.push_str(&format!(
                "{BOLD}{severity_colour}{}{RESET}: {}\n",
                match self.severity {
                    Severity::Error => "error",
                    Severity::Warning => "warning",
                },
                self.message
            ));

            let line_start = source.line_starts[start_position.line as usize - 1];
            let line_end = source
                .line_starts
                .get(start_position.line as usize)
                .copied()
                .unwrap_or(source.content.len());

            let line = String::from_utf8_lossy(&source.content[line_start..line_end]);
            output.push_str(&format!("\n{line}\n"));

            let pointer_indent = start_position.column as usize - 1;
            let pointer_width = if start_position.line == end_position.line {
                end_position.column - start_position.column + 1
            } else {
                (line_end - line_start - pointer_indent + 1)
                    .try_into()
                    .expect("line length too long")
            } as usize;

            output.push_str(&format!(
                "{}{severity_colour}{}{RESET}\n",
                " ".repeat(pointer_indent),
                "^".repeat(pointer_width)
            ));
        }

        output
    }
}

impl std::error::Error for Diagnostic {}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}

pub struct ErrorReporter {
    pub diagnostics: Vec<Diagnostic>,
    pub error_count: usize,
    source: Option<&'static Source>,
}

impl ErrorReporter {
    #[must_use]
    pub const fn new(source: Option<&'static Source>) -> Self {
        Self {
            diagnostics: vec![],
            error_count: 0,
            source,
        }
    }

    pub fn error(&mut self, message: impl Into<String>, span: Span) {
        self.error_count += 1;

        let mut diagnostic = Diagnostic::error(message, span);
        if let Some(source) = self.source {
            diagnostic = diagnostic.with_source(source);
        }
        self.diagnostics.push(diagnostic);
    }

    pub fn warning(&mut self, message: impl Into<String>, span: Span) {
        self.error_count += 1;

        let mut diagnostic = Diagnostic::warning(message, span);
        if let Some(source) = self.source {
            diagnostic = diagnostic.with_source(source);
        }
        self.diagnostics.push(diagnostic);
    }

    #[must_use]
    pub const fn has_errors(&self) -> bool {
        self.error_count > 0
    }
}
