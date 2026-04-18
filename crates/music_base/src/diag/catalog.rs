use std::borrow::Cow;
use std::error::Error;
use std::fmt::{self, Display, Formatter};

use super::{Diag, DiagCode, DiagLevel};

/// Catalog-backed diagnostic metadata shared by compiler, tooling, runtime, and VM errors.
pub trait DiagnosticKind: Copy {
    fn code(self) -> DiagCode;
    fn phase(self) -> &'static str;
    fn level(self) -> DiagLevel {
        DiagLevel::Error
    }
    fn message(self) -> &'static str;
    fn primary(self) -> &'static str;
    fn help(self) -> Option<&'static str> {
        None
    }
}

/// Runtime values used to render catalog message templates.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DiagContext {
    values: Vec<(Cow<'static, str>, String)>,
}

impl DiagContext {
    #[must_use]
    pub const fn new() -> Self {
        Self { values: Vec::new() }
    }

    #[must_use]
    pub fn with(mut self, key: impl Into<Cow<'static, str>>, value: impl Display) -> Self {
        self.values.push((key.into(), value.to_string()));
        self
    }

    #[must_use]
    pub fn render(&self, template: &str) -> String {
        let mut rendered = template.to_owned();
        for (key, value) in &self.values {
            rendered = rendered.replace(format!("{{{key}}}").as_str(), value);
        }
        rendered
    }
}

/// A diagnostic value that can be rendered through the shared catalog path.
#[derive(Debug, Clone)]
pub struct CatalogDiagnostic<K> {
    kind: K,
    context: DiagContext,
}

impl<K> CatalogDiagnostic<K>
where
    K: DiagnosticKind,
{
    #[must_use]
    pub const fn new(kind: K, context: DiagContext) -> Self {
        Self { kind, context }
    }

    #[must_use]
    pub const fn kind(&self) -> K {
        self.kind
    }

    #[must_use]
    pub const fn context(&self) -> &DiagContext {
        &self.context
    }

    #[must_use]
    pub fn message(&self) -> String {
        self.context.render(self.kind.message())
    }

    #[must_use]
    pub fn primary(&self) -> String {
        self.context.render(self.kind.primary())
    }

    #[must_use]
    pub fn help(&self) -> Option<String> {
        self.kind.help().map(|help| self.context.render(help))
    }

    #[must_use]
    pub fn to_diag(&self) -> Diag {
        let message = self.message();
        let mut diag = match self.kind.level() {
            DiagLevel::Fatal => Diag::fatal(message),
            DiagLevel::Error => Diag::error(message),
            DiagLevel::Warning => Diag::warning(message),
            DiagLevel::Note => Diag::note(message),
        }
        .with_code(self.kind.code());
        if let Some(help) = self.help() {
            diag = diag.with_hint(help);
        }
        diag
    }
}

impl<K> Display for CatalogDiagnostic<K>
where
    K: DiagnosticKind,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.message().as_str())
    }
}

/// Formats wrapped source errors before catalog-local diagnostics.
///
/// # Errors
///
/// Returns [`fmt::Error`] when the target formatter fails.
pub fn display_catalog_or_source<K>(
    diagnostic: Option<CatalogDiagnostic<K>>,
    source: Option<&dyn Error>,
    fallback: &str,
    f: &mut Formatter<'_>,
) -> fmt::Result
where
    K: DiagnosticKind,
{
    if let Some(source) = source {
        return Display::fmt(source, f);
    }
    match diagnostic {
        Some(diagnostic) => Display::fmt(&diagnostic, f),
        None => f.write_str(fallback),
    }
}

/// Error type that has canonical catalog-backed diagnostic output.
pub trait DiagnosticError {
    type Kind: DiagnosticKind;

    fn diagnostic(&self) -> CatalogDiagnostic<Self::Kind>;

    fn to_diag(&self) -> Diag {
        self.diagnostic().to_diag()
    }
}
