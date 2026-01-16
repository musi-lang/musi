/// Diagnostic severity level
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Severity {
    /// Error severity
    Error,
    /// Warning severity
    Warning,
    /// Note severity
    Note,
}

impl Severity {
    /// Get severity as string slice
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Error => "error",
            Self::Warning => "warning",
            Self::Note => "note",
        }
    }
}
