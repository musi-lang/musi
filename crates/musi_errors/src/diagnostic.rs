use crate::Severity;
use musi_source::SourceSpan;

/// Single diagnostic message with severity and location
pub struct Diagnostic {
    /// Severity level (error, warning, note)
    pub severity: Severity,
    /// Error code if applicable
    pub code: Option<u32>,
    /// Human-readable message
    pub message: String,
    /// Source code location
    pub span: SourceSpan,
    /// File identifier
    pub file_id: usize,
    /// Additional hints or suggestions
    pub hints: Vec<String>,
}

/// Collection of diagnostics accumulated during compilation
pub struct DiagnosticBag {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticBag {
    /// Create empty diagnostic bag
    #[must_use]
    pub const fn new() -> Self {
        Self {
            diagnostics: vec![],
        }
    }

    /// Add error diagnostic
    pub fn error(&mut self, code: u32, span: SourceSpan, file_id: usize, msg: String) {
        self.add(Severity::Error, Some(code), span, file_id, msg);
    }

    /// Add warning diagnostic
    pub fn warning(&mut self, code: u32, span: SourceSpan, file_id: usize, msg: String) {
        self.add(Severity::Warning, Some(code), span, file_id, msg);
    }

    /// Add note diagnostic
    pub fn note(&mut self, span: SourceSpan, file_id: usize, msg: String) {
        self.add(Severity::Note, None, span, file_id, msg);
    }

    /// Add hint to existing diagnostic at given index
    pub fn add_hint(&mut self, index: usize, hint: String) {
        if let Some(diag) = self.diagnostics.get_mut(index) {
            diag.hints.push(hint);
        }
    }

    /// Check if bag contains no diagnostics
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    /// Get number of diagnostics in bag
    #[must_use]
    pub const fn len(&self) -> usize {
        self.diagnostics.len()
    }

    /// Merge diagnostics from another bag into this one
    pub fn extend(&mut self, other: Self) {
        self.diagnostics.extend(other.diagnostics);
    }

    /// Convert diagnostic bag to `Result`
    ///
    /// # Errors
    /// Returns `Err` with `self` if bag contains any diagnostics
    pub fn into_result<T>(self, value: T) -> Result<T, Self> {
        if self.is_empty() {
            Ok(value)
        } else {
            Err(self)
        }
    }

    /// Iterate over diagnostics
    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics.iter()
    }

    fn add(
        &mut self,
        severity: Severity,
        code: Option<u32>,
        span: SourceSpan,
        file_id: usize,
        msg: String,
    ) {
        self.diagnostics.push(Diagnostic {
            severity,
            code,
            message: msg,
            span,
            file_id,
            hints: vec![],
        });
    }
}

impl Default for DiagnosticBag {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
