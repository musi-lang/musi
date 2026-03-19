// Each field maps 1:1 to a compiler flag; bools are the correct representation.
// Using an enum or bitfield would obscure the direct CLI flag correspondence.
#[allow(clippy::struct_excessive_bools)] // false positive: each bool IS a distinct compiler flag
#[derive(Debug, Default)]
pub struct SemaOptions {
    /// Enable all strict-mode checks. Equivalent to enabling every other flag.
    pub strict: bool,
    /// Treat unused local bindings as errors rather than warnings.
    pub no_unused_locals: bool,
    /// Treat unused function parameters as errors rather than warnings.
    pub no_unused_parameters: bool,
    /// Warn when a function with an explicit return type has a piecewise body
    /// that is not total (i.e., no catch-all arm).
    pub no_implicit_returns: bool,
    /// Suppress warnings for code that follows a `Never`-typed expression.
    ///
    /// By default, statements after a diverging expression produce an
    /// `UnreachableCode` warning. Set this flag to silence those warnings.
    pub allow_unreachable_code: bool,
    /// Error when a binding is inferred to have the `Any` type.
    pub no_implicit_any: bool,
    /// Disable truncation of long type names in diagnostic messages.
    pub no_error_truncation: bool,
}
