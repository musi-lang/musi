// Each field maps 1:1 to a compiler flag — bools are the natural representation.
#[allow(clippy::struct_excessive_bools)]
#[derive(Debug, Default)]
pub struct SemaOptions {
    pub strict: bool,
    pub no_unused_locals: bool,
    pub no_unused_parameters: bool,
    pub no_implicit_returns: bool,
    pub allow_unreachable_code: bool,
    pub allow_unused_labels: bool,
    pub no_implicit_any: bool,
    pub exact_optional_property_types: bool,
    pub no_error_truncation: bool,
}
