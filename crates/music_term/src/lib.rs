mod syntax;
mod ty;

pub use syntax::{SyntaxShape, SyntaxTerm, SyntaxTermError, SyntaxTermResult};
pub use ty::{
    TypeDim, TypeField, TypeModuleRef, TypeTerm, TypeTermError, TypeTermKind, parse_type_term,
};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
