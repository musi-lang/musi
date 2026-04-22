mod diag;
mod resolver;
mod string_lit;

pub use diag::ResolveDiagKind;
pub use resolver::{
    ResolveDiagList, ResolveOptions, ResolvedImport, ResolvedImportBinding,
    ResolvedImportBindingList, ResolvedImportList, ResolvedModule, resolve_diag_kind,
    resolve_module,
};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
