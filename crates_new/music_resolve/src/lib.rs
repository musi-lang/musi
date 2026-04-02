mod resolve;
mod string_lit;

pub use resolve::{ResolveOptions, ResolvedImport, ResolvedModule, resolve_module};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
