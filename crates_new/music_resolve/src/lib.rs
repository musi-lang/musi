mod resolver;
mod string_lit;

pub use resolver::{ResolveOptions, ResolvedImport, ResolvedModule, resolve_module};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
