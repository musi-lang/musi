mod api;
pub use diag::SessionDiagKind;
mod diag;
mod session;

pub use api::{
    CompiledOutput, LawSuiteModule, ParsedModule, SessionDiagList, SessionError, SessionOptions,
    SessionSourceMapError, SessionStats, SessionSyntaxErrors,
};
pub use session::Session;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
