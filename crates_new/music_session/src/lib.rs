mod api;
mod session;

pub use api::{
    CompiledOutput, ParsedModule, SessionDiagList, SessionError, SessionOptions, SessionStats,
};
pub use session::Session;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
