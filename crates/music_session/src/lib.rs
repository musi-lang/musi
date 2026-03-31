//! Compiler session state and shared compile context.
//!
//! `music_session` is the embeddable orchestration layer over the compiler crates.
//! It owns:
//! - loaded sources
//! - an import environment for `music_resolve`
//! - convenience entrypoints for lex/parse/resolve/check on in-memory sources

mod import_env;
mod session;

pub use import_env::{SessionImportEnv, SessionImportModule};
pub use session::{Session, SessionAnalyzedSource, SessionParsedSource};
