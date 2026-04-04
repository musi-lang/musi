mod binary;
mod error;
mod text;

pub use binary::{decode_binary, encode_binary, validate_binary};
pub use error::AssemblyError;
pub use text::{format_text, parse_text, validate_text};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
