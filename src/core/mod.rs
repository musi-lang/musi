use diagnostics::Diagnostic;

pub mod diagnostics;
pub mod source;
pub mod span;
pub mod value;

pub const CHAR_QUOTE: u8 = b'"';
pub const CHAR_APOSTROPHE: u8 = b'\'';
pub const CHAR_BACKSLASH: u8 = b'\\';
pub const CHAR_UNDERSCORE: u8 = b'_';
pub const CHAR_DOT: u8 = b'.';
pub const CHAR_CR: u8 = b'\r';
pub const CHAR_LF: u8 = b'\n';
pub const CHAR_ZERO: u8 = b'0';

pub type MusiResult<T> = Result<T, Diagnostic>;
