use music_module::{ImportMap, ModuleKey};
use music_session::{Session, SessionError};

mod registry;

pub mod env {
    pub const EFFECT: &str = "musi:env::Env";
    pub const GET_OP: &str = "get";
    pub const HAS_OP: &str = "has";
    pub const SET_OP: &str = "set";
    pub const REMOVE_OP: &str = "remove";
}

pub mod process {
    pub const EFFECT: &str = "musi:process::Process";
    pub const ARG_COUNT_OP: &str = "argCount";
    pub const ARG_AT_OP: &str = "argAt";
    pub const CWD_OP: &str = "cwd";
    pub const RUN_OP: &str = "run";
    pub const EXIT_OP: &str = "exit";
}

pub mod io {
    pub const EFFECT: &str = "musi:io::Io";
    pub const PRINT_OP: &str = "print";
    pub const PRINT_LINE_OP: &str = "printLine";
    pub const PRINT_ERROR_OP: &str = "printError";
    pub const PRINT_ERROR_LINE_OP: &str = "printErrorLine";
    pub const READ_LINE_OP: &str = "readLine";
}

pub mod fs {
    pub const EFFECT: &str = "musi:fs::Fs";
    pub const READ_TEXT_OP: &str = "readText";
    pub const WRITE_TEXT_OP: &str = "writeText";
    pub const EXISTS_OP: &str = "exists";
    pub const APPEND_TEXT_OP: &str = "appendText";
    pub const REMOVE_OP: &str = "remove";
    pub const CREATE_DIR_ALL_OP: &str = "createDirAll";
}

pub mod time {
    pub const EFFECT: &str = "musi:time::Time";
    pub const NOW_UNIX_MS_OP: &str = "nowUnixMs";
    pub const MONOTONIC_MS_OP: &str = "monotonicMs";
    pub const SLEEP_MS_OP: &str = "sleepMs";
}

pub mod random {
    pub const EFFECT: &str = "musi:random::Random";
    pub const INT_OP: &str = "int";
    pub const INT_IN_RANGE_OP: &str = "intInRange";
    pub const BOOL_OP: &str = "bool";
    pub const FLOAT_01_OP: &str = "float01";
    pub const ENTROPY_HEX_OP: &str = "entropyHex";
}

pub mod text {
    pub const EFFECT: &str = "musi:text::Text";
    pub const LENGTH_OP: &str = "length";
    pub const CONCAT_OP: &str = "concat";
    pub const SLICE_OP: &str = "slice";
    pub const BYTE_AT_OP: &str = "byteAt";
    pub const FROM_BYTE_OP: &str = "fromByte";
}

pub mod json_host {
    pub const EFFECT: &str = "musi:json::JsonHost";
    pub const IS_VALID_OP: &str = "isValid";
    pub const NORMALIZE_OP: &str = "normalize";
}

pub mod encoding_host {
    pub const EFFECT: &str = "musi:encoding::EncodingHost";
    pub const BASE64_ENCODE_OP: &str = "base64Encode";
    pub const BASE64_DECODE_OP: &str = "base64Decode";
    pub const BASE64_IS_VALID_OP: &str = "base64IsValid";
    pub const UTF8_ENCODE_OP: &str = "utf8Encode";
    pub const UTF8_DECODE_OP: &str = "utf8Decode";
    pub const UTF8_IS_VALID_OP: &str = "utf8IsValid";
}

pub mod fmt {
    pub const EFFECT: &str = "musi:fmt::Format";
    pub const INT_OP: &str = "int";
    pub const FLOAT_OP: &str = "float";
}

pub mod crypto_host {
    pub const EFFECT: &str = "musi:crypto::CryptoHost";
    pub const SHA256_HEX_OP: &str = "sha256Hex";
    pub const SHA256_BASE64_OP: &str = "sha256Base64";
}

pub mod uuid_host {
    pub const EFFECT: &str = "musi:uuid::UuidHost";
    pub const V4_OP: &str = "v4";
}

pub mod log {
    pub const EFFECT: &str = "musi:log::Log";
    pub const INFO_OP: &str = "info";
    pub const WRITE_OP: &str = "write";
}

pub mod test {
    pub const EFFECT: &str = "musi:test::Test";
    pub const SUITE_START_OP: &str = "suiteStart";
    pub const SUITE_END_OP: &str = "suiteEnd";
    pub const TEST_CASE_OP: &str = "testCase";
}

pub mod syntax {
    pub const EFFECT: &str = "musi:syntax::SyntaxOps";
    pub const EVAL_OP: &str = "eval";
    pub const REGISTER_MODULE_OP: &str = "registerModule";
}

pub fn extend_import_map(import_map: &mut ImportMap) {
    registry::extend_import_map(import_map);
}

#[must_use]
pub fn resolve_spec(spec: &str) -> Option<ModuleKey> {
    registry::resolve_spec(spec)
}

#[must_use]
pub fn module_source(spec: &str) -> Option<&'static str> {
    registry::module_source(spec)
}

/// # Errors
///
/// Returns [`SessionError`] if any foundation module cannot be interned into the session source map.
pub fn register_modules(session: &mut Session) -> Result<(), SessionError> {
    registry::register_modules(session)
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
