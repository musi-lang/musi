use self::core::{MODULE as CORE_SOURCE, SPEC as CORE_SPEC};
use self::crypto_host::{MODULE as CRYPTO_HOST_SOURCE, SPEC as CRYPTO_HOST_SPEC};
use self::encoding_host::{MODULE as ENCODING_HOST_SOURCE, SPEC as ENCODING_HOST_SPEC};
use self::env::{MODULE as ENV_SOURCE, SPEC as ENV_SPEC};
use self::ffi::{MODULE as FFI_SOURCE, SPEC as FFI_SPEC};
use self::fmt::{MODULE as FMT_SOURCE, SPEC as FMT_SPEC};
use self::fs::{MODULE as FS_SOURCE, SPEC as FS_SPEC};
use self::intrinsics::{MODULE as INTRINSICS_SOURCE, SPEC as INTRINSICS_SPEC};
use self::io::{MODULE as IO_SOURCE, SPEC as IO_SPEC};
use self::json_host::{MODULE as JSON_HOST_SOURCE, SPEC as JSON_HOST_SPEC};
use self::log::{MODULE as LOG_SOURCE, SPEC as LOG_SPEC};
use self::path_host::{MODULE as PATH_HOST_SOURCE, SPEC as PATH_HOST_SPEC};
use self::process::{MODULE as PROCESS_SOURCE, SPEC as PROCESS_SPEC};
use self::random::{MODULE as RANDOM_SOURCE, SPEC as RANDOM_SPEC};
use self::syntax::{MODULE as SYNTAX_SOURCE, SPEC as SYNTAX_SPEC};
use self::test::{MODULE as TEST_SOURCE, SPEC as TEST_SPEC};
use self::text::{MODULE as TEXT_SOURCE, SPEC as TEXT_SPEC};
use self::time::{MODULE as TIME_SOURCE, SPEC as TIME_SPEC};
use self::uuid_host::{MODULE as UUID_HOST_SOURCE, SPEC as UUID_HOST_SPEC};
use music_builtin::foundation_module_by_spec;
use music_module::{ImportMap, ModuleKey};
use music_session::{Session, SessionError};

pub mod core {
    pub const SPEC: &str = "musi:core";
    pub const MODULE: &str = include_str!("../modules/core.ms");
}

pub mod intrinsics {
    pub const SPEC: &str = "musi:intrinsics";
    pub const MODULE: &str = include_str!("../modules/intrinsics.ms");
}

pub mod env {
    pub const SPEC: &str = "musi:env";
    pub const MODULE: &str = include_str!("../modules/env.ms");
    pub const EFFECT: &str = "musi:env::Env";
    pub const GET_OP: &str = "get";
    pub const HAS_OP: &str = "has";
    pub const SET_OP: &str = "set";
    pub const REMOVE_OP: &str = "remove";
}

pub mod ffi {
    pub const SPEC: &str = "musi:ffi";
    pub const MODULE: &str = include_str!("../modules/ffi.ms");
}

pub mod process {
    pub const SPEC: &str = "musi:process";
    pub const MODULE: &str = include_str!("../modules/process.ms");
    pub const EFFECT: &str = "musi:process::Process";
    pub const ARG_COUNT_OP: &str = "argCount";
    pub const ARG_AT_OP: &str = "argAt";
    pub const CWD_OP: &str = "cwd";
    pub const RUN_OP: &str = "run";
    pub const EXIT_OP: &str = "exit";
}

pub mod io {
    pub const SPEC: &str = "musi:io";
    pub const MODULE: &str = include_str!("../modules/io.ms");
    pub const EFFECT: &str = "musi:io::Io";
    pub const PRINT_OP: &str = "print";
    pub const PRINT_LINE_OP: &str = "printLine";
    pub const PRINT_ERROR_OP: &str = "printError";
    pub const PRINT_ERROR_LINE_OP: &str = "printErrorLine";
    pub const READ_LINE_OP: &str = "readLine";
}

pub mod fs {
    pub const SPEC: &str = "musi:fs";
    pub const MODULE: &str = include_str!("../modules/fs.ms");
    pub const EFFECT: &str = "musi:fs::Fs";
    pub const READ_TEXT_OP: &str = "readText";
    pub const WRITE_TEXT_OP: &str = "writeText";
    pub const EXISTS_OP: &str = "exists";
    pub const APPEND_TEXT_OP: &str = "appendText";
    pub const REMOVE_OP: &str = "remove";
    pub const CREATE_DIR_ALL_OP: &str = "createDirAll";
}

pub mod time {
    pub const SPEC: &str = "musi:time";
    pub const MODULE: &str = include_str!("../modules/time.ms");
    pub const EFFECT: &str = "musi:time::Time";
    pub const NOW_UNIX_MS_OP: &str = "nowUnixMs";
    pub const MONOTONIC_MS_OP: &str = "monotonicMs";
    pub const SLEEP_MS_OP: &str = "sleepMs";
}

pub mod random {
    pub const SPEC: &str = "musi:random";
    pub const MODULE: &str = include_str!("../modules/random.ms");
    pub const EFFECT: &str = "musi:random::Random";
    pub const INT_OP: &str = "int";
    pub const INT_IN_RANGE_OP: &str = "intInRange";
    pub const BOOL_OP: &str = "bool";
    pub const FLOAT_01_OP: &str = "float01";
    pub const ENTROPY_HEX_OP: &str = "entropyHex";
}

pub mod text {
    pub const SPEC: &str = "musi:text";
    pub const MODULE: &str = include_str!("../modules/text.ms");
    pub const EFFECT: &str = "musi:text::Text";
    pub const LENGTH_OP: &str = "length";
    pub const TRIM_OP: &str = "trim";
    pub const CONCAT_OP: &str = "concat";
    pub const TO_LOWERCASE_OP: &str = "toLowerCase";
    pub const TO_UPPERCASE_OP: &str = "toUpperCase";
    pub const CONTAINS_OP: &str = "contains";
    pub const STARTS_WITH_OP: &str = "startsWith";
    pub const ENDS_WITH_OP: &str = "endsWith";
    pub const INDEX_OF_OP: &str = "indexOf";
    pub const SLICE_OP: &str = "slice";
    pub const BYTE_AT_OP: &str = "byteAt";
}

pub mod path_host {
    pub const SPEC: &str = "musi:path";
    pub const MODULE: &str = include_str!("../modules/path_host.ms");
    pub const EFFECT: &str = "musi:path::PathHost";
    pub const JOIN_OP: &str = "join";
    pub const NORMALIZE_OP: &str = "normalize";
    pub const DIRNAME_OP: &str = "dirname";
    pub const BASENAME_OP: &str = "basename";
    pub const EXTNAME_OP: &str = "extname";
    pub const IS_ABSOLUTE_OP: &str = "isAbsolute";
}

pub mod json_host {
    pub const SPEC: &str = "musi:json";
    pub const MODULE: &str = include_str!("../modules/json_host.ms");
    pub const EFFECT: &str = "musi:json::JsonHost";
    pub const IS_VALID_OP: &str = "isValid";
    pub const NORMALIZE_OP: &str = "normalize";
}

pub mod encoding_host {
    pub const SPEC: &str = "musi:encoding";
    pub const MODULE: &str = include_str!("../modules/encoding_host.ms");
    pub const EFFECT: &str = "musi:encoding::EncodingHost";
    pub const HEX_ENCODE_OP: &str = "hexEncode";
    pub const HEX_DECODE_OP: &str = "hexDecode";
    pub const HEX_IS_VALID_OP: &str = "hexIsValid";
    pub const BASE64_ENCODE_OP: &str = "base64Encode";
    pub const BASE64_DECODE_OP: &str = "base64Decode";
    pub const BASE64_IS_VALID_OP: &str = "base64IsValid";
    pub const UTF8_ENCODE_OP: &str = "utf8Encode";
    pub const UTF8_DECODE_OP: &str = "utf8Decode";
    pub const UTF8_IS_VALID_OP: &str = "utf8IsValid";
}

pub mod fmt {
    pub const SPEC: &str = "musi:fmt";
    pub const MODULE: &str = include_str!("../modules/fmt.ms");
    pub const EFFECT: &str = "musi:fmt::Format";
    pub const INT_OP: &str = "int";
    pub const FLOAT_OP: &str = "float";
}

pub mod crypto_host {
    pub const SPEC: &str = "musi:crypto";
    pub const MODULE: &str = include_str!("../modules/crypto_host.ms");
    pub const EFFECT: &str = "musi:crypto::CryptoHost";
    pub const SHA256_HEX_OP: &str = "sha256Hex";
    pub const SHA256_BASE64_OP: &str = "sha256Base64";
}

pub mod uuid_host {
    pub const SPEC: &str = "musi:uuid";
    pub const MODULE: &str = include_str!("../modules/uuid_host.ms");
    pub const EFFECT: &str = "musi:uuid::UuidHost";
    pub const V4_OP: &str = "v4";
}

pub mod log {
    pub const SPEC: &str = "musi:log";
    pub const MODULE: &str = include_str!("../modules/log.ms");
    pub const EFFECT: &str = "musi:log::Log";
    pub const INFO_OP: &str = "info";
    pub const WRITE_OP: &str = "write";
}

pub mod test {
    pub const SPEC: &str = "musi:test";
    pub const MODULE: &str = include_str!("../modules/test.ms");
    pub const EFFECT: &str = "musi:test::Test";
    pub const SUITE_START_OP: &str = "suiteStart";
    pub const SUITE_END_OP: &str = "suiteEnd";
    pub const TEST_CASE_OP: &str = "testCase";
}

pub mod syntax {
    pub const SPEC: &str = "musi:syntax";
    pub const MODULE: &str = include_str!("../modules/syntax.ms");
    pub const EFFECT: &str = "musi:syntax::SyntaxOps";
    pub const EVAL_OP: &str = "eval";
    pub const REGISTER_MODULE_OP: &str = "registerModule";
}

type FoundationModule = (&'static str, &'static str);

const CORE_MODULE: FoundationModule = (CORE_SPEC, CORE_SOURCE);
const INTRINSICS_MODULE: FoundationModule = (INTRINSICS_SPEC, INTRINSICS_SOURCE);
const ENV_MODULE: FoundationModule = (ENV_SPEC, ENV_SOURCE);
const FFI_MODULE: FoundationModule = (FFI_SPEC, FFI_SOURCE);
const PROCESS_MODULE: FoundationModule = (PROCESS_SPEC, PROCESS_SOURCE);
const IO_MODULE: FoundationModule = (IO_SPEC, IO_SOURCE);
const FS_MODULE: FoundationModule = (FS_SPEC, FS_SOURCE);
const TIME_MODULE: FoundationModule = (TIME_SPEC, TIME_SOURCE);
const RANDOM_MODULE: FoundationModule = (RANDOM_SPEC, RANDOM_SOURCE);
const TEXT_MODULE: FoundationModule = (TEXT_SPEC, TEXT_SOURCE);
const PATH_HOST_MODULE: FoundationModule = (PATH_HOST_SPEC, PATH_HOST_SOURCE);
const JSON_HOST_MODULE: FoundationModule = (JSON_HOST_SPEC, JSON_HOST_SOURCE);
const ENCODING_HOST_MODULE: FoundationModule = (ENCODING_HOST_SPEC, ENCODING_HOST_SOURCE);
const FMT_MODULE: FoundationModule = (FMT_SPEC, FMT_SOURCE);
const CRYPTO_HOST_MODULE: FoundationModule = (CRYPTO_HOST_SPEC, CRYPTO_HOST_SOURCE);
const UUID_HOST_MODULE: FoundationModule = (UUID_HOST_SPEC, UUID_HOST_SOURCE);
const LOG_MODULE: FoundationModule = (LOG_SPEC, LOG_SOURCE);
const TEST_MODULE: FoundationModule = (TEST_SPEC, TEST_SOURCE);
const SYNTAX_MODULE: FoundationModule = (SYNTAX_SPEC, SYNTAX_SOURCE);

const FOUNDATION_MODULES: [FoundationModule; 19] = [
    CORE_MODULE,
    INTRINSICS_MODULE,
    ENV_MODULE,
    FFI_MODULE,
    PROCESS_MODULE,
    IO_MODULE,
    FS_MODULE,
    TIME_MODULE,
    RANDOM_MODULE,
    TEXT_MODULE,
    PATH_HOST_MODULE,
    JSON_HOST_MODULE,
    ENCODING_HOST_MODULE,
    FMT_MODULE,
    CRYPTO_HOST_MODULE,
    UUID_HOST_MODULE,
    LOG_MODULE,
    TEST_MODULE,
    SYNTAX_MODULE,
];

pub fn extend_import_map(import_map: &mut ImportMap) {
    for (spec, _) in FOUNDATION_MODULES {
        if foundation_module_by_spec(spec).is_some_and(|module| module.hidden) {
            continue;
        }
        let _ = import_map.imports.insert(spec.into(), spec.into());
    }
}

#[must_use]
pub fn resolve_spec(spec: &str) -> Option<ModuleKey> {
    if foundation_module_by_spec(spec).is_some_and(|module| module.hidden) {
        return None;
    }
    module_source(spec).map(|_| ModuleKey::new(spec))
}

#[must_use]
pub fn module_source(spec: &str) -> Option<&'static str> {
    FOUNDATION_MODULES
        .iter()
        .find_map(|(module_spec, module_text)| (spec == *module_spec).then_some(*module_text))
}

/// # Errors
///
/// Returns [`SessionError`] if any foundation module cannot be interned into the session source map.
pub fn register_modules(session: &mut Session) -> Result<(), SessionError> {
    for (spec, text) in FOUNDATION_MODULES {
        session.set_module_text(&ModuleKey::new(spec), text.to_owned())?;
    }
    Ok(())
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
