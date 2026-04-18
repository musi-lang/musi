use self::core::{MODULE as CORE_SOURCE, SPEC as CORE_SPEC};
use self::intrinsics::{MODULE as INTRINSICS_SOURCE, SPEC as INTRINSICS_SPEC};
use self::runtime::{MODULE as RUNTIME_SOURCE, SPEC as RUNTIME_SPEC};
use self::syntax::{MODULE as SYNTAX_SOURCE, SPEC as SYNTAX_SPEC};
use self::test::{MODULE as TEST_SOURCE, SPEC as TEST_SPEC};
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

pub mod runtime {
    pub const SPEC: &str = "musi:runtime";
    pub const MODULE: &str = include_str!("../modules/runtime.ms");
    pub const EFFECT: &str = "musi:runtime::Runtime";
    pub const ENV_GET_OP: &str = "envGet";
    pub const ENV_HAS_OP: &str = "envHas";
    pub const ENV_SET_OP: &str = "envSet";
    pub const ENV_REMOVE_OP: &str = "envRemove";
    pub const PROCESS_ARG_COUNT_OP: &str = "processArgCount";
    pub const PROCESS_ARG_AT_OP: &str = "processArgAt";
    pub const PROCESS_CWD_OP: &str = "processCwd";
    pub const PROCESS_RUN_OP: &str = "processRun";
    pub const PROCESS_EXIT_OP: &str = "processExit";
    pub const TIME_NOW_UNIX_MS_OP: &str = "timeNowUnixMs";
    pub const TIME_MONOTONIC_MS_OP: &str = "timeMonotonicMs";
    pub const TIME_SLEEP_MS_OP: &str = "timeSleepMs";
    pub const RANDOM_INT_OP: &str = "randomInt";
    pub const RANDOM_INT_IN_RANGE_OP: &str = "randomIntInRange";
    pub const RANDOM_BOOL_OP: &str = "randomBool";
    pub const RANDOM_FLOAT_01_OP: &str = "randomFloat01";
    pub const LOG_INFO_OP: &str = "logInfo";
    pub const LOG_WRITE_OP: &str = "logWrite";
    pub const IO_PRINT_OP: &str = "ioPrint";
    pub const IO_PRINT_LINE_OP: &str = "ioPrintLine";
    pub const IO_PRINT_ERROR_OP: &str = "ioPrintError";
    pub const IO_PRINT_ERROR_LINE_OP: &str = "ioPrintErrorLine";
    pub const IO_READ_LINE_OP: &str = "ioReadLine";
    pub const FS_READ_TEXT_OP: &str = "fsReadText";
    pub const FS_WRITE_TEXT_OP: &str = "fsWriteText";
    pub const FS_EXISTS_OP: &str = "fsExists";
    pub const FS_APPEND_TEXT_OP: &str = "fsAppendText";
    pub const FS_REMOVE_OP: &str = "fsRemove";
    pub const FS_CREATE_DIR_ALL_OP: &str = "fsCreateDirAll";
    pub const TEXT_LENGTH_OP: &str = "textLength";
    pub const TEXT_TRIM_OP: &str = "textTrim";
    pub const TEXT_TO_LOWERCASE_OP: &str = "textToLowerCase";
    pub const TEXT_TO_UPPERCASE_OP: &str = "textToUpperCase";
    pub const TEXT_CONTAINS_OP: &str = "textContains";
    pub const TEXT_STARTS_WITH_OP: &str = "textStartsWith";
    pub const TEXT_ENDS_WITH_OP: &str = "textEndsWith";
    pub const PATH_JOIN_OP: &str = "pathJoin";
    pub const PATH_NORMALIZE_OP: &str = "pathNormalize";
    pub const PATH_DIRNAME_OP: &str = "pathDirname";
    pub const PATH_BASENAME_OP: &str = "pathBasename";
    pub const PATH_EXTNAME_OP: &str = "pathExtname";
    pub const PATH_IS_ABSOLUTE_OP: &str = "pathIsAbsolute";
    pub const JSON_IS_VALID_OP: &str = "jsonIsValid";
    pub const JSON_NORMALIZE_OP: &str = "jsonNormalize";
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
const RUNTIME_MODULE: FoundationModule = (RUNTIME_SPEC, RUNTIME_SOURCE);
const TEST_MODULE: FoundationModule = (TEST_SPEC, TEST_SOURCE);
const SYNTAX_MODULE: FoundationModule = (SYNTAX_SPEC, SYNTAX_SOURCE);

const FOUNDATION_MODULES: [FoundationModule; 5] = [
    CORE_MODULE,
    INTRINSICS_MODULE,
    RUNTIME_MODULE,
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
