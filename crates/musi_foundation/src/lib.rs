use self::core::{MODULE as CORE_SOURCE, SPEC as CORE_SPEC};
use self::intrinsics::{MODULE as INTRINSICS_SOURCE, SPEC as INTRINSICS_SPEC};
use self::syntax::{MODULE as SYNTAX_SOURCE, SPEC as SYNTAX_SPEC};
use self::test::{MODULE as TEST_SOURCE, SPEC as TEST_SPEC};
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
const TEST_MODULE: FoundationModule = (TEST_SPEC, TEST_SOURCE);
const SYNTAX_MODULE: FoundationModule = (SYNTAX_SPEC, SYNTAX_SOURCE);

const FOUNDATION_MODULES: [FoundationModule; 4] =
    [CORE_MODULE, INTRINSICS_MODULE, TEST_MODULE, SYNTAX_MODULE];

pub fn extend_import_map(import_map: &mut ImportMap) {
    for (spec, _) in FOUNDATION_MODULES {
        let _ = import_map.imports.insert(spec.into(), spec.into());
    }
}

#[must_use]
pub fn resolve_spec(spec: &str) -> Option<ModuleKey> {
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
