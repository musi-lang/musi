use music_module::{ImportMap, ModuleKey};
use music_session::{Session, SessionError};

pub mod test {
    pub const SPEC: &str = "musi:test";
    pub const MODULE: &str = r#"
export let Test := effect {
  let suiteStart (name : String) : Unit;
  let suiteEnd () : Unit;
  let testCase (name : String, passed : Bool) : Unit;
};
"#;
    pub const EFFECT: &str = "musi:test::Test";
    pub const SUITE_START_OP: &str = "suiteStart";
    pub const SUITE_END_OP: &str = "suiteEnd";
    pub const TEST_CASE_OP: &str = "testCase";
}

type FoundationModule = (&'static str, &'static str);

const FOUNDATION_MODULES: [FoundationModule; 1] = [(test::SPEC, test::MODULE)];

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

pub fn register_modules(session: &mut Session) -> Result<(), SessionError> {
    for (spec, text) in FOUNDATION_MODULES {
        session.set_module_text(&ModuleKey::new(spec), text.to_owned())?;
    }
    Ok(())
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
