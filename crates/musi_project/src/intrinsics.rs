use music_module::ImportMap;
use music_module::ModuleKey;

pub const TEST_INTRINSIC_SPEC: &str = "musi:test";
pub const TEST_INTRINSIC_MODULE: &str = r"
export let Test := effect {
  let suiteStart (name : String) : Unit;
  let suiteEnd () : Unit;
  let testCase (name : String, passed : Bool) : Unit;
};
";

pub fn extend_import_map(import_map: &mut ImportMap) {
    let _ = import_map
        .imports
        .insert(TEST_INTRINSIC_SPEC.into(), TEST_INTRINSIC_SPEC.into());
}

#[must_use]
pub fn resolve_intrinsic_spec(spec: &str) -> Option<ModuleKey> {
    match spec {
        TEST_INTRINSIC_SPEC => Some(ModuleKey::new(TEST_INTRINSIC_SPEC)),
        _ => None,
    }
}
