use super::ImportMap;
use crate::{ModuleKey, ModuleSpecifier};

#[test]
fn resolves_exact_match() {
    let mut map = ImportMap::default();
    let _prev = map.imports.insert("@std".into(), "../std".into());
    let resolved = map.resolve(&ModuleKey::new("main"), &ModuleSpecifier::new("@std"));
    assert_eq!(resolved, Some(ModuleSpecifier::new("../std")));
}

#[test]
fn resolves_longest_prefix_match() {
    let mut map = ImportMap::default();
    let _prev = map.imports.insert("@std/".into(), "../std/".into());
    let _prev = map.imports.insert("@std/io/".into(), "../std/io/".into());

    let resolved = map.resolve(
        &ModuleKey::new("main"),
        &ModuleSpecifier::new("@std/io/tty"),
    );
    assert_eq!(resolved, Some(ModuleSpecifier::new("../std/io/tty")));
}

#[test]
fn scoped_mapping_overrides_global() {
    let mut map = ImportMap::default();
    let _prev = map.imports.insert("@std/".into(), "../std/".into());
    let _prev = map.scopes.insert(
        "pkg/".into(),
        [("@std/".to_owned(), "./local_std/".to_owned())].into(),
    );

    let resolved = map.resolve(
        &ModuleKey::new("pkg/main"),
        &ModuleSpecifier::new("@std/io"),
    );
    assert_eq!(resolved, Some(ModuleSpecifier::new("./local_std/io")));
}
