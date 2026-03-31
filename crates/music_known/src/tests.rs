use music_names::Interner;

use crate::KnownSymbols;

#[test]
fn test_known_symbols_have_canonical_spelling() {
    let mut interner = Interner::default();
    let k = KnownSymbols::new(&mut interner);

    assert_eq!(interner.resolve(k.type_), "Type");
    assert_eq!(interner.resolve(k.any), "Any");
    assert_eq!(interner.resolve(k.unknown), "Unknown");
    assert_eq!(interner.resolve(k.syntax), "Syntax");
    assert_eq!(interner.resolve(k.empty), "Empty");
    assert_eq!(interner.resolve(k.unit), "Unit");
    assert_eq!(interner.resolve(k.bool_), "Bool");
    assert_eq!(interner.resolve(k.int_), "Int");
    assert_eq!(interner.resolve(k.float_), "Float");
    assert_eq!(interner.resolve(k.string_), "String");
    assert_eq!(interner.resolve(k.cstring), "CString");
    assert_eq!(interner.resolve(k.cptr), "CPtr");
    assert_eq!(interner.resolve(k.abort), "Abort");
    assert_eq!(interner.resolve(k.abort_op), "abort");
    assert_eq!(interner.resolve(k.musi), "musi");
    assert_eq!(interner.resolve(k.lang), "lang");
    assert_eq!(interner.resolve(k.name_key), "name");
    assert_eq!(interner.resolve(k.lang_option), "Option");
    assert_eq!(interner.resolve(k.some), "Some");
    assert_eq!(interner.resolve(k.none), "None");
}

#[test]
fn test_compiler_prelude_is_stable() {
    let mut interner = Interner::default();
    let k = KnownSymbols::new(&mut interner);
    let prelude = k.compiler_prelude();
    let names: Vec<_> = prelude.iter().map(|&s| interner.resolve(s)).collect();

    assert_eq!(
        names,
        vec![
            "Type", "Any", "Unknown", "Syntax", "Empty", "Unit", "Bool", "Int", "Float", "String",
            "CString", "CPtr", "Abort",
        ]
    );
}
