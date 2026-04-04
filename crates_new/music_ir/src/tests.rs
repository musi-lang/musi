use music_base::SourceId;
use music_module::ModuleKey;
use music_names::Interner;
use music_resolve::{ResolveOptions, resolve_module};
use music_sema::{SemaOptions, check_module};
use music_syntax::{Lexer, parse};

use crate::lower_module;

fn lower(src: &str) -> crate::IrModule {
    let lexed = Lexer::new(src).lex();
    let parsed = parse(lexed);
    assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

    let mut interner = Interner::new();
    let resolved = resolve_module(
        SourceId::from_raw(1),
        &ModuleKey::new("main"),
        parsed.tree(),
        &mut interner,
        ResolveOptions::default(),
    );
    let sema = check_module(
        resolved,
        &mut interner,
        SemaOptions {
            target: None,
            env: None,
        },
    );
    lower_module(&sema).expect("ir lowering should succeed")
}

#[test]
fn lowers_exports_and_semantic_metadata() {
    let ir = lower(
        r"
        export let id[T] (x : T) : T := x;
        export let Console := effect {
          let readln () : String;
        };
        export let Eq[T] := class {
          let (=) (a : T, b : T) : Bool;
        };
        export instance[T] Eq[T] {
          let (=) (a : T, b : T) : Bool := true;
        };
    ",
    );

    assert!(ir.exported_value("id").is_some());
    assert_eq!(ir.callables.len(), 1);
    assert_eq!(ir.effects.len(), 1);
    assert_eq!(ir.classes.len(), 1);
    assert_eq!(ir.instances.len(), 1);
    assert!(ir.static_imports.is_empty());
}

#[test]
fn lowers_data_and_foreign_facts() {
    let ir = lower(
        r#"
        let Maybe := data { | Some : Int | None };
        foreign "c" (
          let puts (value : CString) : Int;
        );
        export let answer () : Int := 42;
    "#,
    );

    assert_eq!(ir.data_defs.len(), 1);
    assert_eq!(ir.data_defs[0].variant_count, 2);
    assert_eq!(ir.foreigns.len(), 1);
    assert_eq!(ir.foreigns[0].abi.as_ref(), "c");
    assert_eq!(ir.foreigns[0].param_count, 1);
    assert_eq!(ir.callables.len(), 1);
    assert_eq!(ir.exports.len(), 1);
}
