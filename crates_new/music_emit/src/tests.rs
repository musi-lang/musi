use music_base::SourceId;
use music_module::ModuleKey;
use music_names::Interner;
use music_resolve::{ResolveOptions, resolve_module};
use music_sema::{SemaOptions, check_module};
use music_syntax::{Lexer, parse};

use crate::{EmitOptions, lower_ir_module, lower_ir_program};

fn lower_ir(src: &str, key: &str) -> music_ir::IrModule {
    let lexed = Lexer::new(src).lex();
    let parsed = parse(lexed);
    assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

    let mut interner = Interner::new();
    let resolved = resolve_module(
        SourceId::from_raw(1),
        &ModuleKey::new(key),
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
    music_ir::lower_module(&sema, &interner).expect("ir lowering should succeed")
}

#[test]
fn emits_artifact_for_literal_exports_and_metadata() {
    let ir = lower_ir(
        r#"
        let Option := data { | Some : Int | None };
        foreign "c" (
          let puts (value : CString) : Int;
        );
        export let answer : Int := 42;
        export let forty_two () : Int := 42;
    "#,
        "main",
    );

    let emitted = lower_ir_module(&ir, EmitOptions).expect("emit should succeed");
    assert!(emitted.artifact.validate().is_ok());
    assert_eq!(emitted.exports.len(), 2);
    assert!(!emitted.artifact.types.is_empty());
    assert_eq!(emitted.artifact.foreigns.len(), 1);
}

#[test]
fn emits_merged_program_for_reachable_modules() {
    let dep = lower_ir(
        r"
        export let base : Int := 41;
    ",
        "dep",
    );
    let main = lower_ir(
        r#"
        import "dep";
        export let answer : Int := 42;
    "#,
        "main",
    );

    let program = lower_ir_program(&[dep, main], &ModuleKey::new("main"), EmitOptions)
        .expect("program emit should succeed");
    assert!(program.artifact.validate().is_ok());
    assert_eq!(program.modules.len(), 2);
}
