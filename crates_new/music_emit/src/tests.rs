use music_base::SourceId;
use music_bc::descriptor::ConstantValue;
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

#[test]
fn emits_float_tuple_array_and_type_apply() {
    let ir = lower_ir(
        r"
        let id[T] (x : T) : T := x;
        export let pair := (1, 2);
        export let items := [1, 2, 3];
        export let pi : Float := 3.5;
        export let answer () : Int := id[Int](42);
    ",
        "main",
    );

    let emitted = lower_ir_module(&ir, EmitOptions).expect("emit should succeed");
    assert!(emitted.artifact.validate().is_ok());
    assert!(
        emitted
            .artifact
            .constants
            .iter()
            .any(|(_, constant)| matches!(constant.value, ConstantValue::Float(_)))
    );
}

#[test]
fn emits_globals_locals_assignment_index_and_case() {
    let ir = lower_ir(
        r"
        export let base : Int := 41;
        export let answer (x : Int) : Int := (
          let mut items := [1, 2, 3];
          items.[0] <- base;
          case x of (| 0 => items.[0] | value => value + base);
        );
    ",
        "main",
    );

    let emitted = lower_ir_module(&ir, EmitOptions).expect("emit should succeed");
    let opcodes = emitted
        .artifact
        .methods
        .iter()
        .flat_map(|(_, method)| method.code.iter())
        .filter_map(|entry| match entry {
            music_bc::CodeEntry::Instruction(instruction) => Some(instruction.opcode),
            music_bc::CodeEntry::Label(_) => None,
        })
        .collect::<Vec<_>>();

    assert!(emitted.artifact.validate().is_ok());
    assert!(opcodes.contains(&music_bc::Opcode::LdGlob));
    assert!(opcodes.contains(&music_bc::Opcode::StGlob));
    assert!(opcodes.contains(&music_bc::Opcode::SeqGet));
    assert!(opcodes.contains(&music_bc::Opcode::SeqSet));
    assert!(opcodes.contains(&music_bc::Opcode::BrFalse));
}

#[test]
fn emits_case_tuple_and_array_patterns() {
    let ir = lower_ir(
        r"
        export let answer () : Int := (
          let pair := (1, 2);
          let items := [3, 4];
          let p : Int := case pair of (| (1, b) => b | _ => 0);
          let q : Int := case items of (| [3, b] => b | _ => 0);
          p + q
        );
    ",
        "main",
    );

    let emitted = lower_ir_module(&ir, EmitOptions).expect("emit should succeed");
    assert!(emitted.artifact.validate().is_ok());
    let opcodes = emitted
        .artifact
        .methods
        .iter()
        .flat_map(|(_, method)| method.code.iter())
        .filter_map(|entry| match entry {
            music_bc::CodeEntry::Instruction(instruction) => Some(instruction.opcode),
            music_bc::CodeEntry::Label(_) => None,
        })
        .collect::<Vec<_>>();
    assert!(opcodes.contains(&music_bc::Opcode::SeqGet));
    assert!(opcodes.contains(&music_bc::Opcode::BrFalse));
}

#[test]
fn emits_records_with_projection_and_update() {
    let ir = lower_ir(
        r"
        export let answer () : Int := (
          let r := { y := 2, x := 1 };
          let a : Int := r.x;
          let s := r.{ x := 3 };
          a + s.x
        );
    ",
        "main",
    );

    let emitted = lower_ir_module(&ir, EmitOptions).expect("emit should succeed");
    assert!(emitted.artifact.validate().is_ok());
    let opcodes = emitted
        .artifact
        .methods
        .iter()
        .flat_map(|(_, method)| method.code.iter())
        .filter_map(|entry| match entry {
            music_bc::CodeEntry::Instruction(instruction) => Some(instruction.opcode),
            music_bc::CodeEntry::Label(_) => None,
        })
        .collect::<Vec<_>>();
    assert!(opcodes.contains(&music_bc::Opcode::DataNew));
    assert!(opcodes.contains(&music_bc::Opcode::DataGet));
}

#[test]
fn emits_foreign_calls() {
    let ir = lower_ir(
        r#"
        foreign "c" (
          let puts (value : CString) : Int;
        );
        export let answer () : Int := puts("hello");
    "#,
        "main",
    );

    let emitted = lower_ir_module(&ir, EmitOptions).expect("emit should succeed");
    assert!(emitted.artifact.validate().is_ok());
    assert!(
        emitted
            .artifact
            .methods
            .iter()
            .flat_map(|(_, method)| method.code.iter())
            .filter_map(|entry| match entry {
                music_bc::CodeEntry::Instruction(instruction) => Some(instruction.opcode),
                music_bc::CodeEntry::Label(_) => None,
            })
            .any(|opcode| opcode == music_bc::Opcode::FfiCall)
    );
}

#[test]
fn emits_closures_and_higher_order_calls() {
    let ir = lower_ir(
        r"
        let apply (f : Int -> Int, x : Int) : Int := f(x);

        export let answer (x : Int) : Int := (
          let base : Int := 41;
          let add_base (y : Int) : Int := y + base;
          apply(add_base, x);
        );
    ",
        "main",
    );

    let emitted = lower_ir_module(&ir, EmitOptions).expect("emit should succeed");
    assert!(emitted.artifact.validate().is_ok());

    let mut has_indirect_call = false;
    let mut has_capturing_closure = false;
    for (_, method) in emitted.artifact.methods.iter() {
        for entry in &method.code {
            let music_bc::CodeEntry::Instruction(instruction) = entry else {
                continue;
            };
            if instruction.opcode == music_bc::Opcode::CallCls {
                has_indirect_call = true;
            }
            if instruction.opcode == music_bc::Opcode::ClsNew {
                if let music_bc::Operand::WideMethodCaptures { captures, .. } =
                    &instruction.operand
                {
                    if *captures != 0 {
                        has_capturing_closure = true;
                    }
                }
            }
        }
    }

    assert!(has_indirect_call);
    assert!(has_capturing_closure);
}
