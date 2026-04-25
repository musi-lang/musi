#![allow(unused_imports)]

use music_base::SourceId;
use music_base::diag::{Diag, DiagCode};
use music_ir::IrModule;
use music_ir_lower::lower_module;
use music_module::ModuleKey;
use music_names::Interner;
use music_resolve::{ResolveOptions, resolve_module};
use music_seam::descriptor::ConstantValue;
use music_seam::{CodeEntry, Opcode};
use music_sema::{SemaOptions, check_module};
use music_syntax::{Lexer, parse};

use crate::{
    EmitDiagKind, EmitDiagList, EmitOptions, EmittedModule, emit_diag_kind, lower_ir_module,
    lower_ir_program,
};

fn lower_ir(src: &str, key: &str) -> IrModule {
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
            prelude: None,
        },
    );
    lower_module(&sema, &interner).expect("ir lowering should succeed")
}

fn emit_module(src: &str) -> Result<EmittedModule, EmitDiagList> {
    let ir = lower_ir(src, "main");
    lower_ir_module(&ir, EmitOptions)
}

fn emitted_opcodes(emitted: &EmittedModule) -> Vec<Opcode> {
    emitted
        .artifact
        .procedures
        .iter()
        .flat_map(|(_, procedure)| procedure.code.iter())
        .filter_map(|entry| match entry {
            CodeEntry::Instruction(instruction) => Some(instruction.opcode),
            CodeEntry::Label(_) => None,
        })
        .collect()
}

fn assert_module_opcodes(src: &str, expected: &[Opcode]) {
    let emitted = emit_module(src).expect("emit should succeed");
    assert!(
        emitted.artifact.validate().is_ok(),
        "{:?}",
        emitted.artifact.validate()
    );
    let opcodes = emitted_opcodes(&emitted);
    for opcode in expected {
        assert!(opcodes.contains(opcode));
    }
}

mod success {
    use super::*;

    #[test]
    fn emits_artifact_for_literal_exports_and_metadata() {
        let ir = lower_ir(
            r#"
        let Option := data { | Some(Int) | None };
        native "c" (
          let puts (value : CString) : Int;
        );
        export let result : Int := 42;
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
        export let result : Int := 42;
    "#,
            "main",
        );

        let program = lower_ir_program(&[dep, main], &ModuleKey::new("main"), EmitOptions)
            .expect("program emit should succeed");
        assert!(
            program.artifact.validate().is_ok(),
            "{:?}",
            program.artifact.validate()
        );
        assert_eq!(program.modules.len(), 2);
    }

    #[test]
    fn emits_logical_operator_family_opcodes() {
        assert_module_opcodes(
            r"
        export let boolAnd (left : Bool, right : Bool) : Bool := left and right;
        export let boolOr (left : Bool, right : Bool) : Bool := left or right;
        export let boolXor (left : Bool, right : Bool) : Bool := left xor right;
        export let bitsAnd (left : Bits[4], right : Bits[4]) : Bits[4] := left and right;
        export let bitsOr (left : Bits[4], right : Bits[4]) : Bits[4] := left or right;
        export let bitsXor (left : Bits[4], right : Bits[4]) : Bits[4] := left xor right;
        export let bitsNot (value : Bits[4]) : Bits[4] := not value;
    ",
            &[
                Opcode::BrFalse,
                Opcode::And,
                Opcode::Or,
                Opcode::Xor,
                Opcode::Not,
            ],
        );
    }

    #[test]
    fn emits_module_entry_for_top_level_expression_statement() {
        let emitted = emit_module(
            r"
        let result () : Int := 42;
        result();
    ",
        )
        .expect("emit should succeed");
        let entry_procedure = emitted.entry_procedure.expect("module entry expected");
        let entry = emitted.artifact.procedures.get(entry_procedure);

        assert!(entry.code.iter().any(|entry| {
            matches!(
                entry,
                CodeEntry::Instruction(instruction) if instruction.opcode == Opcode::Call
            )
        }));
    }

    #[test]
    fn emits_float_tuple_array_and_type_apply() {
        let ir = lower_ir(
            r"
        let id[T] (x : T) : T := x;
        export let pair := (1, 2);
        export let items := [1, 2, 3];
        export let pi : Float := 3.5;
        export let result () : Int := id[Int](42);
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
        assert_module_opcodes(
            r"
        export let base : Int := 41;
        export let result (x : Int) : Int := (
          let items := mut [1, 2, 3];
          items.[0] := base;
          match x (| 0 => items.[0] | value => value + base);
        );
    ",
            &[
                Opcode::LdGlob,
                Opcode::StGlob,
                Opcode::LdElem,
                Opcode::StElem,
                Opcode::BrFalse,
            ],
        );
    }

    #[test]
    fn emits_generic_callable_param_name_refs() {
        let emitted = emit_module(
            r"
        export let equal [T] (actual : T, expected : T) : Bool :=
          actual = expected;
    ",
        )
        .expect("emit should succeed");

        assert!(emitted.artifact.validate().is_ok());
        let opcodes = emitted_opcodes(&emitted);
        assert!(opcodes.contains(&Opcode::LdLoc));
        assert!(opcodes.contains(&Opcode::Ceq));
    }

    #[test]
    fn emits_generic_callable_param_refs_through_type_apply_call() {
        let emitted = emit_module(
            r"
        let equal [T] (actual : T, expected : T) : Bool :=
          actual = expected;
        export let toBe (actual : Int, expected : Int) : Bool :=
          equal[Int](actual, expected);
    ",
        )
        .expect("emit should succeed");

        assert!(emitted.artifact.validate().is_ok());
        let opcodes = emitted_opcodes(&emitted);
        assert!(opcodes.contains(&Opcode::LdLoc));
        assert!(opcodes.contains(&Opcode::Ceq));
    }

    #[test]
    fn emits_param_name_refs_inside_match_guards() {
        let emitted = emit_module(
            r#"
        let fail (message : String) : Bool := 0 = 1;
        export let equal [T] (actual : T, expected : T) : Bool :=
          match () (
          | _ if actual = expected => 0 = 0
          | _ => fail("expected values to be equal")
          );
    "#,
        )
        .expect("emit should succeed");

        assert!(emitted.artifact.validate().is_ok());
        let opcodes = emitted_opcodes(&emitted);
        assert!(opcodes.contains(&Opcode::LdLoc));
        assert!(opcodes.contains(&Opcode::Ceq));
    }

    #[test]
    fn emits_local_callable_captures_outer_param_in_call_args() {
        let emitted = emit_module(
            r"
        let equal [T] (actual : T, expected : T) : Bool :=
          actual = expected;
        export let expectInt (actual : Int) :=
          (
            let shouldEqual (expected : Int) := equal[Int](actual, expected);
            { equal := shouldEqual }
          );
    ",
        )
        .expect("emit should succeed");

        assert!(emitted.artifact.validate().is_ok());
        let opcodes = emitted_opcodes(&emitted);
        assert!(opcodes.contains(&Opcode::NewFn));
        assert!(opcodes.contains(&Opcode::LdLoc));
    }

    #[test]
    fn emits_multi_index_get_set() {
        assert_module_opcodes(
            r"
        export let touch (name : String, grid : mut [2][2]Int) : Int := (
          grid.[0, 1] := 7;
          grid.[0, 1]
        );
    ",
            &[Opcode::LdElem, Opcode::StElem],
        );
    }

    #[test]
    fn emits_dynamic_module_load() {
        assert_module_opcodes(
            r"
        export let read (name : String) : Any := (
          let loaded := import name;
          loaded
        );
    ",
            &[Opcode::MdlLoad],
        );
    }

    #[test]
    fn emits_case_tuple_and_array_patterns() {
        let emitted = emit_module(
            r"
        export let result () : Int := (
          let pair := (1, 2);
          let items := [3, 4];
          let p : Int := match pair (| (1, b) => b | _ => 0);
          let q : Int := match items (| [3, b] => b | _ => 0);
          p + q
        );
    ",
        )
        .expect("emit should succeed");
        assert!(emitted.artifact.validate().is_ok());
        let opcodes = emitted_opcodes(&emitted);
        assert!(opcodes.contains(&Opcode::LdElem));
        assert!(opcodes.contains(&Opcode::LdLen));
        assert!(opcodes.contains(&Opcode::BrFalse));
    }

    #[test]
    fn emits_quote_as_syntax_constant() {
        let ir = lower_ir(
            r"
        export let quoted : Syntax := quote (#(1 + 2));
    ",
            "main",
        );

        let emitted = lower_ir_module(&ir, EmitOptions).expect("emit should succeed");
        assert!(emitted.artifact.validate().is_ok());
        assert!(emitted.artifact.constants.iter().any(|(_, constant)| {
            matches!(
                constant.value,
                ConstantValue::Syntax { shape: music_term::SyntaxShape::Expr, text }
                    if emitted.artifact.string_text(text).contains("#(1 + 2)")
            )
        }));
    }

    #[test]
    fn emits_named_type_values_as_ty_id() {
        let ir = lower_ir(
            r"
        export let ty : Type := Int;
    ",
            "main",
        );

        let emitted = lower_ir_module(&ir, EmitOptions).expect("emit should succeed");
        assert!(emitted.artifact.validate().is_ok());
        assert!(
            emitted
                .artifact
                .procedures
                .iter()
                .flat_map(|(_, procedure)| procedure.code.iter())
                .any(|entry| matches!(
                    entry,
                    CodeEntry::Instruction(instruction) if instruction.opcode == Opcode::LdType
                ))
        );
    }

    #[test]
    fn emits_records_with_projection_and_update() {
        let emitted = emit_module(
            r"
        export let result () : Int := (
          let r := { y := 2, x := 1 };
          let a : Int := r.x;
          let s := { ...r, x := 3 };
          a + s.x
        );
    ",
        )
        .expect("emit should succeed");
        assert!(emitted.artifact.validate().is_ok());
        let opcodes = emitted_opcodes(&emitted);
        assert!(opcodes.contains(&Opcode::NewObj));
        assert!(opcodes.contains(&Opcode::LdFld));
    }

    #[test]
    fn emits_foreign_calls() {
        let emitted = emit_module(
            r#"
        native "c" (
          let puts (value : Int) : Int;
        );
        export let result () : Int := unsafe { puts(1); };
    "#,
        )
        .expect("emit should succeed");
        assert!(emitted.artifact.validate().is_ok());
        assert!(
            emitted_opcodes(&emitted)
                .into_iter()
                .any(|opcode| opcode == Opcode::CallFfi)
        );
    }

    #[test]
    fn emits_closures_and_higher_order_calls() {
        let ir = lower_ir(
            r"
        let apply (f : Int -> Int, x : Int) : Int := f(x);

        export let result (x : Int) : Int := (
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
        for (_, procedure) in emitted.artifact.procedures.iter() {
            for entry in &procedure.code {
                let CodeEntry::Instruction(instruction) = entry else {
                    continue;
                };
                if instruction.opcode == Opcode::CallInd {
                    has_indirect_call = true;
                }
                if instruction.opcode == Opcode::NewFn {
                    if let music_seam::Operand::WideProcedureCaptures { captures, .. } =
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

    #[test]
    fn emits_local_recursive_callable_lets() {
        let ir = lower_ir(
            r"
        export let result (n : Int) : Int := (
          let rec loop (x : Int) : Int := match x (| 0 => 0 | _ => loop(x - 1));
          loop(n)
        );
    ",
            "main",
        );

        let emitted = lower_ir_module(&ir, EmitOptions).expect("emit should succeed");
        assert!(emitted.artifact.validate().is_ok());
        assert!(emitted.artifact.procedures.iter().any(|(_, procedure)| {
            emitted
                .artifact
                .string_text(procedure.name)
                .contains("loop")
        }));
    }

    #[test]
    fn emits_type_test_and_cast() {
        let emitted = emit_module(
            r"
        export let check (x : Any) : Bool := x :? Int;
        export let cast (x : Any) : Int := x :?> Int;
    ",
        )
        .expect("emit should succeed");
        assert!(emitted.artifact.validate().is_ok());
        let opcodes = emitted_opcodes(&emitted);
        assert!(opcodes.contains(&Opcode::IsInst));
        assert!(opcodes.contains(&Opcode::Cast));
    }

    #[test]
    fn emits_type_values_record_patterns_and_capturing_recursion() {
        let emitted = emit_module(
            r"
        export let result (n : Int) : Int := (
          let base := 1;
          let rec loop (x : Int) : Int := match x (| 0 => base | _ => loop(x - 1));
          let point := { x := 1, y := 2 };
          let picked : Int := match point (| { x } => x | _ => 0);
          picked + loop(n)
        );
    ",
        )
        .expect("emit should succeed");
        assert!(emitted.artifact.validate().is_ok());
        let opcodes = emitted_opcodes(&emitted);
        assert!(opcodes.contains(&Opcode::LdFld));
        assert!(opcodes.contains(&Opcode::CallInd));

        assert!(
            emitted.artifact.procedures.iter().any(|(_, procedure)| {
                procedure.code.iter().any(|entry| {
                    matches!(
                        entry,
                        CodeEntry::Instruction(music_seam::Instruction {
                            opcode: Opcode::NewFn,
                            operand: music_seam::Operand::WideProcedureCaptures { captures, .. },
                        }) if *captures > 0
                    )
                })
            }),
            "expected capturing recursive closure"
        );
    }

    #[test]
    fn emit_diag_kind_extracts_every_known_emit_code() {
        for code in 3500u16..=3517u16 {
            let diag = Diag::error(EmitDiagKind::EmitInvariantViolated.message())
                .with_code(DiagCode::new(code));
            let kind = emit_diag_kind(&diag).expect("all emit diagnostic codes must map to a kind");
            assert_eq!(kind.code().raw(), code);
        }
    }

    #[test]
    fn emit_diag_kind_is_code_based_not_message_based() {
        let diag = Diag::error(EmitDiagKind::EmitInvariantViolated.message())
            .with_code(EmitDiagKind::UnknownEffect.code());
        let kind = emit_diag_kind(&diag).expect("emit diagnostic code should map");
        assert_eq!(kind, EmitDiagKind::UnknownEffect);
    }
}

mod failure {
    use super::*;

    #[test]
    fn emit_diag_kind_rejects_unknown_emit_code() {
        let diag = Diag::error(EmitDiagKind::EmitInvariantViolated.message())
            .with_code(DiagCode::new(3999));

        assert_eq!(emit_diag_kind(&diag), None);
    }
}
