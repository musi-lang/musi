#![allow(unused_imports)]

use music_base::SourceId;
use music_base::diag::{Diag, DiagCode};
use music_ir::IrModule;
use music_ir_lower::lower_module;
use music_module::ModuleKey;
use music_names::Interner;
use music_resolve::{ResolveOptions, resolve_module};
use music_seam::descriptor::{ConstantValue, SafePointKind};
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

fn safe_point_kind_for_opcode(opcode: Opcode) -> Option<SafePointKind> {
    match opcode {
        Opcode::Call | Opcode::TailCall => Some(SafePointKind::Call),
        Opcode::CallInd => Some(SafePointKind::CallIndirect),
        Opcode::CallFfi => Some(SafePointKind::CallForeign),
        Opcode::NewFn | Opcode::NewObj | Opcode::NewArr => Some(SafePointKind::Allocation),
        _ => None,
    }
}

mod success {
    use super::*;

    #[test]
    fn emits_artifact_for_literal_exports_and_metadata() {
        let ir = lower_ir(
            r"
        let Option := data { | Some(Int) | None };
        @foreign(abi := .c)
        let puts (value : CString) : Int;
        export let result : Int := 42;
        export let forty_two () : Int := 42;
    ",
            "main",
        );

        let emitted = lower_ir_module(&ir, EmitOptions).expect("emit should succeed");
        assert!(emitted.artifact.validate().is_ok());
        assert_eq!(emitted.exports.len(), 2);
        assert!(!emitted.artifact.types.is_empty());
        assert_eq!(emitted.artifact.foreigns.len(), 1);
    }

    #[test]
    fn emits_procedure_param_type_metadata() {
        let emitted = emit_module(
            r"
        export let add (left : Int, right : Int) : Int := left + right;
    ",
        )
        .expect("emit should succeed");
        let procedure = emitted
            .artifact
            .procedures
            .iter()
            .find_map(|(_, procedure)| {
                emitted
                    .artifact
                    .string_text(procedure.name)
                    .ends_with("::add")
                    .then_some(procedure)
            })
            .expect("add procedure should exist");
        let param_tys = procedure
            .param_tys
            .iter()
            .map(|ty| emitted.artifact.type_name(*ty))
            .collect::<Vec<_>>();
        assert_eq!(param_tys, vec!["Int", "Int"]);
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
        export let boolAnd (left : Bit, right : Bit) : Bit := left and right;
        export let boolOr (left : Bit, right : Bit) : Bit := left or right;
        export let boolXor (left : Bit, right : Bit) : Bit := left xor right;
        export let bitsAnd (left : Bits[4], right : Bits[4]) : Bits[4] := left and right;
        export let bitsOr (left : Bits[4], right : Bits[4]) : Bits[4] := left or right;
        export let bitsXor (left : Bits[4], right : Bits[4]) : Bits[4] := left xor right;
        export let bitsNot (value : Bits[4]) : Bits[4] := not value;
    ",
            &[
                Opcode::BrZ,
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
                Opcode::BrZ,
            ],
        );
    }

    #[test]
    fn emits_generic_callable_param_name_refs() {
        let emitted = emit_module(
            r"
        export let equal [T] (actual : T, expected : T) : Bit :=
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
        let equal [T] (actual : T, expected : T) : Bit :=
          actual = expected;
        export let toBe (actual : Int, expected : Int) : Bit :=
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
        let fail (message : String) : Bit := 0 = 1;
        export let equal [T] (actual : T, expected : T) : Bit :=
          match () (
          | _ where actual = expected => 0 = 0
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
        let equal [T] (actual : T, expected : T) : Bit :=
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
            &[Opcode::LdModDyn],
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
        assert!(opcodes.contains(&Opcode::BrZ));
    }

    #[test]
    fn emits_quote_as_syntax_constant() {
        let emitted = emit_module(
            r##"
        export let quoted : String := "#(1 + 2)";
    "##,
        )
        .expect("emit should succeed");
        assert!(emitted.artifact.validate().is_ok());
        assert!(emitted.artifact.constants.iter().any(|(_, constant)| {
            matches!(
                constant.value,
                ConstantValue::String(text)
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
            r"
        @foreign(abi := .c)
        let puts (value : Int) : Int;
        export let result () : Int := unsafe (puts(1));
    ",
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
    fn emits_root_maps_for_known_call_and_allocation_safe_points() {
        let emitted = emit_module(
            r"
        @foreign(abi := .c)
        let puts (value : Int) : Int;
        let id (value : Int) : Int := value;
        let apply (f : Int -> Int, x : Int) : Int := f(x);
        export let result (x : Int) : Int := (
          let pair := { left := x, right := x + 1 };
          let items := [x, x + 1];
          let closure := id;
          let called := apply(closure, x);
          unsafe (puts(called))
        );
    ",
        )
        .expect("emit should succeed");
        assert!(emitted.artifact.validate().is_ok());

        let mut saw_call = false;
        let mut saw_call_indirect = false;
        let mut saw_call_foreign = false;
        let mut saw_allocation = false;

        for (procedure_id, procedure) in emitted.artifact.procedures.iter() {
            let expected_safe_points = procedure
                .code
                .iter()
                .filter_map(|entry| {
                    let CodeEntry::Instruction(instruction) = entry else {
                        return None;
                    };
                    safe_point_kind_for_opcode(instruction.opcode)
                })
                .collect::<Vec<_>>();

            let procedure_root_maps = emitted
                .artifact
                .root_maps
                .iter()
                .filter_map(|(root_map_id, descriptor)| {
                    (descriptor.procedure == Some(procedure_id))
                        .then_some((root_map_id, descriptor))
                })
                .collect::<Vec<_>>();

            if expected_safe_points.is_empty() {
                assert!(procedure.root_map_table.is_none());
                continue;
            }

            let first_root_map = procedure_root_maps
                .first()
                .map(|(root_map_id, _)| *root_map_id);
            assert_eq!(first_root_map, procedure.root_map_table);
            assert_eq!(procedure_root_maps.len(), expected_safe_points.len());

            let procedure_name = emitted.artifact.string_text(procedure.name);
            for ((_, descriptor), expected_kind) in
                procedure_root_maps.iter().zip(expected_safe_points.iter())
            {
                let safe_point_name = emitted.artifact.string_text(descriptor.safe_point);
                assert!(safe_point_name.starts_with(procedure_name));
                assert_eq!(descriptor.kind, *expected_kind);
                match descriptor.kind {
                    SafePointKind::Call => saw_call = true,
                    SafePointKind::CallIndirect => saw_call_indirect = true,
                    SafePointKind::CallForeign => saw_call_foreign = true,
                    SafePointKind::Allocation => saw_allocation = true,
                    SafePointKind::Collection
                    | SafePointKind::PinEnter
                    | SafePointKind::PinExit
                    | SafePointKind::Yield
                    | SafePointKind::Trap => {}
                }
            }
        }

        assert!(saw_call);
        assert!(saw_call_indirect);
        assert!(saw_call_foreign);
        assert!(saw_allocation);
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
                if instruction.opcode == Opcode::NewFn
                    && let music_seam::Operand::WideProcedureCaptures { captures, .. } =
                        &instruction.operand
                    && *captures != 0
                {
                    has_capturing_closure = true;
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
          let recur loop (x : Int) : Int := match x (| 0 => 0 | _ => loop(x - 1));
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
        export let check (x : Any) : Bit := 0 = 0;
        export let cast (x : Any) : Int := 42;
    ",
        )
        .expect("emit should succeed");
        assert!(emitted.artifact.validate().is_ok());
        let opcodes = emitted_opcodes(&emitted);
        assert!(opcodes.contains(&Opcode::Ceq));
    }

    #[test]
    fn emits_type_values_record_patterns_and_capturing_recursion() {
        let emitted = emit_module(
            r"
        export let result (n : Int) : Int := (
          let base := 1;
          let recur loop (x : Int) : Int := match x (| 0 => base | _ => loop(x - 1));
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
        for code in [
            3500u16, 3501, 3502, 3503, 3504, 3505, 3506, 3507, 3510, 3511, 3512, 3513, 3514, 3515,
            3516, 3517, 3518, 3519,
        ] {
            let diag = Diag::error(EmitDiagKind::EmitInvariantViolated.message())
                .with_code(DiagCode::new(code));
            let kind = emit_diag_kind(&diag).expect("all emit diagnostic codes must map to a kind");
            assert_eq!(kind.code().raw(), code);
        }
    }

    #[test]
    fn emit_diag_kind_is_code_based_not_message_based() {
        let diag = Diag::error(EmitDiagKind::EmitInvariantViolated.message())
            .with_code(EmitDiagKind::UnknownRecordType.code());
        let kind = emit_diag_kind(&diag).expect("emit diagnostic code should map");
        assert_eq!(kind, EmitDiagKind::UnknownRecordType);
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
