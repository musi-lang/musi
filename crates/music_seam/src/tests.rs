#![allow(unused_imports)]

use crate::artifact::{Artifact, ArtifactError};
use crate::descriptor::{
    BlockSignatureDescriptor, ClosureDescriptor, ConstantDescriptor, ConstantValue, DataDescriptor,
    DataFieldDescriptor, DataVariantDescriptor, ExportDescriptor, ExportTarget, ForeignDescriptor,
    GlobalDescriptor, ObjectHeaderDescriptor, ProcedureCallingConvention, ProcedureDescriptor,
    ProcedureVisibility, RootMapDescriptor, SafePointKind, ShapeDescriptor, StackEffectDescriptor,
    TypeDescriptor,
};
use crate::instruction::{CodeEntry, Instruction, Label, Operand, OperandShape};
use crate::opcode::{Opcode, OpcodeVisibility};
use crate::{
    AssemblyError, decode_binary, encode_binary, format_debug_hil, format_decomp, format_disasm,
    parse_disasm,
};
use music_arena::Idx;

mod success {
    use super::*;

    #[test]
    fn string_interning_reuses_existing_record() {
        let mut artifact = Artifact::new();
        let first = artifact.intern_string("shared");
        let second = artifact.intern_string("shared");

        assert_eq!(first, second);
        assert_eq!(artifact.strings.len(), 1);
    }

    #[test]
    fn validates_well_formed_artifact() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let answer_name = artifact.intern_string("answer");
        let int_name = artifact.intern_string("Int");
        let const_name = artifact.intern_string("answer.const");
        let capability_name = artifact.intern_string("Abort");
        let puts_name = artifact.intern_string("puts");
        let c_name = artifact.intern_string("c");
        let symbol_name = artifact.intern_string("puts");
        let callee_name = artifact.intern_string("callee");

        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let const_id = artifact
            .constants
            .alloc(ConstantDescriptor::new(const_name, ConstantValue::Int(41)));
        let foreign_id = artifact.foreigns.alloc(ForeignDescriptor::new(
            puts_name,
            Box::new([int_ty]),
            int_ty,
            c_name,
            symbol_name,
        ));
        let callee_id = artifact.procedures.alloc(ProcedureDescriptor::new(
            callee_name,
            0,
            0,
            Box::new([CodeEntry::Instruction(Instruction::new(
                Opcode::Ret,
                Operand::None,
            ))]),
        ));
        let procedure_id = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                1,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::LdC,
                        Operand::Constant(const_id),
                    )),
                    CodeEntry::Instruction(Instruction::new(Opcode::StLoc, Operand::Local(0))),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdLoc, Operand::Local(0))),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::NewArr,
                        Operand::TypeLen { ty: int_ty, len: 2 },
                    )),
                    CodeEntry::Instruction(Instruction::new(Opcode::CallInd, Operand::None)),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::Call,
                        Operand::Procedure(callee_id),
                    )),
                    CodeEntry::Instruction(Instruction::new(Opcode::CallInd, Operand::None)),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::CallFfi,
                        Operand::Foreign(foreign_id),
                    )),
                    CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
                ]),
            )
            .with_labels(Box::new([entry_name])),
        );
        let _global_id = artifact.globals.alloc(
            GlobalDescriptor::new(answer_name)
                .with_export(true)
                .with_initializer(procedure_id),
        );
        let _shape_id = artifact.shapes.alloc(ShapeDescriptor::new(capability_name));
        let _ = foreign_id;

        assert!(artifact.validate().is_ok());
    }

    #[test]
    fn roundtrips_erased_shape_representation_metadata() {
        let mut artifact = Artifact::new();
        let shape_name = artifact.intern_string("Logger");
        let payload_name = artifact.intern_string("LoggerPayload");
        let payload_ty = artifact
            .types
            .alloc(TypeDescriptor::new(payload_name, payload_name));
        let witness = artifact.intern_string("LoggerWitness");
        let dispatch = artifact.intern_string("LoggerDispatch");
        let layout_name = artifact.intern_string("LoggerLayout");
        let layout_ty = artifact
            .types
            .alloc(TypeDescriptor::new(layout_name, layout_name));
        let _ = artifact.shapes.alloc(
            ShapeDescriptor::new(shape_name)
                .with_payload_ty(payload_ty)
                .with_witness(witness)
                .with_dispatch_table(dispatch)
                .with_layout_identity(layout_ty)
                .with_root_visible(true),
        );

        let binary = encode_binary(&artifact).expect("binary encode should succeed");
        let decoded = decode_binary(&binary).expect("binary decode should succeed");
        let (_, decoded_shape) = decoded
            .shapes
            .iter()
            .next()
            .expect("decoded shape should exist");
        assert_eq!(decoded_shape.payload_ty, Some(payload_ty));
        assert_eq!(decoded_shape.witness, Some(witness));
        assert_eq!(decoded_shape.dispatch_table, Some(dispatch));
        assert_eq!(decoded_shape.layout_identity, Some(layout_ty));
        assert!(decoded_shape.root_visible);

        let text = format_disasm(&artifact);
        assert!(text.contains(
            ".capability $Logger payload $LoggerPayload witness $LoggerWitness dispatch $LoggerDispatch layout $LoggerLayout root"
        ));
        let parsed = parse_disasm(&text).expect("text parse should succeed");
        let (_, parsed_shape) = parsed
            .shapes
            .iter()
            .next()
            .expect("parsed shape should exist");
        assert!(parsed_shape.payload_ty.is_some());
        assert!(parsed_shape.witness.is_some());
        assert!(parsed_shape.dispatch_table.is_some());
        assert!(parsed_shape.layout_identity.is_some());
        assert!(parsed_shape.root_visible);
    }

    #[test]
    fn validates_float_constants() {
        let mut artifact = Artifact::new();
        let name = artifact.intern_string("pi.const");
        let _ = artifact
            .constants
            .alloc(ConstantDescriptor::new(name, ConstantValue::Float(3.5)));

        assert!(artifact.validate().is_ok());
    }

    #[test]
    fn validates_global_and_sequence_operands() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let label_name = artifact.intern_string("L0");
        let global_name = artifact.intern_string("answer");
        let int_name = artifact.intern_string("Int");

        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let global_id = artifact
            .globals
            .alloc(GlobalDescriptor::new(global_name).with_export(true));
        let _ = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                1,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::LdGlob,
                        Operand::Global(global_id),
                    )),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::StGlob,
                        Operand::Global(global_id),
                    )),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::NewArr,
                        Operand::TypeLen { ty: int_ty, len: 2 },
                    )),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdCI4, Operand::I16(0))),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdElem, Operand::None)),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdCI4, Operand::I16(0))),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdCI4, Operand::I16(1))),
                    CodeEntry::Instruction(Instruction::new(Opcode::StElem, Operand::None)),
                    CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
                ]),
            )
            .with_labels(Box::new([label_name])),
        );

        assert!(artifact.validate().is_ok());
    }

    #[test]
    fn validates_closures_and_indirect_calls() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let closure_name = artifact.intern_string("closure");
        let label_name = artifact.intern_string("L0");

        let closure_procedure = artifact.procedures.alloc(ProcedureDescriptor::new(
            closure_name,
            0,
            0,
            Box::new([CodeEntry::Instruction(Instruction::new(
                Opcode::Ret,
                Operand::None,
            ))]),
        ));

        let _ = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                0,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdCI4, Operand::I16(1))),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdCI4, Operand::I16(2))),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::NewFn,
                        Operand::WideProcedureCaptures {
                            procedure: closure_procedure,
                            captures: 2,
                        },
                    )),
                    CodeEntry::Instruction(Instruction::new(Opcode::CallInd, Operand::None)),
                    CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
                ]),
            )
            .with_labels(Box::new([label_name])),
        );

        assert!(artifact.validate().is_ok());
    }

    #[test]
    fn roundtrips_closure_representation_metadata() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let closure_name = artifact.intern_string("entry::closure");
        let closure_descriptor_name = artifact.intern_string("entry::closure.value");
        let int_name = artifact.intern_string("Int");
        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let env_name = artifact.intern_string("entry::closure.env");
        let env_data = artifact.data.alloc(DataDescriptor::new(
            env_name,
            Box::new([DataVariantDescriptor::new(env_name, 0, Box::new([int_ty]))]),
        ));
        let closure_procedure = artifact.procedures.alloc(ProcedureDescriptor::new(
            closure_name,
            1,
            0,
            Box::new([CodeEntry::Instruction(Instruction::new(
                Opcode::Ret,
                Operand::None,
            ))]),
        ));
        let domain = artifact.intern_string("managed");
        let effect = artifact.intern_string("io");
        let _ = artifact.closures.alloc(
            ClosureDescriptor::new(closure_descriptor_name, closure_procedure, 1)
                .with_capture_tys(Box::new([int_ty]))
                .with_env_layout(env_data)
                .with_param_tys(Box::new([int_ty]))
                .with_result_tys(Box::new([int_ty]))
                .with_domain(domain)
                .with_effect(effect)
                .with_suspending(true),
        );
        let _ = artifact.procedures.alloc(ProcedureDescriptor::new(
            entry_name,
            0,
            0,
            Box::new([CodeEntry::Instruction(Instruction::new(
                Opcode::NewFn,
                Operand::WideProcedureCaptures {
                    procedure: closure_procedure,
                    captures: 1,
                },
            ))]),
        ));

        let binary = encode_binary(&artifact).expect("binary encode should succeed");
        let decoded = decode_binary(&binary).expect("binary decode should succeed");
        let (_, decoded_closure) = decoded
            .closures
            .iter()
            .next()
            .expect("decoded closure should exist");
        assert_eq!(decoded_closure.procedure, closure_procedure);
        assert_eq!(decoded_closure.capture_count, 1);
        assert_eq!(decoded_closure.capture_tys.as_ref(), &[int_ty]);
        assert_eq!(decoded_closure.env_layout, Some(env_data));
        assert_eq!(decoded_closure.param_tys.as_ref(), &[int_ty]);
        assert_eq!(decoded_closure.result_tys.as_ref(), &[int_ty]);
        assert_eq!(decoded_closure.domain, Some(domain));
        assert_eq!(decoded_closure.effect, Some(effect));
        assert!(decoded_closure.suspending);

        let text = format_disasm(&artifact);
        assert!(text.contains(
            ".closure $entry::closure.value procedure $entry::closure captures 1 capture $Int env $entry::closure.env param $Int result $Int domain \"managed\" effect \"io\" suspend"
        ));
        let parsed = parse_disasm(&text).expect("text parse should succeed");
        let (_, parsed_closure) = parsed
            .closures
            .iter()
            .next()
            .expect("parsed closure should exist");
        assert_eq!(parsed_closure.capture_count, 1);
        assert_eq!(parsed_closure.capture_tys.len(), 1);
        assert_eq!(parsed_closure.param_tys.len(), 1);
        assert_eq!(parsed_closure.result_tys.len(), 1);
        assert!(parsed_closure.domain.is_some());
        assert!(parsed_closure.effect.is_some());
        assert!(parsed_closure.suspending);
    }

    #[test]
    fn validates_root_map_descriptors() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let label_name = artifact.intern_string("L0");
        let safe_point = artifact.intern_string("entry.L0");
        let procedure_id = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                1,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
                ]),
            )
            .with_labels(Box::new([label_name])),
        );
        let _ = artifact.root_maps.alloc(
            RootMapDescriptor::new(safe_point, Box::new([0]), Box::new([0]))
                .with_kind(SafePointKind::CallIndirect)
                .with_procedure(procedure_id)
                .with_capture_slots(Box::new([1]))
                .with_defer_slots(Box::new([2]))
                .with_pin_slots(Box::new([3])),
        );

        assert!(artifact.validate().is_ok());
    }

    #[test]
    fn validates_tail_call_with_empty_cleanup_root_map() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("caller");
        let target_name = artifact.intern_string("callee");
        let safe_point = artifact.intern_string("caller.L0");
        let target_id =
            artifact
                .procedures
                .alloc(ProcedureDescriptor::new(target_name, 0, 0, Box::new([])));
        let entry_id =
            artifact
                .procedures
                .alloc(ProcedureDescriptor::new(entry_name, 0, 0, Box::new([])));
        let root_map = artifact.root_maps.alloc(
            RootMapDescriptor::new(safe_point, Box::new([]), Box::new([])).with_procedure(entry_id),
        );
        *artifact.procedures.get_mut(entry_id) = ProcedureDescriptor::new(
            entry_name,
            0,
            0,
            Box::new([CodeEntry::Instruction(Instruction::new(
                Opcode::TailCall,
                Operand::Procedure(target_id),
            ))]),
        )
        .with_root_map_table(root_map);

        assert!(artifact.validate().is_ok());
    }

    #[test]
    fn roundtrips_stack_effect_table_through_binary_and_text() {
        let mut artifact = Artifact::new();
        let effect_name = artifact.intern_string("core::add.i32");
        let int_name = artifact.intern_string("Int32");
        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let _ = artifact.stack_effects.alloc(StackEffectDescriptor::new(
            effect_name,
            Box::new([int_ty, int_ty]),
            Box::new([int_ty]),
        ));

        let binary = encode_binary(&artifact).expect("binary encode should succeed");
        let decoded = decode_binary(&binary).expect("binary decode should succeed");
        let (_, binary_effect) = decoded
            .stack_effects
            .iter()
            .next()
            .expect("binary stack effect should exist");
        assert_eq!(binary_effect.name, effect_name);
        assert_eq!(binary_effect.input_tys.as_ref(), &[int_ty, int_ty]);
        assert_eq!(binary_effect.output_tys.as_ref(), &[int_ty]);
        assert_eq!(binary_effect.input_top(), Some(int_ty));
        assert_eq!(binary_effect.output_top(), Some(int_ty));

        let text = format_disasm(&artifact);
        assert!(text.contains(".stack_effect $core::add.i32 input $Int32 $Int32 output $Int32"));
        let parsed = parse_disasm(&text).expect("text parse should succeed");
        let (_, text_effect) = parsed
            .stack_effects
            .iter()
            .next()
            .expect("text stack effect should exist");
        assert_eq!(text_effect.input_tys.len(), 2);
        assert_eq!(text_effect.output_tys.len(), 1);
        assert_eq!(text_effect.output_tys[0], int_ty);
        assert_eq!(text_effect.input_top(), Some(int_ty));
        assert_eq!(text_effect.output_top(), Some(int_ty));
    }

    #[test]
    fn roundtrips_block_signature_table_through_binary_and_text() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let label_name = artifact.intern_string("L0");
        let int_name = artifact.intern_string("Int32");
        let bool_name = artifact.intern_string("Bool");
        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let bool_ty = artifact
            .types
            .alloc(TypeDescriptor::new(bool_name, bool_name));
        let procedure = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                0,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
                ]),
            )
            .with_labels(Box::new([label_name]))
            .with_result_tys(Box::new([int_ty, bool_ty])),
        );
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(
                procedure,
                0,
                Box::new([int_ty, bool_ty]),
            ));

        let binary = encode_binary(&artifact).expect("binary encode should succeed");
        let decoded = decode_binary(&binary).expect("binary decode should succeed");
        let (_, decoded_signature) = decoded
            .block_signatures
            .iter()
            .next()
            .expect("decoded block signature should exist");
        assert_eq!(decoded_signature.procedure, procedure);
        assert_eq!(decoded_signature.label, 0);
        assert_eq!(decoded_signature.incoming_tys.as_ref(), &[int_ty, bool_ty]);

        let text = format_disasm(&artifact);
        assert!(text.contains(".block_sig procedure $entry label $L0 stack [$Int32 $Bool]"));
        let parsed = parse_disasm(&text).expect("text parse should succeed");
        let (_, parsed_signature) = parsed
            .block_signatures
            .iter()
            .next()
            .expect("parsed block signature should exist");
        assert_eq!(parsed_signature.incoming_tys.as_ref(), &[int_ty, bool_ty]);
    }

    #[test]
    fn opcode_table_matches_bytecode_spec() {
        let spec = include_str!("../../../specs/seam/bytecode.md");
        let mut expected = Vec::new();
        for line in spec.lines() {
            let mut cells = line.split('|').map(str::trim);
            let _ = cells.next();
            let Some(hex_cell) = cells.next() else {
                continue;
            };
            let Some(mnemonic_cell) = cells.next() else {
                continue;
            };
            let Some(operand_cell) = cells.next() else {
                continue;
            };
            if !hex_cell.starts_with('`') || mnemonic_cell == "reserved" {
                continue;
            }
            let hex = hex_cell.trim_matches('`');
            if hex.contains('-') || hex == "Hex" {
                continue;
            }
            let Ok(code) = u16::from_str_radix(hex, 16) else {
                continue;
            };
            let mnemonic = mnemonic_cell.trim_matches('`');
            if mnemonic.is_empty() {
                continue;
            }
            let Some(operand_shape) = spec_operand_shape(operand_cell) else {
                continue;
            };
            expected.push((code, mnemonic, operand_shape));
        }

        assert_eq!(expected.len(), 44);
        for (code, mnemonic, operand_shape) in expected {
            let opcode = Opcode::from_wire_code(code).expect("opcode from spec code");
            assert_eq!(opcode.mnemonic(), mnemonic);
            assert_eq!(opcode.operand_shape(), operand_shape);
            assert_eq!(Opcode::from_mnemonic(mnemonic), Some(opcode));
        }
        assert_eq!(Opcode::from_mnemonic("range.new"), None);
        assert_eq!(Opcode::from_mnemonic("range.has"), None);
        assert_eq!(Opcode::from_mnemonic("range.mat"), None);
    }

    #[test]
    fn opcode_table_declares_public_serialized_visibility() {
        let mut public_count = 0;
        for code in 0..=u16::from(u8::MAX) {
            if let Some(opcode) = Opcode::from_wire_code(code) {
                assert_eq!(opcode.visibility(), OpcodeVisibility::Public);
                assert!(opcode.is_public());
                assert!(!opcode.is_internal());
                public_count += 1;
            }
        }
        assert_eq!(public_count, 44);
    }

    #[test]
    fn accepts_branch_table_targets_with_common_incoming_stack() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let int_name = artifact.intern_string("Int");
        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let l0 = artifact.intern_string("L0");
        let l1 = artifact.intern_string("L1");
        let l2 = artifact.intern_string("L2");
        let procedure = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                0,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::BrTbl,
                        Operand::BranchTable(Box::new([1, 2])),
                    )),
                    CodeEntry::Label(Label { id: 1 }),
                    CodeEntry::Label(Label { id: 2 }),
                ]),
            )
            .with_labels(Box::new([l0, l1, l2])),
        );
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(
                procedure,
                1,
                Box::new([int_ty]),
            ));
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(
                procedure,
                2,
                Box::new([int_ty]),
            ));

        artifact.validate().expect("artifact should validate");
        let _ = encode_binary(&artifact).expect("binary encode should succeed");
    }

    #[test]
    fn validates_branch_stack_rules_with_sufficient_metadata() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let int_name = artifact.intern_string("Int");
        let bit_name = artifact.intern_string("Bit");
        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let bit_ty = artifact
            .types
            .alloc(TypeDescriptor::new(bit_name, bit_name));
        let l0 = artifact.intern_string("L0");
        let l1 = artifact.intern_string("L1");
        let l2 = artifact.intern_string("L2");
        let l3 = artifact.intern_string("L3");
        let procedure = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                3,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdLoc, Operand::Local(0))),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdLoc, Operand::Local(1))),
                    CodeEntry::Instruction(Instruction::new(Opcode::BrZ, Operand::Label(1))),
                    CodeEntry::Instruction(Instruction::new(Opcode::Br, Operand::Label(2))),
                    CodeEntry::Label(Label { id: 1 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::Br, Operand::Label(2))),
                    CodeEntry::Label(Label { id: 2 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdLoc, Operand::Local(2))),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::BrTbl,
                        Operand::BranchTable(Box::new([3, 3])),
                    )),
                    CodeEntry::Label(Label { id: 3 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
                ]),
            )
            .with_labels(Box::new([l0, l1, l2, l3]))
            .with_local_tys(Box::new([int_ty, bit_ty, int_ty]))
            .with_result_tys(Box::new([int_ty])),
        );
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(procedure, 0, Box::new([])));
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(
                procedure,
                1,
                Box::new([int_ty]),
            ));
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(
                procedure,
                2,
                Box::new([int_ty]),
            ));
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(
                procedure,
                3,
                Box::new([int_ty]),
            ));

        artifact.validate().expect("artifact should validate");
        let _ = encode_binary(&artifact).expect("binary encode should succeed");
    }

    #[test]
    fn resolves_data_descriptors_from_type_and_name() {
        let mut artifact = Artifact::new();
        let point_name = artifact.intern_string("main::Point");
        let point_ty = artifact
            .types
            .alloc(TypeDescriptor::new(point_name, point_name));
        let point_data = artifact.data.alloc(DataDescriptor::new(
            point_name,
            Box::new([DataVariantDescriptor::new(
                point_name,
                0,
                Box::new([point_ty, point_ty]),
            )]),
        ));

        let (from_ty_id, from_ty) = artifact.data_for_type(point_ty).expect("data by type");
        assert_eq!(from_ty_id, point_data);
        assert_eq!(from_ty.field_count, 2);

        let (from_name_id, from_name) = artifact.data_by_name("main::Point").expect("data by name");
        assert_eq!(from_name_id, point_data);
        assert_eq!(from_name.variant_count, 1);
    }

    fn build_data_layout_roundtrip_fixture() -> Artifact {
        let mut artifact = Artifact::new();
        let point_name = artifact.intern_string("main::Point");
        let point_ty = artifact
            .types
            .alloc(TypeDescriptor::new(point_name, point_name));
        let repr_c = artifact.intern_string("c");
        let x_name = artifact.intern_string("x");
        let y_name = artifact.intern_string("y");
        let inline_storage = artifact.intern_string("inline");
        let _ = artifact.data.alloc(
            DataDescriptor::new(
                point_name,
                Box::new([DataVariantDescriptor::new(
                    point_name,
                    30,
                    Box::new([point_ty, point_ty]),
                )
                .with_layout_fields(Box::new([
                    DataFieldDescriptor::new(point_ty, 0)
                        .with_name(x_name)
                        .with_offset(0)
                        .with_storage(inline_storage)
                        .with_mutable(true)
                        .with_gc_pointer(true)
                        .with_public(true),
                    DataFieldDescriptor::new(point_ty, 1)
                        .with_name(y_name)
                        .with_offset(8)
                        .with_storage(inline_storage)
                        .with_hidden(true),
                ]))
                .with_public(true)
                .with_hidden(true)]),
            )
            .with_repr_kind(repr_c)
            .with_layout_align(8)
            .with_layout_pack(4)
            .with_frozen(true)
            .with_object_header(
                ObjectHeaderDescriptor::new()
                    .with_layout_ty(point_ty)
                    .with_mark_bits(2)
                    .with_generation_bits(3)
                    .with_pinned(true)
                    .with_remembered(true)
                    .with_large(true)
                    .with_weak_capable(true)
                    .with_forwarding(true)
                    .with_size_field(true),
            ),
        );
        let procedure_name = artifact.intern_string("main::work");
        let _ = artifact.procedures.alloc(
            ProcedureDescriptor::new(procedure_name, 0, 0, Box::new([]))
                .with_hot(true)
                .with_export(true),
        );
        let foreign_name = artifact.intern_string("main::puts");
        let int_name = artifact.intern_string("Int");
        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let abi = artifact.intern_string("c");
        let symbol = artifact.intern_string("puts");
        let _ = artifact.foreigns.alloc(
            ForeignDescriptor::new(foreign_name, Box::new([int_ty]), int_ty, abi, symbol)
                .with_cold(true),
        );

        artifact
    }

    fn assert_binary_data_layout_roundtrip(artifact: &Artifact) {
        let binary = encode_binary(artifact).expect("binary encode should succeed");
        let decoded = decode_binary(&binary).expect("binary decode should succeed");
        let (_, binary_layout) = decoded
            .data_by_name("main::Point")
            .expect("binary data layout by name");
        assert!(binary_layout.variants.len() == 1);
        let variant = &binary_layout.variants[0];
        assert!(variant.layout_fields.len() == 2);
        let first_field = &variant.layout_fields[0];
        let second_field = &variant.layout_fields[1];
        let binary_header = binary_layout
            .object_header
            .as_ref()
            .expect("binary object header");

        assert_eq!(
            decoded.string_text(binary_layout.repr_kind.expect("repr kind")),
            "c"
        );
        assert_eq!(binary_layout.layout_align, Some(8));
        assert_eq!(binary_layout.layout_pack, Some(4));
        assert!(binary_layout.frozen);
        assert_eq!(binary_header.mark_bits, 2);
        assert_eq!(binary_header.generation_bits, 3);
        assert!(binary_header.shape_flags.pinned);
        assert!(binary_header.shape_flags.remembered);
        assert!(binary_header.shape_flags.large);
        assert!(binary_header.runtime_flags.weak_capable);
        assert!(binary_header.runtime_flags.forwarding);
        assert!(binary_header.runtime_flags.size_field);
        assert_eq!(binary_layout.variants.len(), 1);
        assert_eq!(variant.tag, 30);
        assert_eq!(variant.field_tys.len(), 2);
        assert_eq!(variant.layout_fields.len(), 2);
        assert!(variant.public);
        assert!(variant.hidden);
        assert_eq!(
            decoded.string_text(first_field.name.expect("x field name")),
            "x"
        );
        assert_eq!(first_field.offset, Some(0));
        assert_eq!(
            decoded.string_text(first_field.storage.expect("x storage")),
            "inline"
        );
        assert!(first_field.mutability.mutable);
        assert!(first_field.mutability.gc_pointer);
        assert!(first_field.visibility.public);
        assert!(second_field.visibility.hidden);
        let (_, decoded_procedure) = decoded
            .procedures
            .iter()
            .next()
            .expect("decoded procedure should exist");
        assert!(decoded_procedure.hot);
        assert!(!decoded_procedure.cold);
        let (_, decoded_foreign) = decoded
            .foreigns
            .iter()
            .next()
            .expect("decoded native should exist");
        assert!(!decoded_foreign.behavior.hot);
        assert!(decoded_foreign.cold);
    }

    fn assert_text_data_layout_roundtrip(artifact: &Artifact) {
        let text = format_disasm(artifact);
        let parsed = parse_disasm(&text).expect("text parse should succeed");
        let (_, text_layout) = parsed
            .data_by_name("main::Point")
            .expect("text data layout by name");
        assert!(text_layout.variants.len() == 1);
        let variant = &text_layout.variants[0];
        assert!(variant.layout_fields.len() == 2);
        let first_field = &variant.layout_fields[0];
        let second_field = &variant.layout_fields[1];

        assert_eq!(text_layout.variant_count, 1);
        assert_eq!(text_layout.field_count, 2);
        assert_eq!(text_layout.layout_align, Some(8));
        assert_eq!(text_layout.layout_pack, Some(4));
        assert!(text_layout.frozen);
        assert!(text_layout.object_header.is_some());
        assert_eq!(text_layout.variants.len(), 1);
        assert_eq!(variant.tag, 30);
        assert_eq!(variant.field_tys.len(), 2);
        assert_eq!(variant.layout_fields.len(), 2);
        assert!(variant.public);
        assert!(variant.hidden);
        assert!(first_field.mutability.mutable);
        assert!(first_field.mutability.gc_pointer);
        assert!(first_field.visibility.public);
        assert!(second_field.visibility.hidden);
        assert!(text.contains("variant $main::Point tag 30 public hidden"));
        assert!(text.contains(
            "layout_field name $x type $main::Point index 0 offset 0 storage \"inline\" mut gc public"
        ));
        assert!(text.contains(
            "layout_field name $y type $main::Point index 1 offset 8 storage \"inline\" hidden"
        ));
        assert!(text.contains(
            "header layout $main::Point mark_bits 2 generation_bits 3 pinned remembered large weak_capable forwarding size_field"
        ));
        assert!(text.contains(
            ".procedure $main::work params 0 locals 0 entry 0 body 0 callconv managed visibility private export hot"
        ));
        assert!(
            text.contains(
                ".native $main::puts param $Int result $Int abi \"c\" symbol \"puts\" cold"
            )
        );
    }

    #[test]
    fn roundtrips_data_layout_metadata_through_binary_and_text() {
        let artifact = build_data_layout_roundtrip_fixture();
        assert_binary_data_layout_roundtrip(&artifact);
        assert_text_data_layout_roundtrip(&artifact);
    }

    #[test]
    fn roundtrips_function_descriptor_fields_through_binary_and_text() {
        let mut artifact = Artifact::new();
        let procedure_name = artifact.intern_string("main::worker");
        let entry_label_name = artifact.intern_string("L0");
        let int_name = artifact.intern_string("Int");
        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let domain = artifact.intern_string("native");
        let procedure_id =
            artifact
                .procedures
                .alloc(ProcedureDescriptor::new(procedure_name, 1, 2, Box::new([])));
        let safe_point = artifact.intern_string("main::worker:L0");
        let root_map = artifact.root_maps.alloc(
            RootMapDescriptor::new(safe_point, Box::new([0]), Box::new([]))
                .with_procedure(procedure_id),
        );
        let block_signature = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(
                procedure_id,
                0,
                Box::new([int_ty]),
            ));
        *artifact.procedures.get_mut(procedure_id) = ProcedureDescriptor::new(
            procedure_name,
            1,
            2,
            Box::new([CodeEntry::Label(Label { id: 0 })]),
        )
        .with_param_tys(Box::new([int_ty]))
        .with_local_tys(Box::new([int_ty]))
        .with_result_tys(Box::new([int_ty]))
        .with_entry_label(0)
        .with_bytecode_body(7)
        .with_block_signature_table(block_signature)
        .with_root_map_table(root_map)
        .with_domain_requirements(Box::new([domain]))
        .with_calling_convention(ProcedureCallingConvention::RuntimeHelper)
        .with_visibility(ProcedureVisibility::ExternalExport)
        .with_export(true)
        .with_labels(Box::new([entry_label_name]));

        let binary = encode_binary(&artifact).expect("binary encode should succeed");
        let decoded = decode_binary(&binary).expect("binary decode should succeed");
        let (_, decoded_procedure) = decoded
            .procedures
            .iter()
            .next()
            .expect("decoded procedure should exist");
        assert_eq!(decoded_procedure.local_tys.as_ref(), &[int_ty]);
        assert_eq!(decoded_procedure.bytecode_body, 7);
        assert_eq!(
            decoded_procedure.calling_convention,
            ProcedureCallingConvention::RuntimeHelper
        );
        assert_eq!(
            decoded_procedure.visibility,
            ProcedureVisibility::ExternalExport
        );
        assert_eq!(decoded_procedure.domain_requirements.as_ref(), &[domain]);

        let text = format_disasm(&artifact);
        assert!(text.contains(
            ".procedure $main::worker params 1 param_types [$Int] locals 2 local_types [$Int] result [$Int] entry 0 body 7 block_table 0 root_map 0 domains [\"native\"] callconv runtime-helper visibility external-export export"
        ));
        let parsed = parse_disasm(&text).expect("text parse should succeed");
        let (_, parsed_procedure) = parsed
            .procedures
            .iter()
            .next()
            .expect("parsed procedure should exist");
        assert_eq!(parsed_procedure.local_tys.len(), 1);
        assert_eq!(parsed_procedure.bytecode_body, 7);
        assert_eq!(
            parsed_procedure.calling_convention,
            ProcedureCallingConvention::RuntimeHelper
        );
        assert_eq!(
            parsed_procedure.visibility,
            ProcedureVisibility::ExternalExport
        );
        assert_eq!(parsed_procedure.domain_requirements.len(), 1);
    }

    #[test]
    fn hil_projection_uses_profile_attribute_spelling() {
        let mut artifact = Artifact::new();
        let procedure_name = artifact.intern_string("main::work");
        let _ = artifact.procedures.alloc(
            ProcedureDescriptor::new(procedure_name, 0, 0, Box::new([]))
                .with_hot(true)
                .with_cold(true),
        );

        let projection = format_debug_hil(&artifact);
        assert!(projection.contains("@profile(level := .hot)"));
        assert!(projection.contains("@profile(level := .cold)"));
        assert!(!projection.contains(concat!("@", "hot")));
        assert!(!projection.contains(concat!("@", "cold")));
    }

    #[test]
    fn hil_projection_applies_hidden_decompilation_name_policy() {
        let mut artifact = Artifact::new();
        let private_data_name = artifact.intern_string("main::Secret");
        let public_data_name = artifact.intern_string("main::Public");
        let secret_variant = artifact.intern_string("SecretCase");
        let public_variant = artifact.intern_string("PublicCase");
        let private_procedure_name = artifact.intern_string("main::helper");
        let public_procedure_name = artifact.intern_string("main::api");
        let public_export_name = artifact.intern_string("Public");
        let public_type = artifact
            .types
            .alloc(TypeDescriptor::new(public_data_name, public_data_name));
        let _ = artifact.data.alloc(DataDescriptor::new(
            private_data_name,
            Box::new([
                DataVariantDescriptor::new(secret_variant, 0, Box::new([])).with_hidden(true)
            ]),
        ));
        let _ = artifact.data.alloc(DataDescriptor::new(
            public_data_name,
            Box::new([
                DataVariantDescriptor::new(public_variant, 0, Box::new([])).with_public(true)
            ]),
        ));
        let private_procedure = artifact.procedures.alloc(ProcedureDescriptor::new(
            private_procedure_name,
            0,
            0,
            Box::new([]),
        ));
        let public_procedure = artifact.procedures.alloc(
            ProcedureDescriptor::new(public_procedure_name, 0, 0, Box::new([])).with_export(true),
        );
        let _ = artifact.exports.alloc(ExportDescriptor::new(
            public_export_name,
            false,
            ExportTarget::Type(public_type),
        ));
        let _ = public_procedure;
        let _ = private_procedure;

        let projection = format_decomp(&artifact);
        assert!(projection.contains("data __t0"));
        assert!(projection.contains(".__v0("));
        assert!(projection.contains("data main::Public"));
        assert!(projection.contains(".PublicCase("));
        assert!(projection.contains("fn __f0("));
        assert!(projection.contains("fn main::api("));
        assert!(!projection.contains("main::Secret"));
        assert!(!projection.contains("SecretCase"));
        assert!(!projection.contains("main::helper"));
    }
}

fn spec_operand_shape(text: &str) -> Option<OperandShape> {
    match text {
        "none" => Some(OperandShape::None),
        "i16" => Some(OperandShape::I16),
        "u16" => Some(OperandShape::Local),
        "str" => Some(OperandShape::String),
        "type" => Some(OperandShape::Type),
        "const" => Some(OperandShape::Constant),
        "global" => Some(OperandShape::Global),
        "method" => Some(OperandShape::Procedure),
        "method,u8" => Some(OperandShape::WideProcedureCaptures),
        "foreign" => Some(OperandShape::Foreign),
        "block" => Some(OperandShape::Label),
        "type,u16" => Some(OperandShape::TypeLen),
        "btbl" => Some(OperandShape::BranchTable),
        _ => None,
    }
}

mod failure {
    use super::*;

    #[test]
    fn rejects_missing_label_reference() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let _procedure_id = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                0,
                Box::new([CodeEntry::Instruction(Instruction::new(
                    Opcode::Br,
                    Operand::Label(1),
                ))]),
            )
            .with_labels(Box::new([entry_name])),
        );

        assert!(artifact.validate().is_err());
    }

    #[test]
    fn rejects_older_binary_major_version() {
        let artifact = Artifact::new();
        let mut binary = encode_binary(&artifact).expect("binary encode should succeed");
        binary[4] = 12;
        binary[5] = 0;
        let err = decode_binary(&binary).expect_err("binary decode should fail");
        assert!(matches!(
            err,
            AssemblyError::UnsupportedBinaryVersion(version) if version == (12u32 << 16)
        ));
    }

    #[test]
    fn rejects_root_map_with_unknown_procedure_reference() {
        let mut artifact = Artifact::new();
        let safe_point = artifact.intern_string("entry.L0");
        let _ = artifact.root_maps.alloc(
            RootMapDescriptor::new(safe_point, Box::new([0]), Box::new([]))
                .with_procedure(Idx::from_raw(9)),
        );

        assert!(artifact.validate().is_err());
    }

    #[test]
    fn rejects_root_map_with_unknown_safe_point_label_when_labels_available() {
        let mut artifact = Artifact::new();
        let procedure_name = artifact.intern_string("entry");
        let label_name = artifact.intern_string("L0");
        let procedure = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                procedure_name,
                0,
                1,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
                ]),
            )
            .with_labels(Box::new([label_name])),
        );
        let safe_point = artifact.intern_string("entry.L1");
        let _ = artifact.root_maps.alloc(
            RootMapDescriptor::new(safe_point, Box::new([0]), Box::new([]))
                .with_procedure(procedure),
        );

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "root map safe point"
            })
        ));
    }

    #[test]
    fn rejects_root_map_with_out_of_bounds_local_slot() {
        let mut artifact = Artifact::new();
        let procedure_name = artifact.intern_string("entry");
        let label_name = artifact.intern_string("L0");
        let procedure = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                procedure_name,
                0,
                1,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
                ]),
            )
            .with_labels(Box::new([label_name])),
        );
        let safe_point = artifact.intern_string("entry.L0");
        let _ = artifact.root_maps.alloc(
            RootMapDescriptor::new(safe_point, Box::new([1]), Box::new([]))
                .with_procedure(procedure),
        );

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "root map local slots"
            })
        ));
    }

    #[test]
    fn rejects_root_map_with_out_of_bounds_stack_slot_when_metadata_available() {
        let mut artifact = Artifact::new();
        let procedure_name = artifact.intern_string("entry");
        let label_name = artifact.intern_string("L0");
        let procedure = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                procedure_name,
                0,
                0,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
                ]),
            )
            .with_labels(Box::new([label_name])),
        );
        let block_signature = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(procedure, 0, Box::new([])));
        *artifact.procedures.get_mut(procedure) = ProcedureDescriptor::new(
            procedure_name,
            0,
            0,
            Box::new([
                CodeEntry::Label(Label { id: 0 }),
                CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
            ]),
        )
        .with_labels(Box::new([label_name]))
        .with_block_signature_table(block_signature);
        let safe_point = artifact.intern_string("entry.L0");
        let _ = artifact.root_maps.alloc(
            RootMapDescriptor::new(safe_point, Box::new([]), Box::new([0]))
                .with_procedure(procedure),
        );

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "root map stack slots"
            })
        ));
    }

    #[test]
    fn rejects_root_map_with_out_of_bounds_capture_slot_when_metadata_available() {
        let mut artifact = Artifact::new();
        let procedure_name = artifact.intern_string("entry");
        let procedure =
            artifact
                .procedures
                .alloc(ProcedureDescriptor::new(procedure_name, 0, 0, Box::new([])));
        let closure_name = artifact.intern_string("entry::closure");
        let _ = artifact
            .closures
            .alloc(ClosureDescriptor::new(closure_name, procedure, 1));
        let safe_point = artifact.intern_string("entry.L0");
        let _ = artifact.root_maps.alloc(
            RootMapDescriptor::new(safe_point, Box::new([]), Box::new([]))
                .with_procedure(procedure)
                .with_capture_slots(Box::new([1])),
        );

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "root map capture slots"
            })
        ));
    }

    #[test]
    fn rejects_root_map_with_duplicate_local_slots() {
        let mut artifact = Artifact::new();
        let safe_point = artifact.intern_string("entry.L0");
        let _ = artifact.root_maps.alloc(RootMapDescriptor::new(
            safe_point,
            Box::new([0, 0]),
            Box::new([]),
        ));

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "root map local slots"
            })
        ));
    }

    #[test]
    fn rejects_root_map_with_duplicate_stack_slots() {
        let mut artifact = Artifact::new();
        let safe_point = artifact.intern_string("entry.L0");
        let _ = artifact.root_maps.alloc(RootMapDescriptor::new(
            safe_point,
            Box::new([]),
            Box::new([0, 0]),
        ));

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "root map stack slots"
            })
        ));
    }

    #[test]
    fn rejects_root_map_with_duplicate_capture_slots() {
        let mut artifact = Artifact::new();
        let safe_point = artifact.intern_string("entry.L0");
        let _ = artifact.root_maps.alloc(
            RootMapDescriptor::new(safe_point, Box::new([]), Box::new([]))
                .with_capture_slots(Box::new([0, 0])),
        );

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "root map capture slots"
            })
        ));
    }

    #[test]
    fn rejects_root_map_with_duplicate_defer_slots() {
        let mut artifact = Artifact::new();
        let safe_point = artifact.intern_string("entry.L0");
        let _ = artifact.root_maps.alloc(
            RootMapDescriptor::new(safe_point, Box::new([]), Box::new([]))
                .with_defer_slots(Box::new([0, 0])),
        );

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "root map defer slots"
            })
        ));
    }

    #[test]
    fn rejects_root_map_with_duplicate_pin_slots() {
        let mut artifact = Artifact::new();
        let safe_point = artifact.intern_string("entry.L0");
        let _ = artifact.root_maps.alloc(
            RootMapDescriptor::new(safe_point, Box::new([]), Box::new([]))
                .with_pin_slots(Box::new([0, 0])),
        );

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "root map pin slots"
            })
        ));
    }

    #[test]
    fn rejects_tail_call_without_cleanup_root_map_metadata() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("caller");
        let target_name = artifact.intern_string("callee");
        let target_id =
            artifact
                .procedures
                .alloc(ProcedureDescriptor::new(target_name, 0, 0, Box::new([])));
        let _ = artifact.procedures.alloc(ProcedureDescriptor::new(
            entry_name,
            0,
            0,
            Box::new([CodeEntry::Instruction(Instruction::new(
                Opcode::TailCall,
                Operand::Procedure(target_id),
            ))]),
        ));

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "tail-call cleanup root map"
            })
        ));
    }

    #[test]
    fn rejects_tail_call_with_pending_defer_cleanup_metadata() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("caller");
        let target_name = artifact.intern_string("callee");
        let safe_point = artifact.intern_string("caller.L0");
        let target_id =
            artifact
                .procedures
                .alloc(ProcedureDescriptor::new(target_name, 0, 0, Box::new([])));
        let entry_id =
            artifact
                .procedures
                .alloc(ProcedureDescriptor::new(entry_name, 0, 0, Box::new([])));
        let root_map = artifact.root_maps.alloc(
            RootMapDescriptor::new(safe_point, Box::new([]), Box::new([]))
                .with_procedure(entry_id)
                .with_defer_slots(Box::new([0])),
        );
        *artifact.procedures.get_mut(entry_id) = ProcedureDescriptor::new(
            entry_name,
            0,
            0,
            Box::new([CodeEntry::Instruction(Instruction::new(
                Opcode::TailCall,
                Operand::Procedure(target_id),
            ))]),
        )
        .with_root_map_table(root_map);

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "tail-call cleanup defer slots"
            })
        ));
    }

    #[test]
    fn rejects_tail_call_with_active_pin_cleanup_metadata() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("caller");
        let target_name = artifact.intern_string("callee");
        let safe_point = artifact.intern_string("caller.L0");
        let target_id =
            artifact
                .procedures
                .alloc(ProcedureDescriptor::new(target_name, 0, 0, Box::new([])));
        let entry_id =
            artifact
                .procedures
                .alloc(ProcedureDescriptor::new(entry_name, 0, 0, Box::new([])));
        let root_map = artifact.root_maps.alloc(
            RootMapDescriptor::new(safe_point, Box::new([]), Box::new([]))
                .with_procedure(entry_id)
                .with_pin_slots(Box::new([0])),
        );
        *artifact.procedures.get_mut(entry_id) = ProcedureDescriptor::new(
            entry_name,
            0,
            0,
            Box::new([CodeEntry::Instruction(Instruction::new(
                Opcode::TailCall,
                Operand::Procedure(target_id),
            ))]),
        )
        .with_root_map_table(root_map);

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "tail-call cleanup pin slots"
            })
        ));
    }

    #[test]
    fn rejects_stack_effect_with_unknown_name_reference() {
        let mut artifact = Artifact::new();
        let int_name = artifact.intern_string("Int");
        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let _ = artifact.stack_effects.alloc(StackEffectDescriptor::new(
            Idx::from_raw(999),
            Box::new([int_ty]),
            Box::new([int_ty]),
        ));

        assert!(artifact.validate().is_err());
        assert!(encode_binary(&artifact).is_err());
    }

    #[test]
    fn rejects_stack_effect_with_unknown_type_reference() {
        let mut artifact = Artifact::new();
        let effect_name = artifact.intern_string("core::bad");
        let _ = artifact.stack_effects.alloc(StackEffectDescriptor::new(
            effect_name,
            Box::new([Idx::from_raw(42)]),
            Box::new([]),
        ));

        assert!(artifact.validate().is_err());
        assert!(encode_binary(&artifact).is_err());
    }

    #[test]
    fn rejects_procedure_with_unknown_result_type_reference() {
        let mut artifact = Artifact::new();
        let procedure_name = artifact.intern_string("entry");
        let _ = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                procedure_name,
                0,
                0,
                Box::new([CodeEntry::Instruction(Instruction::new(
                    Opcode::Ret,
                    Operand::None,
                ))]),
            )
            .with_result_tys(Box::new([Idx::from_raw(99)])),
        );

        assert!(artifact.validate().is_err());
        assert!(encode_binary(&artifact).is_err());
    }

    #[test]
    fn rejects_procedure_with_unknown_param_type_reference() {
        let mut artifact = Artifact::new();
        let procedure_name = artifact.intern_string("entry");
        let _ = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                procedure_name,
                1,
                0,
                Box::new([CodeEntry::Instruction(Instruction::new(
                    Opcode::Ret,
                    Operand::None,
                ))]),
            )
            .with_param_tys(Box::new([Idx::from_raw(99)])),
        );

        assert!(artifact.validate().is_err());
        assert!(encode_binary(&artifact).is_err());
    }

    #[test]
    fn rejects_foreign_with_invalid_param_index_metadata() {
        let mut artifact = Artifact::new();
        let int_name = artifact.intern_string("Int");
        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let foreign_name = artifact.intern_string("main::puts");
        let abi = artifact.intern_string("c");
        let symbol = artifact.intern_string("puts");
        let _ = artifact.foreigns.alloc(
            ForeignDescriptor::new(foreign_name, Box::new([int_ty]), int_ty, abi, symbol)
                .with_pinned_params(Box::new([1])),
        );

        assert!(artifact.validate().is_err());
        assert!(encode_binary(&artifact).is_err());
    }

    #[test]
    fn rejects_root_map_with_too_many_local_slots() {
        let mut artifact = Artifact::new();
        let safe_point = artifact.intern_string("entry.L0");
        let _ = artifact.root_maps.alloc(RootMapDescriptor::new(
            safe_point,
            vec![0; usize::from(u16::MAX) + 1].into_boxed_slice(),
            Box::new([]),
        ));

        assert!(artifact.validate().is_err());
        assert!(encode_binary(&artifact).is_err());
    }

    #[test]
    fn rejects_text_root_map_with_unknown_procedure() {
        let text = ".root_map point $entry.L0 procedure $missing local %0\n";

        assert!(parse_disasm(text).is_err());
    }

    #[test]
    fn rejects_block_signature_with_invalid_label_reference() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let label_name = artifact.intern_string("L0");
        let procedure = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                0,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
                ]),
            )
            .with_labels(Box::new([label_name])),
        );
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(procedure, 1, Box::new([])));

        assert!(artifact.validate().is_err());
        assert!(encode_binary(&artifact).is_err());
    }

    #[test]
    fn rejects_block_signature_with_invalid_type_reference() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let label_name = artifact.intern_string("L0");
        let procedure = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                0,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
                ]),
            )
            .with_labels(Box::new([label_name])),
        );
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(
                procedure,
                0,
                Box::new([Idx::from_raw(42)]),
            ));

        assert!(artifact.validate().is_err());
        assert!(encode_binary(&artifact).is_err());
    }

    #[test]
    fn rejects_branch_table_targets_without_block_signatures() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let l0 = artifact.intern_string("L0");
        let l1 = artifact.intern_string("L1");
        let l2 = artifact.intern_string("L2");
        let procedure = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                0,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::BrTbl,
                        Operand::BranchTable(Box::new([1, 2])),
                    )),
                    CodeEntry::Label(Label { id: 1 }),
                    CodeEntry::Label(Label { id: 2 }),
                ]),
            )
            .with_labels(Box::new([l0, l1, l2])),
        );
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(procedure, 1, Box::new([])));

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "branch table target block signatures"
            })
        ));
        assert!(encode_binary(&artifact).is_err());
    }

    #[test]
    fn rejects_branch_table_targets_with_different_incoming_stacks() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let int_name = artifact.intern_string("Int");
        let bit_name = artifact.intern_string("Bit");
        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let bit_ty = artifact
            .types
            .alloc(TypeDescriptor::new(bit_name, bit_name));
        let l0 = artifact.intern_string("L0");
        let l1 = artifact.intern_string("L1");
        let l2 = artifact.intern_string("L2");
        let procedure = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                0,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::BrTbl,
                        Operand::BranchTable(Box::new([1, 2])),
                    )),
                    CodeEntry::Label(Label { id: 1 }),
                    CodeEntry::Label(Label { id: 2 }),
                ]),
            )
            .with_labels(Box::new([l0, l1, l2])),
        );
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(
                procedure,
                1,
                Box::new([int_ty]),
            ));
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(
                procedure,
                2,
                Box::new([bit_ty]),
            ));

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::BranchTableTargetStackMismatch { .. })
        ));
        assert!(encode_binary(&artifact).is_err());
    }

    #[test]
    fn rejects_branch_target_stack_mismatch_with_signatures() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let int_name = artifact.intern_string("Int");
        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let l0 = artifact.intern_string("L0");
        let l1 = artifact.intern_string("L1");
        let procedure = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                1,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdLoc, Operand::Local(0))),
                    CodeEntry::Instruction(Instruction::new(Opcode::Br, Operand::Label(1))),
                    CodeEntry::Label(Label { id: 1 }),
                ]),
            )
            .with_labels(Box::new([l0, l1]))
            .with_local_tys(Box::new([int_ty])),
        );
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(procedure, 0, Box::new([])));
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(procedure, 1, Box::new([])));

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "branch target incoming stack"
            })
        ));
        assert!(encode_binary(&artifact).is_err());
    }

    #[test]
    fn rejects_branch_false_target_stack_mismatch_with_signatures() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let int_name = artifact.intern_string("Int");
        let bit_name = artifact.intern_string("Bit");
        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let bit_ty = artifact
            .types
            .alloc(TypeDescriptor::new(bit_name, bit_name));
        let l0 = artifact.intern_string("L0");
        let l1 = artifact.intern_string("L1");
        let procedure = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                2,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdLoc, Operand::Local(0))),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdLoc, Operand::Local(1))),
                    CodeEntry::Instruction(Instruction::new(Opcode::BrZ, Operand::Label(1))),
                    CodeEntry::Label(Label { id: 1 }),
                ]),
            )
            .with_labels(Box::new([l0, l1]))
            .with_local_tys(Box::new([int_ty, bit_ty])),
        );
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(procedure, 0, Box::new([])));
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(procedure, 1, Box::new([])));

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "branch-false target incoming stack"
            })
        ));
        assert!(encode_binary(&artifact).is_err());
    }

    #[test]
    fn rejects_branch_table_current_stack_mismatch_with_signatures() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let int_name = artifact.intern_string("Int");
        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let l0 = artifact.intern_string("L0");
        let l1 = artifact.intern_string("L1");
        let l2 = artifact.intern_string("L2");
        let procedure = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                2,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdLoc, Operand::Local(0))),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdLoc, Operand::Local(1))),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::BrTbl,
                        Operand::BranchTable(Box::new([1, 2])),
                    )),
                    CodeEntry::Label(Label { id: 1 }),
                    CodeEntry::Label(Label { id: 2 }),
                ]),
            )
            .with_labels(Box::new([l0, l1, l2]))
            .with_local_tys(Box::new([int_ty, int_ty])),
        );
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(procedure, 0, Box::new([])));
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(procedure, 1, Box::new([])));
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(procedure, 2, Box::new([])));

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "branch table incoming stack"
            })
        ));
        assert!(encode_binary(&artifact).is_err());
    }

    #[test]
    fn rejects_return_stack_mismatch_with_signatures() {
        let mut artifact = Artifact::new();
        let entry_name = artifact.intern_string("entry");
        let int_name = artifact.intern_string("Int");
        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let l0 = artifact.intern_string("L0");
        let procedure = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                0,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
                ]),
            )
            .with_labels(Box::new([l0]))
            .with_result_tys(Box::new([int_ty])),
        );
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(procedure, 0, Box::new([])));

        assert!(matches!(
            artifact.validate(),
            Err(ArtifactError::InvalidReference {
                table: "return result stack"
            })
        ));
        assert!(encode_binary(&artifact).is_err());
    }
}
