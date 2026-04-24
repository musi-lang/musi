#![allow(unused_imports)]

use crate::artifact::Artifact;
use crate::descriptor::{
    ConstantDescriptor, ConstantValue, DataDescriptor, DataVariantDescriptor, EffectDescriptor,
    EffectOpDescriptor, ForeignDescriptor, GlobalDescriptor, ProcedureDescriptor, ShapeDescriptor,
    TypeDescriptor,
};
use crate::instruction::{CodeEntry, Instruction, Label, Operand};
use crate::opcode::Opcode;
use crate::{AssemblyError, decode_binary, encode_binary, format_text, parse_text};

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
        let abort_name = artifact.intern_string("Abort");
        let abort_op_name = artifact.intern_string("abort");
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
        let effect_id = artifact.effects.alloc(EffectDescriptor::new(
            abort_name,
            Box::new([EffectOpDescriptor::new(abort_op_name, Box::new([]), int_ty)]),
        ));
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
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::Raise,
                        Operand::Effect {
                            effect: effect_id,
                            op: 0,
                        },
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
        let _ = effect_id;
        let _shape_id = artifact.shapes.alloc(ShapeDescriptor::new(abort_name));
        let _ = foreign_id;

        assert!(artifact.validate().is_ok());
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
    fn opcode_table_matches_bytecode_spec() {
        let spec = include_str!("../../../specs/seam/bytecode.md");
        let mut expected = Vec::new();
        for line in spec.lines() {
            let cells = line.split('|').map(str::trim).collect::<Vec<_>>();
            if cells.len() < 4 || !cells[1].starts_with('`') || cells[2] == "reserved" {
                continue;
            }
            let hex = cells[1].trim_matches('`');
            if hex.contains('-') || hex == "Hex" {
                continue;
            }
            let Ok(code) = u16::from_str_radix(hex, 16) else {
                continue;
            };
            let mnemonic = cells[2].trim_matches('`');
            if mnemonic.is_empty() {
                continue;
            }
            expected.push((code, mnemonic));
        }

        assert_eq!(expected.len(), 130);
        for (code, mnemonic) in expected {
            let opcode = Opcode::from_wire_code(code).expect("opcode from spec code");
            assert_eq!(opcode.mnemonic(), mnemonic);
            assert_eq!(Opcode::from_mnemonic(mnemonic), Some(opcode));
        }
        assert_eq!(Opcode::from_mnemonic("range.new"), None);
        assert_eq!(Opcode::from_mnemonic("range.has"), None);
        assert_eq!(Opcode::from_mnemonic("range.mat"), None);
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

    #[test]
    fn roundtrips_data_layout_metadata_through_binary_and_text() {
        let mut artifact = Artifact::new();
        let point_name = artifact.intern_string("main::Point");
        let point_ty = artifact
            .types
            .alloc(TypeDescriptor::new(point_name, point_name));
        let repr_c = artifact.intern_string("c");
        let _ = artifact.data.alloc(
            DataDescriptor::new(
                point_name,
                Box::new([DataVariantDescriptor::new(
                    point_name,
                    30,
                    Box::new([point_ty, point_ty]),
                )]),
            )
            .with_repr_kind(repr_c)
            .with_layout_align(8)
            .with_layout_pack(4)
            .with_frozen(true),
        );
        let procedure_name = artifact.intern_string("main::work");
        let _ = artifact.procedures.alloc(
            ProcedureDescriptor::new(procedure_name, 0, 0, Box::new([]))
                .with_hot(true)
                .with_export(true),
        );
        let foreign_name = artifact.intern_string("main::puts");
        let int_ty = artifact.intern_string("Int");
        let int_ty = artifact.types.alloc(TypeDescriptor::new(int_ty, int_ty));
        let abi = artifact.intern_string("c");
        let symbol = artifact.intern_string("puts");
        let _ = artifact.foreigns.alloc(
            ForeignDescriptor::new(foreign_name, Box::new([int_ty]), int_ty, abi, symbol)
                .with_cold(true),
        );

        let binary = encode_binary(&artifact).expect("binary encode should succeed");
        let decoded = decode_binary(&binary).expect("binary decode should succeed");
        let (_, binary_layout) = decoded
            .data_for_type(point_ty)
            .expect("binary data layout by type");
        assert_eq!(binary_layout.repr_kind, Some(repr_c));
        assert_eq!(binary_layout.layout_align, Some(8));
        assert_eq!(binary_layout.layout_pack, Some(4));
        assert!(binary_layout.frozen);
        assert_eq!(binary_layout.variants.len(), 1);
        assert_eq!(binary_layout.variants[0].tag, 30);
        assert_eq!(
            binary_layout.variants[0].field_tys.as_ref(),
            &[point_ty, point_ty]
        );
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
        assert!(!decoded_foreign.hot);
        assert!(decoded_foreign.cold);

        let text = format_text(&artifact);
        let parsed = parse_text(&text).expect("text parse should succeed");
        let (_, text_layout) = parsed
            .data_by_name("main::Point")
            .expect("text data layout by name");
        assert_eq!(text_layout.variant_count, 1);
        assert_eq!(text_layout.field_count, 2);
        assert_eq!(text_layout.layout_align, Some(8));
        assert_eq!(text_layout.layout_pack, Some(4));
        assert!(text_layout.frozen);
        assert_eq!(text_layout.variants.len(), 1);
        assert_eq!(text_layout.variants[0].tag, 30);
        assert_eq!(text_layout.variants[0].field_tys.len(), 2);
        assert!(text.contains("variant $main::Point tag 30"));
        assert!(text.contains(".procedure $main::work params 0 locals 0 export hot"));
        assert!(
            text.contains(
                ".native $main::puts param $Int result $Int abi \"c\" symbol \"puts\" cold"
            )
        );
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
}
