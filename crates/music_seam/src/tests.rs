use crate::artifact::Artifact;
use crate::descriptor::{
    ClassDescriptor, ConstantDescriptor, ConstantValue, DataDescriptor, DataVariantDescriptor,
    EffectDescriptor, EffectOpDescriptor, ForeignDescriptor, GlobalDescriptor, MethodDescriptor,
    TypeDescriptor,
};
use crate::instruction::{CodeEntry, Instruction, Label, Operand};
use crate::opcode::Opcode;
use crate::{decode_binary, encode_binary, format_text, parse_text};

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

    let int_ty = artifact.types.alloc(TypeDescriptor {
        name: int_name,
        term: int_name,
    });
    let const_id = artifact.constants.alloc(ConstantDescriptor {
        name: const_name,
        value: ConstantValue::Int(41),
    });
    let effect_id = artifact.effects.alloc(EffectDescriptor {
        name: abort_name,
        ops: Box::new([EffectOpDescriptor {
            name: abort_op_name,
            param_tys: Box::new([]),
            result_ty: int_ty,
        }]),
    });
    let foreign_id = artifact.foreigns.alloc(ForeignDescriptor {
        name: puts_name,
        param_tys: Box::new([int_ty]),
        result_ty: int_ty,
        abi: c_name,
        symbol: symbol_name,
        link: None,
        export: false,
    });
    let callee_id = artifact.methods.alloc(MethodDescriptor {
        name: callee_name,
        params: 0,
        locals: 0,
        export: false,
        labels: Box::new([]),
        code: Box::new([CodeEntry::Instruction(Instruction::new(
            Opcode::Ret,
            Operand::None,
        ))]),
    });
    let method_id = artifact.methods.alloc(MethodDescriptor {
        name: entry_name,
        params: 0,
        locals: 1,
        export: false,
        labels: Box::new([entry_name]),
        code: Box::new([
            CodeEntry::Label(Label { id: 0 }),
            CodeEntry::Instruction(Instruction::new(
                Opcode::LdConst,
                Operand::Constant(const_id),
            )),
            CodeEntry::Instruction(Instruction::new(Opcode::StLoc, Operand::Local(0))),
            CodeEntry::Instruction(Instruction::new(Opcode::LdLoc, Operand::Local(0))),
            CodeEntry::Instruction(Instruction::new(
                Opcode::SeqNew,
                Operand::TypeLen { ty: int_ty, len: 2 },
            )),
            CodeEntry::Instruction(Instruction::new(Opcode::SeqCat, Operand::None)),
            CodeEntry::Instruction(Instruction::new(
                Opcode::CallSeq,
                Operand::Method(callee_id),
            )),
            CodeEntry::Instruction(Instruction::new(Opcode::CallClsSeq, Operand::None)),
            CodeEntry::Instruction(Instruction::new(
                Opcode::FfiCallSeq,
                Operand::Foreign(foreign_id),
            )),
            CodeEntry::Instruction(Instruction::new(
                Opcode::EffInvkSeq,
                Operand::Effect {
                    effect: effect_id,
                    op: 0,
                },
            )),
            CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
        ]),
    });
    let _global_id = artifact.globals.alloc(GlobalDescriptor {
        name: answer_name,
        export: true,
        initializer: Some(method_id),
    });
    let _ = effect_id;
    let _class_id = artifact.classes.alloc(ClassDescriptor { name: abort_name });
    let _ = foreign_id;

    assert!(artifact.validate().is_ok());
}

#[test]
fn rejects_missing_label_reference() {
    let mut artifact = Artifact::new();
    let entry_name = artifact.intern_string("entry");
    let _method_id = artifact.methods.alloc(MethodDescriptor {
        name: entry_name,
        params: 0,
        locals: 0,
        export: false,
        labels: Box::new([entry_name]),
        code: Box::new([CodeEntry::Instruction(Instruction::new(
            Opcode::Br,
            Operand::Label(1),
        ))]),
    });

    assert!(artifact.validate().is_err());
}

#[test]
fn validates_float_constants() {
    let mut artifact = Artifact::new();
    let name = artifact.intern_string("pi.const");
    let _ = artifact.constants.alloc(ConstantDescriptor {
        name,
        value: ConstantValue::Float(3.5),
    });

    assert!(artifact.validate().is_ok());
}

#[test]
fn validates_global_and_sequence_operands() {
    let mut artifact = Artifact::new();
    let entry_name = artifact.intern_string("entry");
    let label_name = artifact.intern_string("L0");
    let global_name = artifact.intern_string("answer");
    let int_name = artifact.intern_string("Int");

    let int_ty = artifact.types.alloc(TypeDescriptor {
        name: int_name,
        term: int_name,
    });
    let global_id = artifact.globals.alloc(GlobalDescriptor {
        name: global_name,
        export: true,
        initializer: None,
    });
    let _ = artifact.methods.alloc(MethodDescriptor {
        name: entry_name,
        params: 0,
        locals: 1,
        export: false,
        labels: Box::new([label_name]),
        code: Box::new([
            CodeEntry::Label(Label { id: 0 }),
            CodeEntry::Instruction(Instruction::new(Opcode::LdGlob, Operand::Global(global_id))),
            CodeEntry::Instruction(Instruction::new(Opcode::StGlob, Operand::Global(global_id))),
            CodeEntry::Instruction(Instruction::new(
                Opcode::SeqNew,
                Operand::TypeLen { ty: int_ty, len: 2 },
            )),
            CodeEntry::Instruction(Instruction::new(Opcode::LdSmi, Operand::I16(0))),
            CodeEntry::Instruction(Instruction::new(Opcode::SeqGet, Operand::None)),
            CodeEntry::Instruction(Instruction::new(Opcode::LdSmi, Operand::I16(0))),
            CodeEntry::Instruction(Instruction::new(Opcode::LdSmi, Operand::I16(1))),
            CodeEntry::Instruction(Instruction::new(Opcode::SeqSet, Operand::None)),
            CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
        ]),
    });

    assert!(artifact.validate().is_ok());
}

#[test]
fn validates_closures_and_indirect_calls() {
    let mut artifact = Artifact::new();
    let entry_name = artifact.intern_string("entry");
    let closure_name = artifact.intern_string("closure");
    let label_name = artifact.intern_string("L0");

    let closure_method = artifact.methods.alloc(MethodDescriptor {
        name: closure_name,
        params: 0,
        locals: 0,
        export: false,
        labels: Box::new([]),
        code: Box::new([CodeEntry::Instruction(Instruction::new(
            Opcode::Ret,
            Operand::None,
        ))]),
    });

    let _ = artifact.methods.alloc(MethodDescriptor {
        name: entry_name,
        params: 0,
        locals: 0,
        export: false,
        labels: Box::new([label_name]),
        code: Box::new([
            CodeEntry::Label(Label { id: 0 }),
            CodeEntry::Instruction(Instruction::new(Opcode::LdSmi, Operand::I16(1))),
            CodeEntry::Instruction(Instruction::new(Opcode::LdSmi, Operand::I16(2))),
            CodeEntry::Instruction(Instruction::new(
                Opcode::ClsNew,
                Operand::WideMethodCaptures {
                    method: closure_method,
                    captures: 2,
                },
            )),
            CodeEntry::Instruction(Instruction::new(Opcode::CallCls, Operand::None)),
            CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
        ]),
    });

    assert!(artifact.validate().is_ok());
}

#[test]
fn resolves_data_descriptors_from_type_and_name() {
    let mut artifact = Artifact::new();
    let point_name = artifact.intern_string("main::Point");
    let point_ty = artifact.types.alloc(TypeDescriptor {
        name: point_name,
        term: point_name,
    });
    let point_data = artifact.data.alloc(DataDescriptor {
        name: point_name,
        variant_count: 1,
        field_count: 2,
        variants: Box::new([DataVariantDescriptor {
            name: point_name,
            field_tys: Box::new([point_ty, point_ty]),
        }]),
        repr_kind: None,
        layout_align: None,
        layout_pack: None,
    });

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
    let point_ty = artifact.types.alloc(TypeDescriptor {
        name: point_name,
        term: point_name,
    });
    let repr_c = artifact.intern_string("c");
    let _ = artifact.data.alloc(DataDescriptor {
        name: point_name,
        variant_count: 1,
        field_count: 2,
        variants: Box::new([DataVariantDescriptor {
            name: point_name,
            field_tys: Box::new([point_ty, point_ty]),
        }]),
        repr_kind: Some(repr_c),
        layout_align: Some(8),
        layout_pack: Some(4),
    });

    let binary = encode_binary(&artifact).expect("binary encode should succeed");
    let decoded = decode_binary(&binary).expect("binary decode should succeed");
    let (_, binary_layout) = decoded
        .data_for_type(point_ty)
        .expect("binary data layout by type");
    assert_eq!(binary_layout.repr_kind, Some(repr_c));
    assert_eq!(binary_layout.layout_align, Some(8));
    assert_eq!(binary_layout.layout_pack, Some(4));
    assert_eq!(binary_layout.variants.len(), 1);
    assert_eq!(
        binary_layout.variants[0].field_tys.as_ref(),
        &[point_ty, point_ty]
    );

    let text = format_text(&artifact);
    let parsed = parse_text(&text).expect("text parse should succeed");
    let (_, text_layout) = parsed
        .data_by_name("main::Point")
        .expect("text data layout by name");
    assert_eq!(text_layout.variant_count, 1);
    assert_eq!(text_layout.field_count, 2);
    assert_eq!(text_layout.layout_align, Some(8));
    assert_eq!(text_layout.layout_pack, Some(4));
    assert_eq!(text_layout.variants.len(), 1);
    assert_eq!(text_layout.variants[0].field_tys.len(), 2);
}
