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
    let callee_id = artifact.methods.alloc(MethodDescriptor::new(
        callee_name,
        0,
        0,
        Box::new([CodeEntry::Instruction(Instruction::new(
            Opcode::Ret,
            Operand::None,
        ))]),
    ));
    let method_id = artifact.methods.alloc(
        MethodDescriptor::new(
            entry_name,
            0,
            1,
            Box::new([
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
        )
        .with_labels(Box::new([entry_name])),
    );
    let _global_id = artifact.globals.alloc(
        GlobalDescriptor::new(answer_name)
            .with_export(true)
            .with_initializer(method_id),
    );
    let _ = effect_id;
    let _class_id = artifact.classes.alloc(ClassDescriptor::new(abort_name));
    let _ = foreign_id;

    assert!(artifact.validate().is_ok());
}

#[test]
fn rejects_missing_label_reference() {
    let mut artifact = Artifact::new();
    let entry_name = artifact.intern_string("entry");
    let _method_id = artifact.methods.alloc(
        MethodDescriptor::new(
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
    let _ = artifact.methods.alloc(
        MethodDescriptor::new(
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

    let closure_method = artifact.methods.alloc(MethodDescriptor::new(
        closure_name,
        0,
        0,
        Box::new([CodeEntry::Instruction(Instruction::new(
            Opcode::Ret,
            Operand::None,
        ))]),
    ));

    let _ = artifact.methods.alloc(
        MethodDescriptor::new(
            entry_name,
            0,
            0,
            Box::new([
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
        )
        .with_labels(Box::new([label_name])),
    );

    assert!(artifact.validate().is_ok());
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
    let method_name = artifact.intern_string("main::work");
    let _ = artifact.methods.alloc(
        MethodDescriptor::new(method_name, 0, 0, Box::new([]))
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
    let (_, decoded_method) = decoded
        .methods
        .iter()
        .next()
        .expect("decoded method should exist");
    assert!(decoded_method.hot);
    assert!(!decoded_method.cold);
    let (_, decoded_foreign) = decoded
        .foreigns
        .iter()
        .next()
        .expect("decoded foreign should exist");
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
    assert!(text.contains(".method $main::work params 0 locals 0 export hot"));
    assert!(
        text.contains(".foreign $main::puts param $Int result $Int abi \"c\" symbol \"puts\" cold")
    );
}
