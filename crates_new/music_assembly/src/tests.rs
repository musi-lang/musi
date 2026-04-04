use music_bc::descriptor::{
    ConstantDescriptor, ConstantValue, GlobalDescriptor, MethodDescriptor, TypeDescriptor,
};
use music_bc::{Artifact, CodeEntry, Instruction, Label, Opcode, Operand};

use crate::{decode_binary, encode_binary, format_text, parse_text};

fn sample_artifact() -> Artifact {
    let mut artifact = Artifact::new();
    let entry = artifact.intern_string("entry");
    let label = artifact.intern_string("L0");
    let answer = artifact.intern_string("answer");
    let int = artifact.intern_string("Int");
    let constant_name = artifact.intern_string("answer.const");
    let int_ty = artifact.types.alloc(TypeDescriptor { name: int });
    let constant = artifact.constants.alloc(ConstantDescriptor {
        name: constant_name,
        value: ConstantValue::Int(41),
    });
    let method = artifact.methods.alloc(MethodDescriptor {
        name: entry,
        locals: 1,
        export: false,
        labels: Box::new([label]),
        code: Box::new([
            CodeEntry::Label(Label { id: 0 }),
            CodeEntry::Instruction(Instruction::new(
                Opcode::LdConst,
                Operand::Constant(constant),
            )),
            CodeEntry::Instruction(Instruction::new(Opcode::StLoc, Operand::Local(0))),
            CodeEntry::Instruction(Instruction::new(Opcode::LdLoc, Operand::Local(0))),
            CodeEntry::Instruction(Instruction::new(
                Opcode::SeqNew,
                Operand::TypeLen { ty: int_ty, len: 2 },
            )),
            CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
        ]),
    });
    let _ = artifact.globals.alloc(GlobalDescriptor {
        name: answer,
        export: true,
        initializer: Some(method),
    });
    artifact
}

#[test]
fn binary_roundtrip_preserves_artifact_shape() {
    let artifact = sample_artifact();
    let bytes = encode_binary(&artifact).unwrap();
    let decoded = decode_binary(&bytes).unwrap();

    assert_eq!(decoded, artifact);
}

#[test]
fn text_roundtrip_preserves_artifact_shape() {
    let artifact = sample_artifact();
    let text = format_text(&artifact);
    let decoded = parse_text(&text).unwrap();

    assert_eq!(format_text(&decoded), text);
}

#[test]
fn text_roundtrip_supports_quoted_symbolic_names() {
    let mut artifact = sample_artifact();
    let record_name = artifact.intern_string("{ x: Int; y: Int }");
    let _ = artifact.types.alloc(TypeDescriptor { name: record_name });

    let text = format_text(&artifact);
    let decoded = parse_text(&text).unwrap();
    assert_eq!(format_text(&decoded), text);
}

#[test]
fn float_constants_roundtrip_in_text_and_binary() {
    let mut artifact = Artifact::new();
    let float_name = artifact.intern_string("Float");
    let constant_name = artifact.intern_string("pi.const");
    let _ = artifact.types.alloc(TypeDescriptor { name: float_name });
    let _ = artifact.constants.alloc(ConstantDescriptor {
        name: constant_name,
        value: ConstantValue::Float(3.5),
    });

    let text = format_text(&artifact);
    let parsed = parse_text(&text).unwrap();
    assert_eq!(format_text(&parsed), text);

    let bytes = encode_binary(&artifact).unwrap();
    let decoded = decode_binary(&bytes).unwrap();
    assert_eq!(decoded, artifact);
}

#[test]
fn global_and_sequence_operands_roundtrip_in_text_and_binary() {
    let mut artifact = Artifact::new();
    let entry = artifact.intern_string("entry");
    let label = artifact.intern_string("L0");
    let answer = artifact.intern_string("answer");
    let int = artifact.intern_string("Int");
    let int_ty = artifact.types.alloc(TypeDescriptor { name: int });
    let global = artifact.globals.alloc(GlobalDescriptor {
        name: answer,
        export: true,
        initializer: None,
    });
    let _ = artifact.methods.alloc(MethodDescriptor {
        name: entry,
        locals: 1,
        export: false,
        labels: Box::new([label]),
        code: Box::new([
            CodeEntry::Label(Label { id: 0 }),
            CodeEntry::Instruction(Instruction::new(Opcode::LdGlob, Operand::Global(global))),
            CodeEntry::Instruction(Instruction::new(Opcode::StGlob, Operand::Global(global))),
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

    let text = format_text(&artifact);
    let parsed = parse_text(&text).unwrap();
    assert_eq!(format_text(&parsed), text);

    let bytes = encode_binary(&artifact).unwrap();
    let decoded = decode_binary(&bytes).unwrap();
    assert_eq!(decoded, artifact);
}

#[test]
fn closures_roundtrip_in_text_and_binary() {
    let mut artifact = Artifact::new();
    let entry = artifact.intern_string("entry");
    let closure = artifact.intern_string("closure");
    let label = artifact.intern_string("L0");

    let closure_method = artifact.methods.alloc(MethodDescriptor {
        name: closure,
        locals: 0,
        export: false,
        labels: Box::new([]),
        code: Box::new([CodeEntry::Instruction(Instruction::new(
            Opcode::Ret,
            Operand::None,
        ))]),
    });

    let _ = artifact.methods.alloc(MethodDescriptor {
        name: entry,
        locals: 0,
        export: false,
        labels: Box::new([label]),
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

    let text = format_text(&artifact);
    let parsed = parse_text(&text).unwrap();
    assert_eq!(format_text(&parsed), text);

    let bytes = encode_binary(&artifact).unwrap();
    let decoded = decode_binary(&bytes).unwrap();
    assert_eq!(decoded, artifact);
}
