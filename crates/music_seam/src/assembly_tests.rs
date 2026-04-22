#![allow(unused_imports)]

use crate::descriptor::{
    ConstantDescriptor, ConstantValue, EffectDescriptor, EffectOpDescriptor, ForeignDescriptor,
    GlobalDescriptor, MetaDescriptor, ProcedureDescriptor, TypeDescriptor,
};
use crate::{Artifact, CodeEntry, Instruction, Label, Opcode, Operand, SeamDiagKind};
use crate::{decode_binary, encode_binary, format_text, parse_text};
use music_term::SyntaxShape;

fn sample_artifact() -> Artifact {
    let mut artifact = Artifact::new();
    let entry = artifact.intern_string("entry");
    let label = artifact.intern_string("L0");
    let answer = artifact.intern_string("answer");
    let int = artifact.intern_string("Int");
    let constant_name = artifact.intern_string("answer.const");
    let int_ty = artifact.types.alloc(TypeDescriptor::new(int, int));
    let constant = artifact.constants.alloc(ConstantDescriptor::new(
        constant_name,
        ConstantValue::Int(41),
    ));
    let procedure = artifact.procedures.alloc(
        ProcedureDescriptor::new(
            entry,
            0,
            1,
            Box::new([
                CodeEntry::Label(Label { id: 0 }),
                CodeEntry::Instruction(Instruction::new(Opcode::LdC, Operand::Constant(constant))),
                CodeEntry::Instruction(Instruction::new(Opcode::StLoc, Operand::Local(0))),
                CodeEntry::Instruction(Instruction::new(Opcode::LdLoc, Operand::Local(0))),
                CodeEntry::Instruction(Instruction::new(
                    Opcode::NewArr,
                    Operand::TypeLen { ty: int_ty, len: 2 },
                )),
                CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
            ]),
        )
        .with_labels(Box::new([label])),
    );
    let _ = artifact.globals.alloc(
        GlobalDescriptor::new(answer)
            .with_export(true)
            .with_initializer(procedure),
    );
    artifact
}

mod success {
    use super::*;

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
        let _ = artifact
            .types
            .alloc(TypeDescriptor::new(record_name, record_name));

        let text = format_text(&artifact);
        let decoded = parse_text(&text).unwrap();
        assert_eq!(format_text(&decoded), text);
    }

    #[test]
    fn float_constants_roundtrip_in_text_and_binary() {
        let mut artifact = Artifact::new();
        let float_name = artifact.intern_string("Float");
        let constant_name = artifact.intern_string("pi.const");
        let _ = artifact
            .types
            .alloc(TypeDescriptor::new(float_name, float_name));
        let _ = artifact.constants.alloc(ConstantDescriptor::new(
            constant_name,
            ConstantValue::Float(3.5),
        ));

        let text = format_text(&artifact);
        let parsed = parse_text(&text).unwrap();
        assert_eq!(format_text(&parsed), text);

        let bytes = encode_binary(&artifact).unwrap();
        let decoded = decode_binary(&bytes).unwrap();
        assert_eq!(decoded, artifact);
    }

    #[test]
    fn syntax_constants_roundtrip_in_text_and_binary() {
        let mut artifact = Artifact::new();
        let syntax_name = artifact.intern_string("quoted.const");
        let syntax_text = artifact.intern_string("#(1 + 2)");
        let _ = artifact.constants.alloc(ConstantDescriptor::new(
            syntax_name,
            ConstantValue::Syntax {
                shape: SyntaxShape::Expr,
                text: syntax_text,
            },
        ));

        let text = format_text(&artifact);
        assert!(text.contains("syntax expr \"#(1 + 2)\""));

        let parsed = parse_text(&text).unwrap();
        assert_eq!(format_text(&parsed), text);

        let bytes = encode_binary(&artifact).unwrap();
        let decoded = decode_binary(&bytes).unwrap();
        assert_eq!(decoded, artifact);
    }

    #[test]
    fn foreign_link_roundtrips_in_text_and_binary() {
        let mut artifact = Artifact::new();
        let name = artifact.intern_string("main::puts");
        let abi = artifact.intern_string("c");
        let symbol = artifact.intern_string("puts");
        let link = artifact.intern_string("c");
        let int_name = artifact.intern_string("Int");
        let int_ty = artifact
            .types
            .alloc(TypeDescriptor::new(int_name, int_name));
        let _ = artifact.foreigns.alloc(
            ForeignDescriptor::new(name, Box::new([]), int_ty, abi, symbol)
                .with_link(link)
                .with_export(true),
        );

        let text = format_text(&artifact);
        let parsed = parse_text(&text).unwrap();
        assert_eq!(format_text(&parsed), text);

        let bytes = encode_binary(&artifact).unwrap();
        let decoded = decode_binary(&bytes).unwrap();
        assert_eq!(decoded, artifact);
    }

    #[test]
    fn meta_roundtrips_in_text_and_binary() {
        let mut artifact = sample_artifact();
        let target = artifact.intern_string("main::answer");
        let key = artifact.intern_string("inert.attr");
        let value = artifact.intern_string("@foo.bar(baz = \"qux\")");
        let _ = artifact
            .meta
            .alloc(MetaDescriptor::new(target, key, Box::new([value])));

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
        let int_ty = artifact.types.alloc(TypeDescriptor::new(int, int));
        let global = artifact
            .globals
            .alloc(GlobalDescriptor::new(answer).with_export(true));
        let _ = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry,
                0,
                1,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::LdGlob,
                        Operand::Global(global),
                    )),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::StGlob,
                        Operand::Global(global),
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
            .with_labels(Box::new([label])),
        );

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

        let closure_procedure = artifact.procedures.alloc(ProcedureDescriptor::new(
            closure,
            0,
            0,
            Box::new([CodeEntry::Instruction(Instruction::new(
                Opcode::Ret,
                Operand::None,
            ))]),
        ));

        let _ = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry,
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
            .with_labels(Box::new([label])),
        );

        let text = format_text(&artifact);
        let parsed = parse_text(&text).unwrap();
        assert_eq!(format_text(&parsed), text);

        let bytes = encode_binary(&artifact).unwrap();
        let decoded = decode_binary(&bytes).unwrap();
        assert_eq!(decoded, artifact);
    }

    #[test]
    fn seq_call_and_arity_metadata_roundtrip_in_text_and_binary() {
        let mut artifact = Artifact::new();
        let int = artifact.intern_string("Int");
        let _int_ty = artifact.types.alloc(TypeDescriptor::new(int, int));

        let callee_name = artifact.intern_string("callee");
        let callee = artifact.procedures.alloc(ProcedureDescriptor::new(
            callee_name,
            2,
            0,
            Box::new([CodeEntry::Instruction(Instruction::new(
                Opcode::Ret,
                Operand::None,
            ))]),
        ));

        let effect_name = artifact.intern_string("Abort");
        let op_name = artifact.intern_string("abort");
        let int_ty = artifact
            .types
            .iter()
            .next()
            .map(|(id, _)| id)
            .expect("sample artifact type");
        let effect = artifact.effects.alloc(EffectDescriptor::new(
            effect_name,
            Box::new(
                [EffectOpDescriptor::new(op_name, Box::new([int_ty]), int_ty)
                    .with_comptime_safe(true)],
            ),
        ));

        let foreign_name = artifact.intern_string("puts");
        let c_abi = artifact.intern_string("c");
        let symbol = artifact.intern_string("puts");
        let foreign = artifact.foreigns.alloc(ForeignDescriptor::new(
            foreign_name,
            Box::new([int_ty]),
            int_ty,
            c_abi,
            symbol,
        ));

        let entry_name = artifact.intern_string("entry");
        let label = artifact.intern_string("L0");
        let _ = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                entry_name,
                0,
                0,
                Box::new([
                    CodeEntry::Label(Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::CallInd, Operand::None)),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::Call,
                        Operand::Procedure(callee),
                    )),
                    CodeEntry::Instruction(Instruction::new(Opcode::CallInd, Operand::None)),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::CallFfi,
                        Operand::Foreign(foreign),
                    )),
                    CodeEntry::Instruction(Instruction::new(
                        Opcode::Raise,
                        Operand::Effect { effect, op: 0 },
                    )),
                    CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
                ]),
            )
            .with_labels(Box::new([label])),
        );

        let text = format_text(&artifact);
        let parsed = parse_text(&text).unwrap();
        assert_eq!(format_text(&parsed), text);
        assert!(parsed.effects.get(effect).ops[0].is_comptime_safe);

        let bytes = encode_binary(&artifact).unwrap();
        let decoded = decode_binary(&bytes).unwrap();
        assert_eq!(decoded, artifact);
    }
}

mod failure {
    use super::*;

    #[test]
    fn rejects_invalid_binary_magic() {
        let err = decode_binary(b"not-seam").expect_err("invalid magic should reject");

        assert_eq!(err.to_string(), SeamDiagKind::InvalidBinaryHeader.message());
    }
}
