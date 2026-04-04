use crate::artifact::Artifact;
use crate::descriptor::{
    ClassDescriptor, ConstantDescriptor, ConstantValue, EffectDescriptor, EffectOpDescriptor,
    ForeignDescriptor, GlobalDescriptor, MethodDescriptor, TypeDescriptor,
};
use crate::instruction::{CodeEntry, Instruction, Label, Operand};
use crate::opcode::Opcode;

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

    let int_ty = artifact.types.alloc(TypeDescriptor { name: int_name });
    let const_id = artifact.constants.alloc(ConstantDescriptor {
        name: const_name,
        value: ConstantValue::Int(41),
    });
    let method_id = artifact.methods.alloc(MethodDescriptor {
        name: entry_name,
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
            CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
        ]),
    });
    let _global_id = artifact.globals.alloc(GlobalDescriptor {
        name: answer_name,
        export: true,
        initializer: Some(method_id),
    });
    let _effect_id = artifact.effects.alloc(EffectDescriptor {
        name: abort_name,
        ops: Box::new([EffectOpDescriptor {
            name: abort_op_name,
        }]),
    });
    let _class_id = artifact.classes.alloc(ClassDescriptor { name: abort_name });
    let _foreign_id = artifact.foreigns.alloc(ForeignDescriptor {
        name: puts_name,
        abi: c_name,
        symbol: symbol_name,
    });

    assert!(artifact.validate().is_ok());
}

#[test]
fn rejects_missing_label_reference() {
    let mut artifact = Artifact::new();
    let entry_name = artifact.intern_string("entry");
    let _method_id = artifact.methods.alloc(MethodDescriptor {
        name: entry_name,
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
