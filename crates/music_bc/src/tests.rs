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
    let callee_name = artifact.intern_string("callee");

    let int_ty = artifact.types.alloc(TypeDescriptor { name: int_name });
    let const_id = artifact.constants.alloc(ConstantDescriptor {
        name: const_name,
        value: ConstantValue::Int(41),
    });
    let effect_id = artifact.effects.alloc(EffectDescriptor {
        name: abort_name,
        ops: Box::new([EffectOpDescriptor {
            name: abort_op_name,
            params: 0,
        }]),
    });
    let foreign_id = artifact.foreigns.alloc(ForeignDescriptor {
        name: puts_name,
        params: 1,
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

    let int_ty = artifact.types.alloc(TypeDescriptor { name: int_name });
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
