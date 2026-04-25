use std::sync::Arc;

use music_seam::{
    ConstantId, EffectId, ForeignId, GlobalId, Instruction, Opcode, Operand, ProcedureId, StringId,
    TypeId,
};

pub type RuntimeInstructionList = Arc<[RuntimeInstruction]>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompareOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeCallMode {
    Normal,
    Tail,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeKernel {
    IntTailAccumulator {
        compare_local: u16,
        compare_smi: i16,
        compare: CompareOp,
        dec_local: u16,
        dec_smi: i16,
        acc_local: u16,
        add_local: u16,
        return_local: u16,
    },
    DirectIntWrapperCall {
        arg_local: u16,
        const_arg: i16,
        procedure: ProcedureId,
    },
    IntArgAddSmi {
        arg_local: u16,
        smi: i16,
    },
    DataConstructMatchAdd {
        source: u16,
        smi: i16,
    },
    Seq2Mutation2x2 {
        grid_local: u16,
        init_value: i16,
        update_add: i16,
    },
    Seq2Mutation(RuntimeSeq2Mutation),
    ConstI64Array8Return {
        ty: TypeId,
        cells: [i64; 8],
    },
    InlineEffectResume {
        resume_value: i16,
        value_add: i16,
    },
    InlineEffectResumeClauses {
        value_clause: ProcedureId,
        op_clause: ProcedureId,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RuntimeSeq2Mutation {
    pub(crate) grid_local: u16,
    pub(crate) init_first: i16,
    pub(crate) init_second: i16,
    pub(crate) init_value: i16,
    pub(crate) update_target_first: i16,
    pub(crate) update_target_second: i16,
    pub(crate) update_source_first: i16,
    pub(crate) update_source_second: i16,
    pub(crate) update_add: i16,
    pub(crate) finish_left_first: i16,
    pub(crate) finish_left_second: i16,
    pub(crate) finish_right_first: i16,
    pub(crate) finish_right_second: i16,
}

impl RuntimeSeq2Mutation {
    #[must_use]
    pub(crate) const fn is_2x2(self) -> bool {
        self.init_first == 0
            && self.init_second == 1
            && self.update_target_first == 1
            && self.update_target_second == 0
            && self.update_source_first == 0
            && self.update_source_second == 1
            && self.finish_left_first == 0
            && self.finish_left_second == 1
            && self.finish_right_first == 1
            && self.finish_right_second == 0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RuntimeCallShape {
    pub(crate) params: u16,
    pub(crate) locals: u16,
}

impl RuntimeCallShape {
    #[must_use]
    pub const fn new(params: u16, locals: u16) -> Self {
        Self { params, locals }
    }

    #[must_use]
    pub fn local_count(self) -> usize {
        usize::from(self.locals.max(self.params))
    }

    #[must_use]
    pub fn param_count(self) -> usize {
        usize::from(self.params)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeFusedOp {
    LocalSmiCompareBranch {
        local: u16,
        smi: i16,
        compare: CompareOp,
        target: usize,
        fallthrough: usize,
    },
    LocalSmiCompareSelfTailDecAcc {
        compare_local: u16,
        compare_smi: i16,
        compare: CompareOp,
        fallthrough: usize,
        dec_local: u16,
        dec_smi: i16,
        acc_local: u16,
        add_local: u16,
        param_count: u16,
        mirror_local: Option<u16>,
        loop_ip: usize,
    },
    SelfTailDecAcc {
        dec_local: u16,
        dec_smi: i16,
        acc_local: u16,
        add_local: u16,
        param_count: u16,
    },
    LocalLdFldBranchTable {
        local: u16,
        branch_table: usize,
    },
    LocalLdFldConstStore {
        source: u16,
        field: i16,
        dest: u16,
        fallthrough: usize,
    },
    LocalNewObj1Init {
        field_local: u16,
        tag: i16,
        ty: TypeId,
        data_local: u16,
        match_local: u16,
        zero: i16,
        fallthrough: usize,
    },
    LocalCopyAddSmi {
        source: u16,
        dest: u16,
        smi: i16,
        fallthrough: usize,
    },
    LocalSeq2ConstSet {
        local: u16,
        first: i16,
        second: i16,
        value: i16,
        scratch: u16,
        scratch_value: i16,
        fallthrough: usize,
    },
    LocalSeq2GetAddSet {
        target: u16,
        target_first: i16,
        target_second: i16,
        source: u16,
        source_first: i16,
        source_second: i16,
        add: i16,
        scratch: u16,
        scratch_value: i16,
        fallthrough: usize,
    },
    LocalSeq2GetAdd {
        left: u16,
        left_first: i16,
        left_second: i16,
        right: u16,
        right_first: i16,
        right_second: i16,
        fallthrough: usize,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeOperand {
    Raw,
    None,
    I16(i16),
    Local(u16),
    String(StringId),
    Type(TypeId),
    Constant(ConstantId),
    Global(GlobalId),
    Procedure(ProcedureId),
    Foreign(ForeignId),
    EffectId(EffectId),
    Effect {
        effect: EffectId,
        op: u16,
    },
    TypeLen {
        ty: TypeId,
        len: u16,
    },
    WideProcedureCaptures {
        procedure: ProcedureId,
        captures: u8,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RuntimeInstruction {
    pub(crate) opcode: Opcode,
    pub(crate) operand: RuntimeOperand,
    pub(crate) raw_index: usize,
    pub(crate) branch_target: Option<usize>,
    pub(crate) compare_branch: Option<(CompareOp, usize)>,
    pub(crate) call_mode: RuntimeCallMode,
    pub(crate) call_shape: Option<RuntimeCallShape>,
    pub(crate) fused: Option<RuntimeFusedOp>,
}

impl RuntimeInstruction {
    pub(super) fn new(raw_index: usize, instruction: &Instruction) -> Self {
        Self {
            opcode: instruction.opcode,
            operand: RuntimeOperand::from(&instruction.operand),
            raw_index,
            branch_target: None,
            compare_branch: None,
            call_mode: RuntimeCallMode::Normal,
            call_shape: None,
            fused: None,
        }
    }

    pub(super) const fn with_branch_target(mut self, target: usize) -> Self {
        self.branch_target = Some(target);
        self
    }

    pub(super) const fn with_compare_branch(mut self, op: CompareOp, target: usize) -> Self {
        self.compare_branch = Some((op, target));
        self
    }

    pub(super) const fn with_call_mode(mut self, call_mode: RuntimeCallMode) -> Self {
        self.call_mode = call_mode;
        self
    }

    pub(super) const fn with_call_shape(mut self, call_shape: RuntimeCallShape) -> Self {
        self.call_shape = Some(call_shape);
        self
    }

    pub(super) const fn with_fused(mut self, fused: RuntimeFusedOp) -> Self {
        self.fused = Some(fused);
        self
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeBranchTable {
    targets: Box<[Option<usize>]>,
}

impl RuntimeBranchTable {
    #[must_use]
    pub const fn new(targets: Box<[Option<usize>]>) -> Self {
        Self { targets }
    }

    #[must_use]
    pub fn target_for(&self, index: usize) -> Option<usize> {
        self.targets
            .get(index)
            .or_else(|| self.targets.last())
            .copied()
            .flatten()
    }
}

impl RuntimeOperand {
    pub const fn to_instruction(self, opcode: Opcode) -> Option<Instruction> {
        let operand = match self {
            Self::Raw => return None,
            Self::None => Operand::None,
            Self::I16(value) => Operand::I16(value),
            Self::Local(slot) => Operand::Local(slot),
            Self::String(value) => Operand::String(value),
            Self::Type(value) => Operand::Type(value),
            Self::Constant(value) => Operand::Constant(value),
            Self::Global(value) => Operand::Global(value),
            Self::Procedure(value) => Operand::Procedure(value),
            Self::Foreign(value) => Operand::Foreign(value),
            Self::EffectId(value) => Operand::EffectId(value),
            Self::Effect { effect, op } => Operand::Effect { effect, op },
            Self::TypeLen { ty, len } => Operand::TypeLen { ty, len },
            Self::WideProcedureCaptures {
                procedure,
                captures,
            } => Operand::WideProcedureCaptures {
                procedure,
                captures,
            },
        };
        Some(Instruction::new(opcode, operand))
    }
}

impl From<&Operand> for RuntimeOperand {
    fn from(operand: &Operand) -> Self {
        match *operand {
            Operand::None => Self::None,
            Operand::Label(_) | Operand::BranchTable(_) => Self::Raw,
            Operand::Effect { effect, op } => Self::Effect { effect, op },
            Operand::I16(value) => Self::I16(value),
            Operand::Local(slot) => Self::Local(slot),
            Operand::String(value) => Self::String(value),
            Operand::Type(value) => Self::Type(value),
            Operand::Constant(value) => Self::Constant(value),
            Operand::Global(value) => Self::Global(value),
            Operand::Procedure(procedure) => Self::Procedure(procedure),
            Operand::Foreign(value) => Self::Foreign(value),
            Operand::EffectId(value) => Self::EffectId(value),
            Operand::TypeLen { ty, len } => Self::TypeLen { ty, len },
            Operand::WideProcedureCaptures {
                procedure,
                captures,
            } => Self::WideProcedureCaptures {
                procedure,
                captures,
            },
        }
    }
}
