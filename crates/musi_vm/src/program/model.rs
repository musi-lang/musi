use std::collections::HashMap;
use std::sync::Arc;

use music_seam::{Artifact, Instruction, LabelId, ProcedureId};

use super::layout::{DataLayoutMap, ExportMap, ProgramDataLayout, ProgramExport};
use super::runtime::{RuntimeBranchTable, RuntimeInstructionList, RuntimeKernel};
use crate::Value;

pub(super) type InstructionList = Box<[Instruction]>;
pub(super) type RuntimeBranchTableList = Box<[Option<RuntimeBranchTable>]>;
pub(super) type LabelIndexMap = HashMap<LabelId, usize>;

#[derive(Debug, Clone)]
pub struct Program {
    pub(super) inner: Arc<ProgramInner>,
}

#[derive(Debug)]
pub struct ProgramInner {
    pub(super) artifact: Artifact,
    pub(super) procedures: Box<[LoadedProcedure]>,
    pub(super) exports: ExportMap,
    pub(super) export_list: Box<[ProgramExport]>,
    pub(super) data_layouts: DataLayoutMap,
    pub(super) data_layout_list: Box<[ProgramDataLayout]>,
    pub(super) entry_procedure: Option<ProcedureId>,
    pub(super) module_init_procedure: Option<ProcedureId>,
    pub(super) global_init_image: Option<Arc<[Value]>>,
}

#[derive(Debug, Clone)]
pub struct LoadedProcedure {
    pub name: Box<str>,
    pub params: u16,
    pub locals: u16,
    pub instructions: InstructionList,
    pub(crate) runtime_instructions: RuntimeInstructionList,
    pub(crate) runtime_branch_tables: RuntimeBranchTableList,
    pub(crate) runtime_kernel: Option<RuntimeKernel>,
    pub labels: LabelIndexMap,
}

impl LoadedProcedure {
    #[must_use]
    pub fn new(
        name: impl Into<Box<str>>,
        params: u16,
        locals: u16,
        instructions: InstructionList,
        runtime_instructions: RuntimeInstructionList,
        runtime_branch_tables: RuntimeBranchTableList,
        runtime_kernel: Option<RuntimeKernel>,
    ) -> Self {
        Self {
            name: name.into(),
            params,
            locals,
            instructions,
            runtime_instructions,
            runtime_branch_tables,
            runtime_kernel,
            labels: LabelIndexMap::new(),
        }
    }

    #[must_use]
    pub fn with_labels(mut self, labels: LabelIndexMap) -> Self {
        self.labels = labels;
        self
    }

    #[must_use]
    pub(crate) fn runtime_branch_table(&self, raw_index: usize) -> Option<&RuntimeBranchTable> {
        self.runtime_branch_tables.get(raw_index)?.as_ref()
    }

    #[must_use]
    pub const fn runtime_kernel(&self) -> Option<RuntimeKernel> {
        self.runtime_kernel
    }
}
