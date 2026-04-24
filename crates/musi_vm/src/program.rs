use std::collections::HashMap;
use std::sync::Arc;

use music_seam::decode_binary;
use music_seam::descriptor::ExportTarget;
use music_seam::{
    Artifact, ConstantId, DataId, EffectId, ExportId, ForeignId, GlobalId, Instruction, LabelId,
    Opcode, Operand, ProcedureId, ShapeId, StringId, TypeId,
};
use music_term::{TypeTerm, TypeTermKind};

use super::{Value, VmError, VmErrorKind, VmIndexSpace, VmResult};
use crate::program_init::{build_global_init_image, specialize_runtime_kernels};
mod decode;

use decode::build_procedures;

type InstructionList = Box<[Instruction]>;
pub type RuntimeInstructionList = Arc<[RuntimeInstruction]>;
type RuntimeBranchTableList = Box<[Option<RuntimeBranchTable>]>;
type LabelIndexMap = HashMap<LabelId, usize>;
type ExportMap = HashMap<Box<str>, ExportId>;
type DataLayoutMap = HashMap<TypeId, ProgramDataLayout>;

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
    fn new(raw_index: usize, instruction: &Instruction) -> Self {
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

    const fn with_branch_target(mut self, target: usize) -> Self {
        self.branch_target = Some(target);
        self
    }

    const fn with_compare_branch(mut self, op: CompareOp, target: usize) -> Self {
        self.compare_branch = Some((op, target));
        self
    }

    const fn with_call_mode(mut self, call_mode: RuntimeCallMode) -> Self {
        self.call_mode = call_mode;
        self
    }

    const fn with_call_shape(mut self, call_shape: RuntimeCallShape) -> Self {
        self.call_shape = Some(call_shape);
        self
    }

    const fn with_fused(mut self, fused: RuntimeFusedOp) -> Self {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProgramExportKind {
    Procedure,
    Global,
    Foreign,
    Type,
    Effect,
    Shape,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProgramExport {
    pub name: Box<str>,
    pub opaque: bool,
    pub kind: ProgramExportKind,
}

impl ProgramExport {
    #[must_use]
    pub const fn new(name: Box<str>, opaque: bool, kind: ProgramExportKind) -> Self {
        Self { name, opaque, kind }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProgramDataVariantLayout {
    pub name: Box<str>,
    pub tag: i64,
    pub field_tys: Box<[TypeId]>,
    pub field_ty_names: Box<[Box<str>]>,
}

impl ProgramDataVariantLayout {
    #[must_use]
    pub const fn new(
        name: Box<str>,
        tag: i64,
        field_tys: Box<[TypeId]>,
        field_ty_names: Box<[Box<str>]>,
    ) -> Self {
        Self {
            name,
            tag,
            field_tys,
            field_ty_names,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProgramDataLayout {
    pub data: DataId,
    pub ty: TypeId,
    pub name: Box<str>,
    pub variant_count: u32,
    pub field_count: u32,
    pub variants: Box<[ProgramDataVariantLayout]>,
    pub repr_kind: Option<Box<str>>,
    pub layout_align: Option<u32>,
    pub layout_pack: Option<u32>,
    pub frozen: bool,
}

impl ProgramDataLayout {
    /// # Panics
    ///
    /// Panics if the variant count or total field count does not fit in `u32`.
    #[must_use]
    pub fn new(
        data: DataId,
        ty: TypeId,
        name: Box<str>,
        variants: Box<[ProgramDataVariantLayout]>,
    ) -> Self {
        let variant_count =
            u32::try_from(variants.len()).expect("program data variant count should fit in u32");
        let field_count = variants
            .iter()
            .map(|variant| variant.field_tys.len())
            .sum::<usize>();
        let field_count =
            u32::try_from(field_count).expect("program data field count should fit in u32");
        Self {
            data,
            ty,
            name,
            variant_count,
            field_count,
            variants,
            repr_kind: None,
            layout_align: None,
            layout_pack: None,
            frozen: false,
        }
    }

    #[must_use]
    pub fn with_repr_kind(mut self, repr_kind: impl Into<Box<str>>) -> Self {
        self.repr_kind = Some(repr_kind.into());
        self
    }

    #[must_use]
    pub const fn with_layout_align(mut self, layout_align: u32) -> Self {
        self.layout_align = Some(layout_align);
        self
    }

    #[must_use]
    pub const fn with_layout_pack(mut self, layout_pack: u32) -> Self {
        self.layout_pack = Some(layout_pack);
        self
    }

    #[must_use]
    pub const fn with_frozen(mut self, frozen: bool) -> Self {
        self.frozen = frozen;
        self
    }

    #[must_use]
    pub const fn is_single_variant_product(&self) -> bool {
        self.variant_count == 1
    }

    #[must_use]
    pub fn is_repr_c(&self) -> bool {
        self.repr_kind.as_deref() == Some("c")
    }

    #[must_use]
    pub fn is_repr_transparent(&self) -> bool {
        self.repr_kind.as_deref() == Some("transparent")
    }

    #[must_use]
    pub fn is_repr_c_single_variant_product(&self) -> bool {
        self.is_repr_c() && self.variant_count == 1
    }

    #[must_use]
    pub fn is_repr_transparent_wrapper(&self) -> bool {
        self.is_repr_transparent()
            && self.variant_count == 1
            && self
                .single_variant()
                .is_some_and(|variant| variant.field_tys.len() == 1)
    }

    #[must_use]
    pub fn native_abi_kind(&self) -> ProgramTypeAbiKind {
        if self.is_repr_transparent_wrapper() {
            ProgramTypeAbiKind::DataTransparent
        } else if self.is_repr_c_single_variant_product() {
            ProgramTypeAbiKind::DataReprCProduct
        } else {
            ProgramTypeAbiKind::Unsupported
        }
    }

    #[must_use]
    pub fn single_variant(&self) -> Option<&ProgramDataVariantLayout> {
        (self.variants.len() == 1).then_some(&self.variants[0])
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProgramTypeAbiKind {
    Unsupported,
    Unit,
    Bool,
    Int { signed: bool, bits: u8 },
    Float { bits: u8 },
    CString,
    CPtr,
    DataTransparent,
    DataReprCProduct,
}

impl ProgramTypeAbiKind {
    #[must_use]
    pub const fn is_supported_native_abi(self) -> bool {
        !matches!(self, Self::Unsupported)
    }

    #[must_use]
    pub const fn uses_data_layout(self) -> bool {
        matches!(self, Self::DataTransparent | Self::DataReprCProduct)
    }
}

const fn fsize_bits() -> u8 {
    if cfg!(target_pointer_width = "32") {
        32
    } else {
        64
    }
}

const fn target_pointer_bits() -> u8 {
    if cfg!(target_pointer_width = "16") {
        16
    } else if cfg!(target_pointer_width = "32") {
        32
    } else {
        64
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    inner: Arc<ProgramInner>,
}

#[derive(Debug)]
pub struct ProgramInner {
    artifact: Artifact,
    procedures: Box<[LoadedProcedure]>,
    exports: ExportMap,
    export_list: Box<[ProgramExport]>,
    data_layouts: DataLayoutMap,
    data_layout_list: Box<[ProgramDataLayout]>,
    entry_procedure: Option<ProcedureId>,
    module_init_procedure: Option<ProcedureId>,
    global_init_image: Option<Arc<[Value]>>,
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

impl Program {
    /// Loads validated SEAM bytes into an opaque runtime program.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if decoding, validation, or runtime indexing fails.
    pub fn from_bytes(bytes: &[u8]) -> VmResult<Self> {
        let artifact = decode_binary(bytes)?;
        Self::from_artifact(artifact)
    }

    pub(crate) fn from_artifact(artifact: Artifact) -> VmResult<Self> {
        let mut procedures = build_procedures(&artifact)?;
        specialize_runtime_kernels(&mut procedures);
        let (exports, export_list) = build_exports(&artifact);
        let (data_layouts, data_layout_list) = build_data_layouts(&artifact);
        let entry_procedure = find_suffix_procedure(&artifact, "::__entry");
        let module_init_procedure = find_suffix_procedure(&artifact, "::__module_init");
        let global_init_image = build_global_init_image(
            &procedures,
            artifact.globals.len(),
            entry_procedure.or(module_init_procedure),
        )
        .map(Arc::from);
        Ok(Self {
            inner: Arc::new(ProgramInner {
                artifact,
                procedures,
                exports,
                export_list,
                data_layouts,
                data_layout_list,
                entry_procedure,
                module_init_procedure,
                global_init_image,
            }),
        })
    }
}

impl Program {
    #[must_use]
    pub fn string_text(&self, id: StringId) -> &str {
        self.inner.artifact.string_text(id)
    }

    #[must_use]
    pub fn type_name(&self, id: TypeId) -> &str {
        let descriptor = self.inner.artifact.types.get(id);
        self.string_text(descriptor.name)
    }

    #[must_use]
    pub fn procedure_name(&self, id: ProcedureId) -> &str {
        let descriptor = self.inner.artifact.procedures.get(id);
        self.string_text(descriptor.name)
    }

    #[must_use]
    pub fn procedure_source_name(&self, id: ProcedureId) -> &str {
        source_export_name(self.procedure_name(id))
    }

    #[must_use]
    pub fn foreign_name(&self, id: ForeignId) -> &str {
        let descriptor = self.inner.artifact.foreigns.get(id);
        self.string_text(descriptor.name)
    }

    #[must_use]
    pub fn foreign_source_name(&self, id: ForeignId) -> &str {
        source_export_name(self.foreign_name(id))
    }

    #[must_use]
    pub fn effect_name(&self, id: EffectId) -> &str {
        let descriptor = self.inner.artifact.effects.get(id);
        self.string_text(descriptor.name)
    }

    #[must_use]
    pub fn effect_source_name(&self, id: EffectId) -> &str {
        source_export_name(self.effect_name(id))
    }

    #[must_use]
    pub fn shape_name(&self, id: ShapeId) -> &str {
        let descriptor = self.inner.artifact.shapes.get(id);
        self.string_text(descriptor.name)
    }

    #[must_use]
    pub fn shape_source_name(&self, id: ShapeId) -> &str {
        source_export_name(self.shape_name(id))
    }
}

impl Program {
    /// # Errors
    ///
    /// Returns [`VmErrorKind::InvalidTypeTerm`] when the stored type-term JSON cannot be decoded.
    pub fn try_type_term(&self, id: TypeId) -> VmResult<TypeTerm> {
        TypeTerm::from_json(self.inner.artifact.type_term_json(id)).map_err(|detail| {
            VmError::new(VmErrorKind::InvalidTypeTerm {
                ty: self.type_name(id).into(),
                detail: detail.to_string().into(),
            })
        })
    }

    #[must_use]
    pub fn type_term(&self, id: TypeId) -> TypeTerm {
        self.try_type_term(id)
            .unwrap_or_else(|_| TypeTerm::new(TypeTermKind::Unknown))
    }

    #[must_use]
    pub fn export_count(&self) -> usize {
        self.inner.export_list.len()
    }

    #[must_use]
    pub fn exports(&self) -> &[ProgramExport] {
        &self.inner.export_list
    }

    #[must_use]
    pub fn data_layouts(&self) -> &[ProgramDataLayout] {
        &self.inner.data_layout_list
    }

    #[must_use]
    pub fn type_data_layout(&self, ty: TypeId) -> Option<&ProgramDataLayout> {
        self.inner.data_layouts.get(&ty)
    }

    #[must_use]
    pub fn type_abi_kind(&self, ty: TypeId) -> ProgramTypeAbiKind {
        match self.type_term(ty).kind {
            TypeTermKind::Unit => ProgramTypeAbiKind::Unit,
            TypeTermKind::Bool => ProgramTypeAbiKind::Bool,
            TypeTermKind::Nat => ProgramTypeAbiKind::Int {
                signed: false,
                bits: target_pointer_bits(),
            },
            TypeTermKind::Int => ProgramTypeAbiKind::Int {
                signed: true,
                bits: target_pointer_bits(),
            },
            TypeTermKind::Int8 => ProgramTypeAbiKind::Int {
                signed: true,
                bits: 8,
            },
            TypeTermKind::Int16 => ProgramTypeAbiKind::Int {
                signed: true,
                bits: 16,
            },
            TypeTermKind::Int32 => ProgramTypeAbiKind::Int {
                signed: true,
                bits: 32,
            },
            TypeTermKind::Int64 => ProgramTypeAbiKind::Int {
                signed: true,
                bits: 64,
            },
            TypeTermKind::Nat8 => ProgramTypeAbiKind::Int {
                signed: false,
                bits: 8,
            },
            TypeTermKind::Nat16 => ProgramTypeAbiKind::Int {
                signed: false,
                bits: 16,
            },
            TypeTermKind::Nat32 => ProgramTypeAbiKind::Int {
                signed: false,
                bits: 32,
            },
            TypeTermKind::Nat64 => ProgramTypeAbiKind::Int {
                signed: false,
                bits: 64,
            },
            TypeTermKind::Float => ProgramTypeAbiKind::Float { bits: fsize_bits() },
            TypeTermKind::Float32 => ProgramTypeAbiKind::Float { bits: 32 },
            TypeTermKind::Float64 => ProgramTypeAbiKind::Float { bits: 64 },
            TypeTermKind::CString => ProgramTypeAbiKind::CString,
            TypeTermKind::CPtr => ProgramTypeAbiKind::CPtr,
            TypeTermKind::Named { .. } => self.type_data_layout(ty).map_or(
                ProgramTypeAbiKind::Unsupported,
                ProgramDataLayout::native_abi_kind,
            ),
            _ => ProgramTypeAbiKind::Unsupported,
        }
    }
}

impl Program {
    #[must_use]
    pub(crate) fn artifact(&self) -> &Artifact {
        &self.inner.artifact
    }

    pub(crate) fn loaded_procedure(&self, id: ProcedureId) -> VmResult<&LoadedProcedure> {
        let len = self.inner.procedures.len();
        self.inner
            .procedures
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or_else(|| {
                VmError::new(VmErrorKind::IndexOutOfBounds {
                    space: VmIndexSpace::Procedure,
                    owner: None,
                    index: i64::from(id.raw()),
                    len,
                })
            })
    }

    pub(crate) fn loaded_runtime_code(&self, id: ProcedureId) -> VmResult<RuntimeInstructionList> {
        Ok(Arc::<[RuntimeInstruction]>::clone(
            &self.loaded_procedure(id)?.runtime_instructions,
        ))
    }

    #[must_use]
    pub(crate) fn export_target(&self, name: &str) -> Option<ExportTarget> {
        let export_id = self.inner.exports.get(name).copied()?;
        Some(self.inner.artifact.exports.get(export_id).target)
    }

    #[must_use]
    pub(crate) fn is_export_opaque(&self, name: &str) -> Option<bool> {
        let export_id = self.inner.exports.get(name).copied()?;
        Some(self.inner.artifact.exports.get(export_id).opaque)
    }

    #[must_use]
    pub(crate) fn entry_procedure(&self) -> Option<ProcedureId> {
        self.inner
            .entry_procedure
            .or(self.inner.module_init_procedure)
    }

    #[must_use]
    pub(crate) fn global_init_image(&self) -> Option<&Arc<[Value]>> {
        self.inner.global_init_image.as_ref()
    }
}

fn build_exports(artifact: &Artifact) -> (ExportMap, Box<[ProgramExport]>) {
    let export_list = artifact
        .exports
        .iter()
        .map(|(_, export)| {
            ProgramExport::new(
                source_export_name(artifact.string_text(export.name)).into(),
                export.opaque,
                export_kind(export.target),
            )
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let exports = artifact
        .exports
        .iter()
        .map(|(id, export)| {
            (
                source_export_name(artifact.string_text(export.name)).into(),
                id,
            )
        })
        .collect();
    (exports, export_list)
}

fn build_data_layouts(artifact: &Artifact) -> (DataLayoutMap, Box<[ProgramDataLayout]>) {
    let mut layout_map = DataLayoutMap::new();
    let mut layout_list = Vec::new();
    for (ty, _) in artifact.types.iter() {
        let Some((data_id, descriptor)) = artifact.data_for_type(ty) else {
            continue;
        };
        let name = artifact.string_text(descriptor.name);
        let variants = descriptor
            .variants
            .iter()
            .map(|variant| {
                ProgramDataVariantLayout::new(
                    artifact.string_text(variant.name).into(),
                    variant.tag,
                    variant.field_tys.clone(),
                    variant
                        .field_tys
                        .iter()
                        .map(|field_ty| artifact.type_name(*field_ty).into())
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                )
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let mut layout = ProgramDataLayout::new(data_id, ty, name.into(), variants);
        debug_assert_eq!(layout.variant_count, descriptor.variant_count);
        debug_assert_eq!(layout.field_count, descriptor.field_count);
        if let Some(repr_kind) = descriptor.repr_kind.map(|id| artifact.string_text(id)) {
            layout = layout.with_repr_kind(repr_kind);
        }
        if let Some(layout_align) = descriptor.layout_align {
            layout = layout.with_layout_align(layout_align);
        }
        if let Some(layout_pack) = descriptor.layout_pack {
            layout = layout.with_layout_pack(layout_pack);
        }
        if descriptor.frozen {
            layout = layout.with_frozen(true);
        }
        let _ = layout_map.insert(ty, layout.clone());
        layout_list.push(layout);
    }
    (layout_map, layout_list.into_boxed_slice())
}

const fn export_kind(target: ExportTarget) -> ProgramExportKind {
    match target {
        ExportTarget::Procedure(_) => ProgramExportKind::Procedure,
        ExportTarget::Global(_) => ProgramExportKind::Global,
        ExportTarget::Foreign(_) => ProgramExportKind::Foreign,
        ExportTarget::Type(_) => ProgramExportKind::Type,
        ExportTarget::Effect(_) => ProgramExportKind::Effect,
        ExportTarget::Shape(_) => ProgramExportKind::Shape,
    }
}

fn find_suffix_procedure(artifact: &Artifact, suffix: &str) -> Option<ProcedureId> {
    artifact.procedures.iter().find_map(|(id, procedure)| {
        artifact
            .string_text(procedure.name)
            .ends_with(suffix)
            .then_some(id)
    })
}

fn source_export_name(name: &str) -> &str {
    name.rsplit_once("::").map_or(name, |(_, tail)| tail)
}
