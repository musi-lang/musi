use std::vec;

use music_arena::Idx;
use std::error::Error;
use std::fmt::{self, Display, Formatter};

use music_base::diag::{CatalogDiagnostic, DiagContext};

use crate::SeamDiagKind;
use crate::diag::artifact_error_kind;

use crate::Opcode;
use crate::descriptor::{
    BlockSignatureDescriptor, ClosureDescriptor, ConstantDescriptor, ConstantValue, DataDescriptor,
    ExportDescriptor, ExportTarget, ForeignDescriptor, GlobalDescriptor, ImportDescriptor,
    ManifestDescriptor, MetaDescriptor, ProcedureDescriptor, RootMapDescriptor, ShapeDescriptor,
    StackEffectDescriptor, TypeDescriptor,
};
use crate::instruction::{CodeEntry, Instruction, Label, LabelId, Operand, OperandShape};

mod root_maps;

pub const SEAM_MAGIC: [u8; 4] = *b"SEAM";
const BINARY_MAJOR_VERSION_U32: u32 = 18;
const BINARY_MINOR_VERSION_U32: u32 = 0;
pub const BINARY_MAJOR_VERSION: u16 = 18;
pub const BINARY_MINOR_VERSION: u16 = 0;
pub const BINARY_VERSION: u32 = (BINARY_MAJOR_VERSION_U32 << 16) | BINARY_MINOR_VERSION_U32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SectionTag {
    Strings = 1,
    Types = 2,
    Constants = 3,
    Globals = 4,
    Procedures = 5,
    Shapes = 6,
    Foreigns = 7,
    Exports = 8,
    Data = 9,
    Meta = 10,
    Manifest = 11,
    Imports = 12,
    RootMaps = 13,
    StackEffects = 14,
    BlockSignatures = 15,
    Closures = 16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringRecord {
    pub text: Box<str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Table<T> {
    items: Vec<T>,
}

impl<T> Table<T> {
    #[must_use]
    pub const fn new() -> Self {
        Self { items: Vec::new() }
    }

    #[must_use]
    pub const fn len(&self) -> usize {
        self.items.len()
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Appends a value to the table and returns its typed id.
    ///
    /// # Panics
    ///
    /// Panics if the table grows beyond `u32::MAX` entries.
    pub fn alloc(&mut self, value: T) -> Idx<T> {
        let raw = u32::try_from(self.items.len()).expect("table overflow");
        self.items.push(value);
        Idx::from_raw(raw)
    }

    /// Returns the entry for a previously allocated typed id.
    ///
    /// # Panics
    ///
    /// Panics if `id` does not refer to an entry in this table.
    #[must_use]
    pub fn get(&self, id: Idx<T>) -> &T {
        self.items
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .expect("table id out of bounds")
    }

    /// Returns a mutable reference to a previously allocated typed id.
    ///
    /// # Panics
    ///
    /// Panics if `id` does not refer to an entry in this table.
    pub fn get_mut(&mut self, id: Idx<T>) -> &mut T {
        self.items
            .get_mut(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .expect("table id out of bounds")
    }

    /// Iterates over all entries together with their typed ids.
    ///
    /// # Panics
    ///
    /// Panics if an entry index cannot be represented as `u32`.
    pub fn iter(&self) -> impl Iterator<Item = (Idx<T>, &T)> {
        self.items.iter().enumerate().map(|(idx, item)| {
            (
                Idx::from_raw(u32::try_from(idx).expect("table overflow")),
                item,
            )
        })
    }

    #[must_use]
    pub fn as_slice(&self) -> &[T] {
        &self.items
    }
}

impl<T> Default for Table<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub type StringId = Idx<StringRecord>;
pub type TypeId = Idx<TypeDescriptor>;
pub type ConstantId = Idx<ConstantDescriptor>;
pub type GlobalId = Idx<GlobalDescriptor>;
pub type ProcedureId = Idx<ProcedureDescriptor>;
pub type ShapeId = Idx<ShapeDescriptor>;
pub type ForeignId = Idx<ForeignDescriptor>;
pub type ExportId = Idx<ExportDescriptor>;
pub type DataId = Idx<DataDescriptor>;
pub type MetaId = Idx<MetaDescriptor>;
pub type RootMapId = Idx<RootMapDescriptor>;
pub type StackEffectId = Idx<StackEffectDescriptor>;
pub type BlockSignatureId = Idx<BlockSignatureDescriptor>;
pub type ClosureId = Idx<ClosureDescriptor>;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Artifact {
    pub strings: Table<StringRecord>,
    pub types: Table<TypeDescriptor>,
    pub constants: Table<ConstantDescriptor>,
    pub globals: Table<GlobalDescriptor>,
    pub procedures: Table<ProcedureDescriptor>,
    pub shapes: Table<ShapeDescriptor>,
    pub foreigns: Table<ForeignDescriptor>,
    pub exports: Table<ExportDescriptor>,
    pub data: Table<DataDescriptor>,
    pub meta: Table<MetaDescriptor>,
    pub manifest: Table<ManifestDescriptor>,
    pub imports: Table<ImportDescriptor>,
    pub root_maps: Table<RootMapDescriptor>,
    pub stack_effects: Table<StackEffectDescriptor>,
    pub block_signatures: Table<BlockSignatureDescriptor>,
    pub closures: Table<ClosureDescriptor>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArtifactError {
    InvalidReference { table: &'static str },
    SectionLimitExceeded { table: &'static str },
    DuplicateLabel { procedure: String },
    MissingLabel { procedure: String },
    OperandShapeMismatch { opcode: &'static str },
    InternalOpcodeSerialized { opcode: &'static str },
    BranchTableTargetStackMismatch { procedure: String },
}

impl ArtifactError {
    #[must_use]
    pub const fn diag_kind(&self) -> SeamDiagKind {
        artifact_error_kind(self)
    }

    #[must_use]
    pub fn diagnostic(&self) -> CatalogDiagnostic<SeamDiagKind> {
        CatalogDiagnostic::new(self.diag_kind(), self.diag_context())
    }

    fn diag_context(&self) -> DiagContext {
        match self {
            Self::InvalidReference { table } | Self::SectionLimitExceeded { table } => {
                DiagContext::new().with("table", *table)
            }
            Self::DuplicateLabel { procedure }
            | Self::MissingLabel { procedure }
            | Self::BranchTableTargetStackMismatch { procedure } => {
                DiagContext::new().with("procedure", procedure)
            }
            Self::OperandShapeMismatch { opcode } | Self::InternalOpcodeSerialized { opcode } => {
                DiagContext::new().with("opcode", *opcode)
            }
        }
    }
}

impl Display for ArtifactError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.diagnostic(), f)
    }
}

impl Error for ArtifactError {}

impl Artifact {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern_string(&mut self, text: &str) -> StringId {
        if let Some((id, _)) = self
            .strings
            .iter()
            .find(|(_, record)| record.text.as_ref() == text)
        {
            return id;
        }
        self.push_string_record(text)
    }

    pub(crate) fn push_string_record(&mut self, text: &str) -> StringId {
        self.strings.alloc(StringRecord { text: text.into() })
    }

    #[must_use]
    pub fn string_text(&self, id: StringId) -> &str {
        &self.strings.get(id).text
    }

    #[must_use]
    pub fn type_name(&self, id: TypeId) -> &str {
        let descriptor = self.types.get(id);
        self.string_text(descriptor.name)
    }

    #[must_use]
    pub fn type_term_json(&self, id: TypeId) -> &str {
        let descriptor = self.types.get(id);
        self.string_text(descriptor.term)
    }

    #[must_use]
    pub fn data_for_type(&self, ty: TypeId) -> Option<(DataId, &DataDescriptor)> {
        let type_name = self.type_name(ty);
        self.data.iter().find(|(_, descriptor)| {
            same_source_or_qualified_name(self.string_text(descriptor.name), type_name)
        })
    }

    #[must_use]
    pub fn data_by_name(&self, name: &str) -> Option<(DataId, &DataDescriptor)> {
        self.data.iter().find(|(_, descriptor)| {
            same_source_or_qualified_name(self.string_text(descriptor.name), name)
        })
    }
}

impl Artifact {
    /// Validates descriptor references, instruction operand shapes, and procedure label usage.
    ///
    /// # Errors
    ///
    /// Returns [`ArtifactError`] when the artifact contains an invalid table reference, label,
    /// foreign call, or opcode/operand pairing.
    pub fn validate(&self) -> Result<(), ArtifactError> {
        self.validate_types()?;
        self.validate_constants()?;
        self.validate_stack_effects()?;
        self.validate_globals()?;
        self.validate_shapes()?;
        self.validate_foreigns()?;
        self.validate_data()?;
        self.validate_exports()?;
        self.validate_procedures()?;
        self.validate_meta()?;
        self.validate_manifest()?;
        self.validate_imports()?;
        self.validate_root_maps()?;
        self.validate_block_signatures()?;
        self.validate_closures()?;
        Ok(())
    }

    fn validate_procedure(
        &self,
        procedure_id: ProcedureId,
        procedure: &ProcedureDescriptor,
    ) -> Result<(), ArtifactError> {
        self.require_string(procedure.name)?;
        if u16::try_from(procedure.param_tys.len()).is_err() {
            return Err(ArtifactError::SectionLimitExceeded {
                table: "procedure parameter types",
            });
        }
        for ty in procedure.param_tys.iter().copied() {
            self.require_type(ty)?;
        }
        if u16::try_from(procedure.local_tys.len()).is_err() {
            return Err(ArtifactError::SectionLimitExceeded {
                table: "procedure local types",
            });
        }
        for ty in procedure.local_tys.iter().copied() {
            self.require_type(ty)?;
        }
        if u16::try_from(procedure.result_tys.len()).is_err() {
            return Err(ArtifactError::SectionLimitExceeded {
                table: "procedure result types",
            });
        }
        for ty in procedure.result_tys.iter().copied() {
            self.require_type(ty)?;
        }
        if let Some(block_signature_table) = procedure.block_signature_table {
            self.require_block_signature(block_signature_table)?;
        }
        if let Some(root_map_table) = procedure.root_map_table {
            self.require_root_map(root_map_table)?;
        }
        if u16::try_from(procedure.domain_requirements.len()).is_err() {
            return Err(ArtifactError::SectionLimitExceeded {
                table: "procedure domain requirements",
            });
        }
        for domain in procedure.domain_requirements.iter().copied() {
            self.require_string(domain)?;
        }
        if procedure_has_tail_call(procedure) {
            self.validate_tail_call_cleanup_root_map(procedure_id, procedure)?;
        }
        let mut defined = vec![false; procedure.labels.len()];
        for label in &procedure.labels {
            self.require_string(*label)?;
        }
        for entry in &procedure.code {
            match entry {
                CodeEntry::Label(Label { id }) => {
                    let index = usize::from(*id);
                    let Some(slot) = defined.get_mut(index) else {
                        return Err(ArtifactError::MissingLabel {
                            procedure: self.procedure_name_owned(procedure),
                        });
                    };
                    if *slot {
                        return Err(ArtifactError::DuplicateLabel {
                            procedure: self.procedure_name_owned(procedure),
                        });
                    }
                    *slot = true;
                }
                CodeEntry::Instruction(instruction) => {
                    self.validate_instruction(procedure_id, procedure, instruction)?;
                }
            }
        }
        self.validate_procedure_branch_stack_rules(procedure_id, procedure)?;
        Ok(())
    }

    fn validate_tail_call_cleanup_root_map(
        &self,
        procedure_id: ProcedureId,
        procedure: &ProcedureDescriptor,
    ) -> Result<(), ArtifactError> {
        let Some(root_map_id) = procedure.root_map_table else {
            return Err(ArtifactError::InvalidReference {
                table: "tail-call cleanup root map",
            });
        };
        let root_map = self
            .root_maps
            .as_slice()
            .get(usize::try_from(root_map_id.raw()).unwrap_or(usize::MAX))
            .ok_or(ArtifactError::InvalidReference {
                table: "tail-call cleanup root map",
            })?;
        if root_map.procedure != Some(procedure_id) {
            return Err(ArtifactError::InvalidReference {
                table: "tail-call cleanup procedure root map",
            });
        }
        if !root_map.defer_slots.is_empty() {
            return Err(ArtifactError::InvalidReference {
                table: "tail-call cleanup defer slots",
            });
        }
        if !root_map.pin_slots.is_empty() {
            return Err(ArtifactError::InvalidReference {
                table: "tail-call cleanup pin slots",
            });
        }
        Ok(())
    }

    fn validate_instruction(
        &self,
        procedure_id: ProcedureId,
        procedure: &ProcedureDescriptor,
        instruction: &Instruction,
    ) -> Result<(), ArtifactError> {
        if !operand_matches_opcode(&instruction.operand, instruction.opcode) {
            return Err(ArtifactError::OperandShapeMismatch {
                opcode: instruction.opcode.mnemonic(),
            });
        }
        if instruction.opcode.is_internal() {
            return Err(ArtifactError::InternalOpcodeSerialized {
                opcode: instruction.opcode.mnemonic(),
            });
        }
        self.validate_instruction_operand(procedure_id, procedure, instruction)
    }

    fn validate_instruction_operand(
        &self,
        procedure_id: ProcedureId,
        procedure: &ProcedureDescriptor,
        instruction: &Instruction,
    ) -> Result<(), ArtifactError> {
        let operand = &instruction.operand;
        match operand {
            Operand::None | Operand::I16(_) | Operand::Local(_) => {}
            Operand::String(id) => {
                self.require_string(*id)?;
            }
            Operand::Type(id) => {
                self.require_type(*id)?;
            }
            Operand::Constant(id) => {
                self.require_constant(*id)?;
            }
            Operand::Global(id) => {
                self.require_global(*id)?;
            }
            Operand::Procedure(id) => {
                self.require_procedure(*id)?;
            }
            Operand::WideProcedureCaptures {
                procedure: procedure_id,
                ..
            } => {
                self.require_procedure(*procedure_id)?;
            }
            Operand::Foreign(id) => {
                self.require_foreign(*id)?;
            }
            Operand::Label(id) => {
                require_label(procedure, *id)?;
            }
            Operand::TypeLen { ty, .. } => {
                self.require_type(*ty)?;
            }
            Operand::BranchTable(labels) => {
                for label in labels.iter().copied() {
                    require_label(procedure, label)?;
                }
                if instruction.opcode == Opcode::BrTbl {
                    self.validate_branch_table_target_stacks(procedure_id, labels)?;
                }
            }
        }
        Ok(())
    }

    fn validate_branch_table_target_stacks(
        &self,
        procedure_id: ProcedureId,
        labels: &[LabelId],
    ) -> Result<(), ArtifactError> {
        let _ = self.branch_table_common_target_stack(procedure_id, labels)?;
        Ok(())
    }

    fn branch_table_common_target_stack<'a>(
        &'a self,
        procedure_id: ProcedureId,
        labels: &[LabelId],
    ) -> Result<Option<&'a [TypeId]>, ArtifactError> {
        if !labels.iter().copied().any(|label| {
            self.block_signature_for_label(procedure_id, label)
                .is_some()
        }) {
            return Ok(None);
        }
        let mut common_stack: Option<&[TypeId]> = None;
        for label in labels.iter().copied() {
            let signature = self.block_signature_for_label(procedure_id, label).ok_or(
                ArtifactError::InvalidReference {
                    table: "branch table target block signatures",
                },
            )?;
            match common_stack {
                Some(expected) if expected != signature.incoming_tys.as_ref() => {
                    let procedure = self.procedures.get(procedure_id);
                    return Err(ArtifactError::BranchTableTargetStackMismatch {
                        procedure: self.procedure_name_owned(procedure),
                    });
                }
                Some(_) => {}
                None => common_stack = Some(signature.incoming_tys.as_ref()),
            }
        }
        Ok(common_stack)
    }

    fn block_signature_for_label(
        &self,
        procedure_id: ProcedureId,
        label: LabelId,
    ) -> Option<&BlockSignatureDescriptor> {
        self.block_signatures.iter().find_map(|(_, signature)| {
            (signature.procedure == procedure_id && signature.label == label).then_some(signature)
        })
    }

    fn validate_procedure_branch_stack_rules(
        &self,
        procedure_id: ProcedureId,
        procedure: &ProcedureDescriptor,
    ) -> Result<(), ArtifactError> {
        if procedure.param_tys.len() != usize::from(procedure.params)
            || procedure.local_tys.len() != usize::from(procedure.locals)
        {
            return Ok(());
        }
        let mut current_stack: Option<ProcedureTypeStack> = None;
        for entry in &procedure.code {
            match entry {
                CodeEntry::Label(Label { id }) => {
                    current_stack = self
                        .block_signature_for_label(procedure_id, *id)
                        .map(block_signature_stack);
                }
                CodeEntry::Instruction(instruction) => {
                    let Some(stack) = current_stack.as_mut() else {
                        continue;
                    };
                    match instruction.opcode {
                        Opcode::Br => {
                            self.verify_branch_stack(procedure_id, instruction, stack)?;
                            current_stack = None;
                        }
                        Opcode::BrZ => {
                            self.verify_branch_false_stack(procedure_id, instruction, stack)?;
                        }
                        Opcode::BrTbl => {
                            self.verify_branch_table_stack(procedure_id, instruction, stack)?;
                            current_stack = None;
                        }
                        Opcode::Ret => {
                            Self::verify_return_stack(procedure, stack)?;
                            current_stack = None;
                        }
                        _ => {
                            if !self.apply_stack_effect(procedure, instruction, stack) {
                                current_stack = None;
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn verify_branch_stack(
        &self,
        procedure_id: ProcedureId,
        instruction: &Instruction,
        current_stack: &[ProcedureStackType],
    ) -> Result<(), ArtifactError> {
        let Operand::Label(label) = instruction.operand else {
            return Ok(());
        };
        let Some(signature) = self.block_signature_for_label(procedure_id, label) else {
            return Ok(());
        };
        if stack_matches_types(current_stack, signature.incoming_tys.as_ref()) {
            Ok(())
        } else {
            Err(ArtifactError::InvalidReference {
                table: "branch target incoming stack",
            })
        }
    }

    fn verify_branch_false_stack(
        &self,
        procedure_id: ProcedureId,
        instruction: &Instruction,
        current_stack: &mut ProcedureTypeStack,
    ) -> Result<(), ArtifactError> {
        let Operand::Label(label) = instruction.operand else {
            return Ok(());
        };
        let remaining_len =
            current_stack
                .len()
                .checked_sub(1)
                .ok_or(ArtifactError::InvalidReference {
                    table: "branch-false target incoming stack",
                })?;
        if let Some(signature) = self.block_signature_for_label(procedure_id, label)
            && !stack_matches_types(
                &current_stack[..remaining_len],
                signature.incoming_tys.as_ref(),
            )
        {
            return Err(ArtifactError::InvalidReference {
                table: "branch-false target incoming stack",
            });
        }
        current_stack.truncate(remaining_len);
        Ok(())
    }

    fn verify_branch_table_stack(
        &self,
        procedure_id: ProcedureId,
        instruction: &Instruction,
        current_stack: &[ProcedureStackType],
    ) -> Result<(), ArtifactError> {
        let Operand::BranchTable(labels) = &instruction.operand else {
            return Ok(());
        };
        let Some(common_stack) = self.branch_table_common_target_stack(procedure_id, labels)?
        else {
            return Ok(());
        };
        let remaining_len =
            current_stack
                .len()
                .checked_sub(1)
                .ok_or(ArtifactError::InvalidReference {
                    table: "branch table incoming stack",
                })?;
        if stack_matches_types(&current_stack[..remaining_len], common_stack) {
            Ok(())
        } else {
            Err(ArtifactError::InvalidReference {
                table: "branch table incoming stack",
            })
        }
    }

    fn verify_return_stack(
        procedure: &ProcedureDescriptor,
        current_stack: &[ProcedureStackType],
    ) -> Result<(), ArtifactError> {
        if stack_matches_types(current_stack, procedure.result_tys.as_ref()) {
            Ok(())
        } else {
            Err(ArtifactError::InvalidReference {
                table: "return result stack",
            })
        }
    }

    fn apply_stack_effect(
        &self,
        procedure: &ProcedureDescriptor,
        instruction: &Instruction,
        stack: &mut ProcedureTypeStack,
    ) -> bool {
        let opcode = instruction.opcode;
        match opcode {
            Opcode::LdC
            | Opcode::LdCI4
            | Opcode::LdStr
            | Opcode::LdGlob
            | Opcode::LdFfi
            | Opcode::LdType => {
                stack.push(ProcedureStackType::Unknown);
                true
            }
            Opcode::LdLoc => {
                let Operand::Local(local) = instruction.operand else {
                    return false;
                };
                let value_ty = procedure
                    .local_tys
                    .get(usize::from(local))
                    .copied()
                    .map_or(ProcedureStackType::Unknown, ProcedureStackType::Known);
                stack.push(value_ty);
                true
            }
            Opcode::StLoc | Opcode::StGlob => pop_stack(stack, 1),
            Opcode::LdFld | Opcode::LdElem => {
                if !pop_stack(stack, 2) {
                    return false;
                }
                let value_ty = match instruction.operand {
                    Operand::Type(ty) => ProcedureStackType::Known(ty),
                    _ => ProcedureStackType::Unknown,
                };
                stack.push(value_ty);
                true
            }
            Opcode::StFld => pop_stack(stack, 2),
            Opcode::Add
            | Opcode::Sub
            | Opcode::Mul
            | Opcode::DivS
            | Opcode::RemS
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::Ceq
            | Opcode::Cne
            | Opcode::CltS
            | Opcode::CgtS
            | Opcode::CleS
            | Opcode::CgeS => push_unknown_after_pop(stack, 2),
            Opcode::Not => {
                let Some(value_ty) = stack.pop() else {
                    return false;
                };
                stack.push(value_ty);
                true
            }
            Opcode::Call => self.apply_call_stack_effect(instruction, stack),
            Opcode::CallInd | Opcode::TailCall => false,
            Opcode::CallFfi => self.apply_call_ffi_stack_effect(instruction, stack),
            Opcode::NewFn => {
                let Operand::WideProcedureCaptures { captures, .. } = instruction.operand else {
                    return false;
                };
                push_unknown_after_pop(stack, usize::from(captures))
            }
            Opcode::NewObj | Opcode::NewArr => {
                let Operand::TypeLen { len, .. } = instruction.operand else {
                    return false;
                };
                push_unknown_after_pop(stack, usize::from(len))
            }
            Opcode::StElem => pop_stack(stack, 3),
            Opcode::LdLen | Opcode::IsInst | Opcode::LdModDyn | Opcode::LdExpDyn => {
                push_unknown_after_pop(stack, 1)
            }
            Opcode::Cast => {
                let Operand::Type(ty) = instruction.operand else {
                    return false;
                };
                push_known_after_pop(stack, 1, ProcedureStackType::Known(ty))
            }
            Opcode::Br | Opcode::BrZ | Opcode::BrTbl | Opcode::Ret => true,
        }
    }

    fn apply_call_stack_effect(
        &self,
        instruction: &Instruction,
        stack: &mut ProcedureTypeStack,
    ) -> bool {
        let Operand::Procedure(callee_id) = instruction.operand else {
            return false;
        };
        let callee = self.procedures.get(callee_id);
        if !pop_stack(stack, callee.param_tys.len()) {
            return false;
        }
        for ty in callee.result_tys.iter().copied() {
            stack.push(ProcedureStackType::Known(ty));
        }
        true
    }

    fn apply_call_ffi_stack_effect(
        &self,
        instruction: &Instruction,
        stack: &mut ProcedureTypeStack,
    ) -> bool {
        let Operand::Foreign(foreign_id) = instruction.operand else {
            return false;
        };
        let foreign = self.foreigns.get(foreign_id);
        push_known_after_pop(
            stack,
            foreign.param_tys.len(),
            ProcedureStackType::Known(foreign.result_ty),
        )
    }

    fn validate_types(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.types.iter() {
            self.require_string(descriptor.name)?;
            self.require_string(descriptor.term)?;
        }
        Ok(())
    }

    fn validate_constants(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.constants.iter() {
            self.require_string(descriptor.name)?;
            match descriptor.value {
                ConstantValue::String(id) => self.require_string(id)?,
                ConstantValue::Syntax { text, .. } => self.require_string(text)?,
                ConstantValue::Int(_) | ConstantValue::Float(_) | ConstantValue::Bool(_) => {}
            }
        }
        Ok(())
    }

    fn validate_stack_effects(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.stack_effects.iter() {
            self.require_string(descriptor.name)?;
            let input_len = u16::try_from(descriptor.input_tys.len());
            if input_len.is_err() {
                return Err(ArtifactError::SectionLimitExceeded {
                    table: "stack effect input types",
                });
            }
            let output_len = u16::try_from(descriptor.output_tys.len());
            if output_len.is_err() {
                return Err(ArtifactError::SectionLimitExceeded {
                    table: "stack effect output types",
                });
            }
            for ty in descriptor.input_tys.iter().copied() {
                self.require_type(ty)?;
            }
            for ty in descriptor.output_tys.iter().copied() {
                self.require_type(ty)?;
            }
        }
        Ok(())
    }

    fn validate_globals(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.globals.iter() {
            self.require_string(descriptor.name)?;
            if let Some(procedure) = descriptor.initializer {
                self.require_procedure(procedure)?;
            }
        }
        Ok(())
    }

    fn validate_shapes(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.shapes.iter() {
            self.require_string(descriptor.name)?;
            if let Some(payload_ty) = descriptor.payload_ty {
                self.require_type(payload_ty)?;
            }
            if let Some(witness) = descriptor.witness {
                self.require_string(witness)?;
            }
            if let Some(dispatch_table) = descriptor.dispatch_table {
                self.require_string(dispatch_table)?;
            }
            if let Some(layout_identity) = descriptor.layout_identity {
                self.require_type(layout_identity)?;
            }
        }
        Ok(())
    }

    fn validate_foreigns(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.foreigns.iter() {
            self.require_string(descriptor.name)?;
            for ty in &descriptor.param_tys {
                self.require_type(*ty)?;
            }
            self.require_type(descriptor.result_ty)?;
            self.require_string(descriptor.abi)?;
            self.require_string(descriptor.symbol)?;
            if let Some(link) = descriptor.link {
                self.require_string(link)?;
            }
            if let Some(domain) = descriptor.domain {
                self.require_string(domain)?;
            }
            if let Some(lifetime) = descriptor.lifetime {
                self.require_string(lifetime)?;
            }
            if u16::try_from(descriptor.pinned_params.len()).is_err() {
                return Err(ArtifactError::SectionLimitExceeded {
                    table: "foreign pinned params",
                });
            }
            if u16::try_from(descriptor.nullable_params.len()).is_err() {
                return Err(ArtifactError::SectionLimitExceeded {
                    table: "foreign nullable params",
                });
            }
            let param_count = descriptor.param_tys.len();
            for index in descriptor.pinned_params.iter().copied() {
                if usize::from(index) >= param_count {
                    return Err(ArtifactError::InvalidReference {
                        table: "foreign pinned params",
                    });
                }
            }
            for index in descriptor.nullable_params.iter().copied() {
                if usize::from(index) >= param_count {
                    return Err(ArtifactError::InvalidReference {
                        table: "foreign nullable params",
                    });
                }
            }
        }
        Ok(())
    }

    fn validate_data(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.data.iter() {
            self.require_string(descriptor.name)?;
            for variant in &descriptor.variants {
                self.require_string(variant.name)?;
                for ty in &variant.field_tys {
                    self.require_type(*ty)?;
                }
                if !variant.layout_fields.is_empty()
                    && variant.layout_fields.len() != variant.field_tys.len()
                {
                    return Err(ArtifactError::InvalidReference {
                        table: "data layout fields",
                    });
                }
                for field in &variant.layout_fields {
                    if let Some(name) = field.name {
                        self.require_string(name)?;
                    }
                    self.require_type(field.ty)?;
                    if let Some(storage) = field.storage {
                        self.require_string(storage)?;
                    }
                }
            }
            if let Some(repr) = descriptor.repr_kind {
                self.require_string(repr)?;
            }
            if let Some(header) = &descriptor.object_header
                && let Some(layout_ty) = header.layout_ty
            {
                self.require_type(layout_ty)?;
            }
        }
        Ok(())
    }

    fn validate_exports(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.exports.iter() {
            self.require_string(descriptor.name)?;
            self.validate_export_target(descriptor.target)?;
        }
        Ok(())
    }

    fn validate_export_target(&self, target: ExportTarget) -> Result<(), ArtifactError> {
        match target {
            ExportTarget::Procedure(procedure) => self.require_procedure(procedure),
            ExportTarget::Global(global) => self.require_global(global),
            ExportTarget::Foreign(foreign) => self.require_foreign(foreign),
            ExportTarget::Type(ty) => self.require_type(ty),
            ExportTarget::Shape(shape) => self.require_shape(shape),
        }
    }

    fn validate_procedures(&self) -> Result<(), ArtifactError> {
        for (procedure_id, descriptor) in self.procedures.iter() {
            self.validate_procedure(procedure_id, descriptor)?;
        }
        Ok(())
    }

    fn validate_meta(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.meta.iter() {
            self.require_string(descriptor.target)?;
            self.require_string(descriptor.key)?;
            for value in &descriptor.values {
                self.require_string(*value)?;
            }
        }
        Ok(())
    }

    fn validate_manifest(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.manifest.iter() {
            self.require_string(descriptor.package)?;
            self.require_string(descriptor.version)?;
            self.require_string(descriptor.profile)?;
            if let Some(entry) = descriptor.entry {
                self.require_string(entry)?;
            }
        }
        Ok(())
    }

    fn validate_imports(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.imports.iter() {
            self.require_string(descriptor.spec)?;
            self.require_string(descriptor.resolved)?;
        }
        Ok(())
    }

    fn validate_block_signatures(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.block_signatures.iter() {
            self.require_procedure(descriptor.procedure)?;
            let procedure = self.procedures.get(descriptor.procedure);
            if procedure
                .labels
                .get(usize::from(descriptor.label))
                .is_none()
            {
                return Err(ArtifactError::InvalidReference {
                    table: "block signature labels",
                });
            }
            if u16::try_from(descriptor.incoming_tys.len()).is_err() {
                return Err(ArtifactError::SectionLimitExceeded {
                    table: "block signature incoming types",
                });
            }
            for ty in descriptor.incoming_tys.iter().copied() {
                self.require_type(ty)?;
            }
        }
        Ok(())
    }

    fn validate_closures(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.closures.iter() {
            self.require_string(descriptor.name)?;
            self.require_procedure(descriptor.procedure)?;
            if u16::try_from(descriptor.capture_tys.len()).is_err() {
                return Err(ArtifactError::SectionLimitExceeded {
                    table: "closure capture types",
                });
            }
            for ty in descriptor.capture_tys.iter().copied() {
                self.require_type(ty)?;
            }
            if let Some(env_layout) = descriptor.env_layout {
                self.require_data(env_layout)?;
            }
            if u16::try_from(descriptor.param_tys.len()).is_err() {
                return Err(ArtifactError::SectionLimitExceeded {
                    table: "closure parameter types",
                });
            }
            for ty in descriptor.param_tys.iter().copied() {
                self.require_type(ty)?;
            }
            if u16::try_from(descriptor.result_tys.len()).is_err() {
                return Err(ArtifactError::SectionLimitExceeded {
                    table: "closure result types",
                });
            }
            for ty in descriptor.result_tys.iter().copied() {
                self.require_type(ty)?;
            }
            if let Some(domain) = descriptor.domain {
                self.require_string(domain)?;
            }
            if let Some(effect) = descriptor.effect {
                self.require_string(effect)?;
            }
        }
        Ok(())
    }
}

impl Artifact {
    fn procedure_name_owned(&self, procedure: &ProcedureDescriptor) -> String {
        self.string_text(procedure.name).to_owned()
    }

    fn require_string(&self, id: StringId) -> Result<(), ArtifactError> {
        let _ = self
            .strings
            .as_slice()
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or(ArtifactError::InvalidReference { table: "strings" })?;
        Ok(())
    }

    fn require_type(&self, id: TypeId) -> Result<(), ArtifactError> {
        let _ = self
            .types
            .as_slice()
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or(ArtifactError::InvalidReference { table: "types" })?;
        Ok(())
    }

    fn require_constant(&self, id: ConstantId) -> Result<(), ArtifactError> {
        let _ = self
            .constants
            .as_slice()
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or(ArtifactError::InvalidReference { table: "constants" })?;
        Ok(())
    }

    fn require_procedure(&self, id: ProcedureId) -> Result<(), ArtifactError> {
        let _ = self
            .procedures
            .as_slice()
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or(ArtifactError::InvalidReference {
                table: "procedures",
            })?;
        Ok(())
    }

    fn require_global(&self, id: GlobalId) -> Result<(), ArtifactError> {
        let _ = self
            .globals
            .as_slice()
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or(ArtifactError::InvalidReference { table: "globals" })?;
        Ok(())
    }

    fn require_foreign(&self, id: ForeignId) -> Result<(), ArtifactError> {
        let _ = self
            .foreigns
            .as_slice()
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or(ArtifactError::InvalidReference { table: "foreigns" })?;
        Ok(())
    }

    fn require_shape(&self, id: ShapeId) -> Result<(), ArtifactError> {
        let _ = self
            .shapes
            .as_slice()
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or(ArtifactError::InvalidReference { table: "shapes" })?;
        Ok(())
    }

    fn require_data(&self, id: DataId) -> Result<(), ArtifactError> {
        let _ = self
            .data
            .as_slice()
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or(ArtifactError::InvalidReference { table: "data" })?;
        Ok(())
    }

    fn require_root_map(&self, id: RootMapId) -> Result<(), ArtifactError> {
        let _ = self
            .root_maps
            .as_slice()
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or(ArtifactError::InvalidReference { table: "root maps" })?;
        Ok(())
    }

    fn require_block_signature(&self, id: BlockSignatureId) -> Result<(), ArtifactError> {
        let _ = self
            .block_signatures
            .as_slice()
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or(ArtifactError::InvalidReference {
                table: "block signatures",
            })?;
        Ok(())
    }
}

fn same_source_or_qualified_name(left: &str, right: &str) -> bool {
    left == right || source_name(left) == source_name(right)
}

fn source_name(name: &str) -> &str {
    name.rsplit_once("::").map_or(name, |(_, tail)| tail)
}

fn require_label(procedure: &ProcedureDescriptor, id: LabelId) -> Result<(), ArtifactError> {
    let index = usize::from(id);
    if procedure.labels.get(index).is_some() {
        Ok(())
    } else {
        Err(ArtifactError::MissingLabel {
            procedure: id.to_string(),
        })
    }
}

const fn operand_matches_shape(operand: &Operand, shape: OperandShape) -> bool {
    matches!(
        (operand, shape),
        (Operand::None, OperandShape::None)
            | (Operand::I16(_), OperandShape::I16)
            | (Operand::Local(_), OperandShape::Local)
            | (Operand::String(_), OperandShape::String)
            | (Operand::Type(_), OperandShape::Type)
            | (Operand::Constant(_), OperandShape::Constant)
            | (Operand::Global(_), OperandShape::Global)
            | (Operand::Procedure(_), OperandShape::Procedure)
            | (
                Operand::WideProcedureCaptures { .. },
                OperandShape::WideProcedureCaptures
            )
            | (Operand::Foreign(_), OperandShape::Foreign)
            | (Operand::Label(_), OperandShape::Label)
            | (Operand::TypeLen { .. }, OperandShape::TypeLen)
            | (Operand::BranchTable(_), OperandShape::BranchTable)
    )
}

fn operand_matches_opcode(operand: &Operand, opcode: Opcode) -> bool {
    match opcode {
        Opcode::CallInd => matches!(operand, Operand::None | Operand::I16(_)),
        Opcode::Call => true,
        Opcode::LdFld => matches!(operand, Operand::None | Operand::I16(_) | Operand::Type(_)),
        Opcode::StFld => matches!(operand, Operand::None | Operand::I16(_) | Operand::Type(_)),
        Opcode::LdElem | Opcode::StElem => {
            matches!(operand, Operand::None | Operand::I16(_) | Operand::Type(_))
        }
        _ => operand_matches_shape(operand, opcode.operand_shape()),
    }
}

fn procedure_has_tail_call(procedure: &ProcedureDescriptor) -> bool {
    procedure.code.iter().any(|entry| {
        matches!(
            entry,
            CodeEntry::Instruction(Instruction {
                opcode: Opcode::TailCall,
                ..
            })
        )
    })
}

type ProcedureTypeStack = Vec<ProcedureStackType>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ProcedureStackType {
    Known(TypeId),
    Unknown,
}

fn block_signature_stack(signature: &BlockSignatureDescriptor) -> ProcedureTypeStack {
    signature
        .incoming_tys
        .iter()
        .copied()
        .map(ProcedureStackType::Known)
        .collect()
}

fn pop_stack(stack: &mut ProcedureTypeStack, count: usize) -> bool {
    if stack.len() < count {
        return false;
    }
    stack.truncate(stack.len() - count);
    true
}

fn push_unknown_after_pop(stack: &mut ProcedureTypeStack, count: usize) -> bool {
    push_known_after_pop(stack, count, ProcedureStackType::Unknown)
}

fn push_known_after_pop(
    stack: &mut ProcedureTypeStack,
    count: usize,
    value: ProcedureStackType,
) -> bool {
    if !pop_stack(stack, count) {
        return false;
    }
    stack.push(value);
    true
}

fn stack_matches_types(stack: &[ProcedureStackType], expected: &[TypeId]) -> bool {
    stack.len() == expected.len()
        && stack
            .iter()
            .zip(expected)
            .all(|(value, expected_ty)| match value {
                ProcedureStackType::Known(found_ty) => found_ty == expected_ty,
                ProcedureStackType::Unknown => true,
            })
}
