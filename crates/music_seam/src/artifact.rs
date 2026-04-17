use std::vec;

use music_arena::Idx;
use thiserror::Error;

use crate::descriptor::{
    ClassDescriptor, ConstantDescriptor, ConstantValue, DataDescriptor, EffectDescriptor,
    ExportDescriptor, ExportTarget, ForeignDescriptor, GlobalDescriptor, MetaDescriptor,
    MethodDescriptor, TypeDescriptor,
};
use crate::instruction::{CodeEntry, Instruction, Label, LabelId, Operand, OperandShape};

pub const SEAM_MAGIC: [u8; 4] = *b"SEAM";
pub const BINARY_VERSION: u16 = 10;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SectionTag {
    Strings = 1,
    Types = 2,
    Constants = 3,
    Globals = 4,
    Methods = 5,
    Effects = 6,
    Classes = 7,
    Foreigns = 8,
    Exports = 9,
    Data = 10,
    Meta = 11,
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
pub type MethodId = Idx<MethodDescriptor>;
pub type EffectId = Idx<EffectDescriptor>;
pub type ClassId = Idx<ClassDescriptor>;
pub type ForeignId = Idx<ForeignDescriptor>;
pub type ExportId = Idx<ExportDescriptor>;
pub type DataId = Idx<DataDescriptor>;
pub type MetaId = Idx<MetaDescriptor>;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Artifact {
    pub strings: Table<StringRecord>,
    pub types: Table<TypeDescriptor>,
    pub constants: Table<ConstantDescriptor>,
    pub globals: Table<GlobalDescriptor>,
    pub methods: Table<MethodDescriptor>,
    pub effects: Table<EffectDescriptor>,
    pub classes: Table<ClassDescriptor>,
    pub foreigns: Table<ForeignDescriptor>,
    pub exports: Table<ExportDescriptor>,
    pub data: Table<DataDescriptor>,
    pub meta: Table<MetaDescriptor>,
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ArtifactError {
    #[error("table reference out of bounds: {table}")]
    InvalidReference { table: &'static str },
    #[error("duplicate label definition in method {method}")]
    DuplicateLabel { method: String },
    #[error("missing label reference in method {method}")]
    MissingLabel { method: String },
    #[error("invalid effect op reference")]
    InvalidEffectOp,
    #[error("operand shape mismatch for opcode {opcode}")]
    OperandShapeMismatch { opcode: &'static str },
}

impl Artifact {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern_string(&mut self, text: &str) -> StringId {
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
    /// Validates descriptor references, instruction operand shapes, and method label usage.
    ///
    /// # Errors
    ///
    /// Returns [`ArtifactError`] when the artifact contains an invalid table reference, label,
    /// effect op, or opcode/operand pairing.
    pub fn validate(&self) -> Result<(), ArtifactError> {
        self.validate_types()?;
        self.validate_constants()?;
        self.validate_globals()?;
        self.validate_effects()?;
        self.validate_classes()?;
        self.validate_foreigns()?;
        self.validate_data()?;
        self.validate_exports()?;
        self.validate_methods()?;
        self.validate_meta()?;
        Ok(())
    }

    fn validate_method(&self, method: &MethodDescriptor) -> Result<(), ArtifactError> {
        self.require_string(method.name)?;
        let mut defined = vec![false; method.labels.len()];
        for label in &method.labels {
            self.require_string(*label)?;
        }
        for entry in &method.code {
            match entry {
                CodeEntry::Label(Label { id }) => {
                    let index = usize::from(*id);
                    let Some(slot) = defined.get_mut(index) else {
                        return Err(ArtifactError::MissingLabel {
                            method: self.string_text(method.name).to_owned(),
                        });
                    };
                    if *slot {
                        return Err(ArtifactError::DuplicateLabel {
                            method: self.string_text(method.name).to_owned(),
                        });
                    }
                    *slot = true;
                }
                CodeEntry::Instruction(instruction) => {
                    self.validate_instruction(method, instruction)?;
                }
            }
        }
        Ok(())
    }

    fn validate_instruction(
        &self,
        method: &MethodDescriptor,
        instruction: &Instruction,
    ) -> Result<(), ArtifactError> {
        if !operand_matches_shape(&instruction.operand, instruction.opcode.operand_shape()) {
            return Err(ArtifactError::OperandShapeMismatch {
                opcode: instruction.opcode.mnemonic(),
            });
        }
        self.validate_instruction_operand(method, &instruction.operand)
    }

    fn validate_instruction_operand(
        &self,
        method: &MethodDescriptor,
        operand: &Operand,
    ) -> Result<(), ArtifactError> {
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
            Operand::Method(id) => {
                self.require_method(*id)?;
            }
            Operand::WideMethodCaptures {
                method: method_id, ..
            } => {
                self.require_method(*method_id)?;
            }
            Operand::Foreign(id) => {
                self.require_foreign(*id)?;
            }
            Operand::Effect { effect, op } => {
                let effect = self.effects.get(*effect);
                let op_index = usize::from(*op);
                if effect.ops.get(op_index).is_none() {
                    return Err(ArtifactError::InvalidEffectOp);
                }
            }
            Operand::EffectId(effect) => {
                self.require_effect(*effect)?;
            }
            Operand::Label(id) => {
                require_label(method, *id)?;
            }
            Operand::TypeLen { ty, .. } => {
                self.require_type(*ty)?;
            }
            Operand::BranchTable(labels) => {
                for label in labels.iter().copied() {
                    require_label(method, label)?;
                }
            }
        }
        Ok(())
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

    fn validate_globals(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.globals.iter() {
            self.require_string(descriptor.name)?;
            if let Some(method) = descriptor.initializer {
                self.require_method(method)?;
            }
        }
        Ok(())
    }

    fn validate_effects(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.effects.iter() {
            self.require_string(descriptor.name)?;
            for op in &descriptor.ops {
                self.require_string(op.name)?;
                for ty in &op.param_tys {
                    self.require_type(*ty)?;
                }
                self.require_type(op.result_ty)?;
            }
        }
        Ok(())
    }

    fn validate_classes(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.classes.iter() {
            self.require_string(descriptor.name)?;
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
            }
            if let Some(repr) = descriptor.repr_kind {
                self.require_string(repr)?;
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
            ExportTarget::Method(method) => self.require_method(method),
            ExportTarget::Global(global) => self.require_global(global),
            ExportTarget::Foreign(foreign) => self.require_foreign(foreign),
            ExportTarget::Type(ty) => self.require_type(ty),
            ExportTarget::Effect(effect) => self.require_effect(effect),
            ExportTarget::Class(class) => self.require_class(class),
        }
    }

    fn validate_methods(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.methods.iter() {
            self.validate_method(descriptor)?;
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

    fn require_method(&self, id: MethodId) -> Result<(), ArtifactError> {
        let _ = self
            .methods
            .as_slice()
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or(ArtifactError::InvalidReference { table: "methods" })?;
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

    fn require_effect(&self, id: EffectId) -> Result<(), ArtifactError> {
        let _ = self
            .effects
            .as_slice()
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or(ArtifactError::InvalidReference { table: "effects" })?;
        Ok(())
    }

    fn require_class(&self, id: ClassId) -> Result<(), ArtifactError> {
        let _ = self
            .classes
            .as_slice()
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or(ArtifactError::InvalidReference { table: "classes" })?;
        Ok(())
    }
}

fn same_source_or_qualified_name(left: &str, right: &str) -> bool {
    left == right || source_name(left) == source_name(right)
}

fn source_name(name: &str) -> &str {
    name.rsplit_once("::").map_or(name, |(_, tail)| tail)
}

fn require_label(method: &MethodDescriptor, id: LabelId) -> Result<(), ArtifactError> {
    let index = usize::from(id);
    if method.labels.get(index).is_some() {
        Ok(())
    } else {
        Err(ArtifactError::MissingLabel {
            method: id.to_string(),
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
            | (Operand::Method(_), OperandShape::Method)
            | (
                Operand::WideMethodCaptures { .. },
                OperandShape::WideMethodCaptures
            )
            | (Operand::Foreign(_), OperandShape::Foreign)
            | (Operand::Effect { .. }, OperandShape::Effect)
            | (Operand::EffectId(_), OperandShape::EffectId)
            | (Operand::Label(_), OperandShape::Label)
            | (Operand::TypeLen { .. }, OperandShape::TypeLen)
            | (Operand::BranchTable(_), OperandShape::BranchTable)
    )
}
