use std::collections::HashMap;

use crate::descriptors::{ClassDescriptor, EffectDescriptor, ForeignDescriptor, TypeDescriptor};
use crate::isa::Instruction;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantEntry {
    Int(i64),
    Float(u64),
    Str(String),
    Tag(u16),
}

pub type ConstantEntries = Vec<ConstantEntry>;
pub type AbsoluteGlobalLoads = Vec<usize>;
pub type MethodEntries = Vec<MethodEntry>;
pub type GlobalEntries = Vec<GlobalEntry>;
pub type TypeDescriptors = Vec<TypeDescriptor>;
pub type EffectDescriptors = Vec<EffectDescriptor>;
pub type ClassDescriptors = Vec<ClassDescriptor>;
pub type ForeignDescriptors = Vec<ForeignDescriptor>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstantPool {
    entries: ConstantEntries,
    dedup: HashMap<ConstantEntry, u16>,
}

impl ConstantPool {
    #[must_use]
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            dedup: HashMap::new(),
        }
    }

    /// Add a constant if it is not present and return its pool index.
    ///
    /// # Panics
    /// Panics if the pool would exceed `u16::MAX` entries.
    pub fn add(&mut self, entry: ConstantEntry) -> u16 {
        if let Some(&index) = self.dedup.get(&entry) {
            return index;
        }

        let index = u16::try_from(self.entries.len()).expect("constant pool overflow (>65535)");
        let _ = self.dedup.insert(entry.clone(), index);
        self.entries.push(entry);
        index
    }

    #[must_use]
    pub fn entries(&self) -> &[ConstantEntry] {
        &self.entries
    }
}

impl Default for ConstantPool {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MethodName {
    Entry,
    Anonymous,
    Named(String),
}

pub type InstructionStream = Vec<Instruction>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodEntry {
    pub name: MethodName,
    pub instructions: InstructionStream,
    pub locals_count: u16,
    pub absolute_global_loads: AbsoluteGlobalLoads,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalEntry {
    pub name: String,
    pub exported: bool,
    pub opaque: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SeamArtifact {
    pub constants: ConstantPool,
    pub methods: MethodEntries,
    pub globals: GlobalEntries,
    pub types: TypeDescriptors,
    pub effects: EffectDescriptors,
    pub classes: ClassDescriptors,
    pub foreigns: ForeignDescriptors,
}
