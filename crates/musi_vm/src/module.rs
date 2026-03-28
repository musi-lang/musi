use music_il::format::{
    ClassDescriptor, ENTRY_METHOD_NAME, EffectDescriptor, ForeignDescriptor, TypeDescriptor,
};

use crate::value::Value;

/// Marks the module entry point in `Method::name`.
pub const ENTRY_POINT_NAME: u32 = ENTRY_METHOD_NAME;

/// A constant pool entry as loaded from a `.seam` binary.
///
/// String constants are stored as `StringRef` indices into the module's string
/// table. The VM resolves these to heap-allocated string objects at startup.
#[derive(Debug, Clone)]
pub enum ConstantEntry {
    Value(Value),
    /// Index into `Module::strings`, resolved to a heap string in `Vm::new`.
    StringRef(u16),
}

pub struct Module {
    pub constants: Vec<ConstantEntry>,
    pub strings: Vec<String>,
    pub methods: Vec<Method>,
    pub globals: Vec<GlobalDef>,
    pub types: Vec<TypeDescriptor>,
    pub effects: Vec<EffectDescriptor>,
    pub classes: Vec<ClassDescriptor>,
    pub foreigns: Vec<ForeignDescriptor>,
}

pub struct Method {
    /// [`ENTRY_POINT_NAME`] marks the entry point. Anonymous non-entry
    /// closures use a separate internal wire sentinel.
    pub name: u32,
    pub locals_count: u16,
    /// Raw instruction bytes; the VM decodes these inline during execution.
    pub code: Vec<u8>,
}

pub struct GlobalDef {
    pub name: u32,
    pub exported: bool,
    pub opaque: bool,
}
