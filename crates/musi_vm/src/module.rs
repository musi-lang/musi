use crate::value::Value;

pub struct Module {
    pub constants: Vec<Value>,
    pub strings: Vec<String>,
    pub methods: Vec<Method>,
    pub globals: Vec<GlobalDef>,
}

pub struct Method {
    /// `u32::MAX` marks the entry point.
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
