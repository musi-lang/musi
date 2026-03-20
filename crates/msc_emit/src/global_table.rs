//! Global variable table for the GLOB section.

use crate::error::{EmitError, EmitResult};

pub struct GlobalEntry {
    pub name_stridx: u16,
    pub type_ref: u16,
    pub flags: u8,
    pub initializer: Option<u16>,
}

pub const FLAG_EXPORTED: u8 = 1 << 0;
pub const FLAG_MUTABLE: u8 = 1 << 1;

pub struct GlobalTable {
    entries: Vec<GlobalEntry>,
}

impl GlobalTable {
    pub const fn new() -> Self {
        Self { entries: vec![] }
    }

    pub fn add(&mut self, entry: GlobalEntry) -> EmitResult<u16> {
        let idx = u16::try_from(self.entries.len()).map_err(|_| EmitError::TooManyGlobals)?;
        self.entries.push(entry);
        Ok(idx)
    }

    pub fn write_into(&self, buf: &mut Vec<u8>) {
        let count = u16::try_from(self.entries.len()).expect("global table fits in u16");
        buf.extend_from_slice(&count.to_be_bytes());
        for entry in &self.entries {
            buf.extend_from_slice(&entry.name_stridx.to_be_bytes());
            buf.extend_from_slice(&entry.type_ref.to_be_bytes());
            buf.push(entry.flags);
            if let Some(init) = entry.initializer {
                buf.extend_from_slice(&init.to_be_bytes());
            }
        }
    }
}
