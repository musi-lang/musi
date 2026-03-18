//! Constant pool builder.
//!
//! Interns constants by value. Str entries hold a u16 stridx into `StringTable`.

use std::collections::HashMap;

use crate::error::EmitError;

// Constant pool entry tags
const TAG_INT: u8 = 0x01;
const TAG_FLOAT: u8 = 0x02;
const TAG_STR: u8 = 0x03;

/// A compile-time constant value for the constant pool.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Int(i64),
    Float(f64),
    /// String constant, stored as a stridx (u16) into `StringTable`.
    Str(u16),
}

/// Key for deduplication in the constant pool.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ConstKey {
    Int(i64),
    Float(u64), // f64 bits
    Str(u16),
}

impl ConstKey {
    const fn from_value(v: &ConstValue) -> Self {
        match v {
            ConstValue::Int(n) => Self::Int(*n),
            ConstValue::Float(f) => Self::Float(f.to_bits()),
            ConstValue::Str(s) => Self::Str(*s),
        }
    }
}

/// Serialized constant pool entry (tag + raw bytes).
struct ConstEntry {
    tag: u8,
    data: Vec<u8>,
}

/// Interning constant pool.
pub struct ConstPool {
    entries: Vec<ConstEntry>,
    index: HashMap<ConstKey, u16>,
}

impl ConstPool {
    pub fn new() -> Self {
        Self {
            entries: vec![],
            index: HashMap::new(),
        }
    }

    /// Intern `value`, returning its pool index.
    pub fn intern(&mut self, value: &ConstValue) -> Result<u16, EmitError> {
        let key = ConstKey::from_value(value);
        if let Some(&idx) = self.index.get(&key) {
            return Ok(idx);
        }
        let idx = u16::try_from(self.entries.len()).map_err(|_| EmitError::TooManyConsts)?;
        let entry = encode_const(value);
        self.entries.push(entry);
        let _ = self.index.insert(key, idx);
        Ok(idx)
    }

    /// Serialize the constant pool into `buf` (BE encoding).
    pub fn write_into(&self, buf: &mut Vec<u8>) -> Result<(), EmitError> {
        let count = u16::try_from(self.entries.len()).map_err(|_| EmitError::TooManyConsts)?;
        buf.extend_from_slice(&count.to_be_bytes());
        for entry in &self.entries {
            buf.push(entry.tag);
            buf.extend_from_slice(&entry.data);
        }
        Ok(())
    }
}

fn encode_const(value: &ConstValue) -> ConstEntry {
    match value {
        ConstValue::Int(n) => encode_int(*n),
        ConstValue::Float(f) => ConstEntry {
            tag: TAG_FLOAT,
            data: f.to_bits().to_be_bytes().to_vec(),
        },
        ConstValue::Str(stridx) => ConstEntry {
            tag: TAG_STR,
            data: stridx.to_be_bytes().to_vec(),
        },
    }
}

fn encode_int(n: i64) -> ConstEntry {
    ConstEntry {
        tag: TAG_INT,
        data: n.to_be_bytes().to_vec(),
    }
}
