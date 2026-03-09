//! Constant pool builder.
//!
//! Interns constants by value so identical values share a single pool entry.
//! Strings are deduplicated separately; other scalars are deduplicated by value.

use std::collections::HashMap;

use music_ir::IrConstValue;
use music_shared::{Interner, Symbol};

use crate::error::EmitError;

// Constant pool entry tags (§11.2)
const TAG_I32: u8 = 0x01;
const TAG_I64: u8 = 0x02;
const TAG_F64: u8 = 0x04;
const TAG_STR: u8 = 0x05;
const TAG_RUNE: u8 = 0x06;
const TAG_FN: u8 = 0x08;

/// Key for deduplication in the constant pool.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ConstKey {
    Int(i64),
    Float(u64), // f64 bits
    Bool(bool),
    Rune(u32),
    Str(Symbol),
    Fn(u32),
}

impl ConstKey {
    const fn from_value(v: &IrConstValue) -> Option<Self> {
        match v {
            IrConstValue::Int(n) => Some(Self::Int(*n)),
            IrConstValue::Float(f) => Some(Self::Float(f.to_bits())),
            IrConstValue::Bool(b) => Some(Self::Bool(*b)),
            IrConstValue::Rune(r) => Some(Self::Rune(*r)),
            IrConstValue::Str(s) => Some(Self::Str(*s)),
            IrConstValue::Unit => None,
            IrConstValue::FnRef(id) => Some(Self::Fn(*id)),
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
            entries: Vec::new(),
            index: HashMap::new(),
        }
    }

    /// Intern `value`, returning its pool index.
    ///
    /// Returns `None` for `IrConstValue::Unit` (unit is encoded inline, not via const pool).
    pub fn intern(
        &mut self,
        value: &IrConstValue,
        interner: &Interner,
    ) -> Result<Option<u16>, EmitError> {
        let Some(key) = ConstKey::from_value(value) else {
            return Ok(None);
        };
        if let Some(&idx) = self.index.get(&key) {
            return Ok(Some(idx));
        }
        let idx = u16::try_from(self.entries.len()).map_err(|_| EmitError::TooManyConsts)?;
        let entry = encode_const(value, interner);
        self.entries.push(entry);
        let _ = self.index.insert(key, idx);
        Ok(Some(idx))
    }

    /// Serialize the constant pool into `buf`.
    pub fn write_into(&self, buf: &mut Vec<u8>) -> Result<(), EmitError> {
        let count = u32::try_from(self.entries.len()).map_err(|_| EmitError::TooManyConsts)?;
        buf.extend_from_slice(&count.to_le_bytes());
        for entry in &self.entries {
            buf.push(entry.tag);
            buf.extend_from_slice(&entry.data);
        }
        Ok(())
    }
}

fn encode_const(value: &IrConstValue, interner: &Interner) -> ConstEntry {
    match value {
        IrConstValue::Int(n) => encode_int(*n),
        IrConstValue::Float(f) => ConstEntry {
            tag: TAG_F64,
            data: f.to_bits().to_le_bytes().to_vec(),
        },
        IrConstValue::Bool(b) => {
            let v = i32::from(*b);
            ConstEntry {
                tag: TAG_I32,
                data: v.to_le_bytes().to_vec(),
            }
        }
        IrConstValue::Rune(r) => ConstEntry {
            tag: TAG_RUNE,
            data: r.to_le_bytes().to_vec(),
        },
        IrConstValue::Str(sym) => encode_str(*sym, interner),
        IrConstValue::Unit => {
            // Unit should not reach here — filtered before calling encode_const
            ConstEntry {
                tag: TAG_I32,
                data: 0i32.to_le_bytes().to_vec(),
            }
        }
        IrConstValue::FnRef(id) => ConstEntry {
            tag: TAG_FN,
            data: id.to_le_bytes().to_vec(),
        },
    }
}

fn encode_int(n: i64) -> ConstEntry {
    if i32::try_from(n).is_ok() {
        let v = i32::try_from(n).expect("just checked above");
        ConstEntry {
            tag: TAG_I32,
            data: v.to_le_bytes().to_vec(),
        }
    } else {
        ConstEntry {
            tag: TAG_I64,
            data: n.to_le_bytes().to_vec(),
        }
    }
}

fn encode_str(sym: Symbol, interner: &Interner) -> ConstEntry {
    let s = interner.resolve(sym);
    let bytes = s.as_bytes();
    let len = u32::try_from(bytes.len()).expect("string too long for u32");
    let mut data = Vec::with_capacity(4 + bytes.len());
    data.extend_from_slice(&len.to_le_bytes());
    data.extend_from_slice(bytes);
    ConstEntry { tag: TAG_STR, data }
}
