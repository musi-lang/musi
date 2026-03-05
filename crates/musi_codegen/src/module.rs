//! The `.mso` binary container: const pool, symbol table, function table, code.

use core::str;

use crate::error::DeserError;

/// Magic bytes at the start of every `.mso` file.
const MAGIC: [u8; 4] = *b"MUSI";
/// The only supported format version.
const VERSION: u16 = 1;

// ── Const pool ────────────────────────────────────────────────────────────────

/// A single entry in the const pool.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstEntry {
    /// A 64-bit signed integer constant.
    Int(i64),
    /// A 64-bit floating-point constant.
    Float(f64),
    /// A UTF-8 string constant.
    String(Box<str>),
    /// A boolean constant.
    Bool(bool),
}

impl ConstEntry {
    fn encode_into(&self, buf: &mut Vec<u8>) {
        match self {
            Self::Int(v) => {
                buf.push(0x01);
                buf.extend_from_slice(&v.to_le_bytes());
            }
            Self::Float(v) => {
                buf.push(0x02);
                buf.extend_from_slice(&v.to_le_bytes());
            }
            Self::String(s) => {
                buf.push(0x03);
                let len = u32::try_from(s.len()).expect("string length fits u32");
                buf.extend_from_slice(&len.to_le_bytes());
                buf.extend_from_slice(s.as_bytes());
            }
            Self::Bool(v) => {
                buf.push(0x04);
                buf.push(u8::from(*v));
            }
        }
    }

    fn decode(reader: &mut Reader<'_>) -> Result<Self, DeserError> {
        let tag = reader.read_u8()?;
        match tag {
            0x01 => Ok(Self::Int(reader.read_i64()?)),
            0x02 => Ok(Self::Float(reader.read_f64()?)),
            0x03 => {
                let len = reader.read_u32()?;
                let len_usize = usize::try_from(len).map_err(|_| DeserError::UnexpectedEof)?;
                Ok(Self::String(reader.read_str(len_usize)?))
            }
            0x04 => Ok(Self::Bool(reader.read_u8()? != 0)),
            _ => Err(DeserError::UnknownConstTag(tag)),
        }
    }
}

// ── Symbol table ──────────────────────────────────────────────────────────────

/// Bit flags for a symbol table entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolFlags(u8);

impl SymbolFlags {
    /// The symbol is implemented by a native (host) function.
    pub const NATIVE: u8 = 0x01;
    /// The symbol is exported from the module.
    pub const EXPORT: u8 = 0x02;

    /// Creates [`SymbolFlags`] from a raw byte.
    #[must_use]
    pub const fn new(raw: u8) -> Self {
        Self(raw)
    }

    /// Returns `true` if the native flag is set.
    #[must_use]
    pub const fn is_native(self) -> bool {
        self.0 & Self::NATIVE != 0
    }

    /// Returns `true` if the export flag is set.
    #[must_use]
    pub const fn is_export(self) -> bool {
        self.0 & Self::EXPORT != 0
    }

    /// Returns the raw byte value.
    #[must_use]
    pub const fn raw(self) -> u8 {
        self.0
    }
}

/// A single entry in the symbol table.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolEntry {
    /// The symbol's name (UTF-8).
    pub name: Box<str>,
    /// Flags (native, export, …).
    pub flags: SymbolFlags,
    /// Intrinsic ID used when the native flag is set; `0xFFFF` means none.
    pub intrinsic_id: u16,
}

impl SymbolEntry {
    fn encode_into(&self, buf: &mut Vec<u8>) {
        let name_bytes = self.name.as_bytes();
        let name_len = u16::try_from(name_bytes.len()).expect("symbol name fits u16");
        buf.extend_from_slice(&name_len.to_le_bytes());
        buf.extend_from_slice(name_bytes);
        buf.push(self.flags.raw());
        buf.extend_from_slice(&self.intrinsic_id.to_le_bytes());
    }

    fn decode(reader: &mut Reader<'_>) -> Result<Self, DeserError> {
        let name_len = reader.read_u16()?;
        let name = reader.read_str(usize::from(name_len))?;
        let flags = SymbolFlags::new(reader.read_u8()?);
        let intrinsic_id = reader.read_u16()?;
        Ok(Self {
            name,
            flags,
            intrinsic_id,
        })
    }
}

// ── Function table ────────────────────────────────────────────────────────────

/// A single entry in the function table.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionEntry {
    /// Index of this function's name in the symbol table.
    pub symbol_idx: u16,
    /// Number of parameters (filled into local slots `0..param_count`).
    pub param_count: u8,
    /// Total local variable slots (≥ `param_count`).
    pub local_count: u16,
    /// Byte offset of this function's bytecode in the code section.
    pub code_offset: u32,
    /// Byte length of this function's bytecode.
    pub code_length: u32,
}

impl FunctionEntry {
    fn encode_into(&self, buf: &mut Vec<u8>) {
        buf.extend_from_slice(&self.symbol_idx.to_le_bytes());
        buf.push(self.param_count);
        buf.extend_from_slice(&self.local_count.to_le_bytes());
        buf.extend_from_slice(&self.code_offset.to_le_bytes());
        buf.extend_from_slice(&self.code_length.to_le_bytes());
    }

    fn decode(reader: &mut Reader<'_>) -> Result<Self, DeserError> {
        Ok(Self {
            symbol_idx: reader.read_u16()?,
            param_count: reader.read_u8()?,
            local_count: reader.read_u16()?,
            code_offset: reader.read_u32()?,
            code_length: reader.read_u32()?,
        })
    }
}

// ── Module ────────────────────────────────────────────────────────────────────

/// A compiled Musi module in memory.
///
/// Corresponds to the `.mso` binary format: header, const pool, symbol table,
/// function table, and a flat code section.
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    /// Constants referenced by bytecode via `LdConst`.
    pub const_pool: Vec<ConstEntry>,
    /// Named symbols; each [`FunctionEntry`] points into this table.
    pub symbol_table: Vec<SymbolEntry>,
    /// Function definitions (code offsets, parameter counts, …).
    pub function_table: Vec<FunctionEntry>,
    /// Raw bytecode for all functions, concatenated.
    pub code: Vec<u8>,
}

impl Module {
    /// Creates a new, empty module.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            const_pool: Vec::new(),
            symbol_table: Vec::new(),
            function_table: Vec::new(),
            code: Vec::new(),
        }
    }

    /// Serializes this module into `.mso` binary format.
    ///
    /// # Panics
    ///
    /// Panics if any section contains more than `u32::MAX` entries or bytes
    /// (impossible in practice — this would require >4 GiB of data).
    #[must_use]
    pub fn serialize(&self) -> Vec<u8> {
        let mut buf = Vec::new();

        // Header
        buf.extend_from_slice(&MAGIC);
        buf.extend_from_slice(&VERSION.to_le_bytes());
        buf.extend_from_slice(&0u16.to_le_bytes()); // flags (reserved)

        // Const pool
        let count = u32::try_from(self.const_pool.len()).expect("const pool count fits u32");
        buf.extend_from_slice(&count.to_le_bytes());
        for entry in &self.const_pool {
            entry.encode_into(&mut buf);
        }

        // Symbol table
        let count = u32::try_from(self.symbol_table.len()).expect("symbol table count fits u32");
        buf.extend_from_slice(&count.to_le_bytes());
        for sym in &self.symbol_table {
            sym.encode_into(&mut buf);
        }

        // Function table
        let count =
            u32::try_from(self.function_table.len()).expect("function table count fits u32");
        buf.extend_from_slice(&count.to_le_bytes());
        for func in &self.function_table {
            func.encode_into(&mut buf);
        }

        // Code section
        let length = u32::try_from(self.code.len()).expect("code section length fits u32");
        buf.extend_from_slice(&length.to_le_bytes());
        buf.extend_from_slice(&self.code);

        buf
    }

    /// Deserializes a module from `.mso` binary format.
    ///
    /// # Errors
    ///
    /// Returns a [`DeserError`] if the bytes are malformed, truncated, contain
    /// an unknown section tag, or use an unsupported version number.
    pub fn deserialize(bytes: &[u8]) -> Result<Self, DeserError> {
        let mut r = Reader::new(bytes);

        // Header
        if r.read_4()? != MAGIC {
            return Err(DeserError::InvalidMagic);
        }
        let version = r.read_u16()?;
        if version != VERSION {
            return Err(DeserError::UnsupportedVersion(version));
        }
        let _flags = r.read_u16()?;

        // Const pool
        let n = r.read_u32()?;
        let mut const_pool = Vec::new();
        for _ in 0..n {
            const_pool.push(ConstEntry::decode(&mut r)?);
        }

        // Symbol table
        let n = r.read_u32()?;
        let mut symbol_table = Vec::new();
        for _ in 0..n {
            symbol_table.push(SymbolEntry::decode(&mut r)?);
        }

        // Function table
        let n = r.read_u32()?;
        let mut function_table = Vec::new();
        for _ in 0..n {
            function_table.push(FunctionEntry::decode(&mut r)?);
        }

        // Code section
        let code_len = r.read_u32()?;
        let code_len_usize = usize::try_from(code_len).map_err(|_| DeserError::UnexpectedEof)?;
        let code = r.read_bytes(code_len_usize)?.to_vec();

        Ok(Self {
            const_pool,
            symbol_table,
            function_table,
            code,
        })
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

// ── Binary reader ─────────────────────────────────────────────────────────────

/// A cursor over a borrowed byte slice.
struct Reader<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> Reader<'a> {
    const fn new(data: &'a [u8]) -> Self {
        Self { data, pos: 0 }
    }

    /// Advances `pos` by `n` and returns a slice of those bytes.
    ///
    /// The returned slice carries the *data* lifetime `'a`, not the `&mut self`
    /// borrow lifetime, so callers can invoke further methods right after.
    fn read_bytes(&mut self, n: usize) -> Result<&'a [u8], DeserError> {
        let end = self.pos.checked_add(n).ok_or(DeserError::UnexpectedEof)?;
        let slice = self
            .data
            .get(self.pos..end)
            .ok_or(DeserError::UnexpectedEof)?;
        self.pos = end;
        Ok(slice)
    }

    fn read_u8(&mut self) -> Result<u8, DeserError> {
        let s = self.read_bytes(1)?;
        s.first().copied().ok_or(DeserError::UnexpectedEof)
    }

    fn read_u16(&mut self) -> Result<u16, DeserError> {
        let s = self.read_bytes(2)?;
        let mut arr = [0u8; 2];
        arr.copy_from_slice(s);
        Ok(u16::from_le_bytes(arr))
    }

    fn read_u32(&mut self) -> Result<u32, DeserError> {
        let s = self.read_bytes(4)?;
        let mut arr = [0u8; 4];
        arr.copy_from_slice(s);
        Ok(u32::from_le_bytes(arr))
    }

    fn read_i64(&mut self) -> Result<i64, DeserError> {
        let s = self.read_bytes(8)?;
        let mut arr = [0u8; 8];
        arr.copy_from_slice(s);
        Ok(i64::from_le_bytes(arr))
    }

    fn read_f64(&mut self) -> Result<f64, DeserError> {
        let s = self.read_bytes(8)?;
        let mut arr = [0u8; 8];
        arr.copy_from_slice(s);
        Ok(f64::from_le_bytes(arr))
    }

    fn read_4(&mut self) -> Result<[u8; 4], DeserError> {
        let s = self.read_bytes(4)?;
        let mut arr = [0u8; 4];
        arr.copy_from_slice(s);
        Ok(arr)
    }

    fn read_str(&mut self, len: usize) -> Result<Box<str>, DeserError> {
        let bytes = self.read_bytes(len)?;
        let s = str::from_utf8(bytes).map_err(|_| DeserError::InvalidUtf8)?;
        Ok(s.into())
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests;
