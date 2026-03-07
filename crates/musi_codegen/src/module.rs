//! The `.mso` binary container: const pool, symbol table, function table, code.

use std::io::{Cursor, Read as _};
use std::str::from_utf8;

use byteorder::{LE, ReadBytesExt as _};

use crate::error::{CodegenError, DeserError};

/// Magic bytes at the start of every `.mso` file.
const MAGIC: [u8; 4] = *b"MUSI";
/// The only supported format version.
const VERSION: u16 = 1;

/// Reads `len` bytes from `r` and interprets them as UTF-8.
fn read_str(r: &mut Cursor<&[u8]>, len: usize) -> Result<Box<str>, DeserError> {
    let mut buf = vec![0u8; len];
    r.read_exact(&mut buf)
        .map_err(|_| DeserError::UnexpectedEof)?;
    from_utf8(&buf)
        .map_err(|_| DeserError::InvalidUtf8)
        .map(Into::into)
}

fn write_section<T>(buf: &mut Vec<u8>, items: &[T], encode: fn(&T, &mut Vec<u8>), msg: &str) {
    let count = u32::try_from(items.len()).expect(msg);
    buf.extend_from_slice(&count.to_le_bytes());
    for item in items {
        encode(item, buf);
    }
}

fn read_section<T>(
    r: &mut Cursor<&[u8]>,
    decode: fn(&mut Cursor<&[u8]>) -> Result<T, DeserError>,
) -> Result<Vec<T>, DeserError> {
    let n = r.read_u32::<LE>().map_err(|_| DeserError::UnexpectedEof)?;
    let mut items = Vec::with_capacity(usize::try_from(n).unwrap_or(0));
    for _ in 0..n {
        items.push(decode(r)?);
    }
    Ok(items)
}

/// Appends `entry` to `vec` and returns its index as a `u16`.
///
/// Returns `err` if the vector already has `u16::MAX` elements.
fn push_entry<T>(vec: &mut Vec<T>, entry: T, err: CodegenError) -> Result<u16, CodegenError> {
    let idx = u16::try_from(vec.len()).map_err(|_| err)?;
    vec.push(entry);
    Ok(idx)
}

/// A single entry in the const pool.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstEntry {
    /// A 64-bit signed integer constant.
    Int(i64),
    /// A 64-bit floating-point constant.
    Float(f64),
    /// A UTF-8 string constant.
    String(Box<str>),
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
        }
    }

    fn decode(r: &mut Cursor<&[u8]>) -> Result<Self, DeserError> {
        let tag = r.read_u8().map_err(|_| DeserError::UnexpectedEof)?;
        match tag {
            0x01 => Ok(Self::Int(
                r.read_i64::<LE>().map_err(|_| DeserError::UnexpectedEof)?,
            )),
            0x02 => Ok(Self::Float(
                r.read_f64::<LE>().map_err(|_| DeserError::UnexpectedEof)?,
            )),
            0x03 => {
                let len = r.read_u32::<LE>().map_err(|_| DeserError::UnexpectedEof)?;
                let len_usize = usize::try_from(len).map_err(|_| DeserError::UnexpectedEof)?;
                Ok(Self::String(read_str(r, len_usize)?))
            }
            _ => Err(DeserError::UnknownConstTag(tag)),
        }
    }
}

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
    /// Flags (native, export, ...).
    pub flags: SymbolFlags,
    /// Intrinsic ID used when the native flag is set; `0xFFFF` means none.
    pub intrinsic_id: u16,
    /// ABI string for native functions (e.g. `"C"`, `"musi"`). Empty string = no ABI.
    pub abi: Box<str>,
    /// Library to load for extrin fns (from `#[link("libm")]`). `None` = default namespace.
    pub link_lib: Option<Box<str>>,
    /// Symbol name override for extrin fns (from `#[link(name := "sym")]`). `None` = use `name`.
    pub link_name: Option<Box<str>>,
}

fn encode_opt_str(buf: &mut Vec<u8>, s: Option<&str>) {
    match s {
        None => buf.push(0),
        Some(v) => {
            let bytes = v.as_bytes();
            let len = u16::try_from(bytes.len()).expect("string fits u16");
            buf.push(1);
            buf.extend_from_slice(&len.to_le_bytes());
            buf.extend_from_slice(bytes);
        }
    }
}

fn decode_opt_str(r: &mut Cursor<&[u8]>) -> Result<Option<Box<str>>, DeserError> {
    let tag = r.read_u8().map_err(|_| DeserError::UnexpectedEof)?;
    if tag == 0 {
        return Ok(None);
    }
    let len = r.read_u16::<LE>().map_err(|_| DeserError::UnexpectedEof)?;
    Ok(Some(read_str(r, usize::from(len))?))
}

impl SymbolEntry {
    fn encode_into(&self, buf: &mut Vec<u8>) {
        let name_bytes = self.name.as_bytes();
        let name_len = u16::try_from(name_bytes.len()).expect("symbol name fits u16");
        buf.extend_from_slice(&name_len.to_le_bytes());
        buf.extend_from_slice(name_bytes);
        buf.push(self.flags.raw());
        buf.extend_from_slice(&self.intrinsic_id.to_le_bytes());
        let abi_bytes = self.abi.as_bytes();
        let abi_len = u16::try_from(abi_bytes.len()).expect("ABI string fits u16");
        buf.extend_from_slice(&abi_len.to_le_bytes());
        buf.extend_from_slice(abi_bytes);
        encode_opt_str(buf, self.link_lib.as_deref());
        encode_opt_str(buf, self.link_name.as_deref());
    }

    fn decode(r: &mut Cursor<&[u8]>) -> Result<Self, DeserError> {
        let name_len = r.read_u16::<LE>().map_err(|_| DeserError::UnexpectedEof)?;
        let name = read_str(r, usize::from(name_len))?;
        let flags = SymbolFlags::new(r.read_u8().map_err(|_| DeserError::UnexpectedEof)?);
        let intrinsic_id = r.read_u16::<LE>().map_err(|_| DeserError::UnexpectedEof)?;
        let abi_len = r.read_u16::<LE>().map_err(|_| DeserError::UnexpectedEof)?;
        let abi = read_str(r, usize::from(abi_len))?;
        let link_lib = decode_opt_str(r)?;
        let link_name = decode_opt_str(r)?;
        Ok(Self {
            name,
            flags,
            intrinsic_id,
            abi,
            link_lib,
            link_name,
        })
    }
}

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

    fn decode(r: &mut Cursor<&[u8]>) -> Result<Self, DeserError> {
        Ok(Self {
            symbol_idx: r.read_u16::<LE>().map_err(|_| DeserError::UnexpectedEof)?,
            param_count: r.read_u8().map_err(|_| DeserError::UnexpectedEof)?,
            local_count: r.read_u16::<LE>().map_err(|_| DeserError::UnexpectedEof)?,
            code_offset: r.read_u32::<LE>().map_err(|_| DeserError::UnexpectedEof)?,
            code_length: r.read_u32::<LE>().map_err(|_| DeserError::UnexpectedEof)?,
        })
    }
}

/// A method registered via `given` — maps (method name, type tag) → function index.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodEntry {
    /// Method name, e.g. `"eq"`.
    pub name: Box<str>,
    /// Type tag of the receiver (0–9 = primitives, 10+ = user-defined types).
    pub type_tag: u16,
    /// Index into `function_table`.
    pub fn_idx: u16,
}

/// A compiled Musi module in memory.
///
/// Corresponds to the `.mso` binary format: header, const pool, symbol table,
/// function table, and a flat code section.
/// `method_table` is runtime-only and is not serialized.
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    /// Constants referenced by bytecode via `LdConst`.
    pub const_pool: Vec<ConstEntry>,
    /// Named symbols; each [`FunctionEntry`] points into this table.
    pub symbol_table: Vec<SymbolEntry>,
    /// Function definitions (code offsets, parameter counts, ...).
    pub function_table: Vec<FunctionEntry>,
    /// Raw bytecode for all functions, concatenated.
    pub code: Vec<u8>,
    /// Runtime dispatch table for class method implementations.
    pub method_table: Vec<MethodEntry>,
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
            method_table: Vec::new(),
        }
    }

    /// Serializes this module into `.mso` binary format.
    ///
    /// # Panics
    ///
    /// Panics if any section contains more than `u32::MAX` entries or bytes
    /// (impossible in practice -- this would require >4 GiB of data).
    #[must_use]
    pub fn serialize(&self) -> Vec<u8> {
        let mut buf = Vec::new();

        // Header
        buf.extend_from_slice(&MAGIC);
        buf.extend_from_slice(&VERSION.to_le_bytes());
        buf.extend_from_slice(&0u16.to_le_bytes()); // flags (reserved)

        write_section(
            &mut buf,
            &self.const_pool,
            ConstEntry::encode_into,
            "const pool count fits u32",
        );
        write_section(
            &mut buf,
            &self.symbol_table,
            SymbolEntry::encode_into,
            "symbol table count fits u32",
        );
        write_section(
            &mut buf,
            &self.function_table,
            FunctionEntry::encode_into,
            "function table count fits u32",
        );

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
        let mut r = Cursor::new(bytes);

        // Header
        let mut magic = [0u8; 4];
        r.read_exact(&mut magic)
            .map_err(|_| DeserError::UnexpectedEof)?;
        if magic != MAGIC {
            return Err(DeserError::InvalidMagic);
        }
        let version = r.read_u16::<LE>().map_err(|_| DeserError::UnexpectedEof)?;
        if version != VERSION {
            return Err(DeserError::UnsupportedVersion(version));
        }
        let _flags = r.read_u16::<LE>().map_err(|_| DeserError::UnexpectedEof)?;

        let const_pool = read_section(&mut r, ConstEntry::decode)?;
        let symbol_table = read_section(&mut r, SymbolEntry::decode)?;
        let function_table = read_section(&mut r, FunctionEntry::decode)?;

        // Code section
        let code_len = r.read_u32::<LE>().map_err(|_| DeserError::UnexpectedEof)?;
        let code_len_usize = usize::try_from(code_len).map_err(|_| DeserError::UnexpectedEof)?;
        let mut code = vec![0u8; code_len_usize];
        r.read_exact(&mut code)
            .map_err(|_| DeserError::UnexpectedEof)?;

        Ok(Self {
            const_pool,
            symbol_table,
            function_table,
            code,
            method_table: Vec::new(),
        })
    }

    /// Appends a symbol entry and returns its index.
    ///
    /// # Errors
    ///
    /// Returns [`CodegenError::TooManySymbols`] if the symbol table already
    /// contains `u16::MAX` entries.
    pub fn push_symbol(&mut self, entry: SymbolEntry) -> Result<u16, CodegenError> {
        push_entry(&mut self.symbol_table, entry, CodegenError::TooManySymbols)
    }

    /// Appends a function entry and returns its index.
    ///
    /// # Errors
    ///
    /// Returns [`CodegenError::TooManyFunctions`] if the function table already
    /// contains `u16::MAX` entries.
    pub fn push_function(&mut self, entry: FunctionEntry) -> Result<u16, CodegenError> {
        push_entry(
            &mut self.function_table,
            entry,
            CodegenError::TooManyFunctions,
        )
    }

    /// Appends a const-pool entry and returns its index.
    ///
    /// # Errors
    ///
    /// Returns [`CodegenError::TooManyConstants`] if the const pool already
    /// contains `u16::MAX` entries.
    pub fn push_const(&mut self, entry: ConstEntry) -> Result<u16, CodegenError> {
        push_entry(&mut self.const_pool, entry, CodegenError::TooManyConstants)
    }

    /// Interns a string in the const pool, reusing an existing entry if present.
    ///
    /// # Errors
    ///
    /// Returns [`CodegenError::TooManyConstants`] if the const pool is full.
    pub fn add_string_const(&mut self, s: &str) -> Result<u16, CodegenError> {
        for (i, entry) in self.const_pool.iter().enumerate() {
            if let ConstEntry::String(existing) = entry
                && existing.as_ref() == s
            {
                return u16::try_from(i).map_err(|_| CodegenError::TooManyConstants);
            }
        }
        self.push_const(ConstEntry::String(s.into()))
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
