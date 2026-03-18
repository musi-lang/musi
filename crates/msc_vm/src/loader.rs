//! `.seam` binary loader.
//!
//! Parses raw bytes into a `LoadedModule` without any dependency on compiler
//! crates. The binary format is the SEAM format described in Phase 3.

use core::str;

use msc_bc::crc32_slice;

use crate::error::VmError;

const MAGIC: &[u8; 4] = b"SEAM";
const HEADER_SIZE: usize = 16;

// Constant pool entry tags (from msc_emit/src/const_pool.rs)
const TAG_CONST_INT: u8 = 0x01;
const TAG_CONST_FLOAT: u8 = 0x02;
const TAG_CONST_STR: u8 = 0x03;

/// A decoded constant pool entry.
#[derive(Debug, Clone)]
pub enum LoadedConst {
    Int(i64),
    Float(f64),
    Str(Box<str>),
}

/// A decoded type pool entry (stored as raw tag + opaque payload).
#[derive(Debug, Clone)]
pub struct LoadedType {
    pub tag: u8,
    pub data: Vec<u8>,
}

/// A decoded global variable entry.
#[derive(Debug, Clone)]
pub struct LoadedGlobal {
    pub name: Box<str>,
    pub type_ref: u16,
    pub flags: u8,
    pub initializer: Option<u16>,
}

/// A decoded effect definition.
#[derive(Debug, Clone)]
pub struct LoadedEffect {
    pub id: u16,
    /// Effect name resolved from the string table.
    pub name: Box<str>,
    pub ops: Vec<LoadedEffectOp>,
}

/// A decoded effect operation.
#[derive(Debug, Clone)]
pub struct LoadedEffectOp {
    pub id: u16,
    /// Op name resolved from the string table.
    pub name: Box<str>,
    pub param_type_ids: Vec<u32>,
    pub ret_type_id: u32,
    pub fatal: bool,
}

/// A decoded foreign (FFI) function entry.
#[derive(Debug, Clone)]
pub struct LoadedForeignFn {
    /// C-side symbol name.
    pub ext_name: Box<str>,
    /// Library name (empty = default C library).
    pub lib_name: Box<str>,
    /// Library kind hint: "dylib", "framework", "raw", or empty for default.
    pub link_kind: Box<str>,
    /// Number of fixed parameters.
    pub param_count: u16,
    /// Type pool ids for each parameter.
    pub param_type_ids: Vec<u32>,
    /// Type pool id for the return type.
    pub ret_type_id: u32,
    /// Whether this function is variadic.
    pub variadic: bool,
}

/// Decoded class table from the CLSS section.
///
/// Currently always empty - the emitter writes a zero-count stub. Stored so
/// the data is not silently discarded when future class definitions are added.
#[derive(Debug, Default, Clone)]
pub struct ClassTable {
    /// Number of class entries present (zero until class emission is implemented).
    pub count: u16,
}

/// A decoded function entry.
#[derive(Debug, Clone)]
pub struct LoadedFn {
    pub name_stridx: u16,
    pub type_id: u16,
    pub param_count: u16,
    pub local_count: u16,
    pub max_stack: u16,
    pub upvalue_count: u16,
    pub code: Box<[u8]>,
    pub handlers: Vec<HandlerEntry>,
}

/// An effect handler registration entry from the function's handler table.
#[derive(Debug, Clone)]
pub struct HandlerEntry {
    pub effect_id: u8,
    pub handler_fn_id: u32,
}

/// The fully decoded contents of a `.seam` module.
#[derive(Debug)]
pub struct LoadedModule {
    pub flags: u8,
    /// `None` for library modules (`FLAG_LIBRARY` set, no entry point).
    pub entry_point: Option<u32>,
    pub strings: Vec<Box<str>>,
    pub consts: Vec<LoadedConst>,
    pub types: Vec<LoadedType>,
    pub globals: Vec<LoadedGlobal>,
    pub effects: Vec<LoadedEffect>,
    pub foreign_fns: Vec<LoadedForeignFn>,
    pub functions: Vec<LoadedFn>,
    /// Class table parsed from the CLSS section. Currently always empty.
    pub classes: ClassTable,
}

impl LoadedModule {
    /// Look up a function by index (`fn_id` = array index). Returns `None` if out of bounds.
    #[must_use]
    pub fn fn_by_index(&self, idx: usize) -> Option<&LoadedFn> {
        self.functions.get(idx)
    }
}

// Section tags
const FLAG_SCRIPT: u8 = 0x04;
const TAG_STRT: &[u8; 4] = b"STRT";
const TAG_TYPE: &[u8; 4] = b"TYPE";
const TAG_CNST: &[u8; 4] = b"CNST";
const TAG_DEPS: &[u8; 4] = b"DEPS";
const TAG_GLOB: &[u8; 4] = b"GLOB";
const TAG_METH: &[u8; 4] = b"METH";
const TAG_EFCT: &[u8; 4] = b"EFCT";
const TAG_CLSS: &[u8; 4] = b"CLSS";
const TAG_FRGN: &[u8; 4] = b"FRGN";

/// Parse raw `.seam` bytes into a `LoadedModule`.
///
/// Validates the magic bytes and CRC-32 before decoding sections.
///
/// # Errors
///
/// Returns `VmError::Malformed` if the input is too short, contains invalid
/// section data, or has structural inconsistencies. Returns `VmError::BadMagic`
/// if the magic bytes do not match. Returns `VmError::BadChecksum` if the
/// CRC-32 does not match.
///
/// # Panics
///
/// Panics if the header length check passes but the bytes slice is shorter than
/// 12 bytes (unreachable in practice due to the prior length guard).
pub fn load(bytes: &[u8]) -> Result<LoadedModule, VmError> {
    if bytes.len() < HEADER_SIZE {
        return Err(VmError::Malformed {
            desc: "file too short to contain header".into(),
        });
    }

    // Magic check.
    if bytes.get(..4) != Some(MAGIC.as_slice()) {
        return Err(VmError::BadMagic);
    }

    // CRC-32 over all section bytes (bytes[16..]).
    let stored_crc = {
        let mut c = Cursor::new(bytes, 12);
        c.read_u32()?
    };
    let section_bytes = bytes.get(HEADER_SIZE..).ok_or_else(|| VmError::Malformed {
        desc: "header truncated before sections".into(),
    })?;
    let computed_crc = crc32_slice(section_bytes);
    if stored_crc != computed_crc {
        return Err(VmError::BadChecksum);
    }

    // Parse header fields.
    assert!(bytes.len() > 11);
    let flags = bytes[7];
    let name_stridx = u16::from_be_bytes([bytes[8], bytes[9]]);
    let path_stridx = u16::from_be_bytes([bytes[10], bytes[11]]);
    // name_stridx and path_stridx are resolved after STRT is parsed.
    let _ = name_stridx;
    let _ = path_stridx;

    // Walk sections by tag+length in order.
    let mut strings: Vec<Box<str>> = vec![];
    let mut consts: Vec<LoadedConst> = vec![];
    let mut types: Vec<LoadedType> = vec![];
    let mut globals: Vec<LoadedGlobal> = vec![];
    let mut effects: Vec<LoadedEffect> = vec![];
    let mut foreign_fns: Vec<LoadedForeignFn> = vec![];
    let mut functions: Vec<LoadedFn> = vec![];
    let mut classes = ClassTable::default();

    let mut cur = Cursor::new(bytes, HEADER_SIZE);
    while cur.pos < bytes.len() {
        let tag = cur.read_bytes(4)?;
        let payload_len = usize::try_from(cur.read_u32()?).map_err(|_| VmError::Malformed {
            desc: "section payload length overflow".into(),
        })?;
        let payload_start = cur.pos;
        let payload = cur.read_bytes(payload_len)?;

        let mut sc = Cursor::new(payload, 0);
        if tag == TAG_STRT {
            strings = parse_string_table(&mut sc)?;
        } else if tag == TAG_TYPE {
            types = parse_type_section(&mut sc)?;
        } else if tag == TAG_CNST {
            consts = parse_const_section(&mut sc, &strings)?;
        } else if tag == TAG_DEPS {
            // Ignore: count is always 0 for now.
        } else if tag == TAG_GLOB {
            globals = parse_glob_section(&mut sc, &strings)?;
        } else if tag == TAG_METH {
            functions = parse_meth_section(&mut sc)?;
        } else if tag == TAG_EFCT {
            effects = parse_efct_section(&mut sc, &strings)?;
        } else if tag == TAG_CLSS {
            classes = parse_clss_section(&mut sc)?;
        } else if tag == TAG_FRGN {
            foreign_fns = parse_frgn_section(&mut sc, &strings)?;
        }
        // DBUG: silently skip.
        let _ = payload_start;
    }

    let entry_point = if flags & FLAG_SCRIPT != 0 && !functions.is_empty() {
        Some(0u32)
    } else {
        None
    };

    Ok(LoadedModule {
        flags,
        entry_point,
        strings,
        consts,
        types,
        globals,
        effects,
        foreign_fns,
        functions,
        classes,
    })
}

fn parse_string_table(cur: &mut Cursor<'_>) -> Result<Vec<Box<str>>, VmError> {
    let count = usize::from(cur.read_u16()?);
    let mut strings = Vec::with_capacity(count);
    for _ in 0..count {
        let len = usize::from(cur.read_u16()?);
        let bytes = cur.read_bytes(len)?;
        let s = str::from_utf8(bytes)
            .map_err(|_| VmError::Malformed {
                desc: "string table entry is not valid UTF-8".into(),
            })?
            .into();
        strings.push(s);
    }
    Ok(strings)
}

fn resolve_stridx(strings: &[Box<str>], idx: u16, ctx: &str) -> Result<Box<str>, VmError> {
    let i = usize::from(idx);
    strings.get(i).cloned().ok_or_else(|| VmError::Malformed {
        desc: format!(
            "{ctx}: string index {idx} out of bounds (table len={})",
            strings.len()
        )
        .into_boxed_str(),
    })
}

fn parse_const_section(
    cur: &mut Cursor<'_>,
    strings: &[Box<str>],
) -> Result<Vec<LoadedConst>, VmError> {
    let count = usize::from(cur.read_u16()?);
    let mut consts = Vec::with_capacity(count);
    for _ in 0..count {
        let tag = cur.read_u8()?;
        let c = match tag {
            TAG_CONST_INT => {
                let v = cur.read_i64()?;
                LoadedConst::Int(v)
            }
            TAG_CONST_FLOAT => {
                let bits = cur.read_u64()?;
                LoadedConst::Float(f64::from_bits(bits))
            }
            TAG_CONST_STR => {
                let stridx = cur.read_u16()?;
                let s = resolve_stridx(strings, stridx, "const pool STR")?;
                LoadedConst::Str(s)
            }
            _ => {
                return Err(VmError::Malformed {
                    desc: format!("unknown const tag {tag:#04x}").into_boxed_str(),
                });
            }
        };
        consts.push(c);
    }
    Ok(consts)
}

// Type tags from the emitter's type_table.rs
const TAG_TY_UNIT: u8 = 0x01;
const TAG_TY_BOOL: u8 = 0x02;
const TAG_TY_I8: u8 = 0x03;
const TAG_TY_I16: u8 = 0x04;
const TAG_TY_I32: u8 = 0x05;
const TAG_TY_I64: u8 = 0x06;
const TAG_TY_U8: u8 = 0x07;
const TAG_TY_U16: u8 = 0x08;
const TAG_TY_U32: u8 = 0x09;
const TAG_TY_U64: u8 = 0x0A;
const TAG_TY_F32: u8 = 0x0B;
const TAG_TY_F64: u8 = 0x0C;
const TAG_TY_RUNE: u8 = 0x0D;
const TAG_TY_PTR: u8 = 0x0E;
const TAG_TY_ARR: u8 = 0x0F;
const TAG_TY_PRODUCT: u8 = 0x10;
const TAG_TY_SUM: u8 = 0x11;
const TAG_TY_FN: u8 = 0x12;
const TAG_TY_REF: u8 = 0x13;
const TAG_TY_ANY: u8 = 0x14;
const TAG_TY_CSTRUCT: u8 = 0x15;

fn parse_type_section(cur: &mut Cursor<'_>) -> Result<Vec<LoadedType>, VmError> {
    let count = usize::from(cur.read_u16()?);
    let mut types = Vec::with_capacity(count);
    for _ in 0..count {
        let tag = cur.read_u8()?;
        let data = match tag {
            // No-payload primitives
            TAG_TY_UNIT | TAG_TY_BOOL | TAG_TY_I8 | TAG_TY_I16 | TAG_TY_I32 | TAG_TY_I64
            | TAG_TY_U8 | TAG_TY_U16 | TAG_TY_U32 | TAG_TY_U64 | TAG_TY_F32 | TAG_TY_F64
            | TAG_TY_RUNE | TAG_TY_ANY => vec![],
            // 4-byte inner type_id
            TAG_TY_PTR | TAG_TY_ARR | TAG_TY_REF => cur.read_bytes(4)?.to_vec(),
            // Product: field_count:u32 BE + field_count * type_id:u32 BE
            TAG_TY_PRODUCT => {
                let field_count =
                    usize::try_from(cur.read_u32()?).map_err(|_| VmError::Malformed {
                        desc: "product field count overflow".into(),
                    })?;
                let byte_count = 4 + field_count * 4;
                let mut data = Vec::with_capacity(byte_count);
                data.extend_from_slice(
                    &u32::try_from(field_count)
                        .map_err(|_| VmError::Malformed {
                            desc: "product field count overflow".into(),
                        })?
                        .to_be_bytes(),
                );
                let fields = cur.read_bytes(field_count * 4)?;
                data.extend_from_slice(fields);
                data
            }
            // Sum: variant_count:u32 BE + variant_count * (tag:u32 BE + payload_id:u32 BE)
            TAG_TY_SUM => {
                let variant_count =
                    usize::try_from(cur.read_u32()?).map_err(|_| VmError::Malformed {
                        desc: "sum variant count overflow".into(),
                    })?;
                let byte_count = 4 + variant_count * 8;
                let mut data = Vec::with_capacity(byte_count);
                data.extend_from_slice(
                    &u32::try_from(variant_count)
                        .map_err(|_| VmError::Malformed {
                            desc: "sum variant count overflow".into(),
                        })?
                        .to_be_bytes(),
                );
                let variants = cur.read_bytes(variant_count * 8)?;
                data.extend_from_slice(variants);
                data
            }
            // CStruct: field_count:u16 BE + per-field(type_id:u32 + size:u16 + offset:u16) + total_size:u32 + alignment:u16
            TAG_TY_CSTRUCT => {
                let field_count = usize::from(cur.read_u16()?);
                let per_field_bytes = field_count * 8; // 4 + 2 + 2 per field
                let byte_count = 2 + per_field_bytes + 6; // header + fields + total_size(4) + alignment(2)
                let mut data = Vec::with_capacity(byte_count);
                data.extend_from_slice(
                    &u16::try_from(field_count)
                        .map_err(|_| VmError::Malformed {
                            desc: "cstruct field count overflow".into(),
                        })?
                        .to_be_bytes(),
                );
                let fields = cur.read_bytes(per_field_bytes)?;
                data.extend_from_slice(fields);
                let trailer = cur.read_bytes(6)?; // total_size:u32 + alignment:u16
                data.extend_from_slice(trailer);
                data
            }
            // Fn: param_count:u32 BE + params:u32[] BE + ret_id:u32 BE + effect_mask:u16 BE
            TAG_TY_FN => {
                let param_count =
                    usize::try_from(cur.read_u32()?).map_err(|_| VmError::Malformed {
                        desc: "fn param count overflow".into(),
                    })?;
                let byte_count = 4 + param_count * 4 + 4 + 2;
                let mut data = Vec::with_capacity(byte_count);
                data.extend_from_slice(
                    &u32::try_from(param_count)
                        .map_err(|_| VmError::Malformed {
                            desc: "fn param count overflow".into(),
                        })?
                        .to_be_bytes(),
                );
                let rest = cur.read_bytes(param_count * 4 + 4 + 2)?;
                data.extend_from_slice(rest);
                data
            }
            _ => {
                return Err(VmError::Malformed {
                    desc: format!("unknown type tag {tag:#04x}").into_boxed_str(),
                });
            }
        };
        types.push(LoadedType { tag, data });
    }
    Ok(types)
}

fn parse_glob_section(
    cur: &mut Cursor<'_>,
    strings: &[Box<str>],
) -> Result<Vec<LoadedGlobal>, VmError> {
    let count = usize::from(cur.read_u16()?);
    let mut globals = Vec::with_capacity(count);
    for _ in 0..count {
        let name_stridx = cur.read_u16()?;
        let type_ref = cur.read_u16()?;
        let flags = cur.read_u8()?;
        let initializer = if flags & 0x04 != 0 {
            Some(cur.read_u16()?)
        } else {
            None
        };
        let name = resolve_stridx(strings, name_stridx, "global name")?;
        globals.push(LoadedGlobal {
            name,
            type_ref,
            flags,
            initializer,
        });
    }
    Ok(globals)
}

fn parse_meth_section(cur: &mut Cursor<'_>) -> Result<Vec<LoadedFn>, VmError> {
    let count = usize::from(cur.read_u16()?);
    let mut functions = Vec::with_capacity(count);
    for _ in 0..count {
        let name_stridx = cur.read_u16()?;
        let type_id = cur.read_u16()?;
        let param_count = cur.read_u16()?;
        let local_count = cur.read_u16()?;
        let max_stack = cur.read_u16()?;
        let upvalue_count = cur.read_u16()?;
        let code_len = usize::try_from(cur.read_u32()?).map_err(|_| VmError::Malformed {
            desc: "function code length overflow".into(),
        })?;
        let code = cur.read_bytes(code_len)?.into();
        let handler_count = usize::from(cur.read_u16()?);
        let mut handlers = Vec::with_capacity(handler_count);
        for _ in 0..handler_count {
            let effect_id = cur.read_u8()?;
            let handler_fn_id = cur.read_u32()?;
            handlers.push(HandlerEntry {
                effect_id,
                handler_fn_id,
            });
        }
        // safepoint_count and effect_set_count - always 0, consume them.
        let _safepoint_count = cur.read_u16()?;
        let _effect_set_count = cur.read_u16()?;
        functions.push(LoadedFn {
            name_stridx,
            type_id,
            param_count,
            local_count,
            max_stack,
            upvalue_count,
            code,
            handlers,
        });
    }
    Ok(functions)
}

fn parse_efct_section(
    cur: &mut Cursor<'_>,
    strings: &[Box<str>],
) -> Result<Vec<LoadedEffect>, VmError> {
    let count = usize::from(cur.read_u16()?);
    let mut effects = Vec::with_capacity(count);
    for _ in 0..count {
        let id = cur.read_u16()?;
        let name_stridx = cur.read_u16()?;
        let name = resolve_stridx(strings, name_stridx, "effect name")?;
        let op_count = usize::from(cur.read_u16()?);
        let mut ops = Vec::with_capacity(op_count);
        for _ in 0..op_count {
            let op_id = cur.read_u16()?;
            let op_name_stridx = cur.read_u16()?;
            let op_name = resolve_stridx(strings, op_name_stridx, "effect op name")?;
            let param_count = usize::from(cur.read_u16()?);
            let mut param_type_ids = Vec::with_capacity(param_count);
            for _ in 0..param_count {
                param_type_ids.push(cur.read_u32()?);
            }
            let ret_type_id = cur.read_u32()?;
            let fatal_byte = cur.read_u8()?;
            ops.push(LoadedEffectOp {
                id: op_id,
                name: op_name,
                param_type_ids,
                ret_type_id,
                fatal: fatal_byte != 0,
            });
        }
        effects.push(LoadedEffect { id, name, ops });
    }
    Ok(effects)
}

fn parse_clss_section(cur: &mut Cursor<'_>) -> Result<ClassTable, VmError> {
    let count = cur.read_u16()?;
    // No class entries are emitted yet; consume any unexpected extra bytes
    // by leaving the cursor at its current position (the caller uses a
    // sub-cursor bounded to the payload length).
    Ok(ClassTable { count })
}

fn parse_frgn_section(
    cur: &mut Cursor<'_>,
    strings: &[Box<str>],
) -> Result<Vec<LoadedForeignFn>, VmError> {
    let count = usize::from(cur.read_u16()?);
    let mut fns = Vec::with_capacity(count);
    for _ in 0..count {
        let ext_name_stridx = cur.read_u16()?;
        let lib_stridx = cur.read_u16()?;
        let kind_stridx = cur.read_u16()?;
        let param_count = cur.read_u16()?;
        let pc = usize::from(param_count);
        let mut param_type_ids = Vec::with_capacity(pc);
        for _ in 0..pc {
            param_type_ids.push(cur.read_u32()?);
        }
        let ret_type_id = cur.read_u32()?;
        let variadic_byte = cur.read_u8()?;

        let ext_name = resolve_stridx(strings, ext_name_stridx, "foreign fn ext_name")?;
        let lib_name = if lib_stridx == 0xFFFF {
            Box::from("")
        } else {
            resolve_stridx(strings, lib_stridx, "foreign fn lib_name")?
        };
        let link_kind = if kind_stridx == 0xFFFF {
            Box::from("")
        } else {
            resolve_stridx(strings, kind_stridx, "foreign fn link_kind")?
        };

        fns.push(LoadedForeignFn {
            ext_name,
            lib_name,
            link_kind,
            param_count,
            param_type_ids,
            ret_type_id,
            variadic: variadic_byte != 0,
        });
    }
    Ok(fns)
}

struct Cursor<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Cursor<'a> {
    const fn new(bytes: &'a [u8], pos: usize) -> Self {
        Self { bytes, pos }
    }

    fn read_array<const N: usize>(&mut self) -> Result<[u8; N], VmError> {
        let slice = self
            .bytes
            .get(self.pos..self.pos + N)
            .ok_or_else(|| VmError::Malformed {
                desc: "unexpected end of section data".into(),
            })?;
        let arr = <[u8; N]>::try_from(slice).map_err(|_| VmError::Malformed {
            desc: "unexpected end of section data".into(),
        })?;
        self.pos += N;
        Ok(arr)
    }

    fn read_u8(&mut self) -> Result<u8, VmError> {
        Ok(self.read_array::<1>()?[0])
    }

    fn read_u16(&mut self) -> Result<u16, VmError> {
        Ok(u16::from_be_bytes(self.read_array()?))
    }

    fn read_u32(&mut self) -> Result<u32, VmError> {
        Ok(u32::from_be_bytes(self.read_array()?))
    }

    fn read_i64(&mut self) -> Result<i64, VmError> {
        Ok(i64::from_be_bytes(self.read_array()?))
    }

    fn read_u64(&mut self) -> Result<u64, VmError> {
        Ok(u64::from_be_bytes(self.read_array()?))
    }

    fn read_bytes(&mut self, len: usize) -> Result<&'a [u8], VmError> {
        let slice = self
            .bytes
            .get(self.pos..self.pos + len)
            .ok_or_else(|| VmError::Malformed {
                desc: "unexpected end of section data".into(),
            })?;
        self.pos += len;
        Ok(slice)
    }
}
