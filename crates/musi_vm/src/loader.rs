//! `.msbc` binary loader.
//!
//! Parses raw bytes into a `LoadedModule` without any dependency on compiler
//! crates. The binary format is described in §11 of the Musi bytecode spec.

use core::str;

use musi_bytecode::crc32_slice;

use crate::error::VmError;

// ── Magic / header constants ────────────────────────────────────────────────

const MAGIC: &[u8; 4] = b"MUSI";
const HEADER_SIZE: usize = 40;

// ── Constant pool tags (§11.2) ──────────────────────────────────────────────

const TAG_CONST_I32: u8 = 0x01;
const TAG_CONST_I64: u8 = 0x02;
const TAG_CONST_F64: u8 = 0x04;
const TAG_CONST_STR: u8 = 0x05;
const TAG_CONST_RUNE: u8 = 0x06;
const TAG_CONST_FN: u8 = 0x08;

// ── Public types ────────────────────────────────────────────────────────────

/// A decoded constant pool entry.
#[derive(Debug, Clone)]
pub enum LoadedConst {
    I32(i32),
    I64(i64),
    F64(f64),
    Str(Box<str>),
    Rune(char),
    Fn(u32),
}

/// A decoded type pool entry (stored as raw tag + opaque payload for the MVP).
#[derive(Debug, Clone)]
pub struct LoadedType {
    pub tag: u8,
    pub data: Vec<u8>,
}

/// A decoded effect definition.
#[derive(Debug, Clone)]
pub struct LoadedEffect {
    pub id: u32,
    /// Index into the const pool for the effect name.
    pub name_const_idx: u32,
    pub ops: Vec<LoadedEffectOp>,
}

/// A decoded effect operation.
#[derive(Debug, Clone)]
pub struct LoadedEffectOp {
    pub id: u32,
    /// Index into the const pool for the op name.
    pub name_const_idx: u32,
    pub param_type_ids: Vec<u32>,
    pub ret_type_id: u32,
}

/// A decoded foreign (FFI) function entry.
#[derive(Debug, Clone)]
pub struct LoadedForeignFn {
    /// C-side symbol name.
    pub ext_name: Box<str>,
    /// Library name (empty = default C library).
    pub lib_name: Box<str>,
    /// Number of fixed parameters.
    pub param_count: u16,
    /// Type pool ids for each parameter.
    pub param_type_ids: Vec<u32>,
    /// Type pool id for the return type.
    pub ret_type_id: u32,
    /// Whether this function is variadic.
    pub variadic: bool,
}

/// A decoded function entry.
#[derive(Debug, Clone)]
pub struct LoadedFn {
    pub fn_id: u32,
    pub type_id: u32,
    pub local_count: u16,
    pub param_count: u16,
    pub max_stack: u16,
    pub effect_mask: u16,
    pub code: Box<[u8]>,
    pub handlers: Vec<HandlerEntry>,
}

/// An effect handler registration entry from the function's handler table.
#[derive(Debug, Clone)]
pub struct HandlerEntry {
    pub effect_id: u8,
    pub handler_fn_id: u32,
}

/// The fully decoded contents of a `.msbc` module.
#[derive(Debug)]
pub struct LoadedModule {
    pub flags: u32,
    /// `None` if the `entry_point` field is `0xFFFF_FFFF` (library).
    pub entry_point: Option<u32>,
    pub consts: Vec<LoadedConst>,
    pub types: Vec<LoadedType>,
    pub effects: Vec<LoadedEffect>,
    pub foreign_fns: Vec<LoadedForeignFn>,
    pub functions: Vec<LoadedFn>,
}

impl LoadedModule {
    /// Look up a function by `fn_id`. Returns `None` if not found.
    #[must_use]
    pub fn fn_by_id(&self, fn_id: u32) -> Option<(usize, &LoadedFn)> {
        self.functions
            .iter()
            .enumerate()
            .find(|(_, f)| f.fn_id == fn_id)
    }
}

// ── Loader entry point ──────────────────────────────────────────────────────

/// Parse raw `.msbc` bytes into a `LoadedModule`.
///
/// Validates the magic bytes and header CRC32 before decoding pool sections.
///
/// # Errors
///
/// Returns `VmError::Malformed` if the input is too short, contains invalid
/// pool entries, or has structural inconsistencies. Returns `VmError::BadMagic`
/// if the magic bytes do not match. Returns `VmError::BadChecksum` if the
/// header CRC32 does not match.
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

    // CRC32 over first 36 bytes, stored at offset 36.
    let stored_crc = read_u32(bytes, 36)?;
    let computed = crc32_slice(bytes.get(..36).ok_or_else(|| VmError::Malformed {
        desc: "header truncated before checksum field".into(),
    })?);
    if stored_crc != computed {
        return Err(VmError::BadChecksum);
    }

    let flags = read_u32(bytes, 8)?;
    let raw_entry = read_u32(bytes, 12)?;
    let entry_point = if raw_entry == 0xFFFF_FFFF {
        None
    } else {
        Some(raw_entry)
    };

    let const_off = usize::try_from(read_u32(bytes, 16)?).map_err(|_| VmError::Malformed {
        desc: "const_off overflows usize".into(),
    })?;
    let type_off = usize::try_from(read_u32(bytes, 20)?).map_err(|_| VmError::Malformed {
        desc: "type_off overflows usize".into(),
    })?;
    let effect_off = usize::try_from(read_u32(bytes, 24)?).map_err(|_| VmError::Malformed {
        desc: "effect_off overflows usize".into(),
    })?;
    let foreign_off = usize::try_from(read_u32(bytes, 28)?).map_err(|_| VmError::Malformed {
        desc: "foreign_off overflows usize".into(),
    })?;
    let fn_off = usize::try_from(read_u32(bytes, 32)?).map_err(|_| VmError::Malformed {
        desc: "fn_off overflows usize".into(),
    })?;

    let consts = parse_const_pool(bytes, const_off)?;
    let types = parse_type_pool(bytes, type_off)?;
    let effects = parse_effect_pool(bytes, effect_off)?;
    let foreign_fns = parse_foreign_pool(bytes, foreign_off, &consts)?;
    let functions = parse_fn_pool(bytes, fn_off)?;

    Ok(LoadedModule {
        flags,
        entry_point,
        consts,
        types,
        effects,
        foreign_fns,
        functions,
    })
}

// ── Const pool parser ───────────────────────────────────────────────────────

fn parse_const_pool(bytes: &[u8], off: usize) -> Result<Vec<LoadedConst>, VmError> {
    let mut cur = off;
    let count = usize::try_from(read_u32_at(bytes, &mut cur)?).map_err(|_| VmError::Malformed {
        desc: "const count overflows usize".into(),
    })?;
    let mut consts = Vec::with_capacity(count);
    for _ in 0..count {
        let tag = read_u8_at(bytes, &mut cur)?;
        let c = match tag {
            TAG_CONST_I32 => {
                let v = read_i32_at(bytes, &mut cur)?;
                LoadedConst::I32(v)
            }
            TAG_CONST_I64 => {
                let v = read_i64_at(bytes, &mut cur)?;
                LoadedConst::I64(v)
            }
            TAG_CONST_F64 => {
                let bits = read_u64_at(bytes, &mut cur)?;
                LoadedConst::F64(f64::from_bits(bits))
            }
            TAG_CONST_STR => {
                let len = usize::try_from(read_u32_at(bytes, &mut cur)?).map_err(|_| {
                    VmError::Malformed {
                        desc: "string length overflows usize".into(),
                    }
                })?;
                let s = read_bytes_at(bytes, &mut cur, len)?;
                let text = str::from_utf8(s)
                    .map_err(|_| VmError::Malformed {
                        desc: "string constant is not valid UTF-8".into(),
                    })?
                    .into();
                LoadedConst::Str(text)
            }
            TAG_CONST_RUNE => {
                let code = read_u32_at(bytes, &mut cur)?;
                let c = char::from_u32(code).ok_or_else(|| VmError::Malformed {
                    desc: "rune constant is not a valid Unicode scalar".into(),
                })?;
                LoadedConst::Rune(c)
            }
            TAG_CONST_FN => {
                let id = read_u32_at(bytes, &mut cur)?;
                LoadedConst::Fn(id)
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

// ── Type pool parser ────────────────────────────────────────────────────────

// Type tags from §11.3.
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

fn parse_type_pool(bytes: &[u8], off: usize) -> Result<Vec<LoadedType>, VmError> {
    let mut cur = off;
    let count = usize::try_from(read_u32_at(bytes, &mut cur)?).map_err(|_| VmError::Malformed {
        desc: "type count overflows usize".into(),
    })?;
    let mut types = Vec::with_capacity(count);
    for _ in 0..count {
        let tag = read_u8_at(bytes, &mut cur)?;
        let data = match tag {
            // No-payload primitive types.
            TAG_TY_UNIT | TAG_TY_BOOL | TAG_TY_I8 | TAG_TY_I16 | TAG_TY_I32 | TAG_TY_I64
            | TAG_TY_U8 | TAG_TY_U16 | TAG_TY_U32 | TAG_TY_U64 | TAG_TY_F32 | TAG_TY_F64
            | TAG_TY_RUNE => vec![],
            // 4-byte inner type_id.
            TAG_TY_PTR | TAG_TY_ARR | TAG_TY_REF => read_bytes_at(bytes, &mut cur, 4)?.to_vec(),
            // product: count:u32 + count * type_id:u32
            TAG_TY_PRODUCT => {
                let field_count = usize::try_from(read_u32_at(bytes, &mut cur)?).map_err(|_| {
                    VmError::Malformed {
                        desc: "product field count overflows usize".into(),
                    }
                })?;
                let byte_count = 4 + field_count * 4;
                let mut data = Vec::with_capacity(byte_count);
                let fc_u32 = u32::try_from(field_count).map_err(|_| VmError::Malformed {
                    desc: "product field count overflows u32".into(),
                })?;
                data.extend_from_slice(&fc_u32.to_le_bytes());
                let fields = read_bytes_at(bytes, &mut cur, field_count * 4)?;
                data.extend_from_slice(fields);
                data
            }
            // sum: variant_count:u32 + variant_count * (tag:u32 + payload_id:u32)
            TAG_TY_SUM => {
                let variant_count =
                    usize::try_from(read_u32_at(bytes, &mut cur)?).map_err(|_| {
                        VmError::Malformed {
                            desc: "sum variant count overflows usize".into(),
                        }
                    })?;
                let byte_count = 4 + variant_count * 8;
                let mut data = Vec::with_capacity(byte_count);
                let vc_u32 = u32::try_from(variant_count).map_err(|_| VmError::Malformed {
                    desc: "sum variant count overflows u32".into(),
                })?;
                data.extend_from_slice(&vc_u32.to_le_bytes());
                let variants = read_bytes_at(bytes, &mut cur, variant_count * 8)?;
                data.extend_from_slice(variants);
                data
            }
            // fn: param_count:u32 + params:u32[] + ret_id:u32 + effect_mask:u16
            TAG_TY_FN => {
                let param_count = usize::try_from(read_u32_at(bytes, &mut cur)?).map_err(|_| {
                    VmError::Malformed {
                        desc: "fn param count overflows usize".into(),
                    }
                })?;
                let byte_count = 4 + param_count * 4 + 4 + 2;
                let mut data = Vec::with_capacity(byte_count);
                let pc_u32 = u32::try_from(param_count).map_err(|_| VmError::Malformed {
                    desc: "fn param count overflows u32".into(),
                })?;
                data.extend_from_slice(&pc_u32.to_le_bytes());
                let rest = read_bytes_at(bytes, &mut cur, param_count * 4 + 4 + 2)?;
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

// ── Effect pool parser ──────────────────────────────────────────────────────

fn parse_effect_pool(bytes: &[u8], off: usize) -> Result<Vec<LoadedEffect>, VmError> {
    let mut cur = off;
    let count = usize::try_from(read_u32_at(bytes, &mut cur)?).map_err(|_| VmError::Malformed {
        desc: "effect count overflows usize".into(),
    })?;
    let mut effects = Vec::with_capacity(count);
    for _ in 0..count {
        let id = read_u32_at(bytes, &mut cur)?;
        let name_const_idx = read_u32_at(bytes, &mut cur)?;
        let op_count = usize::from(read_u16_at(bytes, &mut cur)?);
        let mut ops = Vec::with_capacity(op_count);
        for _ in 0..op_count {
            let op_id = read_u32_at(bytes, &mut cur)?;
            let op_name_idx = read_u32_at(bytes, &mut cur)?;
            let param_count = usize::from(read_u16_at(bytes, &mut cur)?);
            let mut param_type_ids = Vec::with_capacity(param_count);
            for _ in 0..param_count {
                param_type_ids.push(read_u32_at(bytes, &mut cur)?);
            }
            let ret_type_id = read_u32_at(bytes, &mut cur)?;
            ops.push(LoadedEffectOp {
                id: op_id,
                name_const_idx: op_name_idx,
                param_type_ids,
                ret_type_id,
            });
        }
        effects.push(LoadedEffect {
            id,
            name_const_idx,
            ops,
        });
    }
    Ok(effects)
}

// ── Function pool parser ────────────────────────────────────────────────────

fn parse_fn_pool(bytes: &[u8], off: usize) -> Result<Vec<LoadedFn>, VmError> {
    let mut cur = off;
    let count = usize::try_from(read_u32_at(bytes, &mut cur)?).map_err(|_| VmError::Malformed {
        desc: "function count overflows usize".into(),
    })?;
    let mut functions = Vec::with_capacity(count);
    for _ in 0..count {
        let fn_id = read_u32_at(bytes, &mut cur)?;
        let type_id = read_u32_at(bytes, &mut cur)?;
        let local_count = read_u16_at(bytes, &mut cur)?;
        let param_count = read_u16_at(bytes, &mut cur)?;
        let max_stack = read_u16_at(bytes, &mut cur)?;
        let effect_mask = read_u16_at(bytes, &mut cur)?;
        let code_len =
            usize::try_from(read_u32_at(bytes, &mut cur)?).map_err(|_| VmError::Malformed {
                desc: "code length overflows usize".into(),
            })?;
        let code = read_bytes_at(bytes, &mut cur, code_len)?.into();
        let handler_count = usize::from(read_u16_at(bytes, &mut cur)?);
        let mut handlers = Vec::with_capacity(handler_count);
        for _ in 0..handler_count {
            let effect_id = read_u8_at(bytes, &mut cur)?;
            let handler_fn_id = read_u32_at(bytes, &mut cur)?;
            handlers.push(HandlerEntry {
                effect_id,
                handler_fn_id,
            });
        }
        functions.push(LoadedFn {
            fn_id,
            type_id,
            local_count,
            param_count,
            max_stack,
            effect_mask,
            code,
            handlers,
        });
    }
    Ok(functions)
}

// ── Foreign pool parser ──────────────────────────────────────────────────────

fn parse_foreign_pool(
    bytes: &[u8],
    off: usize,
    consts: &[LoadedConst],
) -> Result<Vec<LoadedForeignFn>, VmError> {
    let mut cur = off;
    let count = usize::try_from(read_u32_at(bytes, &mut cur)?).map_err(|_| VmError::Malformed {
        desc: "foreign fn count overflows usize".into(),
    })?;
    let mut fns = Vec::with_capacity(count);
    for _ in 0..count {
        let ext_name_idx =
            usize::try_from(read_u32_at(bytes, &mut cur)?).map_err(|_| VmError::Malformed {
                desc: "ext_name const index overflows usize".into(),
            })?;
        let lib_name_idx =
            usize::try_from(read_u32_at(bytes, &mut cur)?).map_err(|_| VmError::Malformed {
                desc: "lib_name const index overflows usize".into(),
            })?;
        let param_count = read_u16_at(bytes, &mut cur)?;
        let pc = usize::from(param_count);
        let mut param_type_ids = Vec::with_capacity(pc);
        for _ in 0..pc {
            param_type_ids.push(read_u32_at(bytes, &mut cur)?);
        }
        let ret_type_id = read_u32_at(bytes, &mut cur)?;
        let flags = read_u8_at(bytes, &mut cur)?;
        let variadic = flags & 1 != 0;

        let ext_name = resolve_string_const(consts, ext_name_idx)?;
        let lib_name = if lib_name_idx == usize::try_from(0xFFFF_FFFFu32).unwrap_or(usize::MAX) {
            Box::from("")
        } else {
            resolve_string_const(consts, lib_name_idx)?
        };

        fns.push(LoadedForeignFn {
            ext_name,
            lib_name,
            param_count,
            param_type_ids,
            ret_type_id,
            variadic,
        });
    }
    Ok(fns)
}

/// Look up a string constant by index.
fn resolve_string_const(consts: &[LoadedConst], idx: usize) -> Result<Box<str>, VmError> {
    let c = consts.get(idx).ok_or_else(|| VmError::Malformed {
        desc: format!("foreign fn const index {idx} out of bounds").into_boxed_str(),
    })?;
    match c {
        LoadedConst::Str(s) => Ok(s.clone()),
        _ => Err(VmError::Malformed {
            desc: format!("foreign fn const index {idx} is not a string").into_boxed_str(),
        }),
    }
}

// ── Cursor helpers ──────────────────────────────────────────────────────────

fn read_u8_at(bytes: &[u8], cur: &mut usize) -> Result<u8, VmError> {
    let b = bytes.get(*cur).copied().ok_or_else(|| VmError::Malformed {
        desc: "unexpected end of file reading u8".into(),
    })?;
    *cur += 1;
    Ok(b)
}

fn read_u16_at(bytes: &[u8], cur: &mut usize) -> Result<u16, VmError> {
    let slice = bytes
        .get(*cur..*cur + 2)
        .ok_or_else(|| VmError::Malformed {
            desc: "unexpected end of file reading u16".into(),
        })?;
    let arr = <[u8; 2]>::try_from(slice).map_err(|_| VmError::Malformed {
        desc: "unexpected end of file reading u16".into(),
    })?;
    let v = u16::from_le_bytes(arr);
    *cur += 2;
    Ok(v)
}

fn read_u32_at(bytes: &[u8], cur: &mut usize) -> Result<u32, VmError> {
    let slice = bytes
        .get(*cur..*cur + 4)
        .ok_or_else(|| VmError::Malformed {
            desc: "unexpected end of file reading u32".into(),
        })?;
    let arr = <[u8; 4]>::try_from(slice).map_err(|_| VmError::Malformed {
        desc: "unexpected end of file reading u32".into(),
    })?;
    let v = u32::from_le_bytes(arr);
    *cur += 4;
    Ok(v)
}

fn read_i32_at(bytes: &[u8], cur: &mut usize) -> Result<i32, VmError> {
    let slice = bytes
        .get(*cur..*cur + 4)
        .ok_or_else(|| VmError::Malformed {
            desc: "unexpected end of file reading i32".into(),
        })?;
    let arr = <[u8; 4]>::try_from(slice).map_err(|_| VmError::Malformed {
        desc: "unexpected end of file reading i32".into(),
    })?;
    let v = i32::from_le_bytes(arr);
    *cur += 4;
    Ok(v)
}

fn read_i64_at(bytes: &[u8], cur: &mut usize) -> Result<i64, VmError> {
    let slice = bytes
        .get(*cur..*cur + 8)
        .ok_or_else(|| VmError::Malformed {
            desc: "unexpected end of file reading i64".into(),
        })?;
    let arr = <[u8; 8]>::try_from(slice).map_err(|_| VmError::Malformed {
        desc: "unexpected end of file reading i64".into(),
    })?;
    let v = i64::from_le_bytes(arr);
    *cur += 8;
    Ok(v)
}

fn read_u64_at(bytes: &[u8], cur: &mut usize) -> Result<u64, VmError> {
    let slice = bytes
        .get(*cur..*cur + 8)
        .ok_or_else(|| VmError::Malformed {
            desc: "unexpected end of file reading u64".into(),
        })?;
    let arr = <[u8; 8]>::try_from(slice).map_err(|_| VmError::Malformed {
        desc: "unexpected end of file reading u64".into(),
    })?;
    let v = u64::from_le_bytes(arr);
    *cur += 8;
    Ok(v)
}

fn read_bytes_at<'b>(bytes: &'b [u8], cur: &mut usize, len: usize) -> Result<&'b [u8], VmError> {
    let slice = bytes
        .get(*cur..*cur + len)
        .ok_or_else(|| VmError::Malformed {
            desc: "unexpected end of file reading byte slice".into(),
        })?;
    *cur += len;
    Ok(slice)
}

fn read_u32(bytes: &[u8], off: usize) -> Result<u32, VmError> {
    let mut cur = off;
    read_u32_at(bytes, &mut cur)
}
