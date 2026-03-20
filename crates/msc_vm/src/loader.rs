//! `.seam` binary loader.
//!
//! Parses raw bytes into a `LoadedModule` without any dependency on compiler
//! crates. The binary format is the SEAM format described in Phase 3.

use core::str;

use msc_bc::crc32_slice;
use msc_bc::type_tag;

use crate::error::VmError;
use crate::error::VmResult;

const MAGIC: &[u8; 4] = b"SEAM";
const HEADER_SIZE: usize = 16;

// Constant pool entry tags (from msc_emit/src/const_pool.rs)
const TAG_CONST_INT: u8 = 0x01;
const TAG_CONST_FLOAT: u8 = 0x02;
const TAG_CONST_STR: u8 = 0x03;
const TAG_CONST_BIGINT: u8 = 0x04;

/// A decoded constant pool entry.
#[derive(Debug, Clone)]
pub enum LoadedConst {
    Int(i64),
    Float(f64),
    Str(Box<str>),
    /// Big integer stored as two's complement bytes (big-endian).
    BigInt(Box<[u8]>),
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
    /// Effect index within the module's effect table (assigned at load time).
    pub id: u16,
    /// Effect name resolved from the string table.
    pub name: Box<str>,
    /// Type parameter names resolved from the string table.
    pub type_params: Vec<Box<str>>,
    pub ops: Vec<LoadedEffectOp>,
    /// Law entries: (law name, method ref index).
    pub laws: Vec<(Box<str>, u16)>,
}

/// A decoded effect operation.
#[derive(Debug, Clone)]
pub struct LoadedEffectOp {
    /// Op index within the parent effect's op array (assigned at load time).
    /// Matches the `op_id` embedded in `EFF_NEED` bytecode instructions.
    pub id: u32,
    /// Op name resolved from the string table.
    pub name: Box<str>,
    /// Type ref index into the TYPE table (op signature).
    pub signature: u16,
    /// Flags: bit0=fatal.
    pub flags: u8,
}

impl LoadedEffectOp {
    /// Returns `true` if this op is fatal (cannot be resumed).
    #[must_use]
    pub const fn fatal(&self) -> bool {
        self.flags & 0x01 != 0
    }
}

/// A decoded foreign (FFI) function entry.
#[derive(Debug, Clone)]
pub struct LoadedForeignFn {
    /// Musi-side name.
    pub musi_name: Box<str>,
    /// C-side symbol name.
    pub ext_name: Box<str>,
    /// Library name (empty = default C library).
    pub lib_name: Box<str>,
    /// ABI: 0x00=C, 0x01=stdcall, 0x02=system.
    pub abi: u8,
    /// Link kind: 0x00=dynamic, 0x01=static, 0x02=framework.
    pub link_kind_tag: u8,
    /// Type signature index into the TYPE table.
    pub signature_type_id: u16,
    /// Flags: bit0=exported.
    pub ffi_flags: u8,
    /// Number of fixed parameters.
    pub param_count: u16,
    /// Type pool ids for each parameter.
    pub param_type_ids: Vec<u32>,
    /// Type pool id for the return type.
    pub ret_type_id: u32,
    /// Whether this function is variadic.
    pub variadic: bool,
}

/// A decoded method within a class declaration.
#[derive(Debug, Clone)]
pub struct LoadedClassMethod {
    /// Method name resolved from the string table.
    pub name: Box<str>,
    /// Type ref index into the TYPE table (method signature).
    pub signature: u16,
    /// Flags: bit0=has default.
    pub flags: u8,
}

impl LoadedClassMethod {
    /// Returns `true` if this method has a default implementation.
    #[must_use]
    pub const fn has_default(&self) -> bool {
        self.flags & 0x01 != 0
    }
}

/// A decoded class (typeclass) declaration.
#[derive(Debug, Clone)]
pub struct LoadedClass {
    /// Class name resolved from the string table.
    pub name: Box<str>,
    /// Type parameter count.
    pub type_param_count: u8,
    pub methods: Vec<LoadedClassMethod>,
    /// Law entries: (law name, method ref index).
    pub laws: Vec<(Box<str>, u16)>,
    /// Superclass constraint refs (indices into the class table).
    pub superclasses: Vec<u16>,
}

/// A decoded module dependency entry from the DEPS section.
#[derive(Debug, Clone)]
pub struct LoadedDep {
    /// Dependency module name resolved from the string table.
    pub name: Box<str>,
    pub major: u8,
    pub minor: u8,
    pub patch: u8,
}

/// A decoded class instance record.
#[derive(Debug, Clone)]
pub struct LoadedClassInstance {
    /// Index into the CLSS declarations array.
    pub class_ref: u16,
    /// Type ref index into the TYPE table (target type).
    pub type_ref: u16,
    /// Instance kind: 0x00=manual, 0x01=via delegation, 0x02=derives.
    pub kind: u8,
    /// Method function refs for manual instances (one per class method).
    pub method_refs: Vec<u16>,
    /// Delegate type ref for via-delegation instances.
    pub delegate_type_ref: Option<u16>,
}

/// A single source map entry: maps a bytecode offset to a source location.
///
/// The `line` field contains the source byte offset (not a line number) because
/// no line map is computed at emit time. A post-processor can convert byte offsets
/// to line/column using the original source text.
#[derive(Debug, Clone, Copy)]
pub struct SourceMapEntry {
    /// Byte offset within the function's bytecode (across all functions, in emission order).
    pub bytecode_offset: u32,
    /// Source byte offset used in place of a line number until a line map is available.
    pub line: u32,
    /// Column (currently always 0).
    pub column: u16,
}

/// Debug local variable entry for one local slot in a function.
#[derive(Debug, Clone)]
pub struct LocalNameEntry {
    /// String index into the module's string table for the variable name (0 = unnamed).
    pub name_stridx: u16,
    /// Bytecode offset where this local comes into scope.
    pub scope_start: u32,
    /// Bytecode offset where this local goes out of scope (exclusive).
    pub scope_end: u32,
}

/// Debug information parsed from the DBUG section.
#[derive(Debug, Default, Clone)]
pub struct DebugInfo {
    /// Source map entries across all functions, in emission order.
    pub source_map: Vec<SourceMapEntry>,
    /// Per-method local variable name tables. Parallel to the METH function array.
    pub local_names: Vec<Vec<LocalNameEntry>>,
}

/// A GC safepoint entry parsed from the function's safepoint map.
#[derive(Debug, Clone)]
pub struct SafepointMap {
    /// Bytecode offset of this safepoint.
    pub offset: u32,
    /// Operand stack depth at the safepoint.
    pub stack_depth: u16,
    /// Conservative bitmap of live stack slots (all bits set = every slot may be a ref).
    pub stack_bitmap: Vec<u8>,
    /// Conservative bitmap of live local slots (all bits set = every slot may be a ref).
    pub local_bitmap: Vec<u8>,
}

/// A decoded function entry.
#[derive(Debug, Clone)]
pub struct LoadedFn {
    pub name_stridx: u16,
    pub type_id: u16,
    /// Raw flags byte: bit0=exported, bit1=foreign export, bit2=effectful, bit3=has dict params.
    pub flags: u8,
    pub param_count: u16,
    pub local_count: u16,
    pub max_stack: u16,
    pub upvalue_count: u16,
    /// Number of implicit dictionary parameters prepended before explicit params.
    pub dict_param_count: u8,
    pub code: Box<[u8]>,
    pub handlers: Vec<HandlerEntry>,
    /// GC safepoint map entries for this function.
    pub safepoints: Vec<SafepointMap>,
    /// Effect set: indices into the module's effect table used by this function.
    pub effect_refs: Vec<u16>,
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
    /// Direct module dependencies declared in the DEPS section.
    pub deps: Vec<LoadedDep>,
    pub effects: Vec<LoadedEffect>,
    pub foreign_fns: Vec<LoadedForeignFn>,
    pub functions: Vec<LoadedFn>,
    /// Class declarations from the CLSS section.
    pub classes: Vec<LoadedClass>,
    /// Class instances from the CLSS section.
    pub class_instances: Vec<LoadedClassInstance>,
    /// Debug information from the DBUG section (empty if section was absent or stripped).
    pub debug_info: DebugInfo,
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
const TAG_DBUG: &[u8; 4] = b"DBUG";

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
pub fn load(bytes: &[u8]) -> VmResult<LoadedModule> {
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
    let mut deps: Vec<LoadedDep> = vec![];
    let mut effects: Vec<LoadedEffect> = vec![];
    let mut foreign_fns: Vec<LoadedForeignFn> = vec![];
    let mut functions: Vec<LoadedFn> = vec![];
    let mut classes: Vec<LoadedClass> = vec![];
    let mut class_instances: Vec<LoadedClassInstance> = vec![];
    let mut debug_info = DebugInfo::default();

    let mut cur = Cursor::new(bytes, HEADER_SIZE);
    while cur.pos < bytes.len() {
        let tag = cur.read_bytes(4)?;
        let payload_len = usize::try_from(cur.read_u32()?).map_err(|_| VmError::Malformed {
            desc: "section payload length overflow".into(),
        })?;
        let payload = cur.read_bytes(payload_len)?;

        let mut sc = Cursor::new(payload, 0);
        if tag == TAG_STRT {
            strings = parse_string_table(&mut sc)?;
        } else if tag == TAG_TYPE {
            types = parse_type_section(&mut sc)?;
        } else if tag == TAG_CNST {
            consts = parse_const_section(&mut sc, &strings)?;
        } else if tag == TAG_DEPS {
            deps = parse_deps_section(&mut sc, &strings)?;
        } else if tag == TAG_GLOB {
            globals = parse_glob_section(&mut sc, &strings)?;
        } else if tag == TAG_METH {
            functions = parse_meth_section(&mut sc)?;
        } else if tag == TAG_EFCT {
            effects = parse_efct_section(&mut sc, &strings)?;
        } else if tag == TAG_CLSS {
            let (cls, inst) = parse_clss_section(&mut sc, &strings)?;
            classes = cls;
            class_instances = inst;
        } else if tag == TAG_FRGN {
            foreign_fns = parse_frgn_section(&mut sc, &strings)?;
        } else if tag == TAG_DBUG {
            debug_info = parse_dbug_section(&mut sc)?;
        }
        // Unknown tags are silently skipped (forward compatibility).
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
        deps,
        effects,
        foreign_fns,
        functions,
        classes,
        class_instances,
        debug_info,
    })
}

fn parse_string_table(cur: &mut Cursor<'_>) -> VmResult<Vec<Box<str>>> {
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

fn parse_deps_section(cur: &mut Cursor<'_>, strings: &[Box<str>]) -> VmResult<Vec<LoadedDep>> {
    let count = usize::from(cur.read_u16()?);
    let mut deps = Vec::with_capacity(count);
    for _ in 0..count {
        let name_stridx = cur.read_u16()?;
        let name = resolve_stridx(strings, name_stridx, "DEPS")?;
        let major = cur.read_u8()?;
        let minor = cur.read_u8()?;
        let patch = cur.read_u8()?;
        deps.push(LoadedDep {
            name,
            major,
            minor,
            patch,
        });
    }
    Ok(deps)
}

fn resolve_stridx(strings: &[Box<str>], idx: u16, ctx: &str) -> VmResult<Box<str>> {
    let i = usize::from(idx);
    strings.get(i).cloned().ok_or_else(|| VmError::Malformed {
        desc: format!(
            "{ctx}: string index {idx} out of bounds (table len={})",
            strings.len()
        )
        .into_boxed_str(),
    })
}

fn parse_const_section(cur: &mut Cursor<'_>, strings: &[Box<str>]) -> VmResult<Vec<LoadedConst>> {
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
            TAG_CONST_BIGINT => {
                let byte_len = usize::from(cur.read_u16()?);
                let bytes = cur.read_bytes(byte_len)?;
                LoadedConst::BigInt(bytes.into())
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

fn parse_type_section(cur: &mut Cursor<'_>) -> VmResult<Vec<LoadedType>> {
    let count = usize::from(cur.read_u16()?);
    let mut types = Vec::with_capacity(count);
    for _ in 0..count {
        let tag = cur.read_u8()?;
        let payload = match tag {
            type_tag::TAG_UNIT
            | type_tag::TAG_BOOL
            | type_tag::TAG_I8
            | type_tag::TAG_I16
            | type_tag::TAG_I32
            | type_tag::TAG_I64
            | type_tag::TAG_U8
            | type_tag::TAG_U16
            | type_tag::TAG_U32
            | type_tag::TAG_U64
            | type_tag::TAG_F32
            | type_tag::TAG_F64
            | type_tag::TAG_RUNE
            | type_tag::TAG_ANY => vec![],
            type_tag::TAG_PTR | type_tag::TAG_ARR | type_tag::TAG_REF => {
                cur.read_bytes(4)?.to_vec()
            }
            type_tag::TAG_PRODUCT => parse_product_type(cur)?,
            type_tag::TAG_SUM => parse_sum_type(cur)?,
            type_tag::TAG_CSTRUCT => parse_cstruct_type(cur)?,
            type_tag::TAG_FN => parse_fn_type(cur)?,
            _ => {
                return Err(VmError::Malformed {
                    desc: format!("unknown type tag {tag:#04x}").into_boxed_str(),
                });
            }
        };
        types.push(LoadedType { tag, data: payload });
    }
    Ok(types)
}

fn parse_product_type(cur: &mut Cursor<'_>) -> VmResult<Vec<u8>> {
    let field_count = usize::try_from(cur.read_u32()?).map_err(|_| VmError::Malformed {
        desc: "product field count overflow".into(),
    })?;
    let byte_count = 4 + field_count * 4;
    let mut payload = Vec::with_capacity(byte_count);
    payload.extend_from_slice(
        &u32::try_from(field_count)
            .map_err(|_| VmError::Malformed {
                desc: "product field count overflow".into(),
            })?
            .to_be_bytes(),
    );
    let fields = cur.read_bytes(field_count * 4)?;
    payload.extend_from_slice(fields);
    Ok(payload)
}

fn parse_sum_type(cur: &mut Cursor<'_>) -> VmResult<Vec<u8>> {
    let variant_count = usize::try_from(cur.read_u32()?).map_err(|_| VmError::Malformed {
        desc: "sum variant count overflow".into(),
    })?;
    let byte_count = 4 + variant_count * 8;
    let mut payload = Vec::with_capacity(byte_count);
    payload.extend_from_slice(
        &u32::try_from(variant_count)
            .map_err(|_| VmError::Malformed {
                desc: "sum variant count overflow".into(),
            })?
            .to_be_bytes(),
    );
    let variants = cur.read_bytes(variant_count * 8)?;
    payload.extend_from_slice(variants);
    Ok(payload)
}

fn parse_cstruct_type(cur: &mut Cursor<'_>) -> VmResult<Vec<u8>> {
    let field_count = usize::from(cur.read_u16()?);
    let per_field_bytes = field_count * 8;
    let byte_count = 2 + per_field_bytes + 6;
    let mut payload = Vec::with_capacity(byte_count);
    payload.extend_from_slice(
        &u16::try_from(field_count)
            .map_err(|_| VmError::Malformed {
                desc: "cstruct field count overflow".into(),
            })?
            .to_be_bytes(),
    );
    let fields = cur.read_bytes(per_field_bytes)?;
    payload.extend_from_slice(fields);
    let trailer = cur.read_bytes(6)?;
    payload.extend_from_slice(trailer);
    Ok(payload)
}

fn parse_fn_type(cur: &mut Cursor<'_>) -> VmResult<Vec<u8>> {
    let param_count = usize::try_from(cur.read_u32()?).map_err(|_| VmError::Malformed {
        desc: "fn param count overflow".into(),
    })?;
    let byte_count = 4 + param_count * 4 + 4 + 2;
    let mut payload = Vec::with_capacity(byte_count);
    payload.extend_from_slice(
        &u32::try_from(param_count)
            .map_err(|_| VmError::Malformed {
                desc: "fn param count overflow".into(),
            })?
            .to_be_bytes(),
    );
    let rest = cur.read_bytes(param_count * 4 + 4 + 2)?;
    payload.extend_from_slice(rest);
    Ok(payload)
}

fn parse_glob_section(cur: &mut Cursor<'_>, strings: &[Box<str>]) -> VmResult<Vec<LoadedGlobal>> {
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

fn parse_meth_section(cur: &mut Cursor<'_>) -> VmResult<Vec<LoadedFn>> {
    let count = usize::from(cur.read_u16()?);
    let mut functions = Vec::with_capacity(count);
    for _ in 0..count {
        let name_stridx = cur.read_u16()?;
        let type_id = cur.read_u16()?;
        // u8 flags: bit0=exported, bit1=foreign export, bit2=effectful, bit3=has dict params
        let flags = cur.read_u8()?;
        let param_count = cur.read_u16()?;
        let local_count = cur.read_u16()?;
        let max_stack = cur.read_u16()?;
        let upvalue_count = cur.read_u16()?;
        // u8 dict_param_count
        let dict_param_count = cur.read_u8()?;
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
        // Safepoint map: u16 count, each: u32 offset + u16 stack_depth + bitmap(stack) + bitmap(locals).
        let safepoint_count = usize::from(cur.read_u16()?);
        let mut safepoints = Vec::with_capacity(safepoint_count);
        for _ in 0..safepoint_count {
            let offset = cur.read_u32()?;
            let stack_depth = cur.read_u16()?;
            let stack_bm_len = usize::from(stack_depth).div_ceil(8);
            let stack_bitmap = cur.read_bytes(stack_bm_len)?.to_vec();
            let local_bm_len = usize::from(local_count).div_ceil(8);
            let local_bitmap = cur.read_bytes(local_bm_len)?.to_vec();
            safepoints.push(SafepointMap {
                offset,
                stack_depth,
                stack_bitmap,
                local_bitmap,
            });
        }
        // Effect set: u16 count, each: u16 effect ref.
        let effect_count = usize::from(cur.read_u16()?);
        let mut effect_refs = Vec::with_capacity(effect_count);
        for _ in 0..effect_count {
            effect_refs.push(cur.read_u16()?);
        }
        functions.push(LoadedFn {
            name_stridx,
            type_id,
            flags,
            param_count,
            local_count,
            max_stack,
            upvalue_count,
            dict_param_count,
            code,
            handlers,
            safepoints,
            effect_refs,
        });
    }
    Ok(functions)
}

fn parse_efct_section(cur: &mut Cursor<'_>, strings: &[Box<str>]) -> VmResult<Vec<LoadedEffect>> {
    let count = usize::from(cur.read_u16()?);
    let mut effects = Vec::with_capacity(count);
    for eff_idx in 0..count {
        let eff_id = u16::try_from(eff_idx).unwrap_or(u16::MAX);
        // u16 name (string index)
        let name_stridx = cur.read_u16()?;
        let name = resolve_stridx(strings, name_stridx, "effect name")?;
        // u8 type_param_count + per-param u16 name
        let type_param_count = usize::from(cur.read_u8()?);
        let mut type_params = Vec::with_capacity(type_param_count);
        for _ in 0..type_param_count {
            let tp_stridx = cur.read_u16()?;
            type_params.push(resolve_stridx(strings, tp_stridx, "effect type param")?);
        }
        // u16 op_count, per op: u16 name + u16 signature + u8 flags
        let op_count = usize::from(cur.read_u16()?);
        let mut ops = Vec::with_capacity(op_count);
        for op_idx in 0..op_count {
            let op_name_stridx = cur.read_u16()?;
            let op_name = resolve_stridx(strings, op_name_stridx, "effect op name")?;
            let signature = cur.read_u16()?;
            let flags = cur.read_u8()?;
            // op id = index within this effect's op array, matching EFF_NEED bytecode encoding
            let op_id = u32::try_from(op_idx).unwrap_or(u32::MAX);
            ops.push(LoadedEffectOp {
                id: op_id,
                name: op_name,
                signature,
                flags,
            });
        }
        // u16 law_count + per-law: u16 name + u16 method_ref
        let law_count = usize::from(cur.read_u16()?);
        let mut laws = Vec::with_capacity(law_count);
        for _ in 0..law_count {
            let law_name_stridx = cur.read_u16()?;
            let law_name = resolve_stridx(strings, law_name_stridx, "effect law name")?;
            let method_ref = cur.read_u16()?;
            laws.push((law_name, method_ref));
        }
        effects.push(LoadedEffect {
            id: eff_id,
            name,
            type_params,
            ops,
            laws,
        });
    }
    Ok(effects)
}

fn parse_clss_section(
    cur: &mut Cursor<'_>,
    strings: &[Box<str>],
) -> VmResult<(Vec<LoadedClass>, Vec<LoadedClassInstance>)> {
    // Class declarations.
    let class_count = usize::from(cur.read_u16()?);
    let mut classes = Vec::with_capacity(class_count);
    for _ in 0..class_count {
        let name_stridx = cur.read_u16()?;
        let type_param_count = cur.read_u8()?;
        let method_count = usize::from(cur.read_u16()?);
        let mut methods = Vec::with_capacity(method_count);
        for _ in 0..method_count {
            let method_name_stridx = cur.read_u16()?;
            let signature = cur.read_u16()?;
            let flags = cur.read_u8()?;
            let name = resolve_stridx(strings, method_name_stridx, "class method name")?;
            methods.push(LoadedClassMethod {
                name,
                signature,
                flags,
            });
        }
        let law_count = usize::from(cur.read_u16()?);
        let mut laws = Vec::with_capacity(law_count);
        for _ in 0..law_count {
            let law_name_stridx = cur.read_u16()?;
            let method_ref = cur.read_u16()?;
            let law_name = resolve_stridx(strings, law_name_stridx, "class law name")?;
            laws.push((law_name, method_ref));
        }
        let superclass_count = usize::from(cur.read_u16()?);
        let mut superclasses = Vec::with_capacity(superclass_count);
        for _ in 0..superclass_count {
            superclasses.push(cur.read_u16()?);
        }
        let name = resolve_stridx(strings, name_stridx, "class name")?;
        classes.push(LoadedClass {
            name,
            type_param_count,
            methods,
            laws,
            superclasses,
        });
    }
    // Instance records.
    let instance_count = usize::from(cur.read_u16()?);
    let mut instances = Vec::with_capacity(instance_count);
    for _ in 0..instance_count {
        let class_ref = cur.read_u16()?;
        let type_ref = cur.read_u16()?;
        let kind = cur.read_u8()?;
        let (method_refs, delegate_type_ref) = match kind {
            0x00 => {
                // Manual: one method_ref per method in the class.
                let method_count = classes
                    .get(usize::from(class_ref))
                    .map_or(0, |c| c.methods.len());
                let mut refs = Vec::with_capacity(method_count);
                for _ in 0..method_count {
                    refs.push(cur.read_u16()?);
                }
                (refs, None)
            }
            0x01 => {
                // Via delegation: u16 delegate_type_ref.
                let delegate = cur.read_u16()?;
                (vec![], Some(delegate))
            }
            0x02 => {
                // Derives: no additional data.
                (vec![], None)
            }
            _ => {
                return Err(VmError::Malformed {
                    desc: format!("unknown class instance kind 0x{kind:02X}").into(),
                });
            }
        };
        instances.push(LoadedClassInstance {
            class_ref,
            type_ref,
            kind,
            method_refs,
            delegate_type_ref,
        });
    }
    Ok((classes, instances))
}

fn parse_frgn_section(
    cur: &mut Cursor<'_>,
    strings: &[Box<str>],
) -> VmResult<Vec<LoadedForeignFn>> {
    let count = usize::from(cur.read_u16()?);
    let mut fns = Vec::with_capacity(count);
    for _ in 0..count {
        let musi_name_stridx = cur.read_u16()?;
        let ext_name_stridx = cur.read_u16()?;
        let lib_stridx = cur.read_u16()?;
        let abi = cur.read_u8()?;
        let link_kind_tag = cur.read_u8()?;
        let signature_type_id = cur.read_u16()?;
        let ffi_flags = cur.read_u8()?;
        let param_count = cur.read_u16()?;
        let pc = usize::from(param_count);
        let mut param_type_ids = Vec::with_capacity(pc);
        for _ in 0..pc {
            param_type_ids.push(cur.read_u32()?);
        }
        let ret_type_id = cur.read_u32()?;
        let variadic_byte = cur.read_u8()?;

        let musi_name = resolve_stridx(strings, musi_name_stridx, "foreign fn musi_name")?;
        let ext_name = resolve_stridx(strings, ext_name_stridx, "foreign fn ext_name")?;
        let lib_name = if lib_stridx == 0xFFFF {
            Box::from("")
        } else {
            resolve_stridx(strings, lib_stridx, "foreign fn lib_name")?
        };

        fns.push(LoadedForeignFn {
            musi_name,
            ext_name,
            lib_name,
            abi,
            link_kind_tag,
            signature_type_id,
            ffi_flags,
            param_count,
            param_type_ids,
            ret_type_id,
            variadic: variadic_byte != 0,
        });
    }
    Ok(fns)
}

/// Parse the DBUG section.
///
/// Format (mirrors the emitter):
///   Source map: u16 count, per entry: u32 `bytecode_offset` + u32 line + u16 column.
///   Local names: u16 `method_count`, per method: u16 `local_count`,
///                per local: u16 `name_stridx` + u32 `scope_start` + u32 `scope_end`.
///
/// An empty payload (0 bytes) is valid and returns default `DebugInfo`.
fn parse_dbug_section(cur: &mut Cursor<'_>) -> VmResult<DebugInfo> {
    // An empty DBUG section (legacy or stripped) is valid - return defaults.
    if cur.bytes.len() == cur.pos {
        return Ok(DebugInfo::default());
    }

    // Source map entries.
    let entry_count = usize::from(cur.read_u16()?);
    let mut source_map = Vec::with_capacity(entry_count);
    for _ in 0..entry_count {
        let bytecode_offset = cur.read_u32()?;
        let line = cur.read_u32()?;
        let column = cur.read_u16()?;
        source_map.push(SourceMapEntry {
            bytecode_offset,
            line,
            column,
        });
    }

    // Local names table.
    let method_count = usize::from(cur.read_u16()?);
    let mut local_names = Vec::with_capacity(method_count);
    for _ in 0..method_count {
        let local_count = usize::from(cur.read_u16()?);
        let mut locals = Vec::with_capacity(local_count);
        for _ in 0..local_count {
            let name_stridx = cur.read_u16()?;
            let scope_start = cur.read_u32()?;
            let scope_end = cur.read_u32()?;
            locals.push(LocalNameEntry {
                name_stridx,
                scope_start,
                scope_end,
            });
        }
        local_names.push(locals);
    }

    Ok(DebugInfo {
        source_map,
        local_names,
    })
}

struct Cursor<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Cursor<'a> {
    const fn new(bytes: &'a [u8], pos: usize) -> Self {
        Self { bytes, pos }
    }

    fn read_array<const N: usize>(&mut self) -> VmResult<[u8; N]> {
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

    fn read_u8(&mut self) -> VmResult<u8> {
        Ok(self.read_array::<1>()?[0])
    }

    fn read_u16(&mut self) -> VmResult<u16> {
        Ok(u16::from_be_bytes(self.read_array()?))
    }

    fn read_u32(&mut self) -> VmResult<u32> {
        Ok(u32::from_be_bytes(self.read_array()?))
    }

    fn read_i64(&mut self) -> VmResult<i64> {
        Ok(i64::from_be_bytes(self.read_array()?))
    }

    fn read_u64(&mut self) -> VmResult<u64> {
        Ok(u64::from_be_bytes(self.read_array()?))
    }

    fn read_bytes(&mut self, len: usize) -> VmResult<&'a [u8]> {
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
