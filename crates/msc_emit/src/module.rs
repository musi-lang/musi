//! `.seam` binary module serializer.
//!
//! Writes the 16-byte BE "SEAM" header followed by 10 tagged sections.

use msc_shared::{Interner, Symbol};

use msc_bc::crc32_slice;

use crate::const_pool::ConstPool;
use crate::emitter::{FnBytecode, write_function_pool};
use crate::error::{EmitError, EmitResult};
use crate::global_table::GlobalTable;
use crate::section::write_section;
use crate::string_table::StringTable;
use crate::type_table::TypeTable;

// Header flag bits
const FLAG_LIBRARY: u8 = 1 << 1;
const FLAG_SCRIPT: u8 = 1 << 2;

/// A foreign (FFI) function declaration for the foreign pool.
pub struct ForeignFn {
    pub ext_name: Symbol,
    pub library: Option<Symbol>,
    pub link_kind: Option<Symbol>,
    pub param_type_ids: Vec<u32>,
    pub ret_type_id: u32,
    pub variadic: bool,
}

/// An effect definition for the effect pool.
pub struct EffectDef {
    pub id: u32,
    pub name: Symbol,
    pub ops: Vec<EffectOpDef>,
}

/// An effect operation definition.
pub struct EffectOpDef {
    pub id: u32,
    pub name: Symbol,
    pub param_type_ids: Vec<u32>,
    pub ret_type_id: u32,
    pub fatal: bool,
}

/// Parameters for assembling a `.seam` binary.
pub struct AssembleParams<'a> {
    pub cp: &'a mut ConstPool,
    pub tp: &'a mut TypeTable,
    pub st: &'a mut StringTable,
    pub gt: &'a GlobalTable,
    pub functions: &'a [FnBytecode],
    pub effects: &'a [EffectDef],
    pub foreign_fns: &'a [ForeignFn],
    pub interner: &'a Interner,
    pub entry_fn_id: Option<u32>,
    pub module_name: &'a str,
    pub source_path: &'a str,
}

/// Assembles the complete `.seam` binary.
pub fn assemble(params: AssembleParams<'_>) -> EmitResult<Vec<u8>> {
    let AssembleParams {
        cp,
        tp,
        st,
        gt,
        functions,
        effects,
        foreign_fns,
        interner,
        entry_fn_id,
        module_name,
        source_path,
    } = params;

    // Intern strings for effect/foreign pools into StringTable.
    // These must happen before we serialize st.
    let mut effect_payload: Vec<u8> = vec![];
    write_effect_pool(&mut effect_payload, effects, st, interner)?;

    let mut foreign_payload: Vec<u8> = vec![];
    write_foreign_pool(&mut foreign_payload, foreign_fns, st, interner)?;

    // ConstPool serialization (cp was mutated by callers; we serialize as-is).
    let mut const_payload: Vec<u8> = vec![];
    cp.write_into(&mut const_payload)?;

    // TypeTable serialization.
    let mut type_payload: Vec<u8> = vec![];
    tp.write_into(&mut type_payload)?;

    // DEPS - empty (no dependencies).
    let deps_payload: Vec<u8> = 0u16.to_be_bytes().to_vec();

    // GLOB section.
    let mut glob_payload: Vec<u8> = vec![];
    gt.write_into(&mut glob_payload);

    // METH section (function pool).
    let mut meth_payload: Vec<u8> = vec![];
    write_function_pool(&mut meth_payload, functions)?;

    // EFCT section (effect pool).
    // Already built above in effect_payload.

    // CLSS section - empty stub.
    let clss_payload: Vec<u8> = 0u16.to_be_bytes().to_vec();

    // FRGN section (foreign pool).
    // Already built above in foreign_payload.

    // DBUG section - empty.
    let dbug_payload: Vec<u8> = vec![];

    // Intern module_name and source_path into StringTable before final STRT build.
    let name_stridx = st.intern_str(module_name)?;
    let path_stridx = st.intern_str(source_path)?;

    // Final STRT payload including module_name and source_path.
    let mut strt_final: Vec<u8> = vec![];
    st.write_into(&mut strt_final);

    // Build the 10 sections in order.
    let mut sections: Vec<u8> = vec![];
    write_section(&mut sections, *b"STRT", &strt_final);
    write_section(&mut sections, *b"TYPE", &type_payload);
    write_section(&mut sections, *b"CNST", &const_payload);
    write_section(&mut sections, *b"DEPS", &deps_payload);
    write_section(&mut sections, *b"GLOB", &glob_payload);
    write_section(&mut sections, *b"METH", &meth_payload);
    write_section(&mut sections, *b"EFCT", &effect_payload);
    write_section(&mut sections, *b"CLSS", &clss_payload);
    write_section(&mut sections, *b"FRGN", &foreign_payload);
    write_section(&mut sections, *b"DBUG", &dbug_payload);

    // CRC-32 of all section bytes.
    let crc2 = crc32_slice(&sections);

    // 16-byte header (BE).
    let flags: u8 = if entry_fn_id.is_none() {
        FLAG_LIBRARY
    } else {
        FLAG_SCRIPT
    };

    let mut header: Vec<u8> = Vec::with_capacity(16);
    header.extend_from_slice(b"SEAM");
    header.push(1u8); // major
    header.push(0u8); // minor
    header.push(0u8); // patch
    header.push(flags);
    header.extend_from_slice(&name_stridx.to_be_bytes());
    header.extend_from_slice(&path_stridx.to_be_bytes());
    header.extend_from_slice(&crc2.to_be_bytes());

    debug_assert_eq!(header.len(), 16, "header must be exactly 16 bytes");

    let mut out = Vec::with_capacity(16 + sections.len());
    out.extend_from_slice(&header);
    out.extend_from_slice(&sections);
    Ok(out)
}

/// Serialize the effect pool into `buf`, interning names into `st`.
fn write_effect_pool(
    buf: &mut Vec<u8>,
    effects: &[EffectDef],
    st: &mut StringTable,
    interner: &Interner,
) -> EmitResult {
    let count = u16::try_from(effects.len()).map_err(|_| EmitError::OperandOverflow {
        desc: "too many effects".into(),
    })?;
    buf.extend_from_slice(&count.to_be_bytes());
    for eff in effects {
        buf.extend_from_slice(&u16::try_from(eff.id).unwrap_or(u16::MAX).to_be_bytes());
        let name_stridx = st.intern(eff.name, interner)?;
        buf.extend_from_slice(&name_stridx.to_be_bytes());
        let op_count = u16::try_from(eff.ops.len()).map_err(|_| EmitError::OperandOverflow {
            desc: "too many effect operations".into(),
        })?;
        buf.extend_from_slice(&op_count.to_be_bytes());
        for op in &eff.ops {
            buf.extend_from_slice(&u16::try_from(op.id).unwrap_or(u16::MAX).to_be_bytes());
            let op_name_stridx = st.intern(op.name, interner)?;
            buf.extend_from_slice(&op_name_stridx.to_be_bytes());
            let param_count =
                u16::try_from(op.param_type_ids.len()).map_err(|_| EmitError::OperandOverflow {
                    desc: "too many effect op params".into(),
                })?;
            buf.extend_from_slice(&param_count.to_be_bytes());
            for &type_id in &op.param_type_ids {
                buf.extend_from_slice(&type_id.to_be_bytes());
            }
            buf.extend_from_slice(&op.ret_type_id.to_be_bytes());
            buf.push(u8::from(op.fatal));
        }
    }
    Ok(())
}

/// Serialize the foreign function pool into `buf`, interning names into `st`.
fn write_foreign_pool(
    buf: &mut Vec<u8>,
    foreign_fns: &[ForeignFn],
    st: &mut StringTable,
    interner: &Interner,
) -> EmitResult {
    let count = u16::try_from(foreign_fns.len()).map_err(|_| EmitError::OperandOverflow {
        desc: "too many foreign functions".into(),
    })?;
    buf.extend_from_slice(&count.to_be_bytes());
    for ff in foreign_fns {
        let ext_name_stridx = st.intern(ff.ext_name, interner)?;
        buf.extend_from_slice(&ext_name_stridx.to_be_bytes());

        if let Some(lib) = ff.library {
            let lib_stridx = st.intern(lib, interner)?;
            buf.extend_from_slice(&lib_stridx.to_be_bytes());
        } else {
            buf.extend_from_slice(&0xFFFFu16.to_be_bytes());
        }

        if let Some(kind) = ff.link_kind {
            let kind_stridx = st.intern(kind, interner)?;
            buf.extend_from_slice(&kind_stridx.to_be_bytes());
        } else {
            buf.extend_from_slice(&0xFFFFu16.to_be_bytes());
        }

        let param_count =
            u16::try_from(ff.param_type_ids.len()).map_err(|_| EmitError::OperandOverflow {
                desc: "too many foreign fn params".into(),
            })?;
        buf.extend_from_slice(&param_count.to_be_bytes());
        for &type_id in &ff.param_type_ids {
            buf.extend_from_slice(&type_id.to_be_bytes());
        }
        buf.extend_from_slice(&ff.ret_type_id.to_be_bytes());
        buf.push(u8::from(ff.variadic));
    }
    Ok(())
}
