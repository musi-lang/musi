//! `.msbc` binary module serializer.
//!
//! Writes the 40-byte header followed by the constant pool, type pool,
//! effect pool, foreign pool, and function pool.

use music_shared::{Interner, Symbol};

use musi_bc::crc32_slice;

use crate::const_pool::{ConstPool, ConstValue};
use crate::emitter::{FnBytecode, write_function_pool};
use crate::error::EmitError;
use crate::type_pool::TypePool;

// Header flags (§11.1)
const FLAG_IS_LIB: u32 = 1 << 2;
const FLAG_IS_SCRIPT: u32 = 1 << 3;

/// A foreign (FFI) function declaration for the foreign pool.
pub struct ForeignFn {
    pub ext_name: Symbol,
    pub library: Option<Symbol>,
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

/// Parameters for assembling a `.msbc` binary.
pub struct AssembleParams<'a> {
    pub cp: &'a mut ConstPool,
    pub tp: &'a mut TypePool,
    pub functions: &'a [FnBytecode],
    pub effects: &'a [EffectDef],
    pub foreign_fns: &'a [ForeignFn],
    pub interner: &'a Interner,
    pub entry_fn_id: Option<u32>,
}

/// Assembles the complete `.msbc` binary.
pub fn assemble(params: AssembleParams<'_>) -> Result<Vec<u8>, EmitError> {
    let AssembleParams {
        cp,
        tp,
        functions,
        effects,
        foreign_fns,
        interner,
        entry_fn_id,
    } = params;
    let mut type_section: Vec<u8> = vec![];
    tp.write_into(&mut type_section)?;

    let mut effect_section: Vec<u8> = vec![];
    write_effect_pool(&mut effect_section, effects, cp, interner)?;

    let mut foreign_section: Vec<u8> = vec![];
    write_foreign_pool(&mut foreign_section, foreign_fns, cp, interner)?;

    // Serialize const pool AFTER effect/foreign pools, since those intern
    // new strings (effect names, foreign fn ext_name/lib_name) into `cp`.
    let mut const_section: Vec<u8> = vec![];
    cp.write_into(&mut const_section)?;

    let mut fn_section: Vec<u8> = vec![];
    write_function_pool(&mut fn_section, functions)?;

    let header_size: u32 = 40;
    let const_section_len =
        u32::try_from(const_section.len()).map_err(|_| EmitError::TooManyConsts)?;
    let type_section_len =
        u32::try_from(type_section.len()).map_err(|_| EmitError::TooManyTypes)?;
    let effect_section_len =
        u32::try_from(effect_section.len()).map_err(|_| EmitError::FunctionTooLarge)?;
    let foreign_section_len =
        u32::try_from(foreign_section.len()).map_err(|_| EmitError::FunctionTooLarge)?;

    let const_off = header_size;
    let type_off = const_off
        .checked_add(const_section_len)
        .ok_or(EmitError::FunctionTooLarge)?;
    let effect_off = type_off
        .checked_add(type_section_len)
        .ok_or(EmitError::FunctionTooLarge)?;
    let foreign_off = effect_off
        .checked_add(effect_section_len)
        .ok_or(EmitError::FunctionTooLarge)?;
    let fn_off = foreign_off
        .checked_add(foreign_section_len)
        .ok_or(EmitError::FunctionTooLarge)?;

    let mut header: Vec<u8> = Vec::with_capacity(40);
    header.extend_from_slice(b"MUSI");
    header.extend_from_slice(&2u16.to_le_bytes());
    header.extend_from_slice(&0u16.to_le_bytes());
    let flags: u32 = if entry_fn_id.is_none() {
        FLAG_IS_LIB
    } else {
        FLAG_IS_SCRIPT
    };
    header.extend_from_slice(&flags.to_le_bytes());
    let entry = entry_fn_id.unwrap_or(0xFFFF_FFFF);
    header.extend_from_slice(&entry.to_le_bytes());
    header.extend_from_slice(&const_off.to_le_bytes());
    header.extend_from_slice(&type_off.to_le_bytes());
    header.extend_from_slice(&effect_off.to_le_bytes());
    header.extend_from_slice(&foreign_off.to_le_bytes());
    header.extend_from_slice(&fn_off.to_le_bytes());
    let checksum = crc32_slice(header.get(..36).expect("header is 36 bytes here"));
    header.extend_from_slice(&checksum.to_le_bytes());

    debug_assert_eq!(header.len(), 40, "header must be exactly 40 bytes");

    let total_len = 40
        + const_section.len()
        + type_section.len()
        + effect_section.len()
        + foreign_section.len()
        + fn_section.len();
    let mut out = Vec::with_capacity(total_len);
    out.extend_from_slice(&header);
    out.extend_from_slice(&const_section);
    out.extend_from_slice(&type_section);
    out.extend_from_slice(&effect_section);
    out.extend_from_slice(&foreign_section);
    out.extend_from_slice(&fn_section);
    Ok(out)
}

/// Serialize the effect pool section into `buf`.
fn write_effect_pool(
    buf: &mut Vec<u8>,
    effects: &[EffectDef],
    cp: &mut ConstPool,
    interner: &Interner,
) -> Result<(), EmitError> {
    let count = u32::try_from(effects.len()).map_err(|_| EmitError::UnresolvableType {
        desc: "too many effects".into(),
    })?;
    buf.extend_from_slice(&count.to_le_bytes());
    for eff in effects {
        buf.extend_from_slice(&eff.id.to_le_bytes());
        let name_val = ConstValue::Str(eff.name);
        let name_idx = cp.intern(&name_val, interner)?;
        let name_const_idx = u32::from(name_idx);
        buf.extend_from_slice(&name_const_idx.to_le_bytes());
        let op_count = u16::try_from(eff.ops.len()).map_err(|_| EmitError::OperandOverflow {
            desc: "too many effect operations".into(),
        })?;
        buf.extend_from_slice(&op_count.to_le_bytes());
        for op in &eff.ops {
            buf.extend_from_slice(&op.id.to_le_bytes());
            let op_name_val = ConstValue::Str(op.name);
            let op_name_idx = cp.intern(&op_name_val, interner)?;
            let op_name_const_idx = u32::from(op_name_idx);
            buf.extend_from_slice(&op_name_const_idx.to_le_bytes());
            let param_count =
                u16::try_from(op.param_type_ids.len()).map_err(|_| EmitError::OperandOverflow {
                    desc: "too many effect op params".into(),
                })?;
            buf.extend_from_slice(&param_count.to_le_bytes());
            for &type_id in &op.param_type_ids {
                buf.extend_from_slice(&type_id.to_le_bytes());
            }
            buf.extend_from_slice(&op.ret_type_id.to_le_bytes());
            let flags: u8 = u8::from(op.fatal);
            buf.push(flags);
        }
    }
    Ok(())
}

/// Serialize the foreign function pool section into `buf`.
fn write_foreign_pool(
    buf: &mut Vec<u8>,
    foreign_fns: &[ForeignFn],
    cp: &mut ConstPool,
    interner: &Interner,
) -> Result<(), EmitError> {
    let count = u32::try_from(foreign_fns.len()).map_err(|_| EmitError::UnresolvableType {
        desc: "too many foreign functions".into(),
    })?;
    buf.extend_from_slice(&count.to_le_bytes());
    for ff in foreign_fns {
        let ext_name_val = ConstValue::Str(ff.ext_name);
        let ext_name_idx = cp.intern(&ext_name_val, interner)?;
        buf.extend_from_slice(&u32::from(ext_name_idx).to_le_bytes());

        if let Some(lib) = ff.library {
            let lib_name_val = ConstValue::Str(lib);
            let lib_name_idx = cp.intern(&lib_name_val, interner)?;
            buf.extend_from_slice(&u32::from(lib_name_idx).to_le_bytes());
        } else {
            buf.extend_from_slice(&0xFFFF_FFFFu32.to_le_bytes());
        }

        let param_count =
            u16::try_from(ff.param_type_ids.len()).map_err(|_| EmitError::OperandOverflow {
                desc: "too many foreign fn params".into(),
            })?;
        buf.extend_from_slice(&param_count.to_le_bytes());
        for &type_id in &ff.param_type_ids {
            buf.extend_from_slice(&type_id.to_le_bytes());
        }
        buf.extend_from_slice(&ff.ret_type_id.to_le_bytes());

        let flags = u8::from(ff.variadic);
        buf.push(flags);
    }
    Ok(())
}
