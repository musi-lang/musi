//! `.msbc` binary module serializer.
//!
//! Writes the 32-byte header followed by the constant pool, type pool,
//! and function pool, as specified in §11 of the Musi bytecode spec.

use music_ir::{IrEffectDef, IrType};
use music_shared::{Arena, Interner};

use crate::const_pool::ConstPool;
use crate::emitter::{FnBytecode, write_function_pool};
use crate::error::EmitError;
use crate::type_pool::TypePool;

// Header flags (§11.1)
const FLAG_IS_LIB: u32 = 1 << 2;
const FLAG_IS_SCRIPT: u32 = 1 << 3;

/// Assembles the complete `.msbc` binary.
pub fn assemble(
    cp: &mut ConstPool,
    tp: &mut TypePool,
    functions: &[FnBytecode],
    effects: &[IrEffectDef],
    type_arena: &Arena<IrType>,
    interner: &Interner,
    entry_fn_id: Option<u32>,
) -> Result<Vec<u8>, EmitError> {
    // ── Build section buffers ───────────────────────────────────────────
    let mut const_section: Vec<u8> = vec![];
    cp.write_into(&mut const_section)?;

    let mut type_section: Vec<u8> = vec![];
    tp.write_into(&mut type_section)?;

    let mut effect_section: Vec<u8> = vec![];
    write_effect_pool(&mut effect_section, effects, cp, tp, type_arena, interner)?;

    let mut fn_section: Vec<u8> = vec![];
    write_function_pool(&mut fn_section, functions)?;

    // ── Compute offsets ─────────────────────────────────────────────────
    let header_size: u32 = 36;
    let const_section_len =
        u32::try_from(const_section.len()).map_err(|_| EmitError::TooManyConsts)?;
    let type_section_len =
        u32::try_from(type_section.len()).map_err(|_| EmitError::TooManyTypes)?;
    let effect_section_len =
        u32::try_from(effect_section.len()).map_err(|_| EmitError::FunctionTooLarge)?;

    let const_off = header_size;
    let type_off = const_off
        .checked_add(const_section_len)
        .ok_or(EmitError::FunctionTooLarge)?;
    let effect_off = type_off
        .checked_add(type_section_len)
        .ok_or(EmitError::FunctionTooLarge)?;
    let fn_off = effect_off
        .checked_add(effect_section_len)
        .ok_or(EmitError::FunctionTooLarge)?;

    // ── Build the 36-byte header ────────────────────────────────────────
    let mut header: Vec<u8> = Vec::with_capacity(36);
    // magic "MUSI" = 0x4D 0x55 0x53 0x49
    header.extend_from_slice(b"MUSI");
    // version_maj u16 LE
    header.extend_from_slice(&1u16.to_le_bytes());
    // version_min u16 LE
    header.extend_from_slice(&0u16.to_le_bytes());
    // flags u32 LE
    let flags: u32 = if entry_fn_id.is_none() {
        FLAG_IS_LIB
    } else {
        FLAG_IS_SCRIPT
    };
    header.extend_from_slice(&flags.to_le_bytes());
    // entry_point u32 LE (0xFFFFFFFF = library)
    let entry = entry_fn_id.unwrap_or(0xFFFF_FFFF);
    header.extend_from_slice(&entry.to_le_bytes());
    // const_off, type_off, effect_off, fn_off
    header.extend_from_slice(&const_off.to_le_bytes());
    header.extend_from_slice(&type_off.to_le_bytes());
    header.extend_from_slice(&effect_off.to_le_bytes());
    header.extend_from_slice(&fn_off.to_le_bytes());
    // checksum = CRC32 of bytes [0, 32) — computed over the first 32 header bytes
    let checksum = crc32_slice(header.get(..32).expect("header is 32 bytes here"));
    header.extend_from_slice(&checksum.to_le_bytes());

    debug_assert_eq!(header.len(), 36, "header must be exactly 36 bytes");

    // ── Assemble final output ───────────────────────────────────────────
    let total_len =
        36 + const_section.len() + type_section.len() + effect_section.len() + fn_section.len();
    let mut out = Vec::with_capacity(total_len);
    out.extend_from_slice(&header);
    out.extend_from_slice(&const_section);
    out.extend_from_slice(&type_section);
    out.extend_from_slice(&effect_section);
    out.extend_from_slice(&fn_section);
    Ok(out)
}

/// Serialize the effect pool section into `buf`.
fn write_effect_pool(
    buf: &mut Vec<u8>,
    effects: &[IrEffectDef],
    cp: &mut ConstPool,
    tp: &mut TypePool,
    type_arena: &Arena<IrType>,
    interner: &Interner,
) -> Result<(), EmitError> {
    let count = u32::try_from(effects.len()).map_err(|_| EmitError::UnresolvableType {
        desc: "too many effects".into(),
    })?;
    buf.extend_from_slice(&count.to_le_bytes());
    for eff in effects {
        buf.extend_from_slice(&eff.id.0.to_le_bytes());
        // Intern effect name into const pool
        let name_val = music_ir::IrConstValue::Str(eff.name);
        let name_idx = cp.intern(&name_val, interner)?.unwrap_or(0);
        let name_const_idx = u32::from(name_idx);
        buf.extend_from_slice(&name_const_idx.to_le_bytes());
        let op_count = u16::try_from(eff.ops.len()).map_err(|_| EmitError::OperandOverflow {
            desc: "too many effect operations".into(),
        })?;
        buf.extend_from_slice(&op_count.to_le_bytes());
        for op in &eff.ops {
            buf.extend_from_slice(&op.id.0.to_le_bytes());
            let op_name_val = music_ir::IrConstValue::Str(op.name);
            let op_name_idx = cp.intern(&op_name_val, interner)?.unwrap_or(0);
            let op_name_const_idx = u32::from(op_name_idx);
            buf.extend_from_slice(&op_name_const_idx.to_le_bytes());
            let param_count =
                u16::try_from(op.param_tys.len()).map_err(|_| EmitError::OperandOverflow {
                    desc: "too many effect op params".into(),
                })?;
            buf.extend_from_slice(&param_count.to_le_bytes());
            for &param_ty in &op.param_tys {
                let type_id = tp.lower_ir_type(param_ty, type_arena)?;
                buf.extend_from_slice(&type_id.to_le_bytes());
            }
            let ret_type_id = tp.lower_ir_type(op.ret_ty, type_arena)?;
            buf.extend_from_slice(&ret_type_id.to_le_bytes());
        }
    }
    Ok(())
}

/// CRC-32/ISO-HDLC (standard Ethernet/ZIP CRC32).
fn crc32_slice(data: &[u8]) -> u32 {
    let mut crc: u32 = 0xFFFF_FFFF;
    for &byte in data {
        // Extract lowest byte of crc without `as` cast.
        let low_byte = crc.to_le_bytes()[0];
        let table_idx = usize::from(low_byte ^ byte);
        crc = CRC32_TABLE[table_idx] ^ (crc >> 8);
    }
    crc ^ 0xFFFF_FFFF
}

/// Pre-computed CRC-32 lookup table (polynomial 0xEDB88320).
const CRC32_TABLE: [u32; 256] = {
    let poly: u32 = 0xEDB8_8320;
    let mut table = [0u32; 256];
    let mut i = 0usize;
    while i < 256 {
        // Convert i (0..256) to u32 without `as` cast via le_bytes round-trip.
        let i_bytes = i.to_le_bytes();
        let mut crc = u32::from_le_bytes([i_bytes[0], i_bytes[1], i_bytes[2], i_bytes[3]]);
        let mut j = 0;
        while j < 8 {
            if crc & 1 != 0 {
                crc = (crc >> 1) ^ poly;
            } else {
                crc >>= 1;
            }
            j += 1;
        }
        table[i] = crc;
        i += 1;
    }
    table
};
