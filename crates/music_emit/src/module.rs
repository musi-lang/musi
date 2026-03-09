//! `.msbc` binary module serializer.
//!
//! Writes the 32-byte header followed by the constant pool, type pool,
//! and function pool, as specified in §11 of the Musi bytecode spec.

use crate::const_pool::ConstPool;
use crate::emitter::{FnBytecode, write_function_pool};
use crate::error::EmitError;
use crate::type_pool::TypePool;

// Header flags (§11.1)
const FLAG_IS_LIB: u32 = 1 << 2;
const FLAG_IS_SCRIPT: u32 = 1 << 3;

/// Assembles the complete `.msbc` binary.
pub fn assemble(
    cp: &ConstPool,
    tp: &TypePool,
    functions: &[FnBytecode],
    entry_fn_id: Option<u32>,
) -> Result<Vec<u8>, EmitError> {
    // ── Build section buffers ───────────────────────────────────────────
    let mut const_section: Vec<u8> = Vec::new();
    cp.write_into(&mut const_section)?;

    let mut type_section: Vec<u8> = Vec::new();
    tp.write_into(&mut type_section)?;

    let mut fn_section: Vec<u8> = Vec::new();
    write_function_pool(&mut fn_section, functions)?;

    // ── Compute offsets ─────────────────────────────────────────────────
    let header_size: u32 = 32;
    let const_section_len =
        u32::try_from(const_section.len()).map_err(|_| EmitError::TooManyConsts)?;
    let type_section_len =
        u32::try_from(type_section.len()).map_err(|_| EmitError::TooManyTypes)?;

    let const_off = header_size;
    let type_off = const_off
        .checked_add(const_section_len)
        .ok_or(EmitError::FunctionTooLarge)?;
    let fn_off = type_off
        .checked_add(type_section_len)
        .ok_or(EmitError::FunctionTooLarge)?;

    // ── Build the 32-byte header ────────────────────────────────────────
    let mut header: Vec<u8> = Vec::with_capacity(32);
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
    // const_off, type_off, fn_off
    header.extend_from_slice(&const_off.to_le_bytes());
    header.extend_from_slice(&type_off.to_le_bytes());
    header.extend_from_slice(&fn_off.to_le_bytes());
    // checksum = CRC32 of bytes [0, 28) — computed over the first 28 header bytes
    let checksum = crc32_slice(header.get(..28).expect("header is 28 bytes here"));
    header.extend_from_slice(&checksum.to_le_bytes());

    debug_assert_eq!(header.len(), 32, "header must be exactly 32 bytes");

    // ── Assemble final output ───────────────────────────────────────────
    let total_len = 32 + const_section.len() + type_section.len() + fn_section.len();
    let mut out = Vec::with_capacity(total_len);
    out.extend_from_slice(&header);
    out.extend_from_slice(&const_section);
    out.extend_from_slice(&type_section);
    out.extend_from_slice(&fn_section);
    Ok(out)
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
