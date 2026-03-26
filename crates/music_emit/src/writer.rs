use music_found::Symbol;
use music_il::format::{self, HEADER_SIZE};
use music_il::instruction::{Instruction, Operand};

use crate::emitter::SeamModule;
use crate::pool::ConstantEntry;

/// Serialize a [`SeamModule`] into `.seam` binary format bytes.
#[must_use]
pub fn write_seam(module: &SeamModule) -> Vec<u8> {
    let mut buf = Vec::with_capacity(256);

    buf.resize(HEADER_SIZE, 0);

    let mut section_count: u32 = 0;

    let string_data = build_string_table(module);
    if !string_data.is_empty() {
        write_section(&mut buf, format::section::STRT, &string_data);
        section_count = section_count.saturating_add(1);
    }

    let const_data = build_constant_pool(module);
    if !const_data.is_empty() {
        write_section(&mut buf, format::section::CNST, &const_data);
        section_count = section_count.saturating_add(1);
    }

    let method_data = build_methods(module);
    if !method_data.is_empty() {
        write_section(&mut buf, format::section::METH, &method_data);
        section_count = section_count.saturating_add(1);
    }

    let global_data = build_globals(module);
    if !global_data.is_empty() {
        write_section(&mut buf, format::section::GLOB, &global_data);
        section_count = section_count.saturating_add(1);
    }

    write_header(&mut buf, section_count);

    buf
}

/// Write the 16-byte fixed header at position 0.
fn write_header(buf: &mut [u8], section_count: u32) {
    assert!(buf.len() > 15);
    buf[0..4].copy_from_slice(&format::MAGIC);
    buf[4] = format::VERSION_MAJOR;
    buf[5] = format::VERSION_MINOR;
    buf[6] = 0;
    buf[7] = 0;
    buf[8..12].copy_from_slice(&section_count.to_le_bytes());
    let total = u32::try_from(buf.len()).expect("module too large (>4 GiB)");
    buf[12..16].copy_from_slice(&total.to_le_bytes());
}

/// Write a section: `tag(4)` + `length(4)` + data.
fn write_section(buf: &mut Vec<u8>, tag: [u8; 4], data: &[u8]) {
    buf.extend_from_slice(&tag);
    let len = u32::try_from(data.len()).expect("section too large (>4 GiB)");
    buf.extend_from_slice(&len.to_le_bytes());
    buf.extend_from_slice(data);
}

fn build_string_table(module: &SeamModule) -> Vec<u8> {
    let mut out = Vec::new();
    for entry in module.constants.entries() {
        if let ConstantEntry::Str(s) = entry {
            out.extend_from_slice(s.as_bytes());
            out.push(0);
        }
    }
    out
}

/// Encode the constant pool: `entry_count(u16)` + entries.
///
/// Each entry: `tag(u8)` + payload.
///   - Int:   `tag=0x01` + `i64` LE (8 bytes)
///   - Float: `tag=0x02` + `u64` LE (8 bytes, IEEE 754 bits)
///   - Str:   `tag=0x03` + `u16` LE string-table offset
fn build_constant_pool(module: &SeamModule) -> Vec<u8> {
    let entries = module.constants.entries();
    if entries.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    let count = u16::try_from(entries.len()).expect("constant pool overflow (>65535)");
    out.extend_from_slice(&count.to_le_bytes());

    let mut string_offset: u16 = 0;
    for entry in entries {
        match entry {
            ConstantEntry::Int(n) => {
                out.push(0x01);
                out.extend_from_slice(&n.to_le_bytes());
            }
            ConstantEntry::Float(bits) => {
                out.push(0x02);
                out.extend_from_slice(&bits.to_le_bytes());
            }
            ConstantEntry::Str(s) => {
                out.push(0x03);
                out.extend_from_slice(&string_offset.to_le_bytes());
                let str_len =
                    u16::try_from(s.len().saturating_add(1)).expect("string too long (>65535)");
                string_offset = string_offset.saturating_add(str_len);
            }
        }
    }
    out
}

/// Encode method table: `method_count(u16)` + methods.
///
/// Each method: `name_symbol(u32)` + `locals_count(u16)` + `instruction_count(u16)` + encoded instructions.
fn build_methods(module: &SeamModule) -> Vec<u8> {
    if module.methods.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    let count = u16::try_from(module.methods.len()).expect("too many methods (>65535)");
    out.extend_from_slice(&count.to_le_bytes());

    for method in &module.methods {
        let name_raw = method.name.map_or(u32::MAX, Symbol::raw);
        out.extend_from_slice(&name_raw.to_le_bytes());
        out.extend_from_slice(&method.locals_count.to_le_bytes());
        let instr_count =
            u16::try_from(method.instructions.len()).expect("too many instructions (>65535)");
        out.extend_from_slice(&instr_count.to_le_bytes());

        for instr in &method.instructions {
            encode_instruction(&mut out, instr);
        }
    }
    out
}

/// Encode a single instruction: `opcode(u8)` + operand bytes.
fn encode_instruction(out: &mut Vec<u8>, instr: &Instruction) {
    // Opcode is #[repr(u8)] -- this cast is lossless by definition
    #[allow(clippy::as_conversions)]
    let opcode_byte = instr.opcode as u8;
    out.push(opcode_byte);

    match &instr.operand {
        Operand::None => {}
        Operand::U8(v) => out.push(*v),
        Operand::I16(v) => out.extend_from_slice(&v.to_le_bytes()),
        Operand::U16(v) => out.extend_from_slice(&v.to_le_bytes()),
        Operand::Wide(primary, secondary) => {
            out.extend_from_slice(&primary.to_le_bytes());
            out.push(*secondary);
        }
        Operand::Tagged(tag, length) => {
            out.push(*tag);
            out.extend_from_slice(&length.to_le_bytes());
        }
        Operand::IndexedJump(idx, jump) => {
            out.extend_from_slice(&idx.to_le_bytes());
            out.extend_from_slice(&jump.to_le_bytes());
        }
        Operand::Table(offsets) => {
            let len = u16::try_from(offsets.len()).expect("branch table too large (>65535)");
            out.extend_from_slice(&len.to_le_bytes());
            for &offset in offsets {
                out.extend_from_slice(&offset.to_le_bytes());
            }
        }
    }
}

/// Encode global table: `global_count(u16)` + globals.
///
/// Each global: `name_symbol(u32)` + `flags(u8)`.
fn build_globals(module: &SeamModule) -> Vec<u8> {
    if module.globals.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    let count = u16::try_from(module.globals.len()).expect("too many globals (>65535)");
    out.extend_from_slice(&count.to_le_bytes());

    for global in &module.globals {
        out.extend_from_slice(&global.name.raw().to_le_bytes());
        let mut flags: u8 = 0;
        if global.exported {
            flags |= 0x01;
        }
        if global.opaque {
            flags |= 0x02;
        }
        out.push(flags);
    }
    out
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic, clippy::as_conversions)]
mod tests;
