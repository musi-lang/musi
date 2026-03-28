use music_il::format::{self, HEADER_SIZE};
use music_il::instruction::{Instruction, Operand};
use music_shared::Symbol;

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

    let type_data = build_type_table(module);
    if !type_data.is_empty() {
        write_section(&mut buf, format::section::TYPE, &type_data);
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

    let class_data = build_class_table(module);
    if !class_data.is_empty() {
        write_section(&mut buf, format::section::CLSS, &class_data);
        section_count = section_count.saturating_add(1);
    }

    let effect_data = build_effect_table(module);
    if !effect_data.is_empty() {
        write_section(&mut buf, format::section::EFCT, &effect_data);
        section_count = section_count.saturating_add(1);
    }

    let foreign_data = build_foreign_table(module);
    if !foreign_data.is_empty() {
        write_section(&mut buf, format::section::FRGN, &foreign_data);
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

/// Encode the type table: `type_count(u16)` + descriptors.
///
/// Each descriptor: `id(u16)` + `kind(u8)` + `member_count(u16)` = 5 bytes.
fn build_type_table(module: &SeamModule) -> Vec<u8> {
    if module.types.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    let count = u16::try_from(module.types.len()).expect("too many type descriptors (>65535)");
    out.extend_from_slice(&count.to_le_bytes());

    for td in &module.types {
        out.extend_from_slice(&td.id.to_le_bytes());
        // TypeKind is #[repr(u8)] — cast is lossless by definition (same as Opcode cast below)
        #[expect(clippy::as_conversions, reason = "repr(u8) enum to u8 is lossless")]
        out.push(td.kind as u8);
        out.extend_from_slice(&td.member_count.to_le_bytes());
    }
    out
}

/// Encode the constant pool: `entry_count(u16)` + entries.
///
/// Each entry: `tag(u8)` + payload.
///   - Int:   `tag=0x01` + `i64` LE (8 bytes)
///   - Float: `tag=0x02` + `u64` LE (8 bytes, IEEE 754 bits)
///   - Str:   `tag=0x03` + `u16` LE string-table offset
///   - Tag:   `tag=0x04` + `u16` LE variant tag id
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
            ConstantEntry::Tag(tag) => {
                out.push(0x04);
                out.extend_from_slice(&tag.to_le_bytes());
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
        Operand::Effect(effect_id, op_id) => {
            out.extend_from_slice(&effect_id.to_le_bytes());
            out.extend_from_slice(&op_id.to_le_bytes());
        }
        Operand::IndexedJump(idx, jump) => {
            out.extend_from_slice(&idx.to_le_bytes());
            out.extend_from_slice(&jump.to_le_bytes());
        }
        Operand::EffectJump(effect_id, op_id, jump) => {
            out.extend_from_slice(&effect_id.to_le_bytes());
            out.extend_from_slice(&op_id.to_le_bytes());
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

/// Encode the CLSS section.
///
/// Wire format:
/// ```text
/// u16 class_count
/// per class:
///   u16 id
///   u32 name_idx
///   u16 method_count
///   [u32; method_count] method_name_indices
///   u16 instance_count
///   per instance:
///     u16 type_id
///     u16 method_count (same as class method_count)
///     [u32 name_idx + u16 method_idx; method_count] method implementations
/// ```
fn build_class_table(module: &SeamModule) -> Vec<u8> {
    if module.classes.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    let count = u16::try_from(module.classes.len()).expect("too many classes (>65535)");
    out.extend_from_slice(&count.to_le_bytes());

    for class in &module.classes {
        out.extend_from_slice(&class.id.to_le_bytes());
        out.extend_from_slice(&class.name_idx.to_le_bytes());
        out.extend_from_slice(&class.method_count.to_le_bytes());

        for &name_idx in &class.method_names {
            out.extend_from_slice(&name_idx.to_le_bytes());
        }

        let inst_count = u16::try_from(class.instances.len()).expect("too many instances (>65535)");
        out.extend_from_slice(&inst_count.to_le_bytes());

        for instance in &class.instances {
            out.extend_from_slice(&instance.type_id.to_le_bytes());
            let m_count = u16::try_from(instance.methods.len()).expect("too many methods (>65535)");
            out.extend_from_slice(&m_count.to_le_bytes());
            for method in &instance.methods {
                out.extend_from_slice(&method.name_idx.to_le_bytes());
                out.extend_from_slice(&method.method_idx.to_le_bytes());
            }
        }
    }
    out
}

/// Encode the EFCT section.
///
/// Wire format:
/// ```text
/// u16 effect_count
/// per effect:
///   u16 effect_id
///   u16 module_name_len + [u8; len]
///   u16 effect_name_len + [u8; len]
///   u16 op_count
///   per op:
///     u16 op_id
///     u16 op_name_len + [u8; len]
/// ```
fn build_effect_table(module: &SeamModule) -> Vec<u8> {
    if module.effects.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    let count = u16::try_from(module.effects.len()).expect("too many effects (>65535)");
    out.extend_from_slice(&count.to_le_bytes());

    for effect in &module.effects {
        out.extend_from_slice(&effect.id.to_le_bytes());
        write_inline_string(&mut out, &effect.module_name);
        write_inline_string(&mut out, &effect.name);
        let op_count =
            u16::try_from(effect.operations.len()).expect("too many effect operations (>65535)");
        out.extend_from_slice(&op_count.to_le_bytes());
        for op in &effect.operations {
            out.extend_from_slice(&op.id.to_le_bytes());
            write_inline_string(&mut out, &op.name);
        }
    }

    out
}

fn write_inline_string(out: &mut Vec<u8>, value: &str) {
    let len = u16::try_from(value.len()).expect("inline string too long (>65535)");
    out.extend_from_slice(&len.to_le_bytes());
    out.extend_from_slice(value.as_bytes());
}

/// Encode the FRGN section.
///
/// Wire format: `u16 count` + variable-length entries.
/// ```text
/// per entry:
///   u32 name_idx
///   u32 symbol_idx
///   u32 lib_idx (0xFFFFFFFF = no lib)
///   u8  abi
///   u8  arity
///   u8  flags (bit 0 = exported)
///   u8  return_type
///   [u8; arity] param_types
/// ```
#[expect(clippy::as_conversions, reason = "repr(u8) enum to u8 is lossless")]
fn build_foreign_table(module: &SeamModule) -> Vec<u8> {
    if module.foreigns.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    let count = u16::try_from(module.foreigns.len()).expect("too many foreigns (>65535)");
    out.extend_from_slice(&count.to_le_bytes());

    for foreign in &module.foreigns {
        out.extend_from_slice(&foreign.name_idx.to_le_bytes());
        out.extend_from_slice(&foreign.symbol_idx.to_le_bytes());
        out.extend_from_slice(&foreign.lib_idx.to_le_bytes());
        out.push(foreign.abi as u8);
        out.push(foreign.arity);
        let flags: u8 = u8::from(foreign.exported);
        out.push(flags);
        out.push(foreign.return_type as u8);
        for &pt in &foreign.param_types {
            out.push(pt as u8);
        }
    }
    out
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic, clippy::as_conversions)]
mod tests;
