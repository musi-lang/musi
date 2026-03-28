use std::collections::HashMap;

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

    let string_table = build_string_table(module);
    if !string_table.bytes.is_empty() {
        write_section(&mut buf, format::section::STRT, &string_table.bytes);
        section_count = section_count.saturating_add(1);
    }

    let type_data = build_type_table(module, &string_table);
    if !type_data.is_empty() {
        write_section(&mut buf, format::section::TYPE, &type_data);
        section_count = section_count.saturating_add(1);
    }

    let const_data = build_constant_pool(module, &string_table);
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

    let class_data = build_class_table(module, &string_table);
    if !class_data.is_empty() {
        write_section(&mut buf, format::section::CLSS, &class_data);
        section_count = section_count.saturating_add(1);
    }

    let effect_data = build_effect_table(module, &string_table);
    if !effect_data.is_empty() {
        write_section(&mut buf, format::section::EFCT, &effect_data);
        section_count = section_count.saturating_add(1);
    }

    let foreign_data = build_foreign_table(module, &string_table);
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

struct StringTable {
    bytes: Vec<u8>,
    offsets_by_text: HashMap<String, u32>,
    indices_by_text: HashMap<String, u32>,
    constant_offsets: HashMap<u16, u16>,
}

fn build_string_table(module: &SeamModule) -> StringTable {
    let mut table = StringTable {
        bytes: Vec::new(),
        offsets_by_text: HashMap::new(),
        indices_by_text: HashMap::new(),
        constant_offsets: HashMap::new(),
    };

    for (idx, entry) in module.constants.entries().iter().enumerate() {
        if let ConstantEntry::Str(text) = entry {
            let offset = intern_string(&mut table, text);
            let const_idx =
                u16::try_from(idx).expect("constant pool string index overflow (>65535)");
            let narrow_offset =
                u16::try_from(offset).expect("string table offset overflow for CNST (>65535)");
            let _ = table.constant_offsets.insert(const_idx, narrow_offset);
        }
    }

    for ty in &module.types {
        let _ = intern_string(&mut table, &ty.key);
    }

    for effect in &module.effects {
        let _ = intern_string(&mut table, &effect.module_name);
        let _ = intern_string(&mut table, &effect.name);
        for op in &effect.operations {
            let _ = intern_string(&mut table, &op.name);
        }
    }

    for class in &module.classes {
        let _ = intern_ref_string(&mut table, module, class.name_idx);
        for &method_name in &class.method_names {
            let _ = intern_ref_string(&mut table, module, method_name);
        }
        for instance in &class.instances {
            for method in &instance.methods {
                let _ = intern_ref_string(&mut table, module, method.name_idx);
            }
        }
    }

    for foreign in &module.foreigns {
        let _ = intern_ref_string(&mut table, module, foreign.name_idx);
        if foreign.symbol_idx != u32::MAX {
            let _ = intern_ref_string(&mut table, module, foreign.symbol_idx);
        }
        if foreign.lib_idx != u32::MAX {
            let _ = intern_ref_string(&mut table, module, foreign.lib_idx);
        }
    }

    table
}

fn intern_ref_string(table: &mut StringTable, module: &SeamModule, ref_idx: u32) -> u32 {
    let text = constant_string(module, ref_idx);
    intern_string(table, text)
}

fn constant_string(module: &SeamModule, ref_idx: u32) -> &str {
    let const_idx = usize::try_from(ref_idx).expect("string ref index fits in usize");
    match module.constants.entries().get(const_idx) {
        Some(ConstantEntry::Str(text)) => text.as_str(),
        Some(_) => panic!("metadata string ref must point to a string constant"),
        None => panic!("metadata string ref out of bounds"),
    }
}

fn intern_string(table: &mut StringTable, text: &str) -> u32 {
    if let Some(&offset) = table.offsets_by_text.get(text) {
        return offset;
    }

    let offset = u32::try_from(table.bytes.len()).expect("string table too large (>4 GiB)");
    table.bytes.extend_from_slice(text.as_bytes());
    table.bytes.push(0);
    let index = u32::try_from(table.indices_by_text.len()).expect("too many strings (>4 GiB)");
    let key = text.to_owned();
    let _ = table.offsets_by_text.insert(key.clone(), offset);
    let _ = table.indices_by_text.insert(key, index);
    offset
}

/// Encode the type table: `type_count(u16)` + descriptors.
///
/// Each descriptor:
///   `id(u16)` + `key_offset(u32)` + `kind(u8)` + `member_count(u16)`.
fn build_type_table(module: &SeamModule, string_table: &StringTable) -> Vec<u8> {
    if module.types.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    let count = u16::try_from(module.types.len()).expect("too many type descriptors (>65535)");
    out.extend_from_slice(&count.to_le_bytes());

    for td in &module.types {
        out.extend_from_slice(&td.id.to_le_bytes());
        let key_offset = string_table
            .offsets_by_text
            .get(&td.key)
            .copied()
            .expect("type key missing from string table");
        out.extend_from_slice(&key_offset.to_le_bytes());
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
fn build_constant_pool(module: &SeamModule, string_table: &StringTable) -> Vec<u8> {
    let entries = module.constants.entries();
    if entries.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    let count = u16::try_from(entries.len()).expect("constant pool overflow (>65535)");
    out.extend_from_slice(&count.to_le_bytes());

    for (idx, entry) in entries.iter().enumerate() {
        match entry {
            ConstantEntry::Int(n) => {
                out.push(0x01);
                out.extend_from_slice(&n.to_le_bytes());
            }
            ConstantEntry::Float(bits) => {
                out.push(0x02);
                out.extend_from_slice(&bits.to_le_bytes());
            }
            ConstantEntry::Str(_) => {
                out.push(0x03);
                let const_idx =
                    u16::try_from(idx).expect("string constant index overflow (>65535)");
                let string_offset = string_table
                    .constant_offsets
                    .get(&const_idx)
                    .copied()
                    .expect("string constant missing from string table");
                out.extend_from_slice(&string_offset.to_le_bytes());
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
        Operand::TypeLen(type_id, length) => {
            out.extend_from_slice(&type_id.to_le_bytes());
            out.extend_from_slice(&length.to_le_bytes());
        }
        Operand::TypeTagged(type_id, tag, length) => {
            out.extend_from_slice(&type_id.to_le_bytes());
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
///   u32 name_offset
///   u16 method_count
///   [u32; method_count] method_name_offsets
///   u16 instance_count
///   per instance:
///     u16 type_id
///     u16 method_count (same as class method_count)
///     [u32 name_offset + u16 method_idx; method_count] method implementations
/// ```
fn build_class_table(module: &SeamModule, string_table: &StringTable) -> Vec<u8> {
    if module.classes.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    let count = u16::try_from(module.classes.len()).expect("too many classes (>65535)");
    out.extend_from_slice(&count.to_le_bytes());

    for class in &module.classes {
        out.extend_from_slice(&class.id.to_le_bytes());
        let class_name_offset = metadata_offset(string_table, constant_string(module, class.name_idx));
        out.extend_from_slice(&class_name_offset.to_le_bytes());
        out.extend_from_slice(&class.method_count.to_le_bytes());

        for &name_idx in &class.method_names {
            let method_name_offset = metadata_offset(string_table, constant_string(module, name_idx));
            out.extend_from_slice(&method_name_offset.to_le_bytes());
        }

        let inst_count = u16::try_from(class.instances.len()).expect("too many instances (>65535)");
        out.extend_from_slice(&inst_count.to_le_bytes());

        for instance in &class.instances {
            out.extend_from_slice(&instance.type_id.to_le_bytes());
            let m_count = u16::try_from(instance.methods.len()).expect("too many methods (>65535)");
            out.extend_from_slice(&m_count.to_le_bytes());
            for method in &instance.methods {
                let method_name_offset =
                    metadata_offset(string_table, constant_string(module, method.name_idx));
                out.extend_from_slice(&method_name_offset.to_le_bytes());
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
///   u32 module_name_offset
///   u32 effect_name_offset
///   u16 op_count
///   per op:
///     u16 op_id
///     u32 op_name_offset
/// ```
fn build_effect_table(module: &SeamModule, string_table: &StringTable) -> Vec<u8> {
    if module.effects.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    let count = u16::try_from(module.effects.len()).expect("too many effects (>65535)");
    out.extend_from_slice(&count.to_le_bytes());

    for effect in &module.effects {
        out.extend_from_slice(&effect.id.to_le_bytes());
        out.extend_from_slice(&metadata_offset(string_table, &effect.module_name).to_le_bytes());
        out.extend_from_slice(&metadata_offset(string_table, &effect.name).to_le_bytes());
        let op_count =
            u16::try_from(effect.operations.len()).expect("too many effect operations (>65535)");
        out.extend_from_slice(&op_count.to_le_bytes());
        for op in &effect.operations {
            out.extend_from_slice(&op.id.to_le_bytes());
            out.extend_from_slice(&metadata_offset(string_table, &op.name).to_le_bytes());
        }
    }

    out
}

/// Encode the FRGN section.
///
/// Wire format: `u16 count` + variable-length entries.
/// ```text
/// per entry:
///   u32 name_offset
///   u32 symbol_offset
///   u32 lib_offset (0xFFFFFFFF = no lib)
///   u8  abi
///   u8  arity
///   u8  flags (bit 0 = exported)
///   u8  return_type
///   [u8; arity] param_types
/// ```
#[expect(clippy::as_conversions, reason = "repr(u8) enum to u8 is lossless")]
fn build_foreign_table(module: &SeamModule, string_table: &StringTable) -> Vec<u8> {
    if module.foreigns.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    let count = u16::try_from(module.foreigns.len()).expect("too many foreigns (>65535)");
    out.extend_from_slice(&count.to_le_bytes());

    for foreign in &module.foreigns {
        out.extend_from_slice(
            &metadata_offset(string_table, constant_string(module, foreign.name_idx)).to_le_bytes(),
        );
        let symbol_offset = if foreign.symbol_idx == u32::MAX {
            u32::MAX
        } else {
            metadata_offset(string_table, constant_string(module, foreign.symbol_idx))
        };
        out.extend_from_slice(&symbol_offset.to_le_bytes());
        let lib_offset = if foreign.lib_idx == u32::MAX {
            u32::MAX
        } else {
            metadata_offset(string_table, constant_string(module, foreign.lib_idx))
        };
        out.extend_from_slice(&lib_offset.to_le_bytes());
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

fn metadata_offset(string_table: &StringTable, text: &str) -> u32 {
    string_table
        .offsets_by_text
        .get(text)
        .copied()
        .expect("metadata string missing from string table")
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic, clippy::as_conversions)]
mod tests;
