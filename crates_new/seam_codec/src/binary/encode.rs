use seam_ir::{
    ClassDescriptors, ConstantEntry, EffectDescriptors, ForeignDescriptors, GlobalEntries,
    Instruction, MethodEntries, MethodName, Operand, SeamArtifact, TypeDescriptors,
};

use super::{
    ANON_METHOD_NAME, CLASS_SECTION_TAG, CONST_SECTION_TAG, ConstantOffsets, EFFECT_SECTION_TAG,
    ENTRY_METHOD_NAME, FOREIGN_SECTION_TAG, GLOBAL_SECTION_TAG, HEADER_SIZE, MAGIC,
    METHOD_SECTION_TAG, STRING_SECTION_TAG, SectionBytes, TYPE_SECTION_TAG, TextOffsets,
    VERSION_MAJOR, VERSION_MINOR,
};
use crate::{CodecError, CodecResult};

/// Encode a SEAM artifact into the `.seam` binary format.
///
/// # Errors
/// Returns an error when the artifact cannot fit in the binary section/index
/// limits or references a missing interned string.
pub fn encode_binary(artifact: &SeamArtifact) -> CodecResult<SectionBytes> {
    let mut output = vec![0; HEADER_SIZE];
    let mut section_count = 0_u32;

    let strings = StringTable::build(artifact)?;
    let sections = [
        (STRING_SECTION_TAG, strings.bytes.clone()),
        (TYPE_SECTION_TAG, encode_types(&artifact.types, &strings)?),
        (
            CONST_SECTION_TAG,
            encode_constants(&artifact.constants, &strings)?,
        ),
        (
            METHOD_SECTION_TAG,
            encode_methods(&artifact.methods, &strings)?,
        ),
        (
            GLOBAL_SECTION_TAG,
            encode_globals(&artifact.globals, &strings)?,
        ),
        (
            EFFECT_SECTION_TAG,
            encode_effects(&artifact.effects, &strings)?,
        ),
        (
            CLASS_SECTION_TAG,
            encode_classes(&artifact.classes, &strings)?,
        ),
        (
            FOREIGN_SECTION_TAG,
            encode_foreigns(&artifact.foreigns, &strings)?,
        ),
    ];

    for (tag, bytes) in sections {
        if bytes.is_empty() {
            continue;
        }

        section_count = section_count
            .checked_add(1)
            .ok_or(CodecError::ModuleTooLarge)?;
        write_section(&mut output, tag, &bytes)?;
    }

    output[0..4].copy_from_slice(&MAGIC);
    output[4] = VERSION_MAJOR;
    output[5] = VERSION_MINOR;
    output[8..12].copy_from_slice(&section_count.to_le_bytes());
    let total = u32::try_from(output.len()).map_err(|_| CodecError::ModuleTooLarge)?;
    output[12..16].copy_from_slice(&total.to_le_bytes());

    Ok(output)
}

struct StringTable {
    bytes: SectionBytes,
    offsets_by_text: TextOffsets,
    constant_offsets: ConstantOffsets,
}

impl StringTable {
    fn build(artifact: &SeamArtifact) -> CodecResult<Self> {
        let mut table = Self {
            bytes: Vec::new(),
            offsets_by_text: TextOffsets::new(),
            constant_offsets: ConstantOffsets::new(),
        };

        for (index, entry) in artifact.constants.entries().iter().enumerate() {
            if let ConstantEntry::Str(text) = entry {
                let offset = table.intern(text)?;
                let index = u16::try_from(index).map_err(|_| CodecError::ModuleTooLarge)?;
                let offset = u16::try_from(offset).map_err(|_| CodecError::ModuleTooLarge)?;
                let _ = table.constant_offsets.insert(index, offset);
            }
        }

        for descriptor in &artifact.types {
            let _ = table.intern(&descriptor.key)?;
        }

        for method in &artifact.methods {
            if let MethodName::Named(name) = &method.name {
                let _ = table.intern(name)?;
            }
        }

        for global in &artifact.globals {
            let _ = table.intern(&global.name)?;
        }

        for effect in &artifact.effects {
            let _ = table.intern(&effect.module_name)?;
            let _ = table.intern(&effect.name)?;
            for operation in &effect.operations {
                let _ = table.intern(&operation.name)?;
            }
        }

        for class in &artifact.classes {
            let _ = table.intern(&class.name)?;
            for method_name in &class.method_names {
                let _ = table.intern(method_name)?;
            }
            for instance in &class.instances {
                for method in &instance.methods {
                    let _ = table.intern(&method.name)?;
                }
            }
        }

        for foreign in &artifact.foreigns {
            let _ = table.intern(&foreign.name)?;
            if let Some(symbol) = &foreign.symbol {
                let _ = table.intern(symbol)?;
            }
            if let Some(library) = &foreign.library {
                let _ = table.intern(library)?;
            }
        }

        Ok(table)
    }

    fn intern(&mut self, text: &str) -> CodecResult<u32> {
        if let Some(&offset) = self.offsets_by_text.get(text) {
            return Ok(offset);
        }

        let offset = u32::try_from(self.bytes.len()).map_err(|_| CodecError::ModuleTooLarge)?;
        self.bytes.extend_from_slice(text.as_bytes());
        self.bytes.push(0);
        let _ = self.offsets_by_text.insert(text.to_owned(), offset);
        Ok(offset)
    }

    fn offset(&self, text: &str) -> CodecResult<u32> {
        self.offsets_by_text
            .get(text)
            .copied()
            .ok_or(CodecError::InvalidStringOffset { offset: u32::MAX })
    }
}

fn write_section(output: &mut SectionBytes, tag: [u8; 4], bytes: &[u8]) -> CodecResult<()> {
    output.extend_from_slice(&tag);
    let length = u32::try_from(bytes.len()).map_err(|_| CodecError::ModuleTooLarge)?;
    output.extend_from_slice(&length.to_le_bytes());
    output.extend_from_slice(bytes);
    Ok(())
}

fn encode_types(types: &TypeDescriptors, strings: &StringTable) -> CodecResult<SectionBytes> {
    if types.is_empty() {
        return Ok(Vec::new());
    }

    let mut output = Vec::new();
    let count = u16::try_from(types.len()).map_err(|_| CodecError::ModuleTooLarge)?;
    output.extend_from_slice(&count.to_le_bytes());

    for descriptor in types {
        output.extend_from_slice(&descriptor.id.to_le_bytes());
        output.extend_from_slice(&strings.offset(&descriptor.key)?.to_le_bytes());
        output.push(descriptor.kind.to_byte());
        output.extend_from_slice(&descriptor.member_count.to_le_bytes());
    }

    Ok(output)
}

fn encode_constants(
    constants: &seam_ir::ConstantPool,
    strings: &StringTable,
) -> CodecResult<SectionBytes> {
    let entries = constants.entries();
    if entries.is_empty() {
        return Ok(Vec::new());
    }

    let mut output = Vec::new();
    let count = u16::try_from(entries.len()).map_err(|_| CodecError::ModuleTooLarge)?;
    output.extend_from_slice(&count.to_le_bytes());

    for (index, entry) in entries.iter().enumerate() {
        match entry {
            ConstantEntry::Int(number) => {
                output.push(0x01);
                output.extend_from_slice(&number.to_le_bytes());
            }
            ConstantEntry::Float(bits) => {
                output.push(0x02);
                output.extend_from_slice(&bits.to_le_bytes());
            }
            ConstantEntry::Str(_) => {
                output.push(0x03);
                let index = u16::try_from(index).map_err(|_| CodecError::ModuleTooLarge)?;
                let offset = strings
                    .constant_offsets
                    .get(&index)
                    .copied()
                    .ok_or_else(|| CodecError::InvalidStringOffset {
                        offset: u32::from(index),
                    })?;
                output.extend_from_slice(&offset.to_le_bytes());
            }
            ConstantEntry::Tag(tag) => {
                output.push(0x04);
                output.extend_from_slice(&tag.to_le_bytes());
            }
        }
    }

    Ok(output)
}

fn encode_methods(methods: &MethodEntries, strings: &StringTable) -> CodecResult<SectionBytes> {
    if methods.is_empty() {
        return Ok(Vec::new());
    }

    let mut output = Vec::new();
    let count = u16::try_from(methods.len()).map_err(|_| CodecError::ModuleTooLarge)?;
    output.extend_from_slice(&count.to_le_bytes());

    for method in methods {
        let name = match &method.name {
            MethodName::Entry => ENTRY_METHOD_NAME,
            MethodName::Anonymous => ANON_METHOD_NAME,
            MethodName::Named(name) => strings.offset(name)?,
        };

        output.extend_from_slice(&name.to_le_bytes());
        output.extend_from_slice(&method.locals_count.to_le_bytes());
        let instruction_count =
            u16::try_from(method.instructions.len()).map_err(|_| CodecError::ModuleTooLarge)?;
        output.extend_from_slice(&instruction_count.to_le_bytes());
        for instruction in &method.instructions {
            encode_instruction(&mut output, instruction);
        }
    }

    Ok(output)
}

fn encode_instruction(output: &mut SectionBytes, instruction: &Instruction) {
    output.push(instruction.opcode.to_byte());

    match &instruction.operand {
        Operand::None => {}
        Operand::U8(value) => output.push(*value),
        Operand::U16(value) => output.extend_from_slice(&value.to_le_bytes()),
        Operand::I16(value) => output.extend_from_slice(&value.to_le_bytes()),
        Operand::Wide(primary, secondary) => {
            output.extend_from_slice(&primary.to_le_bytes());
            output.push(*secondary);
        }
        Operand::TypeLen(type_id, length) => {
            output.extend_from_slice(&type_id.to_le_bytes());
            output.extend_from_slice(&length.to_le_bytes());
        }
        Operand::Effect(effect_id, operation_id) => {
            output.extend_from_slice(&effect_id.to_le_bytes());
            output.extend_from_slice(&operation_id.to_le_bytes());
        }
        Operand::EffectJump(effect_id, operation_id, jump) => {
            output.extend_from_slice(&effect_id.to_le_bytes());
            output.extend_from_slice(&operation_id.to_le_bytes());
            output.extend_from_slice(&jump.to_le_bytes());
        }
        Operand::Table(offsets) => {
            let count = u16::try_from(offsets.len()).expect("branch table length fits in u16");
            output.extend_from_slice(&count.to_le_bytes());
            for offset in offsets {
                output.extend_from_slice(&offset.to_le_bytes());
            }
        }
    }
}

fn encode_globals(globals: &GlobalEntries, strings: &StringTable) -> CodecResult<SectionBytes> {
    if globals.is_empty() {
        return Ok(Vec::new());
    }

    let mut output = Vec::new();
    let count = u16::try_from(globals.len()).map_err(|_| CodecError::ModuleTooLarge)?;
    output.extend_from_slice(&count.to_le_bytes());

    for global in globals {
        output.extend_from_slice(&strings.offset(&global.name)?.to_le_bytes());
        let mut flags = 0_u8;
        if global.exported {
            flags |= 0x01;
        }
        if global.opaque {
            flags |= 0x02;
        }
        output.push(flags);
    }

    Ok(output)
}

fn encode_effects(effects: &EffectDescriptors, strings: &StringTable) -> CodecResult<SectionBytes> {
    if effects.is_empty() {
        return Ok(Vec::new());
    }

    let mut output = Vec::new();
    let count = u16::try_from(effects.len()).map_err(|_| CodecError::ModuleTooLarge)?;
    output.extend_from_slice(&count.to_le_bytes());

    for effect in effects {
        output.extend_from_slice(&effect.id.to_le_bytes());
        output.extend_from_slice(&strings.offset(&effect.module_name)?.to_le_bytes());
        output.extend_from_slice(&strings.offset(&effect.name)?.to_le_bytes());
        let op_count =
            u16::try_from(effect.operations.len()).map_err(|_| CodecError::ModuleTooLarge)?;
        output.extend_from_slice(&op_count.to_le_bytes());
        for operation in &effect.operations {
            output.extend_from_slice(&operation.id.to_le_bytes());
            output.extend_from_slice(&strings.offset(&operation.name)?.to_le_bytes());
        }
    }

    Ok(output)
}

fn encode_classes(classes: &ClassDescriptors, strings: &StringTable) -> CodecResult<SectionBytes> {
    if classes.is_empty() {
        return Ok(Vec::new());
    }

    let mut output = Vec::new();
    let count = u16::try_from(classes.len()).map_err(|_| CodecError::ModuleTooLarge)?;
    output.extend_from_slice(&count.to_le_bytes());

    for class in classes {
        output.extend_from_slice(&class.id.to_le_bytes());
        output.extend_from_slice(&strings.offset(&class.name)?.to_le_bytes());
        let method_count =
            u16::try_from(class.method_names.len()).map_err(|_| CodecError::ModuleTooLarge)?;
        output.extend_from_slice(&method_count.to_le_bytes());
        for method_name in &class.method_names {
            output.extend_from_slice(&strings.offset(method_name)?.to_le_bytes());
        }
        let instance_count =
            u16::try_from(class.instances.len()).map_err(|_| CodecError::ModuleTooLarge)?;
        output.extend_from_slice(&instance_count.to_le_bytes());
        for instance in &class.instances {
            output.extend_from_slice(&instance.type_id.to_le_bytes());
            let impl_count =
                u16::try_from(instance.methods.len()).map_err(|_| CodecError::ModuleTooLarge)?;
            output.extend_from_slice(&impl_count.to_le_bytes());
            for method in &instance.methods {
                output.extend_from_slice(&strings.offset(&method.name)?.to_le_bytes());
                output.extend_from_slice(&method.method_idx.to_le_bytes());
            }
        }
    }

    Ok(output)
}

fn encode_foreigns(
    foreigns: &ForeignDescriptors,
    strings: &StringTable,
) -> CodecResult<SectionBytes> {
    if foreigns.is_empty() {
        return Ok(Vec::new());
    }

    let mut output = Vec::new();
    let count = u16::try_from(foreigns.len()).map_err(|_| CodecError::ModuleTooLarge)?;
    output.extend_from_slice(&count.to_le_bytes());

    for foreign in foreigns {
        output.extend_from_slice(&strings.offset(&foreign.name)?.to_le_bytes());
        let symbol = match &foreign.symbol {
            Some(symbol) => strings.offset(symbol)?,
            None => u32::MAX,
        };
        output.extend_from_slice(&symbol.to_le_bytes());
        let library = match &foreign.library {
            Some(library) => strings.offset(library)?,
            None => u32::MAX,
        };
        output.extend_from_slice(&library.to_le_bytes());
        output.push(foreign.abi.to_byte());
        output.push(foreign.arity);
        output.push(u8::from(foreign.exported));
        output.push(foreign.return_type.to_byte());
        for param_type in &foreign.param_types {
            output.push(param_type.to_byte());
        }
    }

    Ok(output)
}
