#![allow(clippy::arithmetic_side_effects)]

use seam_ir::{
    ClassDescriptor, ClassDescriptors, ClassInstance, ClassInstances, ClassMethod, ConstantEntry,
    ConstantPool, EffectDescriptor, EffectDescriptors, EffectOpDescriptor, EffectOperations,
    FfiType, ForeignAbi, ForeignDescriptor, ForeignDescriptors, GlobalEntries, GlobalEntry,
    Instruction, InstructionStream, MethodEntries, MethodEntry, MethodName, MethodNames, Opcode,
    SeamArtifact, TypeDescriptor, TypeDescriptors, TypeKind,
};

use super::{
    ANON_METHOD_NAME, CLASS_SECTION_TAG, CONST_SECTION_TAG, EFFECT_SECTION_TAG, ENTRY_METHOD_NAME,
    FOREIGN_SECTION_TAG, GLOBAL_SECTION_TAG, HEADER_SIZE, MAGIC, METHOD_SECTION_TAG,
    STRING_SECTION_TAG, SectionBytes, StringOffsets, StringPool, TYPE_SECTION_TAG, VERSION_MAJOR,
    VERSION_MINOR,
};
use crate::{CodecError, CodecResult};

/// Decode a `.seam` artifact from its binary representation.
///
/// # Errors
/// Returns an error when the header, section layout, opcode stream, or
/// referenced metadata is invalid or truncated.
pub fn decode_binary(data: &[u8]) -> CodecResult<SeamArtifact> {
    if data.len() < HEADER_SIZE {
        return Err(CodecError::TruncatedHeader);
    }

    let magic = read_array::<4>(data, 0).ok_or(CodecError::TruncatedHeader)?;
    if magic != MAGIC {
        return Err(CodecError::InvalidMagic);
    }

    let major = *data.get(4).ok_or(CodecError::TruncatedHeader)?;
    let minor = *data.get(5).ok_or(CodecError::TruncatedHeader)?;
    if major != VERSION_MAJOR || minor != VERSION_MINOR {
        return Err(CodecError::UnsupportedVersion { major, minor });
    }

    let section_count = read_u32(data, 8).ok_or(CodecError::TruncatedHeader)?;

    let mut strings = StringPool::new();
    let mut offsets = StringOffsets::new();
    let mut constants = ConstantPool::new();
    let mut methods = Vec::new();
    let mut globals = Vec::new();
    let mut types = Vec::new();
    let mut effects = Vec::new();
    let mut classes = Vec::new();
    let mut foreigns = Vec::new();

    let mut position = HEADER_SIZE;
    for _ in 0..section_count {
        let tag = read_array::<4>(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 4;
        let length = usize::try_from(read_u32(data, position).ok_or(CodecError::TruncatedSection)?)
            .map_err(|_| CodecError::TruncatedSection)?;
        position += 4;

        let section = data
            .get(position..position + length)
            .ok_or(CodecError::TruncatedSection)?;

        match tag {
            STRING_SECTION_TAG => {
                let decoded = decode_strings(section);
                strings = decoded.0;
                offsets = decoded.1;
            }
            TYPE_SECTION_TAG => {
                types = decode_types(section, &strings, &offsets)?;
            }
            CONST_SECTION_TAG => {
                constants = decode_constants(section, &strings, &offsets)?;
            }
            METHOD_SECTION_TAG => {
                methods = decode_methods(section, &strings, &offsets)?;
            }
            GLOBAL_SECTION_TAG => {
                globals = decode_globals(section, &strings, &offsets)?;
            }
            EFFECT_SECTION_TAG => {
                effects = decode_effects(section, &strings, &offsets)?;
            }
            CLASS_SECTION_TAG => {
                classes = decode_classes(section, &strings, &offsets)?;
            }
            FOREIGN_SECTION_TAG => {
                foreigns = decode_foreigns(section, &strings, &offsets)?;
            }
            _ => {}
        }

        position += length;
    }

    Ok(SeamArtifact {
        constants,
        methods,
        globals,
        types,
        effects,
        classes,
        foreigns,
    })
}

fn decode_instruction_stream(code: &[u8]) -> CodecResult<InstructionStream> {
    let mut position = 0_usize;
    let mut instructions = Vec::new();

    while position < code.len() {
        let opcode_byte = *code.get(position).ok_or(CodecError::TruncatedSection)?;
        let opcode = Opcode::from_byte(opcode_byte).ok_or(CodecError::InvalidOpcode {
            byte: opcode_byte,
            offset: position,
        })?;
        position += 1;

        let instruction = match opcode {
            Opcode::LdLoc | Opcode::StLoc | Opcode::Call | Opcode::CallTail | Opcode::EffCont => {
                let raw = *code.get(position).ok_or(CodecError::TruncatedSection)?;
                position += 1;
                Instruction::with_u8(opcode, raw)
            }
            Opcode::LdConst
            | Opcode::LdGlob
            | Opcode::LdUpv
            | Opcode::StGlob
            | Opcode::StUpv
            | Opcode::LdLocW
            | Opcode::StLocW
            | Opcode::LdSmi
            | Opcode::SeqGet
            | Opcode::SeqSet
            | Opcode::SeqLen
            | Opcode::SeqSlice
            | Opcode::DataGet
            | Opcode::DataSet
            | Opcode::DataTag
            | Opcode::TyChk
            | Opcode::TyCast
            | Opcode::TyId
            | Opcode::FfiCall => {
                let raw = read_u16(code, position).ok_or(CodecError::TruncatedSection)?;
                position += 2;
                Instruction::with_u16(opcode, raw)
            }
            Opcode::BrTrue | Opcode::BrFalse | Opcode::BrJmp => {
                let raw = i16::from_le_bytes(
                    read_array::<2>(code, position).ok_or(CodecError::TruncatedSection)?,
                );
                position += 2;
                Instruction::with_i16(opcode, raw)
            }
            Opcode::ClsNew => {
                let primary = read_u16(code, position).ok_or(CodecError::TruncatedSection)?;
                let secondary = *code.get(position + 2).ok_or(CodecError::TruncatedSection)?;
                position += 3;
                Instruction::with_wide(opcode, primary, secondary)
            }
            Opcode::SeqNew | Opcode::DataNew => {
                let type_id = read_u16(code, position).ok_or(CodecError::TruncatedSection)?;
                let length = read_u16(code, position + 2).ok_or(CodecError::TruncatedSection)?;
                position += 4;
                Instruction::with_type_len(opcode, type_id, length)
            }
            Opcode::EffInvk => {
                let effect_id = read_u16(code, position).ok_or(CodecError::TruncatedSection)?;
                let operation_id =
                    read_u16(code, position + 2).ok_or(CodecError::TruncatedSection)?;
                position += 4;
                Instruction::with_effect(opcode, effect_id, operation_id)
            }
            Opcode::HdlPush => {
                let effect_id = read_u16(code, position).ok_or(CodecError::TruncatedSection)?;
                let operation_id =
                    read_u16(code, position + 2).ok_or(CodecError::TruncatedSection)?;
                let jump = i16::from_le_bytes(
                    read_array::<2>(code, position + 4).ok_or(CodecError::TruncatedSection)?,
                );
                position += 6;
                Instruction::with_effect_jump(opcode, effect_id, operation_id, jump)
            }
            Opcode::BrTbl => {
                let entry_count = read_u16(code, position).ok_or(CodecError::TruncatedSection)?;
                position += 2;
                let mut table = Vec::with_capacity(usize::from(entry_count));
                for _ in 0..entry_count {
                    table.push(i16::from_le_bytes(
                        read_array::<2>(code, position).ok_or(CodecError::TruncatedSection)?,
                    ));
                    position += 2;
                }
                Instruction::with_table(opcode, table)
            }
            _ => Instruction::simple(opcode),
        };

        instructions.push(instruction);
    }

    Ok(instructions)
}

fn operand_size(opcode: Opcode, data: &[u8], position: usize) -> CodecResult<usize> {
    match opcode {
        Opcode::LdLoc | Opcode::StLoc | Opcode::Call | Opcode::CallTail | Opcode::EffCont => Ok(1),
        Opcode::LdConst
        | Opcode::LdGlob
        | Opcode::LdUpv
        | Opcode::StGlob
        | Opcode::StUpv
        | Opcode::LdLocW
        | Opcode::StLocW
        | Opcode::LdSmi
        | Opcode::SeqGet
        | Opcode::SeqSet
        | Opcode::SeqLen
        | Opcode::SeqSlice
        | Opcode::DataGet
        | Opcode::DataSet
        | Opcode::DataTag
        | Opcode::TyChk
        | Opcode::TyCast
        | Opcode::TyId
        | Opcode::BrTrue
        | Opcode::BrFalse
        | Opcode::BrJmp
        | Opcode::FfiCall => Ok(2),
        Opcode::ClsNew => Ok(3),
        Opcode::SeqNew | Opcode::DataNew | Opcode::EffInvk => Ok(4),
        Opcode::HdlPush => Ok(6),
        Opcode::BrTbl => {
            let count = usize::from(read_u16(data, position).ok_or(CodecError::TruncatedSection)?);
            Ok(2 + count * 2)
        }
        _ => Ok(0),
    }
}

fn decode_strings(data: &[u8]) -> (StringPool, StringOffsets) {
    let mut strings = StringPool::new();
    let mut offsets = StringOffsets::new();
    let mut position = 0_usize;

    while position < data.len() {
        let offset = u32::try_from(position).unwrap_or(u32::MAX);
        let index = u32::try_from(strings.len()).unwrap_or(u32::MAX);
        let _ = offsets.insert(offset, index);
        let length = data[position..]
            .iter()
            .position(|&byte| byte == 0)
            .unwrap_or(data.len() - position);
        strings.push(String::from_utf8_lossy(&data[position..position + length]).into_owned());
        position += length + 1;
    }

    (strings, offsets)
}

fn decode_types(
    data: &[u8],
    strings: &StringPool,
    offsets: &StringOffsets,
) -> CodecResult<TypeDescriptors> {
    let count = usize::from(read_u16(data, 0).ok_or(CodecError::TruncatedSection)?);
    let mut position = 2_usize;
    let mut descriptors = Vec::with_capacity(count);

    for _ in 0..count {
        let id = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 2;
        let key = read_string_ref(data, &mut position, strings, offsets)?;
        let kind = match *data.get(position).ok_or(CodecError::TruncatedSection)? {
            0 => TypeKind::Builtin,
            1 => TypeKind::Record,
            2 => TypeKind::Choice,
            tag => return Err(CodecError::InvalidConstantTag { tag }),
        };
        position += 1;
        let member_count = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 2;
        descriptors.push(TypeDescriptor {
            id,
            key,
            kind,
            member_count,
        });
    }

    Ok(descriptors)
}

fn decode_constants(
    data: &[u8],
    strings: &StringPool,
    offsets: &StringOffsets,
) -> CodecResult<ConstantPool> {
    let count = usize::from(read_u16(data, 0).ok_or(CodecError::TruncatedSection)?);
    let mut position = 2_usize;
    let mut pool = ConstantPool::new();

    for _ in 0..count {
        let tag = *data.get(position).ok_or(CodecError::TruncatedSection)?;
        position += 1;
        let entry = match tag {
            0x01 => {
                let value = i64::from_le_bytes(
                    read_array::<8>(data, position).ok_or(CodecError::TruncatedSection)?,
                );
                position += 8;
                ConstantEntry::Int(value)
            }
            0x02 => {
                let bits = u64::from_le_bytes(
                    read_array::<8>(data, position).ok_or(CodecError::TruncatedSection)?,
                );
                position += 8;
                ConstantEntry::Float(bits)
            }
            0x03 => {
                let offset =
                    u32::from(read_u16(data, position).ok_or(CodecError::TruncatedSection)?);
                position += 2;
                let string_index = offsets
                    .get(&offset)
                    .copied()
                    .ok_or(CodecError::InvalidStringOffset { offset })?;
                let text = strings
                    .get(usize::try_from(string_index).unwrap_or(usize::MAX))
                    .cloned()
                    .ok_or(CodecError::InvalidStringOffset { offset })?;
                ConstantEntry::Str(text)
            }
            0x04 => {
                let tag_id = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
                position += 2;
                ConstantEntry::Tag(tag_id)
            }
            other => return Err(CodecError::InvalidConstantTag { tag: other }),
        };
        let _ = pool.add(entry);
    }

    Ok(pool)
}

fn decode_methods(
    data: &[u8],
    strings: &StringPool,
    offsets: &StringOffsets,
) -> CodecResult<MethodEntries> {
    let count = usize::from(read_u16(data, 0).ok_or(CodecError::TruncatedSection)?);
    let mut position = 2_usize;
    let mut methods = Vec::with_capacity(count);

    for _ in 0..count {
        let raw_name = read_u32(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 4;
        let name = match raw_name {
            ENTRY_METHOD_NAME => MethodName::Entry,
            ANON_METHOD_NAME => MethodName::Anonymous,
            _ => MethodName::Named(read_named_string(raw_name, strings, offsets)?),
        };
        let locals_count = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 2;
        let instruction_count = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 2;
        let raw_code = scan_method_bytes(data, &mut position, instruction_count)?;
        methods.push(MethodEntry {
            name,
            instructions: decode_instruction_stream(&raw_code)?,
            locals_count,
            absolute_global_loads: Vec::new(),
        });
    }

    Ok(methods)
}

fn decode_globals(
    data: &[u8],
    strings: &StringPool,
    offsets: &StringOffsets,
) -> CodecResult<GlobalEntries> {
    let count = usize::from(read_u16(data, 0).ok_or(CodecError::TruncatedSection)?);
    let mut position = 2_usize;
    let mut globals = Vec::with_capacity(count);

    for _ in 0..count {
        let raw_name = read_u32(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 4;
        let name = read_named_string(raw_name, strings, offsets)?;
        let flags = *data.get(position).ok_or(CodecError::TruncatedSection)?;
        position += 1;
        globals.push(GlobalEntry {
            name,
            exported: (flags & 0x01) != 0,
            opaque: (flags & 0x02) != 0,
        });
    }

    Ok(globals)
}

fn decode_effects(
    data: &[u8],
    strings: &StringPool,
    offsets: &StringOffsets,
) -> CodecResult<EffectDescriptors> {
    let count = usize::from(read_u16(data, 0).ok_or(CodecError::TruncatedSection)?);
    let mut position = 2_usize;
    let mut effects = Vec::with_capacity(count);

    for _ in 0..count {
        let id = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 2;
        let module_name = read_string_ref(data, &mut position, strings, offsets)?;
        let name = read_string_ref(data, &mut position, strings, offsets)?;
        let op_count = usize::from(read_u16(data, position).ok_or(CodecError::TruncatedSection)?);
        position += 2;
        let mut operations = EffectOperations::with_capacity(op_count);
        for _ in 0..op_count {
            let op_id = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
            position += 2;
            let op_name = read_string_ref(data, &mut position, strings, offsets)?;
            operations.push(EffectOpDescriptor {
                id: op_id,
                name: op_name,
            });
        }
        effects.push(EffectDescriptor {
            id,
            module_name,
            name,
            operations,
        });
    }

    Ok(effects)
}

fn decode_classes(
    data: &[u8],
    strings: &StringPool,
    offsets: &StringOffsets,
) -> CodecResult<ClassDescriptors> {
    let count = usize::from(read_u16(data, 0).ok_or(CodecError::TruncatedSection)?);
    let mut position = 2_usize;
    let mut classes = Vec::with_capacity(count);

    for _ in 0..count {
        let id = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 2;
        let name = read_string_ref(data, &mut position, strings, offsets)?;
        let method_count =
            usize::from(read_u16(data, position).ok_or(CodecError::TruncatedSection)?);
        position += 2;
        let mut method_names = MethodNames::with_capacity(method_count);
        for _ in 0..method_count {
            method_names.push(read_string_ref(data, &mut position, strings, offsets)?);
        }
        let instance_count =
            usize::from(read_u16(data, position).ok_or(CodecError::TruncatedSection)?);
        position += 2;
        let mut instances = ClassInstances::with_capacity(instance_count);
        for _ in 0..instance_count {
            let type_id = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
            position += 2;
            let impl_count =
                usize::from(read_u16(data, position).ok_or(CodecError::TruncatedSection)?);
            position += 2;
            let mut methods = Vec::with_capacity(impl_count);
            for _ in 0..impl_count {
                let name = read_string_ref(data, &mut position, strings, offsets)?;
                let method_idx = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
                position += 2;
                methods.push(ClassMethod { name, method_idx });
            }
            instances.push(ClassInstance { type_id, methods });
        }
        classes.push(ClassDescriptor {
            id,
            name,
            method_names,
            instances,
        });
    }

    Ok(classes)
}

fn decode_foreigns(
    data: &[u8],
    strings: &StringPool,
    offsets: &StringOffsets,
) -> CodecResult<ForeignDescriptors> {
    let count = usize::from(read_u16(data, 0).ok_or(CodecError::TruncatedSection)?);
    let mut position = 2_usize;
    let mut foreigns = Vec::with_capacity(count);

    for _ in 0..count {
        let name = read_string_ref(data, &mut position, strings, offsets)?;
        let symbol = read_optional_string_ref(data, &mut position, strings, offsets)?;
        let library = read_optional_string_ref(data, &mut position, strings, offsets)?;
        let abi = ForeignAbi::from_byte(*data.get(position).ok_or(CodecError::TruncatedSection)?);
        position += 1;
        let arity = *data.get(position).ok_or(CodecError::TruncatedSection)?;
        position += 1;
        let exported = (*data.get(position).ok_or(CodecError::TruncatedSection)? & 0x01) != 0;
        position += 1;
        let return_type =
            FfiType::from_byte(*data.get(position).ok_or(CodecError::TruncatedSection)?);
        position += 1;
        let mut param_types = Vec::with_capacity(usize::from(arity));
        for _ in 0..arity {
            param_types.push(FfiType::from_byte(
                *data.get(position).ok_or(CodecError::TruncatedSection)?,
            ));
            position += 1;
        }
        foreigns.push(ForeignDescriptor {
            name,
            symbol,
            library,
            abi,
            arity,
            exported,
            param_types,
            return_type,
        });
    }

    Ok(foreigns)
}

fn scan_method_bytes(
    data: &[u8],
    position: &mut usize,
    instruction_count: u16,
) -> CodecResult<SectionBytes> {
    let start = *position;
    for _ in 0..instruction_count {
        let opcode_position = *position;
        let opcode_byte = *data.get(*position).ok_or(CodecError::TruncatedSection)?;
        *position += 1;
        let opcode = Opcode::from_byte(opcode_byte).ok_or(CodecError::InvalidOpcode {
            byte: opcode_byte,
            offset: opcode_position,
        })?;
        let extra = operand_size(opcode, data, *position)?;
        let _ = data
            .get(*position..*position + extra)
            .ok_or(CodecError::TruncatedSection)?;
        *position += extra;
    }

    data.get(start..*position)
        .map(<[u8]>::to_vec)
        .ok_or(CodecError::TruncatedSection)
}

fn read_named_string(
    offset: u32,
    strings: &StringPool,
    offsets: &StringOffsets,
) -> CodecResult<String> {
    resolve_string(offset, strings, offsets)
        .map_err(|_| CodecError::InvalidMethodName { reference: offset })
}

fn read_string_ref(
    data: &[u8],
    position: &mut usize,
    strings: &StringPool,
    offsets: &StringOffsets,
) -> CodecResult<String> {
    let offset = read_u32(data, *position).ok_or(CodecError::TruncatedSection)?;
    *position += 4;
    resolve_string(offset, strings, offsets)
}

fn read_optional_string_ref(
    data: &[u8],
    position: &mut usize,
    strings: &StringPool,
    offsets: &StringOffsets,
) -> CodecResult<Option<String>> {
    let offset = read_u32(data, *position).ok_or(CodecError::TruncatedSection)?;
    *position += 4;
    if offset == u32::MAX {
        return Ok(None);
    }
    Ok(Some(resolve_string(offset, strings, offsets)?))
}

fn resolve_string(
    offset: u32,
    strings: &StringPool,
    offsets: &StringOffsets,
) -> CodecResult<String> {
    let string_index = offsets
        .get(&offset)
        .copied()
        .ok_or(CodecError::InvalidStringOffset { offset })?;
    strings
        .get(usize::try_from(string_index).unwrap_or(usize::MAX))
        .cloned()
        .ok_or(CodecError::InvalidStringOffset { offset })
}

fn read_u16(data: &[u8], position: usize) -> Option<u16> {
    Some(u16::from_le_bytes(read_array::<2>(data, position)?))
}

fn read_u32(data: &[u8], position: usize) -> Option<u32> {
    Some(u32::from_le_bytes(read_array::<4>(data, position)?))
}

fn read_array<const N: usize>(data: &[u8], position: usize) -> Option<[u8; N]> {
    data.get(position..position + N)?.try_into().ok()
}
