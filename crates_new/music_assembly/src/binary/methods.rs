use music_il::{MethodEntries, MethodEntry, MethodName};

use crate::binary::strings::StringTable;

use super::*;

pub(super) fn decode_methods(
    data: &[u8],
    strings: &StringPool,
    offsets: &StringOffsets,
) -> AssemblyResult<MethodEntries> {
    let count = usize::from(read_u16(data, 0).ok_or(CodecError::TruncatedSection)?);
    let mut position = 2_usize;
    let mut methods = Vec::with_capacity(count);

    for _ in 0..count {
        let raw_name = read_u32(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 4;
        let name = match raw_name {
            ENTRY_METHOD_NAME => MethodName::Entry,
            ANON_METHOD_NAME => MethodName::Anonymous,
            _ => MethodName::Named(strings::read_named_string(raw_name, strings, offsets)?),
        };
        let locals_count = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 2;
        let instruction_count = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 2;
        let raw_code = code::scan_method_bytes(data, &mut position, instruction_count)?;
        methods.push(MethodEntry {
            name,
            instructions: code::decode_instruction_stream(&raw_code)?,
            locals_count,
            absolute_global_loads: vec![],
        });
    }

    Ok(methods)
}

pub(super) fn encode_methods(
    methods: &MethodEntries,
    strings: &StringTable,
) -> AssemblyResult<SectionBytes> {
    if methods.is_empty() {
        return Ok(vec![]);
    }

    let mut output = vec![];
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
            code::encode_instruction(&mut output, instruction);
        }
    }

    Ok(output)
}
