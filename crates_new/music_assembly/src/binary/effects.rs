use music_il::{EffectDescriptor, EffectDescriptors, EffectOpDescriptor, EffectOps};

use crate::binary::strings::StringIndex;

use super::*;

pub(super) fn decode_effects(
    data: &[u8],
    strings: &DecodedStrings,
    offsets: &StringOffsets,
) -> CodecResult<EffectDescriptors> {
    let count = usize::from(read_u16(data, 0).ok_or(CodecError::TruncatedSection)?);
    let mut position = 2_usize;
    let mut effects = Vec::with_capacity(count);

    for _ in 0..count {
        let id = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 2;
        let module_name = strings::read_string_ref(data, &mut position, strings, offsets)?;
        let name = strings::read_string_ref(data, &mut position, strings, offsets)?;
        let op_count = usize::from(read_u16(data, position).ok_or(CodecError::TruncatedSection)?);
        position += 2;
        let mut operations = EffectOps::with_capacity(op_count);
        for _ in 0..op_count {
            let op_id = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
            position += 2;
            let op_name = strings::read_string_ref(data, &mut position, strings, offsets)?;
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

pub(super) fn encode_effects(
    effects: &EffectDescriptors,
    strings: &StringIndex,
) -> CodecResult<SectionBytes> {
    if effects.is_empty() {
        return Ok(vec![]);
    }

    let mut output = vec![];
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
