use music_il::{TypeDescriptor, TypeDescriptors, TypeKind};

use crate::binary::strings::StringIndex;

use super::*;

pub(super) fn decode_types(
    data: &[u8],
    strings: &DecodedStrings,
    offsets: &StringOffsets,
) -> CodecResult<TypeDescriptors> {
    let count = usize::from(read_u16(data, 0).ok_or(CodecError::TruncatedSection)?);
    let mut position = 2_usize;
    let mut descriptors = Vec::with_capacity(count);

    for _ in 0..count {
        let id = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 2;
        let key = strings::read_string_ref(data, &mut position, strings, offsets)?;
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

pub(super) fn encode_types(
    ty_descs: &TypeDescriptors,
    strings: &StringIndex,
) -> CodecResult<SectionBytes> {
    if ty_descs.is_empty() {
        return Ok(Vec::new());
    }

    let mut output = Vec::new();
    let count = u16::try_from(ty_descs.len()).map_err(|_| CodecError::ModuleTooLarge)?;
    output.extend_from_slice(&count.to_le_bytes());

    for descriptor in ty_descs {
        output.extend_from_slice(&descriptor.id.to_le_bytes());
        output.extend_from_slice(&strings.offset(&descriptor.key)?.to_le_bytes());
        output.push(descriptor.kind.to_byte());
        output.extend_from_slice(&descriptor.member_count.to_le_bytes());
    }

    Ok(output)
}
