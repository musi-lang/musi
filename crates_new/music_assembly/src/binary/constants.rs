use music_il::{ConstantEntry, ConstantPool};

use crate::binary::strings::StringIndex;

use super::*;

pub(super) fn decode_constants(
    data: &[u8],
    strings: &DecodedStrings,
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

pub(super) fn encode_constants(
    constants: &ConstantPool,
    strings: &StringIndex,
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
