use music_il::{GlobalEntries, GlobalEntry};

use crate::binary::strings::StringIndex;

use super::*;

pub(super) fn decode_globals(
    data: &[u8],
    strings: &DecodedStrings,
    offsets: &StringOffsets,
) -> CodecResult<GlobalEntries> {
    let count = usize::from(read_u16(data, 0).ok_or(CodecError::TruncatedSection)?);
    let mut position = 2_usize;
    let mut globals = Vec::with_capacity(count);

    for _ in 0..count {
        let raw_name = read_u32(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 4;
        let name = strings::read_named_string(raw_name, strings, offsets)?;
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

pub(super) fn encode_globals(
    globals: &GlobalEntries,
    strings: &StringIndex,
) -> CodecResult<SectionBytes> {
    if globals.is_empty() {
        return Ok(vec![]);
    }

    let mut output = vec![];
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
