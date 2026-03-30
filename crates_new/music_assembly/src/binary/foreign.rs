use music_il::{FfiType, ForeignAbi, ForeignDescriptor, ForeignDescriptors};

use crate::binary::strings::StringIndex;

use super::*;

pub(super) fn decode_foreigns(
    data: &[u8],
    strings: &DecodedStrings,
    offsets: &StringOffsets,
) -> CodecResult<ForeignDescriptors> {
    let count = usize::from(read_u16(data, 0).ok_or(CodecError::TruncatedSection)?);
    let mut position = 2_usize;
    let mut foreigns = Vec::with_capacity(count);

    for _ in 0..count {
        let name = strings::read_string_ref(data, &mut position, strings, offsets)?;
        let symbol = strings::read_optional_string_ref(data, &mut position, strings, offsets)?;
        let library = strings::read_optional_string_ref(data, &mut position, strings, offsets)?;
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

pub(super) fn encode_foreigns(
    foreigns: &ForeignDescriptors,
    strings: &StringIndex,
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
