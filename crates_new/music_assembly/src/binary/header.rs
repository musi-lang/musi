use super::*;

pub(super) fn decode_header(data: &[u8]) -> AssemblyResult<u32> {
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

    read_u32(data, 8).ok_or(CodecError::TruncatedHeader)
}

pub(super) fn finalize_header(output: &mut [u8], section_count: u32) -> AssemblyResult<()> {
    assert!(output.len() > 15);
    output[0..4].copy_from_slice(&MAGIC);
    output[4] = VERSION_MAJOR;
    output[5] = VERSION_MINOR;
    output[8..12].copy_from_slice(&section_count.to_le_bytes());
    let total = u32::try_from(output.len()).map_err(|_| CodecError::ModuleTooLarge)?;
    output[12..16].copy_from_slice(&total.to_le_bytes());
    Ok(())
}
