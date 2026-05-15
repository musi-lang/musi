use super::*;

pub(super) fn decode_foreigns(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Foreigns)?;
    for _ in 0..cursor.read_u32()? {
        let descriptor = decode_foreign(cursor)?;
        let _ = artifact.foreigns.alloc(descriptor);
    }
    Ok(())
}

fn decode_foreign(cursor: &mut Cursor<'_>) -> AssemblyResult<ForeignDescriptor> {
    let name = cursor.read_idx()?;
    let param_tys = read_idx_list(cursor)?;
    let result_ty = cursor.read_idx()?;
    let abi = cursor.read_idx()?;
    let symbol = cursor.read_idx()?;
    let link = decode_optional_idx(cursor, "invalid foreign link marker")?;
    let domain = decode_optional_idx(cursor, "invalid foreign domain marker")?;
    let pinned_params = read_u16_list(cursor)?;
    let nullable_params = read_u16_list(cursor)?;
    let nullable_result = cursor.read_u8()? != 0;
    let lifetime = decode_optional_idx(cursor, "invalid foreign lifetime marker")?;

    let mut descriptor =
        ForeignDescriptor::new(name, param_tys.into_boxed_slice(), result_ty, abi, symbol)
            .with_pinned_params(pinned_params.into_boxed_slice())
            .with_nullable_params(nullable_params.into_boxed_slice())
            .with_nullable_result(nullable_result)
            .with_export(cursor.read_u8()? != 0)
            .with_hot(cursor.read_u8()? != 0)
            .with_cold(cursor.read_u8()? != 0);
    if let Some(link) = link {
        descriptor = descriptor.with_link(link);
    }
    if let Some(domain) = domain {
        descriptor = descriptor.with_domain(domain);
    }
    if let Some(lifetime) = lifetime {
        descriptor = descriptor.with_lifetime(lifetime);
    }
    Ok(descriptor)
}

fn read_idx_list<T>(cursor: &mut Cursor<'_>) -> AssemblyResult<Vec<Idx<T>>> {
    let len = usize::from(cursor.read_u16()?);
    let mut ids = Vec::with_capacity(len);
    for _ in 0..len {
        ids.push(cursor.read_idx()?);
    }
    Ok(ids)
}

fn read_u16_list(cursor: &mut Cursor<'_>) -> AssemblyResult<Vec<u16>> {
    let len = usize::from(cursor.read_u16()?);
    let mut values = Vec::with_capacity(len);
    for _ in 0..len {
        values.push(cursor.read_u16()?);
    }
    Ok(values)
}

fn decode_optional_idx<T>(
    cursor: &mut Cursor<'_>,
    invalid_marker_message: &'static str,
) -> AssemblyResult<Option<Idx<T>>> {
    match cursor.read_u8()? {
        0 => Ok(None),
        1 => Ok(Some(cursor.read_idx()?)),
        _ => Err(AssemblyError::text_parse_source(invalid_marker_message)),
    }
}
