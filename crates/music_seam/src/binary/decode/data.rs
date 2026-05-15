use super::*;

pub(super) fn decode_data(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Data)?;
    let count = cursor.read_u32()?;
    for _ in 0..count {
        let descriptor = decode_data_descriptor(cursor)?;
        let _ = artifact.data.alloc(descriptor);
    }
    Ok(())
}

fn decode_data_descriptor(cursor: &mut Cursor<'_>) -> AssemblyResult<DataDescriptor> {
    let name = Idx::from_raw(cursor.read_u32()?);
    let variant_count = cursor.read_u32()?;
    let field_count = cursor.read_u32()?;
    let variants = decode_data_variants(cursor)?;
    let repr_kind = decode_optional_raw_u32(cursor)?.map(Idx::from_raw);
    let layout_align = decode_optional_u32(cursor)?;
    let layout_pack = decode_optional_u32(cursor)?;
    let frozen = cursor.read_u8()? != 0;

    let mut descriptor = DataDescriptor::new(name, variants.into_boxed_slice()).with_frozen(frozen);
    debug_assert_eq!(descriptor.variant_count, variant_count);
    debug_assert_eq!(descriptor.field_count, field_count);
    if let Some(repr_kind) = repr_kind {
        descriptor = descriptor.with_repr_kind(repr_kind);
    }
    if let Some(layout_align) = layout_align {
        descriptor = descriptor.with_layout_align(layout_align);
    }
    if let Some(layout_pack) = layout_pack {
        descriptor = descriptor.with_layout_pack(layout_pack);
    }
    if cursor.read_u8()? != 0 {
        descriptor = descriptor.with_object_header(decode_object_header(cursor)?);
    }
    Ok(descriptor)
}

fn decode_data_variants(cursor: &mut Cursor<'_>) -> AssemblyResult<Vec<DataVariantDescriptor>> {
    let variant_len = cursor.read_u32()?;
    let mut variants = Vec::with_capacity(to_capacity(variant_len));
    for _ in 0..variant_len {
        variants.push(decode_data_variant(cursor)?);
    }
    Ok(variants)
}

fn decode_data_variant(cursor: &mut Cursor<'_>) -> AssemblyResult<DataVariantDescriptor> {
    let variant_name = Idx::from_raw(cursor.read_u32()?);
    let tag = cursor.read_i64()?;
    let field_tys = decode_variant_field_tys(cursor)?;
    let layout_fields = decode_layout_fields(cursor)?;
    let public = cursor.read_u8()? != 0;
    let hidden = cursor.read_u8()? != 0;
    Ok(
        DataVariantDescriptor::new(variant_name, tag, field_tys.into_boxed_slice())
            .with_layout_fields(layout_fields.into_boxed_slice())
            .with_public(public)
            .with_hidden(hidden),
    )
}

fn decode_variant_field_tys(cursor: &mut Cursor<'_>) -> AssemblyResult<Vec<Idx<TypeDescriptor>>> {
    let field_len = cursor.read_u32()?;
    let mut field_tys = Vec::with_capacity(to_capacity(field_len));
    for _ in 0..field_len {
        field_tys.push(Idx::from_raw(cursor.read_u32()?));
    }
    Ok(field_tys)
}

fn decode_layout_fields(cursor: &mut Cursor<'_>) -> AssemblyResult<Vec<DataFieldDescriptor>> {
    let layout_field_len = cursor.read_u32()?;
    let mut layout_fields = Vec::with_capacity(to_capacity(layout_field_len));
    for _ in 0..layout_field_len {
        layout_fields.push(decode_layout_field(cursor)?);
    }
    Ok(layout_fields)
}

fn decode_layout_field(cursor: &mut Cursor<'_>) -> AssemblyResult<DataFieldDescriptor> {
    let name = decode_optional_idx(cursor)?;
    let ty = cursor.read_idx()?;
    let logical_index = cursor.read_u32()?;
    let offset = decode_optional_u32(cursor)?;
    let storage = decode_optional_idx(cursor)?;
    let mutable = cursor.read_u8()? != 0;
    let gc_pointer = cursor.read_u8()? != 0;
    let public = cursor.read_u8()? != 0;
    let hidden = cursor.read_u8()? != 0;

    let mut field = DataFieldDescriptor::new(ty, logical_index)
        .with_mutable(mutable)
        .with_gc_pointer(gc_pointer)
        .with_public(public)
        .with_hidden(hidden);
    if let Some(name) = name {
        field = field.with_name(name);
    }
    if let Some(offset) = offset {
        field = field.with_offset(offset);
    }
    if let Some(storage) = storage {
        field = field.with_storage(storage);
    }
    Ok(field)
}

fn decode_object_header(cursor: &mut Cursor<'_>) -> AssemblyResult<ObjectHeaderDescriptor> {
    let layout_ty = decode_optional_idx(cursor)?;
    let mut header = ObjectHeaderDescriptor::new()
        .with_mark_bits(cursor.read_u8()?)
        .with_generation_bits(cursor.read_u8()?)
        .with_pinned(cursor.read_u8()? != 0)
        .with_remembered(cursor.read_u8()? != 0)
        .with_large(cursor.read_u8()? != 0)
        .with_weak_capable(cursor.read_u8()? != 0)
        .with_forwarding(cursor.read_u8()? != 0)
        .with_size_field(cursor.read_u8()? != 0);
    if let Some(layout_ty) = layout_ty {
        header = header.with_layout_ty(layout_ty);
    }
    Ok(header)
}

fn decode_optional_raw_u32(cursor: &mut Cursor<'_>) -> AssemblyResult<Option<u32>> {
    match cursor.read_u8()? {
        0 => Ok(None),
        1 => Ok(Some(cursor.read_u32()?)),
        _ => Err(AssemblyError::BinaryPayloadTruncated),
    }
}

fn decode_optional_u32(cursor: &mut Cursor<'_>) -> AssemblyResult<Option<u32>> {
    match cursor.read_u8()? {
        0 => Ok(None),
        1 => Ok(Some(cursor.read_u32()?)),
        _ => Err(AssemblyError::BinaryPayloadTruncated),
    }
}

fn decode_optional_idx<T>(cursor: &mut Cursor<'_>) -> AssemblyResult<Option<Idx<T>>> {
    match cursor.read_u8()? {
        0 => Ok(None),
        1 => Ok(Some(cursor.read_idx()?)),
        _ => Err(AssemblyError::BinaryPayloadTruncated),
    }
}

fn to_capacity(raw_len: u32) -> usize {
    usize::try_from(raw_len).unwrap_or(usize::MAX)
}
