use super::*;

mod data;
mod foreigns;
mod procedures;

/// Decodes a sectioned `.seam` byte stream into a validated SEAM artifact.
///
/// # Errors
///
/// Returns [`AssemblyError`] if the header, sections, payload lengths, opcodes, or references are
/// invalid.
pub fn decode_binary(bytes: &[u8]) -> AssemblyResult<Artifact> {
    let mut cursor = Cursor::new(bytes);
    let magic = cursor.read_exact(4)?;
    if magic != SEAM_MAGIC {
        return Err(AssemblyError::InvalidBinaryHeader);
    }
    let major = cursor.read_u16()?;
    let minor = cursor.read_u16()?;
    if major != BINARY_MAJOR_VERSION || minor != BINARY_MINOR_VERSION {
        let version = (u32::from(major) << 16) | u32::from(minor);
        return Err(AssemblyError::UnsupportedBinaryVersion(version));
    }

    let mut artifact = Artifact::new();
    decode_strings(&mut cursor, &mut artifact)?;
    decode_types(&mut cursor, &mut artifact)?;
    decode_constants(&mut cursor, &mut artifact)?;
    decode_globals(&mut cursor, &mut artifact)?;
    decode_procedures(&mut cursor, &mut artifact)?;
    decode_shapes(&mut cursor, &mut artifact)?;
    decode_foreigns(&mut cursor, &mut artifact)?;
    decode_exports(&mut cursor, &mut artifact)?;
    decode_data(&mut cursor, &mut artifact)?;
    while !cursor.is_eof() {
        decode_next_trailing_section(&mut cursor, &mut artifact)?;
    }
    artifact.validate()?;
    Ok(artifact)
}

/// Validates a `.seam` binary blob by decoding and checking the resulting artifact.
///
/// # Errors
///
/// Returns [`AssemblyError`] if decoding or artifact validation fails.
pub fn validate_binary(bytes: &[u8]) -> AssemblyResult {
    let _ = decode_binary(bytes)?;
    Ok(())
}

fn decode_strings(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Strings)?;
    for _ in 0..cursor.read_u32()? {
        let bytes = cursor.read_bytes()?;
        let text = String::from_utf8(bytes).map_err(AssemblyError::text_parse_source)?;
        let _ = artifact.push_string_record(&text);
    }
    Ok(())
}

fn decode_types(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Types)?;
    for _ in 0..cursor.read_u32()? {
        let name = cursor.read_idx()?;
        let term = cursor.read_idx()?;
        let _ = artifact.types.alloc(TypeDescriptor::new(name, term));
    }
    Ok(())
}

fn decode_constants(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Constants)?;
    for _ in 0..cursor.read_u32()? {
        let name = cursor.read_idx()?;
        let kind = cursor.read_u8()?;
        let constant_value = match kind {
            0 => ConstantValue::Int(cursor.read_i64()?),
            1 => ConstantValue::Float(f64::from_bits(cursor.read_u64()?)),
            2 => ConstantValue::Bool(cursor.read_u8()? != 0),
            3 => ConstantValue::String(cursor.read_idx()?),
            4 => ConstantValue::Syntax {
                shape: match cursor.read_u8()? {
                    0 => SyntaxShape::Expr,
                    1 => SyntaxShape::Module,
                    _ => {
                        return Err(AssemblyError::text_parse_source("unknown syntax shape"));
                    }
                },
                text: cursor.read_idx()?,
            },
            _ => {
                return Err(AssemblyError::text_parse_source("unknown constant kind"));
            }
        };
        let _ = artifact
            .constants
            .alloc(ConstantDescriptor::new(name, constant_value));
    }
    Ok(())
}

fn decode_globals(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Globals)?;
    for _ in 0..cursor.read_u32()? {
        let name = cursor.read_idx()?;
        let export = cursor.read_u8()? != 0;
        let initializer = if cursor.read_u8()? == 0 {
            None
        } else {
            Some(cursor.read_idx()?)
        };
        let mut descriptor = GlobalDescriptor::new(name).with_export(export);
        if let Some(initializer) = initializer {
            descriptor = descriptor.with_initializer(initializer);
        }
        let _ = artifact.globals.alloc(descriptor);
    }
    Ok(())
}

fn decode_next_trailing_section(
    cursor: &mut Cursor<'_>,
    artifact: &mut Artifact,
) -> AssemblyResult {
    let next = cursor
        .peek_u8()
        .ok_or(AssemblyError::BinaryPayloadTruncated)?;
    match next {
        tag if tag == section_tag_byte(SectionTag::StackEffects) => {
            decode_stack_effects(cursor, artifact)
        }
        tag if tag == section_tag_byte(SectionTag::RootMaps) => decode_root_maps(cursor, artifact),
        tag if tag == section_tag_byte(SectionTag::BlockSignatures) => {
            decode_block_signatures(cursor, artifact)
        }
        tag if tag == section_tag_byte(SectionTag::Closures) => decode_closures(cursor, artifact),
        tag if tag == section_tag_byte(SectionTag::Meta) => decode_meta(cursor, artifact),
        tag if tag == section_tag_byte(SectionTag::Manifest) => decode_manifest(cursor, artifact),
        tag if tag == section_tag_byte(SectionTag::Imports) => decode_imports(cursor, artifact),
        _ => Err(AssemblyError::text_parse_source("unknown trailing section")),
    }
}

fn decode_procedures(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    procedures::decode_procedures(cursor, artifact)
}

fn decode_shapes(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Shapes)?;
    for _ in 0..cursor.read_u32()? {
        let name = cursor.read_idx()?;
        let payload_ty = if cursor.read_u8()? != 0 {
            Some(cursor.read_idx()?)
        } else {
            None
        };
        let witness = if cursor.read_u8()? != 0 {
            Some(cursor.read_idx()?)
        } else {
            None
        };
        let dispatch_table = if cursor.read_u8()? != 0 {
            Some(cursor.read_idx()?)
        } else {
            None
        };
        let layout_identity = if cursor.read_u8()? != 0 {
            Some(cursor.read_idx()?)
        } else {
            None
        };
        let mut descriptor = ShapeDescriptor::new(name).with_root_visible(cursor.read_u8()? != 0);
        if let Some(payload_ty) = payload_ty {
            descriptor = descriptor.with_payload_ty(payload_ty);
        }
        if let Some(witness) = witness {
            descriptor = descriptor.with_witness(witness);
        }
        if let Some(dispatch_table) = dispatch_table {
            descriptor = descriptor.with_dispatch_table(dispatch_table);
        }
        if let Some(layout_identity) = layout_identity {
            descriptor = descriptor.with_layout_identity(layout_identity);
        }
        let _ = artifact.shapes.alloc(descriptor);
    }
    Ok(())
}

fn decode_foreigns(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    foreigns::decode_foreigns(cursor, artifact)
}

fn decode_exports(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Exports)?;
    let count = cursor.read_u32()?;
    for _ in 0..count {
        let name = Idx::from_raw(cursor.read_u32()?);
        let kind = cursor.read_u8()?;
        let target_raw = cursor.read_u32()?;
        let target = match kind {
            0 => ExportTarget::Procedure(Idx::from_raw(target_raw)),
            1 => ExportTarget::Global(Idx::from_raw(target_raw)),
            2 => ExportTarget::Foreign(Idx::from_raw(target_raw)),
            3 => ExportTarget::Type(Idx::from_raw(target_raw)),
            4 => ExportTarget::Shape(Idx::from_raw(target_raw)),
            _ => return Err(AssemblyError::InvalidBinaryHeader),
        };
        let opaque = cursor.read_u8()? != 0;
        let _ = artifact
            .exports
            .alloc(ExportDescriptor::new(name, opaque, target));
    }
    Ok(())
}

fn decode_data(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    data::decode_data(cursor, artifact)
}

fn decode_meta(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Meta)?;
    for _ in 0..cursor.read_u32()? {
        let target = cursor.read_idx()?;
        let key = cursor.read_idx()?;
        let value_len = usize::from(cursor.read_u16()?);
        let mut values = Vec::with_capacity(value_len);
        for _ in 0..value_len {
            values.push(cursor.read_idx()?);
        }
        let _ = artifact
            .meta
            .alloc(MetaDescriptor::new(target, key, values.into_boxed_slice()));
    }
    Ok(())
}

fn decode_manifest(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Manifest)?;
    for _ in 0..cursor.read_u32()? {
        let package = cursor.read_idx()?;
        let version = cursor.read_idx()?;
        let profile = cursor.read_idx()?;
        let mut descriptor = ManifestDescriptor::new(package, version, profile);
        match cursor.read_u8()? {
            0 => {}
            1 => descriptor = descriptor.with_entry(cursor.read_idx()?),
            _ => {
                return Err(AssemblyError::text_parse_source(
                    "invalid manifest entry marker",
                ));
            }
        }
        let _ = artifact.manifest.alloc(descriptor);
    }
    Ok(())
}

fn decode_imports(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Imports)?;
    for _ in 0..cursor.read_u32()? {
        let spec = cursor.read_idx()?;
        let resolved = cursor.read_idx()?;
        let _ = artifact
            .imports
            .alloc(ImportDescriptor::new(spec, resolved));
    }
    Ok(())
}

fn decode_stack_effects(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::StackEffects)?;
    for _ in 0..cursor.read_u32()? {
        let name = cursor.read_idx()?;
        let input_len = usize::from(cursor.read_u16()?);
        let mut input_tys = Vec::with_capacity(input_len);
        for _ in 0..input_len {
            input_tys.push(cursor.read_idx()?);
        }
        let output_len = usize::from(cursor.read_u16()?);
        let mut output_tys = Vec::with_capacity(output_len);
        for _ in 0..output_len {
            output_tys.push(cursor.read_idx()?);
        }
        let _ = artifact.stack_effects.alloc(StackEffectDescriptor::new(
            name,
            input_tys.into_boxed_slice(),
            output_tys.into_boxed_slice(),
        ));
    }
    Ok(())
}

fn decode_root_maps(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::RootMaps)?;
    for _ in 0..cursor.read_u32()? {
        let safe_point = cursor.read_idx()?;
        let Some(kind) = SafePointKind::from_wire(cursor.read_u8()?) else {
            return Err(AssemblyError::text_parse_source("unknown safe point kind"));
        };
        let procedure = match cursor.read_u8()? {
            0 => None,
            1 => Some(cursor.read_idx()?),
            _ => {
                return Err(AssemblyError::text_parse_source(
                    "invalid root-map procedure marker",
                ));
            }
        };
        let local_len = usize::from(cursor.read_u16()?);
        let mut local_slots = Vec::with_capacity(local_len);
        for _ in 0..local_len {
            local_slots.push(cursor.read_u16()?);
        }
        let stack_len = usize::from(cursor.read_u16()?);
        let mut stack_slots = Vec::with_capacity(stack_len);
        for _ in 0..stack_len {
            stack_slots.push(cursor.read_u16()?);
        }
        let capture_len = usize::from(cursor.read_u16()?);
        let mut capture_slots = Vec::with_capacity(capture_len);
        for _ in 0..capture_len {
            capture_slots.push(cursor.read_u16()?);
        }
        let defer_len = usize::from(cursor.read_u16()?);
        let mut defer_slots = Vec::with_capacity(defer_len);
        for _ in 0..defer_len {
            defer_slots.push(cursor.read_u16()?);
        }
        let pin_len = usize::from(cursor.read_u16()?);
        let mut pin_slots = Vec::with_capacity(pin_len);
        for _ in 0..pin_len {
            pin_slots.push(cursor.read_u16()?);
        }
        let mut descriptor = RootMapDescriptor::new(
            safe_point,
            local_slots.into_boxed_slice(),
            stack_slots.into_boxed_slice(),
        )
        .with_kind(kind)
        .with_capture_slots(capture_slots.into_boxed_slice())
        .with_defer_slots(defer_slots.into_boxed_slice())
        .with_pin_slots(pin_slots.into_boxed_slice());
        if let Some(procedure) = procedure {
            descriptor = descriptor.with_procedure(procedure);
        }
        let _ = artifact.root_maps.alloc(descriptor);
    }
    Ok(())
}

fn decode_block_signatures(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::BlockSignatures)?;
    for _ in 0..cursor.read_u32()? {
        let procedure = cursor.read_idx()?;
        let label = cursor.read_u16()?;
        let incoming_len = usize::from(cursor.read_u16()?);
        let mut incoming_tys = Vec::with_capacity(incoming_len);
        for _ in 0..incoming_len {
            incoming_tys.push(cursor.read_idx()?);
        }
        let _ = artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(
                procedure,
                label,
                incoming_tys.into_boxed_slice(),
            ));
    }
    Ok(())
}

fn decode_closures(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Closures)?;
    for _ in 0..cursor.read_u32()? {
        let name = cursor.read_idx()?;
        let procedure = cursor.read_idx()?;
        let capture_count = cursor.read_u16()?;
        let capture_ty_len = usize::from(cursor.read_u16()?);
        let mut capture_tys = Vec::with_capacity(capture_ty_len);
        for _ in 0..capture_ty_len {
            capture_tys.push(cursor.read_idx()?);
        }
        let env_layout = if cursor.read_u8()? != 0 {
            Some(cursor.read_idx()?)
        } else {
            None
        };
        let param_ty_len = usize::from(cursor.read_u16()?);
        let mut param_tys = Vec::with_capacity(param_ty_len);
        for _ in 0..param_ty_len {
            param_tys.push(cursor.read_idx()?);
        }
        let result_ty_len = usize::from(cursor.read_u16()?);
        let mut result_tys = Vec::with_capacity(result_ty_len);
        for _ in 0..result_ty_len {
            result_tys.push(cursor.read_idx()?);
        }
        let domain = if cursor.read_u8()? != 0 {
            Some(cursor.read_idx()?)
        } else {
            None
        };
        let effect = if cursor.read_u8()? != 0 {
            Some(cursor.read_idx()?)
        } else {
            None
        };
        let mut descriptor = ClosureDescriptor::new(name, procedure, capture_count)
            .with_capture_tys(capture_tys.into_boxed_slice())
            .with_param_tys(param_tys.into_boxed_slice())
            .with_result_tys(result_tys.into_boxed_slice())
            .with_suspending(cursor.read_u8()? != 0);
        if let Some(env_layout) = env_layout {
            descriptor = descriptor.with_env_layout(env_layout);
        }
        if let Some(domain) = domain {
            descriptor = descriptor.with_domain(domain);
        }
        if let Some(effect) = effect {
            descriptor = descriptor.with_effect(effect);
        }
        let _ = artifact.closures.alloc(descriptor);
    }
    Ok(())
}

fn decode_operand(cursor: &mut Cursor<'_>) -> AssemblyResult<Operand> {
    Ok(match cursor.read_u8()? {
        0 => Operand::None,
        1 => Operand::I16(cursor.read_i16()?),
        2 => Operand::Local(cursor.read_u16()?),
        3 => Operand::String(cursor.read_idx()?),
        4 => Operand::Type(cursor.read_idx()?),
        5 => Operand::Constant(cursor.read_idx()?),
        6 => Operand::Global(cursor.read_idx()?),
        7 => Operand::Procedure(cursor.read_idx()?),
        8 => Operand::Foreign(cursor.read_idx()?),
        13 => Operand::WideProcedureCaptures {
            procedure: cursor.read_idx()?,
            captures: cursor.read_u8()?,
        },
        10 => Operand::Label(cursor.read_u16()?),
        11 => Operand::TypeLen {
            ty: cursor.read_idx()?,
            len: cursor.read_u16()?,
        },
        12 => {
            let count = usize::from(cursor.read_u16()?);
            let mut labels = Vec::with_capacity(count);
            for _ in 0..count {
                labels.push(cursor.read_u16()?);
            }
            Operand::BranchTable(labels.into_boxed_slice())
        }
        _ => return Err(AssemblyError::text_parse_source("unknown operand tag")),
    })
}

fn decode_opcode(cursor: &mut Cursor<'_>) -> AssemblyResult<u16> {
    let lead = cursor.read_u8()?;
    if lead != Opcode::extended_opcode_prefix() {
        return Ok(u16::from(lead));
    }
    cursor.read_u16()
}
