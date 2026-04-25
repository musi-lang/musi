use super::*;

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
    decode_effects(&mut cursor, &mut artifact)?;
    decode_shapes(&mut cursor, &mut artifact)?;
    decode_foreigns(&mut cursor, &mut artifact)?;
    decode_exports(&mut cursor, &mut artifact)?;
    decode_data(&mut cursor, &mut artifact)?;
    if !cursor.is_eof() {
        let next = cursor
            .peek_u8()
            .ok_or(AssemblyError::BinaryPayloadTruncated)?;
        if next == section_tag_byte(SectionTag::Meta) {
            decode_meta(&mut cursor, &mut artifact)?;
        } else {
            return Err(AssemblyError::text_parse_source("unknown trailing section"));
        }
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

fn decode_procedures(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Procedures)?;
    for _ in 0..cursor.read_u32()? {
        let name = cursor.read_idx()?;
        let params = cursor.read_u16()?;
        let locals = cursor.read_u16()?;
        let export = cursor.read_u8()? != 0;
        let hot = cursor.read_u8()? != 0;
        let cold = cursor.read_u8()? != 0;
        let label_count = usize::from(cursor.read_u16()?);
        let mut labels = Vec::with_capacity(label_count);
        for _ in 0..label_count {
            labels.push(cursor.read_idx()?);
        }
        let code_count = read_len(cursor, "code entry count")?;
        let mut code = Vec::with_capacity(code_count);
        for _ in 0..code_count {
            let kind = cursor.read_u8()?;
            let entry = match kind {
                0 => CodeEntry::Label(Label {
                    id: cursor.read_u16()?,
                }),
                1 => {
                    let opcode_code = decode_opcode(cursor)?;
                    let Some(opcode) = Opcode::from_wire_code(opcode_code) else {
                        return Err(AssemblyError::UnknownOpcode(opcode_code));
                    };
                    let operand = decode_operand(cursor)?;
                    CodeEntry::Instruction(Instruction::new(opcode, operand))
                }
                _ => {
                    return Err(AssemblyError::text_parse_source("unknown code entry kind"));
                }
            };
            code.push(entry);
        }
        let _ = artifact.procedures.alloc(
            ProcedureDescriptor::new(name, params, locals, code.into_boxed_slice())
                .with_export(export)
                .with_hot(hot)
                .with_cold(cold)
                .with_labels(labels.into_boxed_slice()),
        );
    }
    Ok(())
}

fn decode_effects(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Effects)?;
    for _ in 0..cursor.read_u32()? {
        let name = cursor.read_idx()?;
        let ops_len = usize::from(cursor.read_u16()?);
        let mut ops = Vec::with_capacity(ops_len);
        for _ in 0..ops_len {
            let name = cursor.read_idx()?;
            let param_len = usize::from(cursor.read_u16()?);
            let mut param_tys = Vec::with_capacity(param_len);
            for _ in 0..param_len {
                param_tys.push(cursor.read_idx()?);
            }
            ops.push(
                EffectOpDescriptor::new(name, param_tys.into_boxed_slice(), cursor.read_idx()?)
                    .with_comptime_safe(cursor.read_u8()? != 0),
            );
        }
        let _ = artifact
            .effects
            .alloc(EffectDescriptor::new(name, ops.into_boxed_slice()));
    }
    Ok(())
}

fn decode_shapes(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Shapes)?;
    for _ in 0..cursor.read_u32()? {
        let _ = artifact
            .shapes
            .alloc(ShapeDescriptor::new(cursor.read_idx()?));
    }
    Ok(())
}

fn decode_foreigns(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Foreigns)?;
    for _ in 0..cursor.read_u32()? {
        let name = cursor.read_idx()?;
        let param_len = usize::from(cursor.read_u16()?);
        let mut param_tys = Vec::with_capacity(param_len);
        for _ in 0..param_len {
            param_tys.push(cursor.read_idx()?);
        }
        let result_ty = cursor.read_idx()?;
        let abi = cursor.read_idx()?;
        let symbol = cursor.read_idx()?;
        let link = match cursor.read_u8()? {
            0 => None,
            1 => Some(cursor.read_idx()?),
            _ => {
                return Err(AssemblyError::text_parse_source(
                    "invalid foreign link marker",
                ));
            }
        };
        let mut descriptor =
            ForeignDescriptor::new(name, param_tys.into_boxed_slice(), result_ty, abi, symbol)
                .with_export(cursor.read_u8()? != 0)
                .with_hot(cursor.read_u8()? != 0)
                .with_cold(cursor.read_u8()? != 0);
        if let Some(link) = link {
            descriptor = descriptor.with_link(link);
        }
        let _ = artifact.foreigns.alloc(descriptor);
    }
    Ok(())
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
            4 => ExportTarget::Effect(Idx::from_raw(target_raw)),
            5 => ExportTarget::Shape(Idx::from_raw(target_raw)),
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
    require_section(cursor, SectionTag::Data)?;
    let count = cursor.read_u32()?;
    for _ in 0..count {
        let name = Idx::from_raw(cursor.read_u32()?);
        let variant_count = cursor.read_u32()?;
        let field_count = cursor.read_u32()?;
        let variant_len = cursor.read_u32()?;
        let mut variants = Vec::with_capacity(usize::try_from(variant_len).unwrap_or(usize::MAX));
        for _ in 0..variant_len {
            let variant_name = Idx::from_raw(cursor.read_u32()?);
            let tag = cursor.read_i64()?;
            let field_len = cursor.read_u32()?;
            let mut field_tys =
                Vec::with_capacity(usize::try_from(field_len).unwrap_or(usize::MAX));
            for _ in 0..field_len {
                field_tys.push(Idx::from_raw(cursor.read_u32()?));
            }
            variants.push(DataVariantDescriptor::new(
                variant_name,
                tag,
                field_tys.into_boxed_slice(),
            ));
        }
        let repr_kind = if cursor.read_u8()? != 0 {
            Some(Idx::from_raw(cursor.read_u32()?))
        } else {
            None
        };
        let layout_align = if cursor.read_u8()? != 0 {
            Some(cursor.read_u32()?)
        } else {
            None
        };
        let layout_pack = if cursor.read_u8()? != 0 {
            Some(cursor.read_u32()?)
        } else {
            None
        };
        let frozen = cursor.read_u8()? != 0;
        let mut descriptor = DataDescriptor::new(name, variants.into_boxed_slice());
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
        descriptor = descriptor.with_frozen(frozen);
        let _ = artifact.data.alloc(descriptor);
    }
    Ok(())
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
        9 => Operand::Effect {
            effect: cursor.read_idx()?,
            op: cursor.read_u16()?,
        },
        14 => Operand::EffectId(cursor.read_idx()?),
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
