use crate::descriptor::{
    ClassDescriptor, ConstantDescriptor, ConstantValue, DataDescriptor, DataVariantDescriptor,
    EffectDescriptor, EffectOpDescriptor, ExportDescriptor, ExportTarget, ForeignDescriptor,
    GlobalDescriptor, MetaDescriptor, ProcedureDescriptor, TypeDescriptor,
};
use crate::{
    Artifact, BINARY_VERSION, CodeEntry, Instruction, Label, Opcode, Operand, SEAM_MAGIC,
    SectionTag,
};
use crate::{AssemblyError, AssemblyResult};
use music_arena::Idx;
use music_term::SyntaxShape;

/// Encodes a validated SEAM artifact into the sectioned `.seam` binary format.
///
/// # Errors
///
/// Returns [`AssemblyError`] if the artifact fails structural validation before encoding.
pub fn encode_binary(artifact: &Artifact) -> AssemblyResult<Vec<u8>> {
    artifact.validate()?;
    let mut out = Vec::new();
    out.extend_from_slice(&SEAM_MAGIC);
    push_u16(&mut out, BINARY_VERSION);
    encode_strings(&mut out, artifact);
    encode_types(&mut out, artifact);
    encode_constants(&mut out, artifact);
    encode_globals(&mut out, artifact);
    encode_procedures(&mut out, artifact);
    encode_effects(&mut out, artifact);
    encode_classes(&mut out, artifact);
    encode_foreigns(&mut out, artifact);
    encode_exports(&mut out, artifact);
    encode_data(&mut out, artifact);
    encode_meta(&mut out, artifact);
    Ok(out)
}

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
    let version = cursor.read_u16()?;
    if version != BINARY_VERSION {
        return Err(AssemblyError::UnsupportedBinaryVersion(version));
    }

    let mut artifact = Artifact::new();
    decode_strings(&mut cursor, &mut artifact)?;
    decode_types(&mut cursor, &mut artifact)?;
    decode_constants(&mut cursor, &mut artifact)?;
    decode_globals(&mut cursor, &mut artifact)?;
    decode_procedures(&mut cursor, &mut artifact)?;
    decode_effects(&mut cursor, &mut artifact)?;
    decode_classes(&mut cursor, &mut artifact)?;
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
            return Err(AssemblyError::TextParseFailed(
                "unknown trailing section".into(),
            ));
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

fn encode_strings(out: &mut Vec<u8>, artifact: &Artifact) {
    push_section_tag(out, SectionTag::Strings);
    push_u32(
        out,
        u32::try_from(artifact.strings.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.strings.iter() {
        push_bytes(out, entry.text.as_bytes());
    }
}

fn encode_types(out: &mut Vec<u8>, artifact: &Artifact) {
    push_section_tag(out, SectionTag::Types);
    push_u32(
        out,
        u32::try_from(artifact.types.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.types.iter() {
        push_u32(out, entry.name.raw());
        push_u32(out, entry.term.raw());
    }
}

fn encode_constants(out: &mut Vec<u8>, artifact: &Artifact) {
    push_section_tag(out, SectionTag::Constants);
    push_u32(
        out,
        u32::try_from(artifact.constants.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.constants.iter() {
        push_u32(out, entry.name.raw());
        match entry.value {
            ConstantValue::Int(value) => {
                out.push(0);
                push_i64(out, value);
            }
            ConstantValue::Float(value) => {
                out.push(1);
                push_u64(out, value.to_bits());
            }
            ConstantValue::Bool(value) => {
                out.push(2);
                out.push(u8::from(value));
            }
            ConstantValue::String(id) => {
                out.push(3);
                push_u32(out, id.raw());
            }
            ConstantValue::Syntax { shape, text } => {
                out.push(4);
                out.push(match shape {
                    SyntaxShape::Expr => 0,
                    SyntaxShape::Module => 1,
                });
                push_u32(out, text.raw());
            }
        }
    }
}

fn encode_globals(out: &mut Vec<u8>, artifact: &Artifact) {
    push_section_tag(out, SectionTag::Globals);
    push_u32(
        out,
        u32::try_from(artifact.globals.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.globals.iter() {
        push_u32(out, entry.name.raw());
        out.push(u8::from(entry.export));
        match entry.initializer {
            Some(id) => {
                out.push(1);
                push_u32(out, id.raw());
            }
            None => out.push(0),
        }
    }
}

fn encode_procedures(out: &mut Vec<u8>, artifact: &Artifact) {
    push_section_tag(out, SectionTag::Procedures);
    push_u32(
        out,
        u32::try_from(artifact.procedures.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.procedures.iter() {
        push_u32(out, entry.name.raw());
        push_u16(out, entry.params);
        push_u16(out, entry.locals);
        out.push(u8::from(entry.export));
        out.push(u8::from(entry.hot));
        out.push(u8::from(entry.cold));
        push_u16(
            out,
            u16::try_from(entry.labels.len()).expect("too many labels"),
        );
        for label in &entry.labels {
            push_u32(out, label.raw());
        }
        push_u32(out, u32::try_from(entry.code.len()).expect("code overflow"));
        for code in &entry.code {
            match code {
                CodeEntry::Label(label) => {
                    out.push(0);
                    push_u16(out, label.id);
                }
                CodeEntry::Instruction(instruction) => {
                    out.push(1);
                    push_u16(out, instruction.opcode.wire_code());
                    encode_operand(out, &instruction.operand);
                }
            }
        }
    }
}

fn encode_effects(out: &mut Vec<u8>, artifact: &Artifact) {
    push_section_tag(out, SectionTag::Effects);
    push_u32(
        out,
        u32::try_from(artifact.effects.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.effects.iter() {
        push_u32(out, entry.name.raw());
        push_u16(
            out,
            u16::try_from(entry.ops.len()).expect("too many effect ops"),
        );
        for op in &entry.ops {
            push_u32(out, op.name.raw());
            push_u16(
                out,
                u16::try_from(op.param_tys.len()).expect("too many effect op params"),
            );
            for ty in &op.param_tys {
                push_u32(out, ty.raw());
            }
            push_u32(out, op.result_ty.raw());
            out.push(u8::from(op.is_comptime_safe));
        }
    }
}

fn encode_classes(out: &mut Vec<u8>, artifact: &Artifact) {
    push_section_tag(out, SectionTag::Classes);
    push_u32(
        out,
        u32::try_from(artifact.classes.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.classes.iter() {
        push_u32(out, entry.name.raw());
    }
}

fn encode_foreigns(out: &mut Vec<u8>, artifact: &Artifact) {
    push_section_tag(out, SectionTag::Foreigns);
    push_u32(
        out,
        u32::try_from(artifact.foreigns.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.foreigns.iter() {
        push_u32(out, entry.name.raw());
        push_u16(
            out,
            u16::try_from(entry.param_tys.len()).expect("too many foreign params"),
        );
        for ty in &entry.param_tys {
            push_u32(out, ty.raw());
        }
        push_u32(out, entry.result_ty.raw());
        push_u32(out, entry.abi.raw());
        push_u32(out, entry.symbol.raw());
        if let Some(link) = entry.link {
            out.push(1);
            push_u32(out, link.raw());
        } else {
            out.push(0);
        }
        out.push(u8::from(entry.export));
        out.push(u8::from(entry.hot));
        out.push(u8::from(entry.cold));
    }
}

fn encode_exports(out: &mut Vec<u8>, artifact: &Artifact) {
    push_section_tag(out, SectionTag::Exports);
    push_u32(
        out,
        u32::try_from(artifact.exports.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.exports.iter() {
        push_u32(out, entry.name.raw());
        match entry.target {
            ExportTarget::Procedure(id) => {
                out.push(0);
                push_u32(out, id.raw());
            }
            ExportTarget::Global(id) => {
                out.push(1);
                push_u32(out, id.raw());
            }
            ExportTarget::Foreign(id) => {
                out.push(2);
                push_u32(out, id.raw());
            }
            ExportTarget::Type(id) => {
                out.push(3);
                push_u32(out, id.raw());
            }
            ExportTarget::Effect(id) => {
                out.push(4);
                push_u32(out, id.raw());
            }
            ExportTarget::Class(id) => {
                out.push(5);
                push_u32(out, id.raw());
            }
        }
        out.push(u8::from(entry.opaque));
    }
}

fn encode_data(out: &mut Vec<u8>, artifact: &Artifact) {
    push_section_tag(out, SectionTag::Data);
    push_u32(
        out,
        u32::try_from(artifact.data.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.data.iter() {
        push_u32(out, entry.name.raw());
        push_u32(out, entry.variant_count);
        push_u32(out, entry.field_count);
        push_u32(
            out,
            u32::try_from(entry.variants.len()).expect("data variant overflow"),
        );
        for variant in &entry.variants {
            push_u32(out, variant.name.raw());
            push_i64(out, variant.tag);
            push_u32(
                out,
                u32::try_from(variant.field_tys.len()).expect("data field overflow"),
            );
            for ty in &variant.field_tys {
                push_u32(out, ty.raw());
            }
        }
        match entry.repr_kind {
            Some(id) => {
                out.push(1);
                push_u32(out, id.raw());
            }
            None => out.push(0),
        }
        match entry.layout_align {
            Some(value) => {
                out.push(1);
                push_u32(out, value);
            }
            None => out.push(0),
        }
        match entry.layout_pack {
            Some(value) => {
                out.push(1);
                push_u32(out, value);
            }
            None => out.push(0),
        }
        out.push(u8::from(entry.frozen));
    }
}

fn encode_meta(out: &mut Vec<u8>, artifact: &Artifact) {
    if artifact.meta.is_empty() {
        return;
    }
    push_section_tag(out, SectionTag::Meta);
    push_u32(
        out,
        u32::try_from(artifact.meta.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.meta.iter() {
        push_u32(out, entry.target.raw());
        push_u32(out, entry.key.raw());
        push_u16(
            out,
            u16::try_from(entry.values.len()).expect("too many meta values"),
        );
        for value in entry.values.iter().copied() {
            push_u32(out, value.raw());
        }
    }
}

fn encode_operand(out: &mut Vec<u8>, operand: &Operand) {
    match operand {
        Operand::None => out.push(0),
        Operand::I16(value) => {
            out.push(1);
            push_i16(out, *value);
        }
        Operand::Local(slot) => {
            out.push(2);
            push_u16(out, *slot);
        }
        Operand::String(id) => {
            out.push(3);
            push_u32(out, id.raw());
        }
        Operand::Type(id) => {
            out.push(4);
            push_u32(out, id.raw());
        }
        Operand::Constant(id) => {
            out.push(5);
            push_u32(out, id.raw());
        }
        Operand::Global(id) => {
            out.push(6);
            push_u32(out, id.raw());
        }
        Operand::Procedure(id) => {
            out.push(7);
            push_u32(out, id.raw());
        }
        Operand::WideProcedureCaptures {
            procedure,
            captures,
        } => {
            out.push(13);
            push_u32(out, procedure.raw());
            out.push(*captures);
        }
        Operand::Foreign(id) => {
            out.push(8);
            push_u32(out, id.raw());
        }
        Operand::Effect { effect, op } => {
            out.push(9);
            push_u32(out, effect.raw());
            push_u16(out, *op);
        }
        Operand::EffectId(effect) => {
            out.push(14);
            push_u32(out, effect.raw());
        }
        Operand::Label(id) => {
            out.push(10);
            push_u16(out, *id);
        }
        Operand::TypeLen { ty, len } => {
            out.push(11);
            push_u32(out, ty.raw());
            push_u16(out, *len);
        }
        Operand::BranchTable(labels) => {
            out.push(12);
            push_u16(
                out,
                u16::try_from(labels.len()).expect("branch table overflow"),
            );
            for label in labels.iter().copied() {
                push_u16(out, label);
            }
        }
    }
}

fn decode_strings(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Strings)?;
    for _ in 0..cursor.read_u32()? {
        let bytes = cursor.read_bytes()?;
        let text = String::from_utf8(bytes)
            .map_err(|err| AssemblyError::TextParseFailed(err.to_string()))?;
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
        let value = match kind {
            0 => ConstantValue::Int(cursor.read_i64()?),
            1 => ConstantValue::Float(f64::from_bits(cursor.read_u64()?)),
            2 => ConstantValue::Bool(cursor.read_u8()? != 0),
            3 => ConstantValue::String(cursor.read_idx()?),
            4 => ConstantValue::Syntax {
                shape: match cursor.read_u8()? {
                    0 => SyntaxShape::Expr,
                    1 => SyntaxShape::Module,
                    _ => {
                        return Err(AssemblyError::TextParseFailed(
                            "unknown syntax shape".into(),
                        ));
                    }
                },
                text: cursor.read_idx()?,
            },
            _ => {
                return Err(AssemblyError::TextParseFailed(
                    "unknown constant kind".into(),
                ));
            }
        };
        let _ = artifact
            .constants
            .alloc(ConstantDescriptor::new(name, value));
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
                    let opcode_code = cursor.read_u16()?;
                    let Some(opcode) = Opcode::from_wire_code(opcode_code) else {
                        return Err(AssemblyError::UnknownOpcode(opcode_code));
                    };
                    let operand = decode_operand(cursor)?;
                    CodeEntry::Instruction(Instruction::new(opcode, operand))
                }
                _ => {
                    return Err(AssemblyError::TextParseFailed(
                        "unknown code entry kind".into(),
                    ));
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

fn decode_classes(cursor: &mut Cursor<'_>, artifact: &mut Artifact) -> AssemblyResult {
    require_section(cursor, SectionTag::Classes)?;
    for _ in 0..cursor.read_u32()? {
        let _ = artifact
            .classes
            .alloc(ClassDescriptor::new(cursor.read_idx()?));
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
                return Err(AssemblyError::TextParseFailed(
                    "invalid foreign link marker".into(),
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
            5 => ExportTarget::Class(Idx::from_raw(target_raw)),
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
        _ => return Err(AssemblyError::TextParseFailed("unknown operand tag".into())),
    })
}

fn push_section_tag(out: &mut Vec<u8>, tag: SectionTag) {
    out.push(section_tag_byte(tag));
}

fn require_section(cursor: &mut Cursor<'_>, tag: SectionTag) -> AssemblyResult {
    let found = cursor.read_u8()?;
    if found == section_tag_byte(tag) {
        Ok(())
    } else {
        Err(AssemblyError::UnknownSectionTag(found))
    }
}

const fn section_tag_byte(tag: SectionTag) -> u8 {
    match tag {
        SectionTag::Strings => 1,
        SectionTag::Types => 2,
        SectionTag::Constants => 3,
        SectionTag::Globals => 4,
        SectionTag::Procedures => 5,
        SectionTag::Effects => 6,
        SectionTag::Classes => 7,
        SectionTag::Foreigns => 8,
        SectionTag::Exports => 9,
        SectionTag::Data => 10,
        SectionTag::Meta => 11,
    }
}

fn push_u16(out: &mut Vec<u8>, value: u16) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn push_i16(out: &mut Vec<u8>, value: i16) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn push_u32(out: &mut Vec<u8>, value: u32) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn push_i64(out: &mut Vec<u8>, value: i64) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn push_u64(out: &mut Vec<u8>, value: u64) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn push_bytes(out: &mut Vec<u8>, bytes: &[u8]) {
    push_u32(out, u32::try_from(bytes.len()).expect("payload overflow"));
    out.extend_from_slice(bytes);
}

struct Cursor<'bytes> {
    bytes: &'bytes [u8],
    offset: usize,
}

impl<'bytes> Cursor<'bytes> {
    const fn new(bytes: &'bytes [u8]) -> Self {
        Self { bytes, offset: 0 }
    }

    const fn is_eof(&self) -> bool {
        self.offset >= self.bytes.len()
    }

    fn peek_u8(&self) -> Option<u8> {
        self.bytes.get(self.offset).copied()
    }

    fn read_exact(&mut self, len: usize) -> AssemblyResult<[u8; 4]> {
        if len != 4 {
            return Err(AssemblyError::BinaryPayloadTruncated);
        }
        let end = self.offset.saturating_add(4);
        let slice = self
            .bytes
            .get(self.offset..end)
            .ok_or(AssemblyError::BinaryPayloadTruncated)?;
        self.offset = end;
        let mut out = [0_u8; 4];
        out.copy_from_slice(slice);
        Ok(out)
    }

    fn read_u8(&mut self) -> AssemblyResult<u8> {
        let value = *self
            .bytes
            .get(self.offset)
            .ok_or(AssemblyError::BinaryPayloadTruncated)?;
        self.offset = self.offset.saturating_add(1);
        Ok(value)
    }

    fn read_u16(&mut self) -> AssemblyResult<u16> {
        let bytes = self.read_array::<2>()?;
        Ok(u16::from_le_bytes(bytes))
    }

    fn read_i16(&mut self) -> AssemblyResult<i16> {
        let bytes = self.read_array::<2>()?;
        Ok(i16::from_le_bytes(bytes))
    }

    fn read_u32(&mut self) -> AssemblyResult<u32> {
        let bytes = self.read_array::<4>()?;
        Ok(u32::from_le_bytes(bytes))
    }

    fn read_i64(&mut self) -> AssemblyResult<i64> {
        let bytes = self.read_array::<8>()?;
        Ok(i64::from_le_bytes(bytes))
    }

    fn read_u64(&mut self) -> AssemblyResult<u64> {
        let bytes = self.read_array::<8>()?;
        Ok(u64::from_le_bytes(bytes))
    }

    fn read_idx<T>(&mut self) -> AssemblyResult<Idx<T>> {
        Ok(Idx::from_raw(self.read_u32()?))
    }

    fn read_bytes(&mut self) -> AssemblyResult<Vec<u8>> {
        let len = read_len(self, "byte payload length")?;
        let end = self.offset.saturating_add(len);
        let slice = self
            .bytes
            .get(self.offset..end)
            .ok_or(AssemblyError::BinaryPayloadTruncated)?;
        self.offset = end;
        Ok(slice.to_vec())
    }

    fn read_array<const N: usize>(&mut self) -> AssemblyResult<[u8; N]> {
        let end = self.offset.saturating_add(N);
        let slice = self
            .bytes
            .get(self.offset..end)
            .ok_or(AssemblyError::BinaryPayloadTruncated)?;
        self.offset = end;
        let mut out = [0_u8; N];
        out.copy_from_slice(slice);
        Ok(out)
    }
}

fn read_len(cursor: &mut Cursor<'_>, what: &'static str) -> AssemblyResult<usize> {
    usize::try_from(cursor.read_u32()?)
        .map_err(|_| AssemblyError::TextParseFailed(format!("{what} does not fit in usize")))
}
