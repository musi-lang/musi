use super::*;

/// Encodes a validated SEAM artifact into a sectioned `.seam` byte stream.
///
/// # Errors
///
/// Returns [`AssemblyError`] if artifact validation fails before encoding.
pub fn encode_binary(artifact: &Artifact) -> AssemblyResult<Vec<u8>> {
    artifact.validate()?;
    let mut out = Vec::new();
    out.extend_from_slice(&SEAM_MAGIC);
    push_u16(&mut out, BINARY_MAJOR_VERSION);
    push_u16(&mut out, BINARY_MINOR_VERSION);
    encode_strings(&mut out, artifact);
    encode_types(&mut out, artifact);
    encode_constants(&mut out, artifact);
    encode_globals(&mut out, artifact);
    encode_procedures(&mut out, artifact);
    encode_effects(&mut out, artifact);
    encode_shapes(&mut out, artifact);
    encode_foreigns(&mut out, artifact);
    encode_exports(&mut out, artifact);
    encode_data(&mut out, artifact);
    encode_meta(&mut out, artifact);
    Ok(out)
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
                    encode_opcode(out, instruction.opcode);
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

fn encode_shapes(out: &mut Vec<u8>, artifact: &Artifact) {
    push_section_tag(out, SectionTag::Shapes);
    push_u32(
        out,
        u32::try_from(artifact.shapes.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.shapes.iter() {
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
            ExportTarget::Shape(id) => {
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

fn encode_opcode(out: &mut Vec<u8>, opcode: Opcode) {
    let code = opcode.wire_code();
    if code <= 0xFE {
        out.push(u8::try_from(code).expect("core opcode range"));
        return;
    }
    out.push(Opcode::extended_opcode_prefix());
    out.extend_from_slice(&code.to_le_bytes());
}
