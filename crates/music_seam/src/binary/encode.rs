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
    encode_shapes(&mut out, artifact);
    encode_foreigns(&mut out, artifact);
    encode_exports(&mut out, artifact);
    encode_data(&mut out, artifact);
    encode_stack_effects(&mut out, artifact);
    encode_root_maps(&mut out, artifact);
    encode_block_signatures(&mut out, artifact);
    encode_closures(&mut out, artifact);
    encode_meta(&mut out, artifact);
    encode_manifest(&mut out, artifact);
    encode_imports(&mut out, artifact);
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
        push_u16(
            out,
            u16::try_from(entry.param_tys.len()).expect("too many procedure parameter types"),
        );
        for ty in entry.param_tys.iter().copied() {
            push_u32(out, ty.raw());
        }
        push_u16(
            out,
            u16::try_from(entry.local_tys.len()).expect("too many procedure local types"),
        );
        for ty in entry.local_tys.iter().copied() {
            push_u32(out, ty.raw());
        }
        push_u16(
            out,
            u16::try_from(entry.result_tys.len()).expect("too many procedure result types"),
        );
        for ty in entry.result_tys.iter().copied() {
            push_u32(out, ty.raw());
        }
        push_u16(out, entry.entry_label);
        push_u32(out, entry.bytecode_body);
        match entry.block_signature_table {
            Some(id) => {
                out.push(1);
                push_u32(out, id.raw());
            }
            None => out.push(0),
        }
        match entry.root_map_table {
            Some(id) => {
                out.push(1);
                push_u32(out, id.raw());
            }
            None => out.push(0),
        }
        push_u16(
            out,
            u16::try_from(entry.domain_requirements.len())
                .expect("too many procedure domain requirements"),
        );
        for domain in entry.domain_requirements.iter().copied() {
            push_u32(out, domain.raw());
        }
        out.push(entry.calling_convention.wire_code());
        out.push(entry.visibility.wire_code());
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

fn encode_shapes(out: &mut Vec<u8>, artifact: &Artifact) {
    push_section_tag(out, SectionTag::Shapes);
    push_u32(
        out,
        u32::try_from(artifact.shapes.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.shapes.iter() {
        push_u32(out, entry.name.raw());
        match entry.payload_ty {
            Some(id) => {
                out.push(1);
                push_u32(out, id.raw());
            }
            None => out.push(0),
        }
        match entry.witness {
            Some(id) => {
                out.push(1);
                push_u32(out, id.raw());
            }
            None => out.push(0),
        }
        match entry.dispatch_table {
            Some(id) => {
                out.push(1);
                push_u32(out, id.raw());
            }
            None => out.push(0),
        }
        match entry.layout_identity {
            Some(id) => {
                out.push(1);
                push_u32(out, id.raw());
            }
            None => out.push(0),
        }
        out.push(u8::from(entry.root_visible));
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
        if let Some(domain) = entry.domain {
            out.push(1);
            push_u32(out, domain.raw());
        } else {
            out.push(0);
        }
        push_u16(
            out,
            u16::try_from(entry.pinned_params.len()).expect("too many foreign pinned params"),
        );
        for index in entry.pinned_params.iter().copied() {
            push_u16(out, index);
        }
        push_u16(
            out,
            u16::try_from(entry.nullable_params.len()).expect("too many foreign nullable params"),
        );
        for index in entry.nullable_params.iter().copied() {
            push_u16(out, index);
        }
        out.push(u8::from(entry.behavior.nullable_result));
        if let Some(lifetime) = entry.lifetime {
            out.push(1);
            push_u32(out, lifetime.raw());
        } else {
            out.push(0);
        }
        out.push(u8::from(entry.behavior.export));
        out.push(u8::from(entry.behavior.hot));
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
            ExportTarget::Shape(id) => {
                out.push(4);
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
        encode_data_entry(out, entry);
    }
}

fn encode_data_entry(out: &mut Vec<u8>, entry: &DataDescriptor) {
    push_u32(out, entry.name.raw());
    push_u32(out, entry.variant_count);
    push_u32(out, entry.field_count);
    push_u32(
        out,
        u32::try_from(entry.variants.len()).expect("data variant overflow"),
    );
    for variant in &entry.variants {
        encode_data_variant(out, variant);
    }
    push_optional_idx(out, entry.repr_kind);
    push_optional_u32(out, entry.layout_align);
    push_optional_u32(out, entry.layout_pack);
    out.push(u8::from(entry.frozen));
    encode_object_header(out, entry.object_header.as_ref());
}

fn encode_data_variant(out: &mut Vec<u8>, variant: &DataVariantDescriptor) {
    push_u32(out, variant.name.raw());
    push_i64(out, variant.tag);
    push_u32(
        out,
        u32::try_from(variant.field_tys.len()).expect("data field overflow"),
    );
    for ty in &variant.field_tys {
        push_u32(out, ty.raw());
    }
    push_u32(
        out,
        u32::try_from(variant.layout_fields.len()).expect("data layout field overflow"),
    );
    for field in &variant.layout_fields {
        encode_data_field(out, field);
    }
    out.push(u8::from(variant.public));
    out.push(u8::from(variant.hidden));
}

fn encode_data_field(out: &mut Vec<u8>, field: &DataFieldDescriptor) {
    push_optional_idx(out, field.name);
    push_u32(out, field.ty.raw());
    push_u32(out, field.logical_index);
    push_optional_u32(out, field.offset);
    push_optional_idx(out, field.storage);
    out.push(u8::from(field.mutability.mutable));
    out.push(u8::from(field.mutability.gc_pointer));
    out.push(u8::from(field.visibility.public));
    out.push(u8::from(field.visibility.hidden));
}

fn encode_object_header(out: &mut Vec<u8>, header: Option<&ObjectHeaderDescriptor>) {
    match header {
        Some(header) => {
            out.push(1);
            push_optional_idx(out, header.layout_ty);
            out.push(header.mark_bits);
            out.push(header.generation_bits);
            out.push(u8::from(header.shape_flags.pinned));
            out.push(u8::from(header.shape_flags.remembered));
            out.push(u8::from(header.shape_flags.large));
            out.push(u8::from(header.runtime_flags.weak_capable));
            out.push(u8::from(header.runtime_flags.forwarding));
            out.push(u8::from(header.runtime_flags.size_field));
        }
        None => out.push(0),
    }
}

fn push_optional_idx<T>(out: &mut Vec<u8>, id: Option<Idx<T>>) {
    match id {
        Some(id) => {
            out.push(1);
            push_u32(out, id.raw());
        }
        None => out.push(0),
    }
}

fn push_optional_u32(out: &mut Vec<u8>, value: Option<u32>) {
    match value {
        Some(value) => {
            out.push(1);
            push_u32(out, value);
        }
        None => out.push(0),
    }
}

fn encode_stack_effects(out: &mut Vec<u8>, artifact: &Artifact) {
    if artifact.stack_effects.is_empty() {
        return;
    }
    push_section_tag(out, SectionTag::StackEffects);
    push_u32(
        out,
        u32::try_from(artifact.stack_effects.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.stack_effects.iter() {
        push_u32(out, entry.name.raw());
        push_u16(
            out,
            u16::try_from(entry.input_tys.len()).expect("too many stack-effect input types"),
        );
        for ty in entry.input_tys.iter().copied() {
            push_u32(out, ty.raw());
        }
        push_u16(
            out,
            u16::try_from(entry.output_tys.len()).expect("too many stack-effect output types"),
        );
        for ty in entry.output_tys.iter().copied() {
            push_u32(out, ty.raw());
        }
    }
}

fn encode_root_maps(out: &mut Vec<u8>, artifact: &Artifact) {
    push_section_tag(out, SectionTag::RootMaps);
    push_u32(
        out,
        u32::try_from(artifact.root_maps.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.root_maps.iter() {
        push_u32(out, entry.safe_point.raw());
        out.push(entry.kind.wire_code());
        if let Some(procedure) = entry.procedure {
            out.push(1);
            push_u32(out, procedure.raw());
        } else {
            out.push(0);
        }
        push_u16(
            out,
            u16::try_from(entry.local_slots.len()).expect("too many local root slots"),
        );
        for slot in entry.local_slots.iter().copied() {
            push_u16(out, slot);
        }
        push_u16(
            out,
            u16::try_from(entry.stack_slots.len()).expect("too many stack root slots"),
        );
        for slot in entry.stack_slots.iter().copied() {
            push_u16(out, slot);
        }
        push_u16(
            out,
            u16::try_from(entry.capture_slots.len()).expect("too many capture root slots"),
        );
        for slot in entry.capture_slots.iter().copied() {
            push_u16(out, slot);
        }
        push_u16(
            out,
            u16::try_from(entry.defer_slots.len()).expect("too many defer root slots"),
        );
        for slot in entry.defer_slots.iter().copied() {
            push_u16(out, slot);
        }
        push_u16(
            out,
            u16::try_from(entry.pin_slots.len()).expect("too many pin root slots"),
        );
        for slot in entry.pin_slots.iter().copied() {
            push_u16(out, slot);
        }
    }
}

fn encode_block_signatures(out: &mut Vec<u8>, artifact: &Artifact) {
    if artifact.block_signatures.is_empty() {
        return;
    }
    push_section_tag(out, SectionTag::BlockSignatures);
    push_u32(
        out,
        u32::try_from(artifact.block_signatures.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.block_signatures.iter() {
        push_u32(out, entry.procedure.raw());
        push_u16(out, entry.label);
        push_u16(
            out,
            u16::try_from(entry.incoming_tys.len())
                .expect("too many block-signature incoming types"),
        );
        for ty in entry.incoming_tys.iter().copied() {
            push_u32(out, ty.raw());
        }
    }
}

fn encode_closures(out: &mut Vec<u8>, artifact: &Artifact) {
    if artifact.closures.is_empty() {
        return;
    }
    push_section_tag(out, SectionTag::Closures);
    push_u32(
        out,
        u32::try_from(artifact.closures.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.closures.iter() {
        push_u32(out, entry.name.raw());
        push_u32(out, entry.procedure.raw());
        push_u16(out, entry.capture_count);
        push_u16(
            out,
            u16::try_from(entry.capture_tys.len()).expect("too many closure capture types"),
        );
        for ty in entry.capture_tys.iter().copied() {
            push_u32(out, ty.raw());
        }
        match entry.env_layout {
            Some(id) => {
                out.push(1);
                push_u32(out, id.raw());
            }
            None => out.push(0),
        }
        push_u16(
            out,
            u16::try_from(entry.param_tys.len()).expect("too many closure parameter types"),
        );
        for ty in entry.param_tys.iter().copied() {
            push_u32(out, ty.raw());
        }
        push_u16(
            out,
            u16::try_from(entry.result_tys.len()).expect("too many closure result types"),
        );
        for ty in entry.result_tys.iter().copied() {
            push_u32(out, ty.raw());
        }
        match entry.domain {
            Some(id) => {
                out.push(1);
                push_u32(out, id.raw());
            }
            None => out.push(0),
        }
        match entry.effect {
            Some(id) => {
                out.push(1);
                push_u32(out, id.raw());
            }
            None => out.push(0),
        }
        out.push(u8::from(entry.suspending));
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

fn encode_manifest(out: &mut Vec<u8>, artifact: &Artifact) {
    if artifact.manifest.is_empty() {
        return;
    }
    push_section_tag(out, SectionTag::Manifest);
    push_u32(
        out,
        u32::try_from(artifact.manifest.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.manifest.iter() {
        push_u32(out, entry.package.raw());
        push_u32(out, entry.version.raw());
        push_u32(out, entry.profile.raw());
        if let Some(id) = entry.entry {
            out.push(1);
            push_u32(out, id.raw());
        } else {
            out.push(0);
        }
    }
}

fn encode_imports(out: &mut Vec<u8>, artifact: &Artifact) {
    if artifact.imports.is_empty() {
        return;
    }
    push_section_tag(out, SectionTag::Imports);
    push_u32(
        out,
        u32::try_from(artifact.imports.len()).expect("section overflow"),
    );
    for (_, entry) in artifact.imports.iter() {
        push_u32(out, entry.spec.raw());
        push_u32(out, entry.resolved.raw());
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
