use super::*;
use std::collections::HashMap;
use std::fmt::Write;

fn symbol_needs_quote(text: &str) -> bool {
    text.chars().any(char::is_whitespace) || text.contains('"') || text.contains('\\')
}

fn push_symbol_ref(out: &mut String, text: &str) {
    out.push('$');
    if symbol_needs_quote(text) {
        push_quoted(out, text);
    } else {
        out.push_str(text);
    }
}

fn push_quoted(out: &mut String, text: &str) {
    out.push('"');
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            _ => out.push(ch),
        }
    }
    out.push('"');
}

#[must_use]
pub fn format_disasm(artifact: &Artifact) -> String {
    let mut out = String::new();

    format_types(&mut out, artifact);
    format_stack_effects(&mut out, artifact);
    format_data(&mut out, artifact);
    format_closures(&mut out, artifact);
    format_constants(&mut out, artifact);
    format_shapes(&mut out, artifact);
    format_foreigns(&mut out, artifact);
    format_globals(&mut out, artifact);
    format_exports(&mut out, artifact);
    format_meta(&mut out, artifact);
    format_manifest(&mut out, artifact);
    format_imports(&mut out, artifact);
    format_procedures(&mut out, artifact);
    format_root_maps(&mut out, artifact);
    format_block_signatures(&mut out, artifact);

    out
}

#[must_use]
pub fn format_debug_hil(artifact: &Artifact) -> String {
    let mut out = String::new();

    out.push_str("module seam.projection {\n");
    for (_, descriptor) in artifact.types.iter() {
        out.push_str("  type ");
        out.push_str(artifact.string_text(descriptor.name));
        out.push_str(" = ");
        push_quoted(&mut out, artifact.string_text(descriptor.term));
        out.push('\n');
    }
    for (_, descriptor) in artifact.data.iter() {
        out.push_str("  data ");
        out.push_str(artifact.string_text(descriptor.name));
        out.push_str(" {\n");
        for variant in &descriptor.variants {
            out.push_str("    .");
            out.push_str(artifact.string_text(variant.name));
            out.push('(');
            for (index, ty) in variant.field_tys.iter().enumerate() {
                if index != 0 {
                    out.push_str(", ");
                }
                out.push_str(artifact.type_name(*ty));
            }
            out.push_str(")\n");
        }
        out.push_str("  }\n");
    }
    for (_, procedure) in artifact.procedures.iter() {
        out.push_str("  fn ");
        out.push_str(artifact.string_text(procedure.name));
        out.push('(');
        for index in 0..procedure.params {
            if index != 0 {
                out.push_str(", ");
            }
            out.push('%');
            write!(out, "{index}").expect("write to string");
            out.push_str(": _");
        }
        out.push_str(") -> _");
        if procedure.hot {
            out.push_str(" @profile(level := .hot)");
        }
        if procedure.cold {
            out.push_str(" @profile(level := .cold)");
        }
        out.push_str(" {\n");
        out.push_str("  entry:\n");
        out.push_str("    seam {\n");
        for entry in &procedure.code {
            match entry {
                CodeEntry::Label(label) => {
                    out.push_str("      ");
                    out.push_str(artifact.string_text(procedure.labels[usize::from(label.id)]));
                    out.push_str(":\n");
                }
                CodeEntry::Instruction(instruction) => {
                    out.push_str("      ");
                    out.push_str(instruction.opcode.mnemonic());
                    if !matches!(instruction.operand, Operand::None) {
                        out.push(' ');
                        format_operand(&mut out, artifact, procedure, &instruction.operand);
                    }
                    out.push('\n');
                }
            }
        }
        out.push_str("    }\n");
        out.push_str("  }\n");
    }
    out.push_str("}\n");
    out
}

#[must_use]
pub fn format_decomp(artifact: &Artifact) -> String {
    let mut out = String::new();
    let name_policy = DecompNamePolicy::new(artifact);

    out.push_str("module seam.projection {\n");
    for (_, descriptor) in artifact.types.iter() {
        out.push_str("  type ");
        out.push_str(artifact.string_text(descriptor.name));
        out.push_str(" = ");
        push_quoted(&mut out, artifact.string_text(descriptor.term));
        out.push('\n');
    }
    for (data_id, descriptor) in artifact.data.iter() {
        out.push_str("  data ");
        out.push_str(name_policy.data_name(data_id));
        out.push_str(" {\n");
        for (variant_index, variant) in descriptor.variants.iter().enumerate() {
            out.push_str("    .");
            out.push_str(&name_policy.variant_name(data_id, variant_index, variant));
            out.push('(');
            for (index, ty) in variant.field_tys.iter().enumerate() {
                if index != 0 {
                    out.push_str(", ");
                }
                out.push_str(artifact.type_name(*ty));
            }
            out.push_str(")\n");
        }
        out.push_str("  }\n");
    }
    for (procedure_id, procedure) in artifact.procedures.iter() {
        out.push_str("  fn ");
        out.push_str(name_policy.procedure_name(procedure_id));
        out.push('(');
        for index in 0..procedure.params {
            if index != 0 {
                out.push_str(", ");
            }
            out.push('%');
            write!(out, "{index}").expect("write to string");
            out.push_str(": _");
        }
        out.push_str(") -> _");
        if procedure.hot {
            out.push_str(" @profile(level := .hot)");
        }
        if procedure.cold {
            out.push_str(" @profile(level := .cold)");
        }
        out.push_str(" {\n");
        out.push_str("  entry:\n");
        out.push_str("    seam {\n");
        for entry in &procedure.code {
            match entry {
                CodeEntry::Label(label) => {
                    out.push_str("      ");
                    out.push_str(artifact.string_text(procedure.labels[usize::from(label.id)]));
                    out.push_str(":\n");
                }
                CodeEntry::Instruction(instruction) => {
                    out.push_str("      ");
                    out.push_str(instruction.opcode.mnemonic());
                    if !matches!(instruction.operand, Operand::None) {
                        out.push(' ');
                        format_operand(&mut out, artifact, procedure, &instruction.operand);
                    }
                    out.push('\n');
                }
            }
        }
        out.push_str("    }\n");
        out.push_str("  }\n");
    }
    out.push_str("}\n");
    out
}

struct DecompNamePolicy<'artifact> {
    artifact: &'artifact Artifact,
    data_names: HashMap<DataId, String>,
    procedure_names: HashMap<ProcedureId, String>,
}

impl<'artifact> DecompNamePolicy<'artifact> {
    fn new(artifact: &'artifact Artifact) -> Self {
        let mut data_names = HashMap::new();
        let mut private_data = 0usize;
        for (data_id, descriptor) in artifact.data.iter() {
            let preserve = exported_type_name(artifact, descriptor.name).is_some();
            let name = if preserve {
                artifact.string_text(descriptor.name).to_owned()
            } else {
                let name = format!("__t{private_data}");
                private_data += 1;
                name
            };
            let _ = data_names.insert(data_id, name);
        }

        let mut procedure_names = HashMap::new();
        let mut private_procedures = 0usize;
        for (procedure_id, procedure) in artifact.procedures.iter() {
            let preserve = procedure.visibility != ProcedureVisibility::Private
                || procedure.export
                || exported_procedure_name(artifact, procedure_id).is_some();
            let name = if preserve {
                artifact.string_text(procedure.name).to_owned()
            } else {
                let name = format!("__f{private_procedures}");
                private_procedures += 1;
                name
            };
            let _ = procedure_names.insert(procedure_id, name);
        }

        Self {
            artifact,
            data_names,
            procedure_names,
        }
    }

    fn data_name(&self, data_id: DataId) -> &str {
        self.data_names.get(&data_id).map_or("__t", String::as_str)
    }

    fn procedure_name(&self, procedure_id: ProcedureId) -> &str {
        self.procedure_names
            .get(&procedure_id)
            .map_or("__f", String::as_str)
    }

    fn variant_name(
        &self,
        _data_id: DataId,
        variant_index: usize,
        variant: &DataVariantDescriptor,
    ) -> String {
        if variant.public && !variant.hidden {
            self.artifact.string_text(variant.name).to_owned()
        } else {
            format!("__v{variant_index}")
        }
    }
}

fn exported_type_name(artifact: &Artifact, name: StringId) -> Option<StringId> {
    artifact.exports.iter().find_map(|(_, descriptor)| {
        let ExportTarget::Type(ty) = descriptor.target else {
            return None;
        };
        (artifact.types.get(ty).name == name).then_some(descriptor.name)
    })
}

fn exported_procedure_name(artifact: &Artifact, procedure_id: ProcedureId) -> Option<StringId> {
    artifact.exports.iter().find_map(|(_, descriptor)| {
        (descriptor.target == ExportTarget::Procedure(procedure_id)).then_some(descriptor.name)
    })
}

fn format_types(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.types.iter() {
        out.push_str(".type ");
        push_symbol_ref(out, artifact.string_text(descriptor.name));
        out.push_str(" term ");
        push_quoted(out, artifact.string_text(descriptor.term));
        out.push('\n');
    }
}

fn format_stack_effects(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.stack_effects.iter() {
        out.push_str(".stack_effect ");
        push_symbol_ref(out, artifact.string_text(descriptor.name));
        out.push_str(" input");
        for ty in descriptor.input_tys.iter().copied() {
            out.push(' ');
            push_symbol_ref(out, artifact.type_name(ty));
        }
        out.push_str(" output");
        for ty in descriptor.output_tys.iter().copied() {
            out.push(' ');
            push_symbol_ref(out, artifact.type_name(ty));
        }
        out.push('\n');
    }
}

fn format_data(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.data.iter() {
        out.push_str(".data ");
        push_symbol_ref(out, artifact.string_text(descriptor.name));
        out.push_str(" variants ");
        write!(out, "{}", descriptor.variant_count).expect("write to string");
        out.push_str(" fields ");
        write!(out, "{}", descriptor.field_count).expect("write to string");
        for variant in &descriptor.variants {
            out.push_str(" variant ");
            push_symbol_ref(out, artifact.string_text(variant.name));
            out.push_str(" tag ");
            write!(out, "{}", variant.tag).expect("write to string");
            if variant.public {
                out.push_str(" public");
            }
            if variant.hidden {
                out.push_str(" hidden");
            }
            for ty in &variant.field_tys {
                out.push_str(" field ");
                push_symbol_ref(out, artifact.type_name(*ty));
            }
            for field in &variant.layout_fields {
                out.push_str(" layout_field");
                if let Some(name) = field.name {
                    out.push_str(" name ");
                    push_symbol_ref(out, artifact.string_text(name));
                }
                out.push_str(" type ");
                push_symbol_ref(out, artifact.type_name(field.ty));
                out.push_str(" index ");
                write!(out, "{}", field.logical_index).expect("write to string");
                if let Some(offset) = field.offset {
                    out.push_str(" offset ");
                    write!(out, "{offset}").expect("write to string");
                }
                if let Some(storage) = field.storage {
                    out.push_str(" storage ");
                    push_quoted(out, artifact.string_text(storage));
                }
                if field.mutability.mutable {
                    out.push_str(" mut");
                }
                if field.mutability.gc_pointer {
                    out.push_str(" gc");
                }
                if field.visibility.public {
                    out.push_str(" public");
                }
                if field.visibility.hidden {
                    out.push_str(" hidden");
                }
            }
        }
        if let Some(repr) = descriptor.repr_kind {
            out.push_str(" repr ");
            push_quoted(out, artifact.string_text(repr));
        }
        if let Some(align) = descriptor.layout_align {
            out.push_str(" align ");
            write!(out, "{align}").expect("write to string");
        }
        if let Some(pack) = descriptor.layout_pack {
            out.push_str(" pack ");
            write!(out, "{pack}").expect("write to string");
        }
        if descriptor.frozen {
            out.push_str(" frozen");
        }
        if let Some(header) = &descriptor.object_header {
            out.push_str(" header");
            if let Some(layout_ty) = header.layout_ty {
                out.push_str(" layout ");
                push_symbol_ref(out, artifact.type_name(layout_ty));
            }
            out.push_str(" mark_bits ");
            write!(out, "{}", header.mark_bits).expect("write to string");
            out.push_str(" generation_bits ");
            write!(out, "{}", header.generation_bits).expect("write to string");
            if header.shape_flags.pinned {
                out.push_str(" pinned");
            }
            if header.shape_flags.remembered {
                out.push_str(" remembered");
            }
            if header.shape_flags.large {
                out.push_str(" large");
            }
            if header.runtime_flags.weak_capable {
                out.push_str(" weak_capable");
            }
            if header.runtime_flags.forwarding {
                out.push_str(" forwarding");
            }
            if header.runtime_flags.size_field {
                out.push_str(" size_field");
            }
        }
        out.push('\n');
    }
}

fn format_closures(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.closures.iter() {
        out.push_str(".closure ");
        push_symbol_ref(out, artifact.string_text(descriptor.name));
        out.push_str(" procedure ");
        push_symbol_ref(
            out,
            artifact.string_text(artifact.procedures.get(descriptor.procedure).name),
        );
        out.push_str(" captures ");
        write!(out, "{}", descriptor.capture_count).expect("write to string");
        for ty in descriptor.capture_tys.iter().copied() {
            out.push_str(" capture ");
            push_symbol_ref(out, artifact.type_name(ty));
        }
        if let Some(env_layout) = descriptor.env_layout {
            out.push_str(" env ");
            push_symbol_ref(
                out,
                artifact.string_text(artifact.data.get(env_layout).name),
            );
        }
        for ty in descriptor.param_tys.iter().copied() {
            out.push_str(" param ");
            push_symbol_ref(out, artifact.type_name(ty));
        }
        for ty in descriptor.result_tys.iter().copied() {
            out.push_str(" result ");
            push_symbol_ref(out, artifact.type_name(ty));
        }
        if let Some(domain) = descriptor.domain {
            out.push_str(" domain ");
            push_quoted(out, artifact.string_text(domain));
        }
        if let Some(effect) = descriptor.effect {
            out.push_str(" effect ");
            push_quoted(out, artifact.string_text(effect));
        }
        if descriptor.suspending {
            out.push_str(" suspend");
        }
        out.push('\n');
    }
}

fn format_constants(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.constants.iter() {
        out.push_str(".const ");
        push_symbol_ref(out, artifact.string_text(descriptor.name));
        match descriptor.value {
            ConstantValue::Int(value) => {
                out.push_str(" int ");
                write!(out, "{value}").expect("write to string");
            }
            ConstantValue::Float(value) => {
                out.push_str(" float ");
                write!(out, "{value}").expect("write to string");
            }
            ConstantValue::Bool(value) => {
                out.push_str(" bool ");
                out.push_str(if value { "true" } else { "false" });
            }
            ConstantValue::String(text) => {
                out.push_str(" string ");
                push_quoted(out, artifact.string_text(text));
            }
            ConstantValue::Syntax { shape, text } => {
                out.push_str(" syntax ");
                out.push_str(match shape {
                    SyntaxShape::Expr => "expr ",
                    SyntaxShape::Module => "module ",
                });
                push_quoted(out, artifact.string_text(text));
            }
        }
        out.push('\n');
    }
}

fn format_shapes(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.shapes.iter() {
        out.push_str(".capability ");
        push_symbol_ref(out, artifact.string_text(descriptor.name));
        if let Some(payload_ty) = descriptor.payload_ty {
            out.push_str(" payload ");
            push_symbol_ref(out, artifact.type_name(payload_ty));
        }
        if let Some(witness) = descriptor.witness {
            out.push_str(" witness ");
            push_symbol_ref(out, artifact.string_text(witness));
        }
        if let Some(dispatch_table) = descriptor.dispatch_table {
            out.push_str(" dispatch ");
            push_symbol_ref(out, artifact.string_text(dispatch_table));
        }
        if let Some(layout_identity) = descriptor.layout_identity {
            out.push_str(" layout ");
            push_symbol_ref(out, artifact.type_name(layout_identity));
        }
        if descriptor.root_visible {
            out.push_str(" root");
        }
        out.push('\n');
    }
}

fn format_meta(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.meta.iter() {
        out.push_str(".meta ");
        push_symbol_ref(out, artifact.string_text(descriptor.target));
        out.push(' ');
        push_symbol_ref(out, artifact.string_text(descriptor.key));
        for value in &descriptor.values {
            out.push(' ');
            push_symbol_ref(out, artifact.string_text(*value));
        }
        out.push('\n');
    }
}

fn format_manifest(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.manifest.iter() {
        out.push_str(".manifest package ");
        push_quoted(out, artifact.string_text(descriptor.package));
        out.push_str(" version ");
        push_quoted(out, artifact.string_text(descriptor.version));
        out.push_str(" profile ");
        push_quoted(out, artifact.string_text(descriptor.profile));
        if let Some(entry) = descriptor.entry {
            out.push_str(" entry ");
            push_symbol_ref(out, artifact.string_text(entry));
        }
        out.push('\n');
    }
}

fn format_imports(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.imports.iter() {
        out.push_str(".import spec ");
        push_quoted(out, artifact.string_text(descriptor.spec));
        out.push_str(" resolved ");
        push_symbol_ref(out, artifact.string_text(descriptor.resolved));
        out.push('\n');
    }
}

fn format_root_maps(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.root_maps.iter() {
        out.push_str(".root_map point ");
        push_symbol_ref(out, artifact.string_text(descriptor.safe_point));
        out.push_str(" kind ");
        out.push_str(descriptor.kind.as_str());
        if let Some(procedure) = descriptor.procedure {
            out.push_str(" procedure ");
            let procedure_name = artifact.procedures.get(procedure).name;
            push_symbol_ref(out, artifact.string_text(procedure_name));
        }
        for local_slot in &descriptor.local_slots {
            out.push_str(" local %");
            write!(out, "{local_slot}").expect("write to string");
        }
        for stack_slot in &descriptor.stack_slots {
            out.push_str(" stack %");
            write!(out, "{stack_slot}").expect("write to string");
        }
        for capture_slot in &descriptor.capture_slots {
            out.push_str(" capture %");
            write!(out, "{capture_slot}").expect("write to string");
        }
        for defer_slot in &descriptor.defer_slots {
            out.push_str(" defer %");
            write!(out, "{defer_slot}").expect("write to string");
        }
        for pin_slot in &descriptor.pin_slots {
            out.push_str(" pin %");
            write!(out, "{pin_slot}").expect("write to string");
        }
        out.push('\n');
    }
}

fn format_block_signatures(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.block_signatures.iter() {
        out.push_str(".block_sig procedure ");
        let procedure_name = artifact.procedures.get(descriptor.procedure).name;
        push_symbol_ref(out, artifact.string_text(procedure_name));
        out.push_str(" label ");
        let procedure = artifact.procedures.get(descriptor.procedure);
        let label_name = procedure.labels[usize::from(descriptor.label)];
        push_symbol_ref(out, artifact.string_text(label_name));
        out.push_str(" stack [");
        for (index, ty) in descriptor.incoming_tys.iter().copied().enumerate() {
            if index != 0 {
                out.push(' ');
            }
            push_symbol_ref(out, artifact.type_name(ty));
        }
        out.push_str("]\n");
    }
}

fn format_procedures(out: &mut String, artifact: &Artifact) {
    for (_, procedure) in artifact.procedures.iter() {
        out.push_str(".procedure ");
        push_symbol_ref(out, artifact.string_text(procedure.name));
        out.push_str(" params ");
        write!(out, "{}", procedure.params).expect("write to string");
        if !procedure.param_tys.is_empty() {
            out.push_str(" param_types [");
            for (index, ty) in procedure.param_tys.iter().copied().enumerate() {
                if index > 0 {
                    out.push(' ');
                }
                push_symbol_ref(out, artifact.type_name(ty));
            }
            out.push(']');
        }
        out.push_str(" locals ");
        write!(out, "{}", procedure.locals).expect("write to string");
        if !procedure.local_tys.is_empty() {
            out.push_str(" local_types [");
            for (index, ty) in procedure.local_tys.iter().copied().enumerate() {
                if index > 0 {
                    out.push(' ');
                }
                push_symbol_ref(out, artifact.type_name(ty));
            }
            out.push(']');
        }
        if !procedure.result_tys.is_empty() {
            out.push_str(" result [");
            for (index, ty) in procedure.result_tys.iter().copied().enumerate() {
                if index > 0 {
                    out.push(' ');
                }
                push_symbol_ref(out, artifact.type_name(ty));
            }
            out.push(']');
        }
        out.push_str(" entry ");
        write!(out, "{}", procedure.entry_label).expect("write to string");
        out.push_str(" body ");
        write!(out, "{}", procedure.bytecode_body).expect("write to string");
        if let Some(block_signature_table) = procedure.block_signature_table {
            out.push_str(" block_table ");
            write!(out, "{}", block_signature_table.raw()).expect("write to string");
        }
        if let Some(root_map_table) = procedure.root_map_table {
            out.push_str(" root_map ");
            write!(out, "{}", root_map_table.raw()).expect("write to string");
        }
        if !procedure.domain_requirements.is_empty() {
            out.push_str(" domains [");
            for (index, domain) in procedure.domain_requirements.iter().copied().enumerate() {
                if index > 0 {
                    out.push(' ');
                }
                push_quoted(out, artifact.string_text(domain));
            }
            out.push(']');
        }
        out.push_str(" callconv ");
        out.push_str(procedure.calling_convention.as_str());
        out.push_str(" visibility ");
        out.push_str(procedure.visibility.as_str());
        if procedure.export {
            out.push_str(" export");
        }
        if procedure.hot {
            out.push_str(" hot");
        }
        if procedure.cold {
            out.push_str(" cold");
        }
        out.push('\n');
        for entry in &procedure.code {
            match entry {
                CodeEntry::Label(label) => {
                    out.push_str(artifact.string_text(procedure.labels[usize::from(label.id)]));
                    out.push_str(":\n");
                }
                CodeEntry::Instruction(instruction) => {
                    out.push_str("  ");
                    out.push_str(instruction.opcode.mnemonic());
                    if !matches!(instruction.operand, Operand::None) {
                        out.push(' ');
                        format_operand(out, artifact, procedure, &instruction.operand);
                    }
                    out.push('\n');
                }
            }
        }
        out.push_str(".end\n");
    }
}

fn format_foreigns(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.foreigns.iter() {
        out.push_str(".native ");
        push_symbol_ref(out, artifact.string_text(descriptor.name));
        for ty in &descriptor.param_tys {
            out.push_str(" param ");
            push_symbol_ref(out, artifact.type_name(*ty));
        }
        out.push_str(" result ");
        push_symbol_ref(out, artifact.type_name(descriptor.result_ty));
        out.push_str(" abi ");
        push_quoted(out, artifact.string_text(descriptor.abi));
        out.push_str(" symbol ");
        push_quoted(out, artifact.string_text(descriptor.symbol));
        if let Some(link) = descriptor.link {
            out.push_str(" link ");
            push_quoted(out, artifact.string_text(link));
        }
        if let Some(domain) = descriptor.domain {
            out.push_str(" domain ");
            push_quoted(out, artifact.string_text(domain));
        }
        for index in &descriptor.pinned_params {
            out.push_str(" pin %");
            write!(out, "{index}").expect("write to string");
        }
        for index in &descriptor.nullable_params {
            out.push_str(" nullable %");
            write!(out, "{index}").expect("write to string");
        }
        if descriptor.behavior.nullable_result {
            out.push_str(" nullable_result");
        }
        if let Some(lifetime) = descriptor.lifetime {
            out.push_str(" lifetime ");
            push_quoted(out, artifact.string_text(lifetime));
        }
        if descriptor.behavior.export {
            out.push_str(" export");
        }
        if descriptor.behavior.hot {
            out.push_str(" hot");
        }
        if descriptor.cold {
            out.push_str(" cold");
        }
        out.push('\n');
    }
}

fn format_globals(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.globals.iter() {
        out.push_str(".global ");
        push_symbol_ref(out, artifact.string_text(descriptor.name));
        if descriptor.export {
            out.push_str(" export");
        }
        if let Some(procedure) = descriptor.initializer {
            out.push(' ');
            push_symbol_ref(
                out,
                artifact.string_text(artifact.procedures.get(procedure).name),
            );
        }
        out.push('\n');
    }
}

fn format_exports(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.exports.iter() {
        out.push_str(".export ");
        push_symbol_ref(out, artifact.string_text(descriptor.name));
        out.push(' ');
        match descriptor.target {
            ExportTarget::Procedure(_) => out.push_str("procedure"),
            ExportTarget::Global(_) => out.push_str("global"),
            ExportTarget::Foreign(_) => out.push_str("native"),
            ExportTarget::Type(_) => out.push_str("type"),
            ExportTarget::Shape(_) => out.push_str("capability"),
        }
        if descriptor.opaque {
            out.push_str(" opaque");
        }
        out.push('\n');
    }
}

fn format_operand(
    out: &mut String,
    artifact: &Artifact,
    procedure: &ProcedureDescriptor,
    operand: &Operand,
) {
    match operand {
        Operand::None => {}
        Operand::I16(value) => {
            write!(out, "{value}").expect("write to string");
        }
        Operand::Local(slot) => {
            out.push('%');
            write!(out, "{slot}").expect("write to string");
        }
        Operand::String(text) => push_quoted(out, artifact.string_text(*text)),
        Operand::Type(id) => {
            push_symbol_ref(out, artifact.string_text(artifact.types.get(*id).name));
        }
        Operand::Constant(id) => {
            push_symbol_ref(out, artifact.string_text(artifact.constants.get(*id).name));
        }
        Operand::Global(id) => {
            push_symbol_ref(out, artifact.string_text(artifact.globals.get(*id).name));
        }
        Operand::Procedure(id) => {
            push_symbol_ref(out, artifact.string_text(artifact.procedures.get(*id).name));
        }
        Operand::WideProcedureCaptures {
            procedure: id,
            captures,
        } => {
            push_symbol_ref(out, artifact.string_text(artifact.procedures.get(*id).name));
            out.push(' ');
            write!(out, "{captures}").expect("write to string");
        }
        Operand::Foreign(id) => {
            push_symbol_ref(out, artifact.string_text(artifact.foreigns.get(*id).name));
        }
        Operand::Label(id) => {
            out.push_str(artifact.string_text(procedure.labels[usize::from(*id)]));
        }
        Operand::TypeLen { ty, len } => {
            push_symbol_ref(out, artifact.string_text(artifact.types.get(*ty).name));
            out.push(' ');
            write!(out, "{len}").expect("write to string");
        }
        Operand::BranchTable(labels) => {
            for (idx, label) in labels.iter().copied().enumerate() {
                if idx != 0 {
                    out.push_str(", ");
                }
                out.push_str(artifact.string_text(procedure.labels[usize::from(label)]));
            }
        }
    }
}
