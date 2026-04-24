use super::*;

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
pub fn format_text(artifact: &Artifact) -> String {
    let mut out = String::new();

    format_types(&mut out, artifact);
    format_data(&mut out, artifact);
    format_constants(&mut out, artifact);
    format_effects(&mut out, artifact);
    format_shapes(&mut out, artifact);
    format_foreigns(&mut out, artifact);
    format_globals(&mut out, artifact);
    format_exports(&mut out, artifact);
    format_meta(&mut out, artifact);
    format_procedures(&mut out, artifact);

    out
}

#[must_use]
pub fn format_hil_projection(artifact: &Artifact) -> String {
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
            out.push_str(&index.to_string());
            out.push_str(": _");
        }
        out.push_str(") -> _");
        if procedure.hot {
            out.push_str(" @hot");
        }
        if procedure.cold {
            out.push_str(" @cold");
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

fn format_types(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.types.iter() {
        out.push_str(".type ");
        push_symbol_ref(out, artifact.string_text(descriptor.name));
        out.push_str(" term ");
        push_quoted(out, artifact.string_text(descriptor.term));
        out.push('\n');
    }
}

fn format_data(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.data.iter() {
        out.push_str(".data ");
        push_symbol_ref(out, artifact.string_text(descriptor.name));
        out.push_str(" variants ");
        out.push_str(&descriptor.variant_count.to_string());
        out.push_str(" fields ");
        out.push_str(&descriptor.field_count.to_string());
        for variant in &descriptor.variants {
            out.push_str(" variant ");
            push_symbol_ref(out, artifact.string_text(variant.name));
            out.push_str(" tag ");
            out.push_str(&variant.tag.to_string());
            for ty in &variant.field_tys {
                out.push_str(" field ");
                push_symbol_ref(out, artifact.type_name(*ty));
            }
        }
        if let Some(repr) = descriptor.repr_kind {
            out.push_str(" repr ");
            push_quoted(out, artifact.string_text(repr));
        }
        if let Some(align) = descriptor.layout_align {
            out.push_str(" align ");
            out.push_str(&align.to_string());
        }
        if let Some(pack) = descriptor.layout_pack {
            out.push_str(" pack ");
            out.push_str(&pack.to_string());
        }
        if descriptor.frozen {
            out.push_str(" frozen");
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
                out.push_str(&value.to_string());
            }
            ConstantValue::Float(value) => {
                out.push_str(" float ");
                out.push_str(&value.to_string());
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

fn format_effects(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.effects.iter() {
        out.push_str(".effect ");
        push_symbol_ref(out, artifact.string_text(descriptor.name));
        for op in &descriptor.ops {
            out.push(' ');
            push_symbol_ref(out, artifact.string_text(op.name));
            for ty in &op.param_tys {
                out.push_str(" param ");
                push_symbol_ref(out, artifact.type_name(*ty));
            }
            out.push_str(" result ");
            push_symbol_ref(out, artifact.type_name(op.result_ty));
            if op.is_comptime_safe {
                out.push_str(" comptime-safe");
            }
        }
        out.push('\n');
    }
}

fn format_shapes(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.shapes.iter() {
        out.push_str(".capability ");
        push_symbol_ref(out, artifact.string_text(descriptor.name));
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

fn format_procedures(out: &mut String, artifact: &Artifact) {
    for (_, procedure) in artifact.procedures.iter() {
        out.push_str(".procedure ");
        push_symbol_ref(out, artifact.string_text(procedure.name));
        out.push_str(" params ");
        out.push_str(&procedure.params.to_string());
        out.push_str(" locals ");
        out.push_str(&procedure.locals.to_string());
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
        if descriptor.export {
            out.push_str(" export");
        }
        if descriptor.hot {
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
            ExportTarget::Effect(_) => out.push_str("effect"),
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
        Operand::I16(value) => out.push_str(&value.to_string()),
        Operand::Local(slot) => {
            out.push('%');
            out.push_str(&slot.to_string());
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
            out.push_str(&captures.to_string());
        }
        Operand::Foreign(id) => {
            push_symbol_ref(out, artifact.string_text(artifact.foreigns.get(*id).name));
        }
        Operand::Effect { effect, op } => {
            let effect = artifact.effects.get(*effect);
            push_symbol_ref(out, artifact.string_text(effect.name));
            out.push(' ');
            push_symbol_ref(out, artifact.string_text(effect.ops[usize::from(*op)].name));
        }
        Operand::EffectId(effect) => {
            let effect = artifact.effects.get(*effect);
            push_symbol_ref(out, artifact.string_text(effect.name));
        }
        Operand::Label(id) => {
            out.push_str(artifact.string_text(procedure.labels[usize::from(*id)]));
        }
        Operand::TypeLen { ty, len } => {
            push_symbol_ref(out, artifact.string_text(artifact.types.get(*ty).name));
            out.push(' ');
            out.push_str(&len.to_string());
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
