use std::collections::HashMap;

use crate::descriptor::{
    ClassDescriptor, ConstantDescriptor, ConstantValue, DataDescriptor, DataVariantDescriptor,
    EffectDescriptor, EffectOpDescriptor, ExportDescriptor, ExportTarget, ForeignDescriptor,
    GlobalDescriptor, MetaDescriptor, MethodDescriptor, TypeDescriptor,
};
use crate::{
    Artifact, ClassId, CodeEntry, ConstantId, DataId, EffectId, ExportId, ForeignId, GlobalId,
    Instruction, Label, MethodId, Opcode, Operand, OperandShape, StringId, TypeId,
};
use crate::{AssemblyError, AssemblyResult};
use music_term::SyntaxShape;

type LabelIdMap = HashMap<String, u16>;

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

#[must_use]
pub fn format_text(artifact: &Artifact) -> String {
    let mut out = String::new();

    format_types(&mut out, artifact);
    format_data(&mut out, artifact);
    format_constants(&mut out, artifact);
    format_effects(&mut out, artifact);
    format_classes(&mut out, artifact);
    format_foreigns(&mut out, artifact);
    format_globals(&mut out, artifact);
    format_exports(&mut out, artifact);
    format_meta(&mut out, artifact);
    format_methods(&mut out, artifact);

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
        }
        out.push('\n');
    }
}

fn format_classes(out: &mut String, artifact: &Artifact) {
    for (_, descriptor) in artifact.classes.iter() {
        out.push_str(".class ");
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

fn format_methods(out: &mut String, artifact: &Artifact) {
    for (_, method) in artifact.methods.iter() {
        out.push_str(".method ");
        push_symbol_ref(out, artifact.string_text(method.name));
        out.push_str(" params ");
        out.push_str(&method.params.to_string());
        out.push_str(" locals ");
        out.push_str(&method.locals.to_string());
        if method.export {
            out.push_str(" export");
        }
        out.push('\n');
        for entry in &method.code {
            match entry {
                CodeEntry::Label(label) => {
                    out.push_str(artifact.string_text(method.labels[usize::from(label.id)]));
                    out.push_str(":\n");
                }
                CodeEntry::Instruction(instruction) => {
                    out.push_str("  ");
                    out.push_str(instruction.opcode.mnemonic());
                    if !matches!(instruction.operand, Operand::None) {
                        out.push(' ');
                        format_operand(out, artifact, method, &instruction.operand);
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
        out.push_str(".foreign ");
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
        if let Some(method) = descriptor.initializer {
            out.push(' ');
            push_symbol_ref(out, artifact.string_text(artifact.methods.get(method).name));
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
            ExportTarget::Method(_) => out.push_str("method"),
            ExportTarget::Global(_) => out.push_str("global"),
            ExportTarget::Foreign(_) => out.push_str("foreign"),
            ExportTarget::Type(_) => out.push_str("type"),
            ExportTarget::Effect(_) => out.push_str("effect"),
            ExportTarget::Class(_) => out.push_str("class"),
        }
        if descriptor.opaque {
            out.push_str(" opaque");
        }
        out.push('\n');
    }
}

/// Parses SEAM text into a validated artifact model.
///
/// # Errors
///
/// Returns [`AssemblyError`] if directives, operands, labels, references, or the final artifact
/// structure are invalid.
pub fn parse_text(text: &str) -> AssemblyResult<Artifact> {
    let mut builder = TextBuilder::new();
    let lines = text.lines().map(str::trim).collect::<Vec<_>>();
    let mut index = 0usize;

    while let Some(line) = lines.get(index).copied() {
        index = index.saturating_add(1);
        if line.is_empty() {
            continue;
        }
        if line == ".end" {
            return Err(AssemblyError::TextParseFailed("unexpected `.end`".into()));
        }
        if line.starts_with(".method ") {
            let method_lines = collect_method_lines(&lines, &mut index)?;
            builder.parse_method(line, &method_lines)?;
            continue;
        }
        builder.parse_directive(line)?;
    }

    let artifact = builder.finish();
    artifact.validate()?;
    Ok(artifact)
}

/// Validates SEAM text by parsing it and checking the resulting artifact.
///
/// # Errors
///
/// Returns [`AssemblyError`] if parsing or artifact validation fails.
pub fn validate_text(text: &str) -> AssemblyResult {
    let _ = parse_text(text)?;
    Ok(())
}

fn collect_method_lines<'text>(
    lines: &'text [&'text str],
    index: &mut usize,
) -> AssemblyResult<Vec<&'text str>> {
    let start = *index;
    while let Some(line) = lines.get(*index).copied() {
        *index = index.saturating_add(1);
        if line == ".end" {
            return Ok(lines[start..index.saturating_sub(1)].to_vec());
        }
    }
    Err(AssemblyError::TextParseFailed(
        "unterminated `.method` block".into(),
    ))
}

fn format_operand(
    out: &mut String,
    artifact: &Artifact,
    method: &MethodDescriptor,
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
        Operand::Method(id) => {
            push_symbol_ref(out, artifact.string_text(artifact.methods.get(*id).name));
        }
        Operand::WideMethodCaptures {
            method: id,
            captures,
        } => {
            push_symbol_ref(out, artifact.string_text(artifact.methods.get(*id).name));
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
            out.push_str(artifact.string_text(method.labels[usize::from(*id)]));
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
                out.push_str(artifact.string_text(method.labels[usize::from(label)]));
            }
        }
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

struct TextBuilder {
    artifact: Artifact,
    types: HashMap<String, TypeId>,
    constants: HashMap<String, ConstantId>,
    globals: HashMap<String, GlobalId>,
    methods: HashMap<String, MethodId>,
    effects: HashMap<String, EffectId>,
    classes: HashMap<String, ClassId>,
    foreigns: HashMap<String, ForeignId>,
    exports: HashMap<String, ExportId>,
    data: HashMap<String, DataId>,
    strings: HashMap<String, StringId>,
}

impl TextBuilder {
    fn new() -> Self {
        Self {
            artifact: Artifact::new(),
            types: HashMap::new(),
            constants: HashMap::new(),
            globals: HashMap::new(),
            methods: HashMap::new(),
            effects: HashMap::new(),
            classes: HashMap::new(),
            foreigns: HashMap::new(),
            exports: HashMap::new(),
            data: HashMap::new(),
            strings: HashMap::new(),
        }
    }

    fn finish(self) -> Artifact {
        self.artifact
    }

    fn parse_directive(&mut self, line: &str) -> AssemblyResult {
        let parts = tokenize(line)?;
        let Some(head) = parts.first() else {
            return Ok(());
        };
        match head.as_str() {
            ".type" => self.parse_type(&parts),
            ".data" => self.parse_data(&parts),
            ".const" => self.parse_const(&parts),
            ".global" => self.parse_global(&parts),
            ".effect" => self.parse_effect(&parts),
            ".class" => self.parse_class(&parts),
            ".foreign" => self.parse_foreign(&parts),
            ".export" => self.parse_export(&parts),
            ".meta" => self.parse_meta(&parts),
            other => Err(AssemblyError::TextParseFailed(format!(
                "unknown directive {other}"
            ))),
        }
    }

    fn parse_type(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() != 4 || parts.get(2).map(String::as_str) != Some("term") {
            return Err(AssemblyError::TextParseFailed(
                "expected `.type $Name term \"...\"`".into(),
            ));
        }
        let name = parse_symbol(must_get(parts.get(1), "type name")?)?;
        let term = parse_quoted(must_get(parts.get(3), "type term")?)?;
        let _ = self.ensure_type_symbol(&name, &term);
        Ok(())
    }

    fn ensure_type_symbol(&mut self, name: &str, term: &str) -> TypeId {
        if let Some(id) = self.types.get(name).copied() {
            return id;
        }
        let name_id = self.intern_string(name);
        let term_id = self.intern_string(term);
        let ty = self
            .artifact
            .types
            .alloc(TypeDescriptor::new(name_id, term_id));
        let _ = self.types.insert(name.into(), ty);
        ty
    }

    fn parse_data(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 6 {
            return Err(AssemblyError::TextParseFailed(
                "expected `.data $Name variants <count> fields <count> ...`".into(),
            ));
        }
        let name = parse_symbol(must_get(parts.get(1), "data name")?)?;
        if self.data.contains_key(&name) {
            return Err(AssemblyError::TextParseFailed("duplicate data".into()));
        }
        if must_get(parts.get(2), "variants keyword")? != "variants" {
            return Err(AssemblyError::TextParseFailed(
                "expected `.data $Name variants <count> fields <count> ...`".into(),
            ));
        }
        let variant_count: u32 = must_get(parts.get(3), "variant count")?
            .parse()
            .map_err(|_| AssemblyError::TextParseFailed("invalid variant count".into()))?;
        if must_get(parts.get(4), "fields keyword")? != "fields" {
            return Err(AssemblyError::TextParseFailed(
                "expected `.data $Name variants <count> fields <count> ...`".into(),
            ));
        }
        let field_count: u32 = must_get(parts.get(5), "field count")?
            .parse()
            .map_err(|_| AssemblyError::TextParseFailed("invalid field count".into()))?;

        let mut variants = Vec::<DataVariantDescriptor>::new();
        let mut repr_kind: Option<StringId> = None;
        let mut layout_align: Option<u32> = None;
        let mut layout_pack: Option<u32> = None;
        let mut idx = 6usize;
        while let Some(key) = parts.get(idx).map(String::as_str) {
            match key {
                "variant" => {
                    let value = must_get(parts.get(idx + 1), "variant name")?;
                    let variant_name = self.intern_string(&parse_symbol(value)?);
                    idx = idx.saturating_add(2);
                    let mut field_tys = Vec::new();
                    while parts.get(idx).map(String::as_str) == Some("field") {
                        let field_value = must_get(parts.get(idx + 1), "field type")?;
                        let field_name = parse_symbol(field_value)?;
                        field_tys.push(self.ensure_type_symbol(&field_name, &field_name));
                        idx = idx.saturating_add(2);
                    }
                    variants.push(DataVariantDescriptor::new(
                        variant_name,
                        field_tys.into_boxed_slice(),
                    ));
                    continue;
                }
                "repr" => {
                    let value = must_get(parts.get(idx + 1), "data metadata value")?;
                    repr_kind = Some(self.intern_string(&parse_quoted(value)?));
                }
                "align" => {
                    let value = must_get(parts.get(idx + 1), "data metadata value")?;
                    layout_align = Some(
                        value
                            .parse()
                            .map_err(|_| AssemblyError::TextParseFailed("invalid align".into()))?,
                    );
                }
                "pack" => {
                    let value = must_get(parts.get(idx + 1), "data metadata value")?;
                    layout_pack = Some(
                        value
                            .parse()
                            .map_err(|_| AssemblyError::TextParseFailed("invalid pack".into()))?,
                    );
                }
                _ => {
                    return Err(AssemblyError::TextParseFailed(
                        "unknown data metadata".into(),
                    ));
                }
            }
            idx = idx.saturating_add(2);
        }

        let name_id = self.intern_string(&name);
        let mut descriptor = DataDescriptor::new(name_id, variants.into_boxed_slice());
        if descriptor.variant_count != variant_count || descriptor.field_count != field_count {
            return Err(AssemblyError::TextParseFailed(
                "data counts disagree with variants".into(),
            ));
        }
        if let Some(repr_kind) = repr_kind {
            descriptor = descriptor.with_repr_kind(repr_kind);
        }
        if let Some(layout_align) = layout_align {
            descriptor = descriptor.with_layout_align(layout_align);
        }
        if let Some(layout_pack) = layout_pack {
            descriptor = descriptor.with_layout_pack(layout_pack);
        }
        let id = self.artifact.data.alloc(descriptor);
        let _ = self.data.insert(name, id);
        Ok(())
    }

    fn parse_const(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 4 {
            return Err(AssemblyError::TextParseFailed(
                "expected `.const $Name <kind> <value>`".into(),
            ));
        }
        let name = parse_symbol(must_get(parts.get(1), "constant name")?)?;
        let name_id = self.intern_string(&name);
        let kind = must_get(parts.get(2), "constant kind")?;
        let raw_value = must_get(parts.get(3), "constant value")?;
        let value =
            match kind {
                "int" => ConstantValue::Int(raw_value.parse().map_err(|_| {
                    AssemblyError::TextParseFailed("invalid integer constant".into())
                })?),
                "float" => ConstantValue::Float(raw_value.parse().map_err(|_| {
                    AssemblyError::TextParseFailed("invalid float constant".into())
                })?),
                "bool" => ConstantValue::Bool(match raw_value {
                    "true" => true,
                    "false" => false,
                    _ => {
                        return Err(AssemblyError::TextParseFailed(
                            "invalid bool constant".into(),
                        ));
                    }
                }),
                "string" => ConstantValue::String(self.intern_string(&parse_quoted(raw_value)?)),
                "syntax" => {
                    let shape = match must_get(parts.get(3), "syntax shape")? {
                        "expr" => SyntaxShape::Expr,
                        "module" => SyntaxShape::Module,
                        _ => {
                            return Err(AssemblyError::TextParseFailed(
                                "invalid syntax constant shape".into(),
                            ));
                        }
                    };
                    let text = parse_quoted(must_get(parts.get(4), "syntax value")?)?;
                    ConstantValue::Syntax {
                        shape,
                        text: self.intern_string(&text),
                    }
                }
                _ => {
                    return Err(AssemblyError::TextParseFailed(
                        "unknown constant kind".into(),
                    ));
                }
            };
        let id = self
            .artifact
            .constants
            .alloc(ConstantDescriptor::new(name_id, value));
        let _ = self.constants.insert(name, id);
        Ok(())
    }

    fn parse_global(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 2 {
            return Err(AssemblyError::TextParseFailed(
                "expected `.global $Name ...`".into(),
            ));
        }
        let name = parse_symbol(&parts[1])?;
        let mut export = false;
        let mut initializer = None;
        for part in parts.iter().skip(2) {
            if part == "export" {
                export = true;
            } else {
                let method_name = parse_symbol(part)?;
                initializer = Some(self.ensure_method_symbol(&method_name));
            }
        }
        let id = self.ensure_global_symbol(&name);
        let descriptor = self.artifact.globals.get_mut(id);
        descriptor.export = export;
        descriptor.initializer = initializer;
        Ok(())
    }

    fn parse_effect(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 2 {
            return Err(AssemblyError::TextParseFailed(
                "expected `.effect $Name ...`".into(),
            ));
        }
        let name = parse_symbol(&parts[1])?;
        let name_id = self.intern_string(&name);
        let mut ops = Vec::<EffectOpDescriptor>::new();
        let mut idx = 2;
        while idx < parts.len() {
            let op_name = parse_symbol(must_get(parts.get(idx), "effect op")?)?;
            idx += 1;
            let mut param_tys = Vec::new();
            while idx < parts.len() && parts[idx] == "param" {
                let ty = parse_symbol(must_get(parts.get(idx + 1), "effect op param type")?)?;
                param_tys.push(self.ensure_type_symbol(&ty, &ty));
                idx += 2;
            }
            if parts.get(idx).map(String::as_str) != Some("result") {
                return Err(AssemblyError::TextParseFailed(
                    "expected `result $Type` in `.effect`".into(),
                ));
            }
            let result_ty = parse_symbol(must_get(parts.get(idx + 1), "effect op result type")?)?;
            idx += 2;
            ops.push(EffectOpDescriptor::new(
                self.intern_string(&op_name),
                param_tys.into_boxed_slice(),
                self.ensure_type_symbol(&result_ty, &result_ty),
            ));
        }
        let id = self
            .artifact
            .effects
            .alloc(EffectDescriptor::new(name_id, ops.into_boxed_slice()));
        let _ = self.effects.insert(name, id);
        Ok(())
    }

    fn parse_class(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() != 2 {
            return Err(AssemblyError::TextParseFailed(
                "expected `.class $Name`".into(),
            ));
        }
        let name = parse_symbol(&parts[1])?;
        let name_id = self.intern_string(&name);
        let id = self.artifact.classes.alloc(ClassDescriptor::new(name_id));
        let _ = self.classes.insert(name, id);
        Ok(())
    }

    fn parse_meta_value(token: &str) -> AssemblyResult<String> {
        if token.starts_with('$') {
            parse_symbol(token)
        } else {
            parse_quoted(token)
        }
    }

    fn parse_meta(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 3 {
            return Err(AssemblyError::TextParseFailed(
                "expected `.meta $Target $Key ...`".into(),
            ));
        }
        let target = parse_symbol(must_get(parts.get(1), "meta target")?)?;
        let key = parse_symbol(must_get(parts.get(2), "meta key")?)?;
        let target_id = self.intern_string(&target);
        let key_id = self.intern_string(&key);
        let mut values = Vec::new();
        for token in parts.iter().skip(3) {
            let value = Self::parse_meta_value(token)?;
            values.push(self.intern_string(&value));
        }
        let _ = self.artifact.meta.alloc(MetaDescriptor::new(
            target_id,
            key_id,
            values.into_boxed_slice(),
        ));
        Ok(())
    }

    fn parse_foreign(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 6 {
            return Err(AssemblyError::TextParseFailed(
                "expected `.foreign $Name [param $Type ...] result $Type abi \"c\" symbol \"puts\" [link \"c\"] [export]`".into(),
            ));
        }
        let mut export = false;
        let mut link = None::<String>;
        let mut param_tys = Vec::new();
        let mut base = 2;
        while parts.get(base).map(String::as_str) == Some("param") {
            let ty = parse_symbol(must_get(parts.get(base + 1), "foreign param type")?)?;
            param_tys.push(self.ensure_type_symbol(&ty, &ty));
            base += 2;
        }
        if parts.get(base).map(String::as_str) != Some("result")
            || parts.get(base + 2).map(String::as_str) != Some("abi")
            || parts.get(base + 4).map(String::as_str) != Some("symbol")
        {
            return Err(AssemblyError::TextParseFailed(
                "expected `.foreign $Name [param $Type ...] result $Type abi \"c\" symbol \"puts\" [link \"c\"] [export]`".into(),
            ));
        }
        let result_ty = parse_symbol(must_get(parts.get(base + 1), "foreign result type")?)?;
        let abi = parse_quoted(must_get(parts.get(base + 3), "foreign abi")?)?;
        let symbol = parse_quoted(must_get(parts.get(base + 5), "foreign symbol")?)?;
        let mut idx = base + 6;
        while idx < parts.len() {
            match parts[idx].as_str() {
                "export" => {
                    export = true;
                    idx += 1;
                }
                "link" => {
                    let value = must_get(parts.get(idx + 1), "foreign link")?;
                    link = Some(parse_quoted(value)?);
                    idx += 2;
                }
                _ => {
                    return Err(AssemblyError::TextParseFailed(
                        "expected `.foreign $Name [param $Type ...] result $Type abi \"c\" symbol \"puts\" [link \"c\"] [export]`".into(),
                    ));
                }
            }
        }
        let name = parse_symbol(must_get(parts.get(1), "foreign name")?)?;
        let mut descriptor = ForeignDescriptor::new(
            self.intern_string(&name),
            param_tys.into_boxed_slice(),
            self.ensure_type_symbol(&result_ty, &result_ty),
            self.intern_string(&abi),
            self.intern_string(&symbol),
        )
        .with_export(export);
        if let Some(link) = link.as_deref().map(|text| self.intern_string(text)) {
            descriptor = descriptor.with_link(link);
        }
        let id = self.artifact.foreigns.alloc(descriptor);
        let _ = self.foreigns.insert(name, id);
        Ok(())
    }

    fn parse_export(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 3 {
            return Err(AssemblyError::TextParseFailed(
                "expected `.export $Name <method|global|foreign|type|effect|class> [opaque]`"
                    .into(),
            ));
        }
        let name = parse_symbol(must_get(parts.get(1), "export name")?)?;
        let kind = must_get(parts.get(2), "export kind")?;
        let opaque = parts.iter().skip(3).any(|part| part == "opaque");
        if self.exports.contains_key(&name) {
            return Err(AssemblyError::TextParseFailed("duplicate export".into()));
        }
        let name_id = self.intern_string(&name);
        let target =
            match kind {
                "method" => ExportTarget::Method(self.ensure_method_symbol(&name)),
                "global" => ExportTarget::Global(self.ensure_global_symbol(&name)),
                "foreign" => {
                    let foreign = *self.foreigns.get(&name).ok_or_else(|| {
                        AssemblyError::TextParseFailed(format!("unknown foreign ${name}"))
                    })?;
                    ExportTarget::Foreign(foreign)
                }
                "type" => {
                    let ty = *self.types.get(&name).ok_or_else(|| {
                        AssemblyError::TextParseFailed(format!("unknown type ${name}"))
                    })?;
                    ExportTarget::Type(ty)
                }
                "effect" => {
                    let effect = *self.effects.get(&name).ok_or_else(|| {
                        AssemblyError::TextParseFailed(format!("unknown effect ${name}"))
                    })?;
                    ExportTarget::Effect(effect)
                }
                "class" => {
                    let class = *self.classes.get(&name).ok_or_else(|| {
                        AssemblyError::TextParseFailed(format!("unknown class ${name}"))
                    })?;
                    ExportTarget::Class(class)
                }
                _ => return Err(AssemblyError::TextParseFailed(
                    "expected `.export $Name <method|global|foreign|type|effect|class> [opaque]`"
                        .into(),
                )),
            };
        let id = self
            .artifact
            .exports
            .alloc(ExportDescriptor::new(name_id, opaque, target));
        let _ = self.exports.insert(name, id);
        Ok(())
    }

    fn parse_method(&mut self, header: &str, lines: &[&str]) -> AssemblyResult {
        let parts = tokenize(header)?;
        if parts.len() < 4 {
            return Err(AssemblyError::TextParseFailed(
                "expected `.method $Name [params <count>] locals <count> [export]`".into(),
            ));
        }
        let name = parse_symbol(&parts[1])?;
        let mut params = 0_u16;
        let mut idx = 2;
        if parts.get(idx).map(String::as_str) == Some("params") {
            params = must_get(parts.get(idx + 1), "method params")?
                .parse()
                .map_err(|_| AssemblyError::TextParseFailed("invalid params count".into()))?;
            idx += 2;
        }
        if parts.get(idx).map(String::as_str) != Some("locals") {
            return Err(AssemblyError::TextParseFailed(
                "expected `.method $Name [params <count>] locals <count> [export]`".into(),
            ));
        }
        let locals = must_get(parts.get(idx + 1), "locals count")?
            .parse()
            .map_err(|_| AssemblyError::TextParseFailed("invalid locals count".into()))?;
        let export = parts.iter().skip(idx + 2).any(|part| part == "export");

        let mut labels = Vec::<StringId>::new();
        let mut label_ids = HashMap::<String, u16>::new();
        let mut code = Vec::<CodeEntry>::new();
        for raw_line in lines {
            if raw_line.is_empty() {
                continue;
            }
            if let Some(label_name) = raw_line.strip_suffix(':') {
                let label_id = ensure_label(
                    &mut self.artifact,
                    &mut labels,
                    &mut label_ids,
                    label_name.to_owned(),
                )?;
                code.push(CodeEntry::Label(Label { id: label_id }));
                continue;
            }
            let entry = self.parse_instruction(raw_line, &mut labels, &mut label_ids)?;
            code.push(CodeEntry::Instruction(entry));
        }

        let method = MethodDescriptor::new(
            self.intern_string(&name),
            params,
            locals,
            code.into_boxed_slice(),
        )
        .with_export(export)
        .with_labels(labels.into_boxed_slice());
        let id = self.ensure_method_symbol(&name);
        *self.artifact.methods.get_mut(id) = method;
        Ok(())
    }

    fn parse_instruction(
        &mut self,
        line: &str,
        labels: &mut Vec<StringId>,
        label_ids: &mut LabelIdMap,
    ) -> AssemblyResult<Instruction> {
        let parts = tokenize(line)?;
        let Some(opcode_text) = parts.first() else {
            return Err(AssemblyError::TextParseFailed("empty instruction".into()));
        };
        let Some(opcode) = Opcode::from_mnemonic(opcode_text) else {
            return Err(AssemblyError::TextParseFailed(format!(
                "unknown opcode {opcode_text}"
            )));
        };
        let operand = self.parse_operand(opcode.operand_shape(), &parts, labels, label_ids)?;
        Ok(Instruction::new(opcode, operand))
    }

    fn parse_operand(
        &mut self,
        shape: OperandShape,
        parts: &[String],
        labels: &mut Vec<StringId>,
        label_ids: &mut LabelIdMap,
    ) -> AssemblyResult<Operand> {
        match shape {
            OperandShape::None => Ok(Operand::None),
            OperandShape::I16 => Self::parse_i16_operand(parts),
            OperandShape::Local => Ok(Operand::Local(parse_local(parts.get(1))?)),
            OperandShape::String => self.parse_string_operand(parts),
            OperandShape::Type => self.parse_type_operand(parts),
            OperandShape::Constant => self.parse_constant_operand(parts),
            OperandShape::Global => self.parse_global_operand(parts),
            OperandShape::Method => self.parse_method_operand(parts),
            OperandShape::WideMethodCaptures => self.parse_wide_method_captures_operand(parts),
            OperandShape::Foreign => self.parse_foreign_operand(parts),
            OperandShape::Effect => self.parse_effect_operand(parts),
            OperandShape::EffectId => self.parse_effect_id_operand(parts),
            OperandShape::Label => self.parse_label_operand(parts, labels, label_ids),
            OperandShape::TypeLen => self.parse_type_len_operand(parts),
            OperandShape::BranchTable => self.parse_branch_table_operand(parts, labels, label_ids),
        }
    }

    fn parse_i16_operand(parts: &[String]) -> AssemblyResult<Operand> {
        Ok(Operand::I16(
            must_get(parts.get(1), "i16 operand")?
                .parse()
                .map_err(|_| AssemblyError::TextParseFailed("invalid i16 operand".into()))?,
        ))
    }

    fn parse_string_operand(&mut self, parts: &[String]) -> AssemblyResult<Operand> {
        Ok(Operand::String(self.intern_string(&parse_quoted(
            must_get(parts.get(1), "string")?,
        )?)))
    }

    fn parse_type_operand(&self, parts: &[String]) -> AssemblyResult<Operand> {
        let name = parse_symbol(must_get(parts.get(1), "type")?)?;
        let ty = *self
            .types
            .get(&name)
            .ok_or_else(|| AssemblyError::TextParseFailed(format!("unknown type ${name}")))?;
        Ok(Operand::Type(ty))
    }

    fn parse_constant_operand(&self, parts: &[String]) -> AssemblyResult<Operand> {
        let name = parse_symbol(must_get(parts.get(1), "constant")?)?;
        let constant = *self
            .constants
            .get(&name)
            .ok_or_else(|| AssemblyError::TextParseFailed(format!("unknown constant ${name}")))?;
        Ok(Operand::Constant(constant))
    }

    fn parse_global_operand(&mut self, parts: &[String]) -> AssemblyResult<Operand> {
        let name = parse_symbol(must_get(parts.get(1), "global")?)?;
        Ok(Operand::Global(self.ensure_global_symbol(&name)))
    }

    fn ensure_global_symbol(&mut self, name: &str) -> GlobalId {
        if let Some(id) = self.globals.get(name).copied() {
            return id;
        }
        let name_id = self.intern_string(name);
        let id = self.artifact.globals.alloc(GlobalDescriptor::new(name_id));
        let _ = self.globals.insert(name.into(), id);
        id
    }

    fn ensure_method_symbol(&mut self, name: &str) -> MethodId {
        if let Some(id) = self.methods.get(name).copied() {
            return id;
        }
        let name_id = self.intern_string(name);
        let id = self
            .artifact
            .methods
            .alloc(MethodDescriptor::new(name_id, 0, 0, Box::new([])));
        let _ = self.methods.insert(name.into(), id);
        id
    }

    fn parse_method_operand(&mut self, parts: &[String]) -> AssemblyResult<Operand> {
        let name = parse_symbol(must_get(parts.get(1), "method")?)?;
        Ok(Operand::Method(self.ensure_method_symbol(&name)))
    }

    fn parse_wide_method_captures_operand(&mut self, parts: &[String]) -> AssemblyResult<Operand> {
        let name = parse_symbol(must_get(parts.get(1), "method")?)?;
        let method = self.ensure_method_symbol(&name);
        let captures = must_get(parts.get(2), "capture count")?
            .parse::<u8>()
            .map_err(|_| AssemblyError::TextParseFailed("invalid capture count".into()))?;
        Ok(Operand::WideMethodCaptures { method, captures })
    }

    fn parse_foreign_operand(&self, parts: &[String]) -> AssemblyResult<Operand> {
        let name = parse_symbol(must_get(parts.get(1), "foreign")?)?;
        let foreign = *self
            .foreigns
            .get(&name)
            .ok_or_else(|| AssemblyError::TextParseFailed(format!("unknown foreign ${name}")))?;
        Ok(Operand::Foreign(foreign))
    }

    fn parse_effect_operand(&self, parts: &[String]) -> AssemblyResult<Operand> {
        let effect_name = parse_symbol(must_get(parts.get(1), "effect")?)?;
        let op_name = parse_symbol(must_get(parts.get(2), "effect op")?)?;
        let effect_id = *self.effects.get(&effect_name).ok_or_else(|| {
            AssemblyError::TextParseFailed(format!("unknown effect ${effect_name}"))
        })?;
        let effect = self.artifact.effects.get(effect_id);
        let op = effect
            .ops
            .iter()
            .position(|candidate| self.artifact.string_text(candidate.name) == op_name)
            .ok_or_else(|| {
                AssemblyError::TextParseFailed(format!("unknown effect op ${op_name}"))
            })?;
        Ok(Operand::Effect {
            effect: effect_id,
            op: u16::try_from(op)
                .map_err(|_| AssemblyError::TextParseFailed("effect op index overflow".into()))?,
        })
    }

    fn parse_effect_id_operand(&self, parts: &[String]) -> AssemblyResult<Operand> {
        let effect_name = parse_symbol(must_get(parts.get(1), "effect")?)?;
        let effect_id = *self.effects.get(&effect_name).ok_or_else(|| {
            AssemblyError::TextParseFailed(format!("unknown effect ${effect_name}"))
        })?;
        Ok(Operand::EffectId(effect_id))
    }

    fn parse_label_operand(
        &mut self,
        parts: &[String],
        labels: &mut Vec<StringId>,
        label_ids: &mut LabelIdMap,
    ) -> AssemblyResult<Operand> {
        let label_name = must_get(parts.get(1), "label")?.to_owned();
        Ok(Operand::Label(ensure_label(
            &mut self.artifact,
            labels,
            label_ids,
            label_name,
        )?))
    }

    fn parse_type_len_operand(&self, parts: &[String]) -> AssemblyResult<Operand> {
        let type_name = parse_symbol(must_get(parts.get(1), "type")?)?;
        let ty = *self
            .types
            .get(&type_name)
            .ok_or_else(|| AssemblyError::TextParseFailed(format!("unknown type ${type_name}")))?;
        let len = must_get(parts.get(2), "length")?
            .parse()
            .map_err(|_| AssemblyError::TextParseFailed("invalid sequence length".into()))?;
        Ok(Operand::TypeLen { ty, len })
    }

    fn parse_branch_table_operand(
        &mut self,
        parts: &[String],
        labels: &mut Vec<StringId>,
        label_ids: &mut LabelIdMap,
    ) -> AssemblyResult<Operand> {
        let joined = parts.iter().skip(1).cloned().collect::<Vec<_>>().join(" ");
        let labels = joined
            .split(',')
            .map(str::trim)
            .filter(|entry| !entry.is_empty())
            .map(|entry| ensure_label(&mut self.artifact, labels, label_ids, entry.to_owned()))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Operand::BranchTable(labels.into_boxed_slice()))
    }

    fn intern_string(&mut self, text: &str) -> StringId {
        if let Some(id) = self.strings.get(text).copied() {
            return id;
        }
        let id = self.artifact.intern_string(text);
        let _ = self.strings.insert(text.to_owned(), id);
        id
    }
}

fn ensure_label(
    artifact: &mut Artifact,
    labels: &mut Vec<StringId>,
    label_ids: &mut LabelIdMap,
    name: String,
) -> AssemblyResult<u16> {
    if let Some(id) = label_ids.get(&name).copied() {
        return Ok(id);
    }
    let name_id = artifact.intern_string(&name);
    let id = u16::try_from(labels.len())
        .map_err(|_| AssemblyError::TextParseFailed("too many labels".into()))?;
    labels.push(name_id);
    let _ = label_ids.insert(name, id);
    Ok(id)
}

fn parse_symbol(token: &str) -> AssemblyResult<String> {
    let body = token.strip_prefix('$').ok_or_else(|| {
        AssemblyError::TextParseFailed(format!("expected symbolic name, got `{token}`"))
    })?;
    if body.starts_with('"') {
        parse_quoted(body)
    } else {
        Ok(body.to_owned())
    }
}

fn parse_local(token: Option<&String>) -> AssemblyResult<u16> {
    let token = must_get(token, "local")?;
    token
        .strip_prefix('%')
        .ok_or_else(|| AssemblyError::TextParseFailed("expected local slot like `%0`".into()))?
        .parse()
        .map_err(|_| AssemblyError::TextParseFailed("invalid local slot".into()))
}

fn parse_quoted(token: &str) -> AssemblyResult<String> {
    let Some(body) = token
        .strip_prefix('"')
        .and_then(|rest| rest.strip_suffix('"'))
    else {
        return Err(AssemblyError::TextParseFailed(format!(
            "expected quoted string, got `{token}`"
        )));
    };
    Ok(body.replace("\\\"", "\"").replace("\\\\", "\\"))
}

fn must_get<'a>(token: Option<&'a String>, name: &str) -> AssemblyResult<&'a str> {
    token
        .map(String::as_str)
        .ok_or_else(|| AssemblyError::TextParseFailed(format!("missing {name} operand")))
}

fn tokenize(line: &str) -> AssemblyResult<Vec<String>> {
    let mut tokens = Vec::new();
    let mut current = String::new();
    let mut chars = line.chars().peekable();
    let mut in_string = false;

    while let Some(ch) = chars.next() {
        if in_string {
            current.push(ch);
            if ch == '"' && !current.ends_with("\\\"") {
                in_string = false;
                tokens.push(current.clone());
                current.clear();
            }
            continue;
        }
        match ch {
            '"' => {
                if !current.is_empty() {
                    tokens.push(current.clone());
                    current.clear();
                }
                current.push(ch);
                in_string = true;
            }
            '$' if matches!(chars.peek().copied(), Some('"')) => {
                if !current.is_empty() {
                    tokens.push(current.clone());
                    current.clear();
                }
                current.push('$');
                current.push('"');
                let _ = chars.next();
                in_string = true;
            }
            ' ' | '\t' => {
                if !current.is_empty() {
                    tokens.push(current.clone());
                    current.clear();
                }
            }
            _ => current.push(ch),
        }
    }

    if in_string {
        return Err(AssemblyError::TextParseFailed(
            "unterminated string literal".into(),
        ));
    }
    if !current.is_empty() {
        tokens.push(current);
    }
    Ok(tokens)
}
