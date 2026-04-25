use super::symbols::{must_get, parse_meta_value, parse_quoted, parse_symbol};
use super::*;

impl TextBuilder {
    fn parse_data_variant(
        &mut self,
        parts: &[String],
        idx: &mut usize,
        implicit_tag: i64,
    ) -> AssemblyResult<DataVariantDescriptor> {
        let variant_token = must_get(parts.get(*idx + 1), "variant name")?;
        let variant_name = self.intern_string(&parse_symbol(variant_token)?);
        *idx = (*idx).saturating_add(2);
        let tag = if parts.get(*idx).map(String::as_str) == Some("tag") {
            let raw_tag = must_get(parts.get(*idx + 1), "variant tag")?;
            *idx = (*idx).saturating_add(2);
            raw_tag
                .parse()
                .map_err(|_| text_invalid_operand("variant tag", raw_tag))?
        } else {
            implicit_tag
        };
        let mut field_tys = Vec::new();
        while parts.get(*idx).map(String::as_str) == Some("field") {
            let field_value = must_get(parts.get(*idx + 1), "field type")?;
            let field_name = parse_symbol(field_value)?;
            field_tys.push(self.ensure_type_symbol(&field_name, &field_name));
            *idx = (*idx).saturating_add(2);
        }
        Ok(DataVariantDescriptor::new(
            variant_name,
            tag,
            field_tys.into_boxed_slice(),
        ))
    }

    fn parse_data_metadata(
        &mut self,
        parts: &[String],
        idx: &mut usize,
        repr_kind: &mut Option<StringId>,
        layout_align: &mut Option<u32>,
        layout_pack: &mut Option<u32>,
        frozen: &mut bool,
    ) -> AssemblyResult {
        match must_get(parts.get(*idx), "data metadata key")? {
            "repr" => {
                let repr_token = must_get(parts.get(*idx + 1), "data metadata value")?;
                *repr_kind = Some(self.intern_string(&parse_quoted(repr_token)?));
                *idx = (*idx).saturating_add(2);
            }
            "align" => {
                let align_token = must_get(parts.get(*idx + 1), "data metadata value")?;
                *layout_align = Some(
                    align_token
                        .parse()
                        .map_err(|_| text_invalid_operand("align", align_token))?,
                );
                *idx = (*idx).saturating_add(2);
            }
            "pack" => {
                let pack_token = must_get(parts.get(*idx + 1), "data metadata value")?;
                *layout_pack = Some(
                    pack_token
                        .parse()
                        .map_err(|_| text_invalid_operand("pack", pack_token))?,
                );
                *idx = (*idx).saturating_add(2);
            }
            "frozen" => {
                *frozen = true;
                *idx = (*idx).saturating_add(1);
            }
            _ => {
                return Err(text_unknown_symbol("data metadata", parts[*idx].as_str()));
            }
        }
        Ok(())
    }

    pub(crate) fn parse_type(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() != 4 || parts.get(2).map(String::as_str) != Some("term") {
            return Err(text_expected_form(r#".type $Name term "...""#));
        }
        let name = parse_symbol(must_get(parts.get(1), "type name")?)?;
        let term = parse_quoted(must_get(parts.get(3), "type term")?)?;
        let _ = self.ensure_type_symbol(&name, &term);
        Ok(())
    }

    pub(crate) fn parse_data(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 6 {
            return Err(text_expected_form(
                ".data $Name variants <count> fields <count> ...",
            ));
        }
        let name = parse_symbol(must_get(parts.get(1), "data name")?)?;
        if self.data.contains_key(&name) {
            return Err(text_duplicate_symbol("data", &name));
        }
        if must_get(parts.get(2), "variants keyword")? != "variants" {
            return Err(text_expected_form(
                ".data $Name variants <count> fields <count> ...",
            ));
        }
        let variant_count: u32 = must_get(parts.get(3), "variant count")?
            .parse()
            .map_err(|_| text_invalid_operand("variant count", parts[3].as_str()))?;
        if must_get(parts.get(4), "fields keyword")? != "fields" {
            return Err(text_expected_form(
                ".data $Name variants <count> fields <count> ...",
            ));
        }
        let field_count: u32 = must_get(parts.get(5), "field count")?
            .parse()
            .map_err(|_| text_invalid_operand("field count", parts[5].as_str()))?;

        let mut variants = Vec::<DataVariantDescriptor>::new();
        let mut repr_kind: Option<StringId> = None;
        let mut layout_align: Option<u32> = None;
        let mut layout_pack: Option<u32> = None;
        let mut frozen = false;
        let mut idx = 6usize;
        while idx < parts.len() {
            if parts[idx].as_str() == "variant" {
                variants.push(self.parse_data_variant(
                    parts,
                    &mut idx,
                    i64::try_from(variants.len()).unwrap_or(i64::MAX),
                )?);
                continue;
            }
            self.parse_data_metadata(
                parts,
                &mut idx,
                &mut repr_kind,
                &mut layout_align,
                &mut layout_pack,
                &mut frozen,
            )?;
        }

        let name_id = self.intern_string(&name);
        let mut descriptor = DataDescriptor::new(name_id, variants.into_boxed_slice());
        if descriptor.variant_count != variant_count || descriptor.field_count != field_count {
            return Err(text_count_mismatch(
                "data fields/variants",
                variant_count.saturating_add(field_count),
                descriptor
                    .variant_count
                    .saturating_add(descriptor.field_count),
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
        if frozen {
            descriptor = descriptor.with_frozen(true);
        }
        let id = self.artifact.data.alloc(descriptor);
        let _ = self.data.insert(name, id);
        Ok(())
    }

    pub(crate) fn parse_const(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 4 {
            return Err(text_expected_form(".const $Name <kind> <value>"));
        }
        let name = parse_symbol(must_get(parts.get(1), "constant name")?)?;
        let name_id = self.intern_string(&name);
        let kind = must_get(parts.get(2), "constant kind")?;
        let raw_value = must_get(parts.get(3), "constant value")?;
        let constant_value = match kind {
            "int" => ConstantValue::Int(
                raw_value
                    .parse()
                    .map_err(|_| text_invalid_operand("integer constant", raw_value))?,
            ),
            "float" => ConstantValue::Float(
                raw_value
                    .parse()
                    .map_err(|_| text_invalid_operand("float constant", raw_value))?,
            ),
            "bool" => ConstantValue::Bool(match raw_value {
                "true" => true,
                "false" => false,
                _ => {
                    return Err(text_invalid_operand("bool constant", raw_value));
                }
            }),
            "string" => ConstantValue::String(self.intern_string(&parse_quoted(raw_value)?)),
            "syntax" => {
                let shape = match must_get(parts.get(3), "syntax shape")? {
                    "expr" => SyntaxShape::Expr,
                    "module" => SyntaxShape::Module,
                    _ => {
                        return Err(text_invalid_operand("syntax constant shape", raw_value));
                    }
                };
                let text = parse_quoted(must_get(parts.get(4), "syntax value")?)?;
                ConstantValue::Syntax {
                    shape,
                    text: self.intern_string(&text),
                }
            }
            _ => {
                return Err(text_unknown_symbol("constant kind", kind));
            }
        };
        let id = self
            .artifact
            .constants
            .alloc(ConstantDescriptor::new(name_id, constant_value));
        let _ = self.constants.insert(name, id);
        Ok(())
    }

    pub(crate) fn parse_global(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 2 {
            return Err(text_expected_form(".global $Name ..."));
        }
        let name = parse_symbol(&parts[1])?;
        let mut export = false;
        let mut initializer = None;
        for part in parts.iter().skip(2) {
            if part == "export" {
                export = true;
            } else {
                let procedure_name = parse_symbol(part)?;
                initializer = Some(self.ensure_procedure_symbol(&procedure_name));
            }
        }
        let id = self.ensure_global_symbol(&name);
        let descriptor = self.artifact.globals.get_mut(id);
        descriptor.export = export;
        descriptor.initializer = initializer;
        Ok(())
    }

    pub(crate) fn parse_effect(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 2 {
            return Err(text_expected_form(".effect $Name ..."));
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
                return Err(text_expected_form("result $Type in .effect"));
            }
            let result_ty = parse_symbol(must_get(parts.get(idx + 1), "effect op result type")?)?;
            idx += 2;
            let is_comptime_safe = parts.get(idx).map(String::as_str) == Some("comptime-safe");
            if is_comptime_safe {
                idx += 1;
            }
            ops.push(
                EffectOpDescriptor::new(
                    self.intern_string(&op_name),
                    param_tys.into_boxed_slice(),
                    self.ensure_type_symbol(&result_ty, &result_ty),
                )
                .with_comptime_safe(is_comptime_safe),
            );
        }
        let id = self
            .artifact
            .effects
            .alloc(EffectDescriptor::new(name_id, ops.into_boxed_slice()));
        let _ = self.effects.insert(name, id);
        Ok(())
    }

    pub(crate) fn parse_capability(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() != 2 {
            return Err(text_expected_form(".capability $Name"));
        }
        let name = parse_symbol(&parts[1])?;
        let name_id = self.intern_string(&name);
        let id = self.artifact.shapes.alloc(ShapeDescriptor::new(name_id));
        let _ = self.shapes.insert(name, id);
        Ok(())
    }

    pub(crate) fn parse_meta(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 3 {
            return Err(text_expected_form(".meta $Target $Key ..."));
        }
        let target = parse_symbol(must_get(parts.get(1), "meta target")?)?;
        let key = parse_symbol(must_get(parts.get(2), "meta key")?)?;
        let target_id = self.intern_string(&target);
        let key_id = self.intern_string(&key);
        let mut values = Vec::new();
        for token in parts.iter().skip(3) {
            let meta_value = parse_meta_value(token)?;
            values.push(self.intern_string(&meta_value));
        }
        let _ = self.artifact.meta.alloc(MetaDescriptor::new(
            target_id,
            key_id,
            values.into_boxed_slice(),
        ));
        Ok(())
    }

    pub(crate) fn parse_foreign(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 6 {
            return Err(text_expected_form(
                r#".native $Name [param $Type ...] result $Type abi "c" symbol "puts" [link "c"] [export]"#,
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
            return Err(text_expected_form(
                r#".native $Name [param $Type ...] result $Type abi "c" symbol "puts" [link "c"] [export]"#,
            ));
        }
        let result_ty = parse_symbol(must_get(parts.get(base + 1), "foreign result type")?)?;
        let abi = parse_quoted(must_get(parts.get(base + 3), "foreign abi")?)?;
        let symbol = parse_quoted(must_get(parts.get(base + 5), "foreign symbol")?)?;
        let mut hot = false;
        let mut cold = false;
        let mut idx = base + 6;
        while idx < parts.len() {
            match parts[idx].as_str() {
                "export" => {
                    export = true;
                    idx += 1;
                }
                "hot" => {
                    hot = true;
                    idx += 1;
                }
                "cold" => {
                    cold = true;
                    idx += 1;
                }
                "link" => {
                    let link_token = must_get(parts.get(idx + 1), "foreign link")?;
                    link = Some(parse_quoted(link_token)?);
                    idx += 2;
                }
                _ => {
                    return Err(text_expected_form(
                        r#".native $Name [param $Type ...] result $Type abi "c" symbol "puts" [link "c"] [export]"#,
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
        .with_export(export)
        .with_hot(hot)
        .with_cold(cold);
        if let Some(link) = link.as_deref().map(|text| self.intern_string(text)) {
            descriptor = descriptor.with_link(link);
        }
        let id = self.artifact.foreigns.alloc(descriptor);
        let _ = self.foreigns.insert(name, id);
        Ok(())
    }

    pub(crate) fn parse_export(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 3 {
            return Err(text_expected_form(
                ".export $Name <procedure|global|native|type|effect|capability> [opaque]",
            ));
        }
        let name = parse_symbol(must_get(parts.get(1), "export name")?)?;
        let kind = must_get(parts.get(2), "export kind")?;
        let opaque = parts.iter().skip(3).any(|part| part == "opaque");
        if self.exports.contains_key(&name) {
            return Err(text_duplicate_symbol("export", &name));
        }
        let name_id = self.intern_string(&name);
        let target = match kind {
            "procedure" => ExportTarget::Procedure(self.ensure_procedure_symbol(&name)),
            "global" => ExportTarget::Global(self.ensure_global_symbol(&name)),
            "native" => {
                let foreign = *self
                    .foreigns
                    .get(&name)
                    .ok_or_else(|| text_unknown_symbol("native", &name))?;
                ExportTarget::Foreign(foreign)
            }
            "type" => {
                let ty = *self
                    .types
                    .get(&name)
                    .ok_or_else(|| text_unknown_symbol("type", &name))?;
                ExportTarget::Type(ty)
            }
            "effect" => {
                let effect = *self
                    .effects
                    .get(&name)
                    .ok_or_else(|| text_unknown_symbol("effect", &name))?;
                ExportTarget::Effect(effect)
            }
            "capability" => {
                let shape = *self
                    .shapes
                    .get(&name)
                    .ok_or_else(|| text_unknown_symbol("shape", &name))?;
                ExportTarget::Shape(shape)
            }
            _ => {
                return Err(text_expected_form(
                    ".export $Name <procedure|global|native|type|effect|capability> [opaque]",
                ));
            }
        };
        let id = self
            .artifact
            .exports
            .alloc(ExportDescriptor::new(name_id, opaque, target));
        let _ = self.exports.insert(name, id);
        Ok(())
    }
}
