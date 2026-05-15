use super::symbols::{must_get, parse_local, parse_meta_value, parse_quoted, parse_symbol};
use super::*;

#[derive(Default)]
struct DataMetadata {
    repr_kind: Option<StringId>,
    layout_align: Option<u32>,
    layout_pack: Option<u32>,
    frozen: bool,
    object_header: Option<ObjectHeaderDescriptor>,
}

#[derive(Default)]
struct ForeignOptions {
    profile: ForeignProfileFlags,
    link: Option<String>,
    domain: Option<String>,
    lifetime: Option<String>,
    pinned_params: Vec<u16>,
    nullable_params: Vec<u16>,
    nullable_result: bool,
}

#[derive(Default)]
struct ForeignProfileFlags {
    export: bool,
    hot: bool,
    cold: bool,
}

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
        let mut public = false;
        let mut hidden = false;
        loop {
            match parts.get(*idx).map(String::as_str) {
                Some("public") => {
                    public = true;
                    *idx = (*idx).saturating_add(1);
                }
                Some("hidden") => {
                    hidden = true;
                    *idx = (*idx).saturating_add(1);
                }
                _ => break,
            }
        }
        let mut field_tys = Vec::new();
        let mut layout_fields = Vec::new();
        while matches!(
            parts.get(*idx).map(String::as_str),
            Some("field" | "layout_field")
        ) {
            if parts.get(*idx).map(String::as_str) == Some("field") {
                let field_value = must_get(parts.get(*idx + 1), "field type")?;
                let field_name = parse_symbol(field_value)?;
                field_tys.push(self.ensure_type_symbol(&field_name, &field_name));
                *idx = (*idx).saturating_add(2);
                continue;
            }
            layout_fields.push(self.parse_data_layout_field(parts, idx)?);
        }
        Ok(DataVariantDescriptor::new(
            variant_name,
            tag,
            field_tys.into_boxed_slice(),
        ))
        .map(|variant| {
            variant
                .with_layout_fields(layout_fields.into_boxed_slice())
                .with_public(public)
                .with_hidden(hidden)
        })
    }

    fn parse_data_layout_field(
        &mut self,
        parts: &[String],
        idx: &mut usize,
    ) -> AssemblyResult<DataFieldDescriptor> {
        *idx = (*idx).saturating_add(1);
        let mut name = None;
        if parts.get(*idx).map(String::as_str) == Some("name") {
            let name_token = must_get(parts.get(*idx + 1), "field name")?;
            name = Some(self.intern_string(&parse_symbol(name_token)?));
            *idx = (*idx).saturating_add(2);
        }
        if parts.get(*idx).map(String::as_str) != Some("type") {
            return Err(text_expected_form(
                "layout_field [name $Name] type $Type index <n> [offset <n>] [storage \"...\"] [mut] [gc] [public] [hidden]",
            ));
        }
        let type_token = must_get(parts.get(*idx + 1), "field type")?;
        let type_name = parse_symbol(type_token)?;
        let ty = self.ensure_type_symbol(&type_name, &type_name);
        *idx = (*idx).saturating_add(2);
        if parts.get(*idx).map(String::as_str) != Some("index") {
            return Err(text_expected_form(
                "layout_field [name $Name] type $Type index <n> [offset <n>] [storage \"...\"] [mut] [gc] [public] [hidden]",
            ));
        }
        let index_token = must_get(parts.get(*idx + 1), "field logical index")?;
        let logical_index = index_token
            .parse()
            .map_err(|_| text_invalid_operand("field logical index", index_token))?;
        *idx = (*idx).saturating_add(2);
        let mut field = DataFieldDescriptor::new(ty, logical_index);
        if let Some(name) = name {
            field = field.with_name(name);
        }
        while let Some(token) = parts.get(*idx).map(String::as_str) {
            match token {
                "offset" => {
                    let offset_token = must_get(parts.get(*idx + 1), "field offset")?;
                    field = field.with_offset(
                        offset_token
                            .parse()
                            .map_err(|_| text_invalid_operand("field offset", offset_token))?,
                    );
                    *idx = (*idx).saturating_add(2);
                }
                "storage" => {
                    let storage_token = must_get(parts.get(*idx + 1), "field storage")?;
                    field = field.with_storage(self.intern_string(&parse_quoted(storage_token)?));
                    *idx = (*idx).saturating_add(2);
                }
                "mut" => {
                    field = field.with_mutable(true);
                    *idx = (*idx).saturating_add(1);
                }
                "gc" => {
                    field = field.with_gc_pointer(true);
                    *idx = (*idx).saturating_add(1);
                }
                "public" => {
                    field = field.with_public(true);
                    *idx = (*idx).saturating_add(1);
                }
                "hidden" => {
                    field = field.with_hidden(true);
                    *idx = (*idx).saturating_add(1);
                }
                _ => break,
            }
        }
        Ok(field)
    }

    fn parse_data_metadata(
        &mut self,
        parts: &[String],
        idx: &mut usize,
        metadata: &mut DataMetadata,
    ) -> AssemblyResult {
        match must_get(parts.get(*idx), "data metadata key")? {
            "repr" => {
                let repr_token = must_get(parts.get(*idx + 1), "data metadata value")?;
                metadata.repr_kind = Some(self.intern_string(&parse_quoted(repr_token)?));
                *idx = (*idx).saturating_add(2);
            }
            "align" => {
                let align_token = must_get(parts.get(*idx + 1), "data metadata value")?;
                metadata.layout_align = Some(
                    align_token
                        .parse()
                        .map_err(|_| text_invalid_operand("align", align_token))?,
                );
                *idx = (*idx).saturating_add(2);
            }
            "pack" => {
                let pack_token = must_get(parts.get(*idx + 1), "data metadata value")?;
                metadata.layout_pack = Some(
                    pack_token
                        .parse()
                        .map_err(|_| text_invalid_operand("pack", pack_token))?,
                );
                *idx = (*idx).saturating_add(2);
            }
            "frozen" => {
                metadata.frozen = true;
                *idx = (*idx).saturating_add(1);
            }
            "header" => {
                *idx = (*idx).saturating_add(1);
                metadata.object_header = Some(self.parse_object_header(parts, idx)?);
            }
            _ => {
                return Err(text_unknown_symbol("data metadata", parts[*idx].as_str()));
            }
        }
        Ok(())
    }

    fn parse_object_header(
        &mut self,
        parts: &[String],
        idx: &mut usize,
    ) -> AssemblyResult<ObjectHeaderDescriptor> {
        let mut header = ObjectHeaderDescriptor::new();
        while let Some(token) = parts.get(*idx).map(String::as_str) {
            match token {
                "layout" => {
                    let ty_name = parse_symbol(must_get(parts.get(*idx + 1), "header layout")?)?;
                    header = header.with_layout_ty(self.ensure_type_symbol(&ty_name, &ty_name));
                    *idx = (*idx).saturating_add(2);
                }
                "mark_bits" => {
                    let bits = must_get(parts.get(*idx + 1), "mark bits")?;
                    header = header.with_mark_bits(
                        bits.parse()
                            .map_err(|_| text_invalid_operand("mark bits", bits))?,
                    );
                    *idx = (*idx).saturating_add(2);
                }
                "generation_bits" => {
                    let bits = must_get(parts.get(*idx + 1), "generation bits")?;
                    header = header.with_generation_bits(
                        bits.parse()
                            .map_err(|_| text_invalid_operand("generation bits", bits))?,
                    );
                    *idx = (*idx).saturating_add(2);
                }
                "pinned" => {
                    header = header.with_pinned(true);
                    *idx = (*idx).saturating_add(1);
                }
                "remembered" => {
                    header = header.with_remembered(true);
                    *idx = (*idx).saturating_add(1);
                }
                "large" => {
                    header = header.with_large(true);
                    *idx = (*idx).saturating_add(1);
                }
                "weak_capable" => {
                    header = header.with_weak_capable(true);
                    *idx = (*idx).saturating_add(1);
                }
                "forwarding" => {
                    header = header.with_forwarding(true);
                    *idx = (*idx).saturating_add(1);
                }
                "size_field" => {
                    header = header.with_size_field(true);
                    *idx = (*idx).saturating_add(1);
                }
                _ => break,
            }
        }
        Ok(header)
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

    pub(crate) fn parse_stack_effect(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 4 || parts.get(2).map(String::as_str) != Some("input") {
            return Err(text_expected_form(
                ".stack_effect $Name input [$Type ...] output [$Type ...]",
            ));
        }
        let name = parse_symbol(must_get(parts.get(1), "stack-effect name")?)?;
        if self.stack_effects.contains_key(&name) {
            return Err(text_duplicate_symbol("stack effect", &name));
        }
        let output_index = parts
            .iter()
            .position(|part| part == "output")
            .ok_or_else(|| {
                text_expected_form(".stack_effect $Name input [$Type ...] output [$Type ...]")
            })?;
        if output_index < 3 {
            return Err(text_expected_form(
                ".stack_effect $Name input [$Type ...] output [$Type ...]",
            ));
        }
        let input_tys = parts[3..output_index]
            .iter()
            .map(|token| {
                let type_name = parse_symbol(token)?;
                Ok(self.ensure_type_symbol(&type_name, &type_name))
            })
            .collect::<AssemblyResult<Vec<_>>>()?;
        let output_tys = parts[output_index + 1..]
            .iter()
            .map(|token| {
                let type_name = parse_symbol(token)?;
                Ok(self.ensure_type_symbol(&type_name, &type_name))
            })
            .collect::<AssemblyResult<Vec<_>>>()?;
        let descriptor = StackEffectDescriptor::new(
            self.intern_string(&name),
            input_tys.into_boxed_slice(),
            output_tys.into_boxed_slice(),
        );
        let id = self.artifact.stack_effects.alloc(descriptor);
        let _ = self.stack_effects.insert(name, id);
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
        let mut metadata = DataMetadata::default();
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
            self.parse_data_metadata(parts, &mut idx, &mut metadata)?;
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
        if let Some(repr_kind) = metadata.repr_kind {
            descriptor = descriptor.with_repr_kind(repr_kind);
        }
        if let Some(layout_align) = metadata.layout_align {
            descriptor = descriptor.with_layout_align(layout_align);
        }
        if let Some(layout_pack) = metadata.layout_pack {
            descriptor = descriptor.with_layout_pack(layout_pack);
        }
        if metadata.frozen {
            descriptor = descriptor.with_frozen(true);
        }
        if let Some(object_header) = metadata.object_header {
            descriptor = descriptor.with_object_header(object_header);
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

    pub(crate) fn parse_capability(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 2 {
            return Err(text_expected_form(
                ".capability $Name [payload $Type] [witness $Name] [dispatch $Name] [layout $Type] [root]",
            ));
        }
        let name = parse_symbol(&parts[1])?;
        let name_id = self.intern_string(&name);
        let mut descriptor = ShapeDescriptor::new(name_id);
        let mut idx = 2usize;
        while idx < parts.len() {
            match parts[idx].as_str() {
                "payload" => {
                    let payload_token = must_get(parts.get(idx + 1), "shape payload type")?;
                    let payload_name = parse_symbol(payload_token)?;
                    descriptor = descriptor
                        .with_payload_ty(self.ensure_type_symbol(&payload_name, &payload_name));
                    idx += 2;
                }
                "witness" => {
                    let witness_token = must_get(parts.get(idx + 1), "shape witness")?;
                    descriptor =
                        descriptor.with_witness(self.intern_string(&parse_symbol(witness_token)?));
                    idx += 2;
                }
                "dispatch" => {
                    let dispatch_token = must_get(parts.get(idx + 1), "shape dispatch table")?;
                    descriptor = descriptor
                        .with_dispatch_table(self.intern_string(&parse_symbol(dispatch_token)?));
                    idx += 2;
                }
                "layout" => {
                    let layout_token = must_get(parts.get(idx + 1), "shape layout identity")?;
                    let layout_name = parse_symbol(layout_token)?;
                    descriptor = descriptor
                        .with_layout_identity(self.ensure_type_symbol(&layout_name, &layout_name));
                    idx += 2;
                }
                "root" => {
                    descriptor = descriptor.with_root_visible(true);
                    idx += 1;
                }
                _ => {
                    return Err(text_unknown_symbol(
                        "capability metadata",
                        parts[idx].as_str(),
                    ));
                }
            }
        }
        let id = self.artifact.shapes.alloc(descriptor);
        let _ = self.shapes.insert(name, id);
        Ok(())
    }

    pub(crate) fn parse_closure(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 6
            || parts.get(2).map(String::as_str) != Some("procedure")
            || parts.get(4).map(String::as_str) != Some("captures")
        {
            return Err(text_expected_form(
                ".closure $Name procedure $Proc captures <n> [capture $Type ...] [env $Data] [param $Type ...] [result $Type ...] [domain \"...\"] [effect \"...\"] [suspend]",
            ));
        }
        let name = parse_symbol(must_get(parts.get(1), "closure name")?)?;
        if self.closures.contains_key(&name) {
            return Err(text_duplicate_symbol("closure", &name));
        }
        let procedure_name = parse_symbol(must_get(parts.get(3), "closure procedure")?)?;
        let procedure = self.ensure_procedure_symbol(&procedure_name);
        let capture_count = must_get(parts.get(5), "closure capture count")?
            .parse()
            .map_err(|_| text_invalid_operand("closure capture count", parts[5].as_str()))?;
        let mut descriptor =
            ClosureDescriptor::new(self.intern_string(&name), procedure, capture_count);
        let mut capture_tys = Vec::new();
        let mut param_tys = Vec::new();
        let mut result_tys = Vec::new();
        let mut idx = 6usize;
        while idx < parts.len() {
            match parts[idx].as_str() {
                "capture" => {
                    let ty_name = parse_symbol(must_get(parts.get(idx + 1), "capture type")?)?;
                    capture_tys.push(self.ensure_type_symbol(&ty_name, &ty_name));
                    idx += 2;
                }
                "env" => {
                    let data_name = parse_symbol(must_get(parts.get(idx + 1), "closure env")?)?;
                    let data = self.data.get(&data_name).copied().ok_or_else(|| {
                        text_unknown_symbol("closure environment layout", &data_name)
                    })?;
                    descriptor = descriptor.with_env_layout(data);
                    idx += 2;
                }
                "param" => {
                    let ty_name = parse_symbol(must_get(parts.get(idx + 1), "parameter type")?)?;
                    param_tys.push(self.ensure_type_symbol(&ty_name, &ty_name));
                    idx += 2;
                }
                "result" => {
                    let ty_name = parse_symbol(must_get(parts.get(idx + 1), "result type")?)?;
                    result_tys.push(self.ensure_type_symbol(&ty_name, &ty_name));
                    idx += 2;
                }
                "domain" => {
                    let domain = parse_quoted(must_get(parts.get(idx + 1), "closure domain")?)?;
                    descriptor = descriptor.with_domain(self.intern_string(&domain));
                    idx += 2;
                }
                "effect" => {
                    let effect = parse_quoted(must_get(parts.get(idx + 1), "closure effect")?)?;
                    descriptor = descriptor.with_effect(self.intern_string(&effect));
                    idx += 2;
                }
                "suspend" => {
                    descriptor = descriptor.with_suspending(true);
                    idx += 1;
                }
                _ => return Err(text_unknown_symbol("closure metadata", parts[idx].as_str())),
            }
        }
        let descriptor = descriptor
            .with_capture_tys(capture_tys.into_boxed_slice())
            .with_param_tys(param_tys.into_boxed_slice())
            .with_result_tys(result_tys.into_boxed_slice());
        let id = self.artifact.closures.alloc(descriptor);
        let _ = self.closures.insert(name, id);
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

    pub(crate) fn parse_manifest(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() != 7 && parts.len() != 9 {
            return Err(text_expected_form(
                r#".manifest package "name" version "version" profile "profile" [entry $Entry]"#,
            ));
        }
        if parts.get(1).map(String::as_str) != Some("package")
            || parts.get(3).map(String::as_str) != Some("version")
            || parts.get(5).map(String::as_str) != Some("profile")
        {
            return Err(text_expected_form(
                r#".manifest package "name" version "version" profile "profile" [entry $Entry]"#,
            ));
        }
        let package = parse_quoted(must_get(parts.get(2), "manifest package")?)?;
        let version = parse_quoted(must_get(parts.get(4), "manifest version")?)?;
        let profile = parse_quoted(must_get(parts.get(6), "manifest profile")?)?;
        let mut descriptor = ManifestDescriptor::new(
            self.intern_string(&package),
            self.intern_string(&version),
            self.intern_string(&profile),
        );
        if parts.len() == 9 {
            if parts.get(7).map(String::as_str) != Some("entry") {
                return Err(text_expected_form(
                    r#".manifest package "name" version "version" profile "profile" [entry $Entry]"#,
                ));
            }
            let entry = parse_symbol(must_get(parts.get(8), "manifest entry")?)?;
            descriptor = descriptor.with_entry(self.intern_string(&entry));
        }
        let _ = self.artifact.manifest.alloc(descriptor);
        Ok(())
    }

    pub(crate) fn parse_import(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() != 5
            || parts.get(1).map(String::as_str) != Some("spec")
            || parts.get(3).map(String::as_str) != Some("resolved")
        {
            return Err(text_expected_form(
                r#".import spec "specifier" resolved $Module"#,
            ));
        }
        let spec = parse_quoted(must_get(parts.get(2), "import spec")?)?;
        let resolved = parse_symbol(must_get(parts.get(4), "import resolved")?)?;
        let spec = self.intern_string(&spec);
        let resolved = self.intern_string(&resolved);
        let _ = self
            .artifact
            .imports
            .alloc(ImportDescriptor::new(spec, resolved));
        Ok(())
    }

    pub(crate) fn parse_root_map(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 3 || parts.get(1).map(String::as_str) != Some("point") {
            return Err(text_expected_form(
                ".root_map point $SafePoint [kind <safe-point-kind>] [procedure $Procedure] [local %slot ...] [stack %slot ...] [capture %slot ...] [defer %slot ...] [pin %slot ...]",
            ));
        }
        let safe_point = parse_symbol(must_get(parts.get(2), "root-map safe point")?)?;
        let mut kind = SafePointKind::Call;
        let mut procedure = None::<ProcedureId>;
        let mut local_slots = Vec::<u16>::new();
        let mut stack_slots = Vec::<u16>::new();
        let mut capture_slots = Vec::<u16>::new();
        let mut defer_slots = Vec::<u16>::new();
        let mut pin_slots = Vec::<u16>::new();
        let mut idx = 3usize;
        while idx < parts.len() {
            match parts[idx].as_str() {
                "kind" => {
                    let token = must_get(parts.get(idx + 1), "root-map kind")?;
                    kind = token
                        .parse::<SafePointKind>()
                        .map_err(|()| text_invalid_operand("root-map kind", token))?;
                    idx = idx.saturating_add(2);
                }
                "procedure" => {
                    let procedure_name =
                        parse_symbol(must_get(parts.get(idx + 1), "root-map procedure")?)?;
                    procedure = Some(
                        self.procedure_symbol(&procedure_name)
                            .ok_or_else(|| text_unknown_symbol("procedure", &procedure_name))?,
                    );
                    idx = idx.saturating_add(2);
                }
                "local" => {
                    local_slots.push(parse_local(parts.get(idx + 1))?);
                    idx = idx.saturating_add(2);
                }
                "stack" => {
                    stack_slots.push(parse_local(parts.get(idx + 1))?);
                    idx = idx.saturating_add(2);
                }
                "capture" => {
                    capture_slots.push(parse_local(parts.get(idx + 1))?);
                    idx = idx.saturating_add(2);
                }
                "defer" => {
                    defer_slots.push(parse_local(parts.get(idx + 1))?);
                    idx = idx.saturating_add(2);
                }
                "pin" => {
                    pin_slots.push(parse_local(parts.get(idx + 1))?);
                    idx = idx.saturating_add(2);
                }
                _ => {
                    return Err(text_expected_form(
                        ".root_map point $SafePoint [kind <safe-point-kind>] [procedure $Procedure] [local %slot ...] [stack %slot ...] [capture %slot ...] [defer %slot ...] [pin %slot ...]",
                    ));
                }
            }
        }
        let mut descriptor = RootMapDescriptor::new(
            self.intern_string(&safe_point),
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
        let _ = self.artifact.root_maps.alloc(descriptor);
        Ok(())
    }

    pub(crate) fn parse_block_sig(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 7
            || parts.get(1).map(String::as_str) != Some("procedure")
            || parts.get(3).map(String::as_str) != Some("label")
            || parts.get(5).map(String::as_str) != Some("stack")
        {
            return Err(text_expected_form(
                ".block_sig procedure $Procedure label $Label stack [$Type ...]",
            ));
        }

        let procedure_name = parse_symbol(must_get(parts.get(2), "block signature procedure")?)?;
        let procedure = self
            .procedure_symbol(&procedure_name)
            .ok_or_else(|| text_unknown_symbol("procedure", &procedure_name))?;

        let label_name = parse_symbol(must_get(parts.get(4), "block signature label")?)?;
        let procedure_descriptor = self.artifact.procedures.get(procedure);
        let label = procedure_descriptor
            .labels
            .iter()
            .position(|label_id| self.artifact.string_text(*label_id) == label_name)
            .ok_or_else(|| text_unknown_symbol("label", &label_name))
            .and_then(|index| {
                u16::try_from(index).map_err(|_| text_invalid_operand("label index", index))
            })?;

        let (incoming_tys, index) = self.parse_block_sig_stack_types(parts, 6)?;

        if index != parts.len() {
            return Err(text_expected_form(
                ".block_sig procedure $Procedure label $Label stack [$Type ...]",
            ));
        }

        let _ = self
            .artifact
            .block_signatures
            .alloc(BlockSignatureDescriptor::new(
                procedure,
                label,
                incoming_tys.into_boxed_slice(),
            ));
        Ok(())
    }

    fn parse_block_sig_stack_types(
        &self,
        parts: &[String],
        mut index: usize,
    ) -> AssemblyResult<(Vec<TypeId>, usize)> {
        let first = must_get(parts.get(index), "block signature incoming types")?;
        let mut incoming_tys = Vec::<TypeId>::new();
        if first == "[" {
            index = index.saturating_add(1);
        } else if let Some(inline) = first.strip_prefix('[') {
            if let Some(single) = inline.strip_suffix(']') {
                if !single.is_empty() {
                    incoming_tys.push(self.lookup_known_type(single)?);
                }
                return Ok((incoming_tys, index.saturating_add(1)));
            }
            if !inline.is_empty() {
                incoming_tys.push(self.lookup_known_type(inline)?);
            }
            index = index.saturating_add(1);
        } else {
            return Err(text_expected_form(
                ".block_sig procedure $Procedure label $Label stack [$Type ...]",
            ));
        }

        loop {
            let Some(token) = parts.get(index) else {
                return Err(text_missing_operand(
                    "block signature incoming types closing `]`",
                ));
            };
            if token == "]" {
                return Ok((incoming_tys, index.saturating_add(1)));
            }
            if let Some(last_type) = token.strip_suffix(']') {
                if !last_type.is_empty() {
                    incoming_tys.push(self.lookup_known_type(last_type)?);
                }
                return Ok((incoming_tys, index.saturating_add(1)));
            }
            incoming_tys.push(self.lookup_known_type(token)?);
            index = index.saturating_add(1);
        }
    }

    fn lookup_known_type(&self, type_token: &str) -> AssemblyResult<TypeId> {
        let type_name = parse_symbol(type_token)?;
        self.types
            .get(&type_name)
            .copied()
            .ok_or_else(|| text_unknown_symbol("type", &type_name))
    }

    pub(crate) fn parse_foreign(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 6 {
            return Err(text_expected_form(
                r#".native $Name [param $Type ...] result $Type abi "c" symbol "puts" [link "c"] [domain "native"] [pin %0] [nullable %1] [nullable_result] [lifetime "call"] [export] [hot] [cold]"#,
            ));
        }
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
                r#".native $Name [param $Type ...] result $Type abi "c" symbol "puts" [link "c"] [domain "native"] [pin %0] [nullable %1] [nullable_result] [lifetime "call"] [export] [hot] [cold]"#,
            ));
        }
        let result_ty = parse_symbol(must_get(parts.get(base + 1), "foreign result type")?)?;
        let abi = parse_quoted(must_get(parts.get(base + 3), "foreign abi")?)?;
        let symbol = parse_quoted(must_get(parts.get(base + 5), "foreign symbol")?)?;
        let options = Self::parse_foreign_options(parts, base + 6)?;
        let name = parse_symbol(must_get(parts.get(1), "foreign name")?)?;
        let mut descriptor = ForeignDescriptor::new(
            self.intern_string(&name),
            param_tys.into_boxed_slice(),
            self.ensure_type_symbol(&result_ty, &result_ty),
            self.intern_string(&abi),
            self.intern_string(&symbol),
        )
        .with_export(options.profile.export)
        .with_hot(options.profile.hot)
        .with_cold(options.profile.cold)
        .with_pinned_params(options.pinned_params.into_boxed_slice())
        .with_nullable_params(options.nullable_params.into_boxed_slice())
        .with_nullable_result(options.nullable_result);
        if let Some(link) = options.link.as_deref().map(|text| self.intern_string(text)) {
            descriptor = descriptor.with_link(link);
        }
        if let Some(domain) = options
            .domain
            .as_deref()
            .map(|text| self.intern_string(text))
        {
            descriptor = descriptor.with_domain(domain);
        }
        if let Some(lifetime) = options
            .lifetime
            .as_deref()
            .map(|text| self.intern_string(text))
        {
            descriptor = descriptor.with_lifetime(lifetime);
        }
        let id = self.artifact.foreigns.alloc(descriptor);
        let _ = self.foreigns.insert(name, id);
        Ok(())
    }

    fn parse_foreign_options(parts: &[String], mut idx: usize) -> AssemblyResult<ForeignOptions> {
        let mut options = ForeignOptions::default();
        while idx < parts.len() {
            match parts[idx].as_str() {
                "export" => options.profile.export = true,
                "hot" => options.profile.hot = true,
                "cold" => options.profile.cold = true,
                "link" => {
                    options.link =
                        Some(parse_quoted(must_get(parts.get(idx + 1), "foreign link")?)?);
                }
                "domain" => {
                    options.domain = Some(parse_quoted(must_get(
                        parts.get(idx + 1),
                        "foreign domain",
                    )?)?);
                }
                "pin" => options.pinned_params.push(parse_local(parts.get(idx + 1))?),
                "nullable" => options
                    .nullable_params
                    .push(parse_local(parts.get(idx + 1))?),
                "nullable_result" => options.nullable_result = true,
                "lifetime" => {
                    options.lifetime = Some(parse_quoted(must_get(
                        parts.get(idx + 1),
                        "foreign lifetime",
                    )?)?);
                }
                _ => {
                    return Err(text_expected_form(
                        r#".native $Name [param $Type ...] result $Type abi "c" symbol "puts" [link "c"] [domain "native"] [pin %0] [nullable %1] [nullable_result] [lifetime "call"] [export] [hot] [cold]"#,
                    ));
                }
            }
            idx += if matches!(
                parts[idx].as_str(),
                "export" | "hot" | "cold" | "nullable_result"
            ) {
                1
            } else {
                2
            };
        }
        Ok(options)
    }

    pub(crate) fn parse_export(&mut self, parts: &[String]) -> AssemblyResult {
        if parts.len() < 3 {
            return Err(text_expected_form(
                ".export $Name <procedure|global|native|type|capability> [opaque]",
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
            "capability" => {
                let shape = *self
                    .shapes
                    .get(&name)
                    .ok_or_else(|| text_unknown_symbol("shape", &name))?;
                ExportTarget::Shape(shape)
            }
            _ => {
                return Err(text_expected_form(
                    ".export $Name <procedure|global|native|type|capability> [opaque]",
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
