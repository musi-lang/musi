use super::symbols::{ensure_label, must_get, parse_quoted, parse_symbol, tokenize};
use super::*;
use crate::{BlockSignatureId, RootMapId};

struct ProcedureHeader {
    name: String,
    params: u16,
    param_tys: Vec<TypeId>,
    locals: u16,
    local_tys: Vec<TypeId>,
    result_tys: Vec<TypeId>,
    entry_label: u16,
    bytecode_body: u32,
    block_signature_table: Option<BlockSignatureId>,
    root_map_table: Option<RootMapId>,
    domain_requirements: Vec<StringId>,
    calling_convention: ProcedureCallingConvention,
    visibility: ProcedureVisibility,
    export: bool,
    hot: bool,
    cold: bool,
}

impl ProcedureHeader {
    const fn new(name: String) -> Self {
        Self {
            name,
            params: 0,
            param_tys: Vec::new(),
            locals: 0,
            local_tys: Vec::new(),
            result_tys: Vec::new(),
            entry_label: 0,
            bytecode_body: 0,
            block_signature_table: None,
            root_map_table: None,
            domain_requirements: Vec::new(),
            calling_convention: ProcedureCallingConvention::Managed,
            visibility: ProcedureVisibility::Private,
            export: false,
            hot: false,
            cold: false,
        }
    }
}

impl TextBuilder {
    pub(crate) fn parse_procedure(&mut self, header: &str, lines: &[&str]) -> AssemblyResult {
        let parts = tokenize(header)?;
        if parts.len() < 4 {
            return Err(text_expected_form(
                ".procedure $Name [params <count>] locals <count> [result [$Type ...]] entry <label> body <id> callconv <kind> visibility <kind> [export] [hot] [cold]",
            ));
        }
        let name = parse_symbol(&parts[1])?;
        let mut parsed = ProcedureHeader::new(name);
        let mut idx = 2;
        idx = Self::parse_optional_params(&parts, idx, &mut parsed)?;
        idx = self.parse_optional_param_types(&parts, idx, &mut parsed)?;
        idx = Self::parse_required_locals(&parts, idx, &mut parsed)?;
        idx = self.parse_optional_local_types(&parts, idx, &mut parsed)?;
        idx = self.parse_optional_result_types(&parts, idx, &mut parsed)?;
        idx = Self::parse_optional_entry_label(&parts, idx, &mut parsed)?;
        idx = Self::parse_optional_bytecode_body(&parts, idx, &mut parsed)?;
        idx = Self::parse_optional_block_signature_table(&parts, idx, &mut parsed)?;
        idx = Self::parse_optional_root_map_table(&parts, idx, &mut parsed)?;
        idx = self.parse_optional_domains(&parts, idx, &mut parsed)?;
        idx = Self::parse_optional_calling_convention(&parts, idx, &mut parsed)?;
        idx = Self::parse_optional_visibility(&parts, idx, &mut parsed)?;
        Self::parse_flags(&parts, idx, &mut parsed)?;

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
                    String::from(label_name),
                )?;
                code.push(CodeEntry::Label(Label { id: label_id }));
                continue;
            }
            let entry = self.parse_instruction(raw_line, &mut labels, &mut label_ids)?;
            code.push(CodeEntry::Instruction(entry));
        }

        let procedure = ProcedureDescriptor::new(
            self.intern_string(&parsed.name),
            parsed.params,
            parsed.locals,
            code.into_boxed_slice(),
        )
        .with_param_tys(parsed.param_tys.into_boxed_slice())
        .with_local_tys(parsed.local_tys.into_boxed_slice())
        .with_result_tys(parsed.result_tys.into_boxed_slice())
        .with_entry_label(parsed.entry_label)
        .with_bytecode_body(parsed.bytecode_body)
        .with_domain_requirements(parsed.domain_requirements.into_boxed_slice())
        .with_calling_convention(parsed.calling_convention)
        .with_visibility(parsed.visibility)
        .with_export(parsed.export)
        .with_hot(parsed.hot)
        .with_cold(parsed.cold)
        .with_labels(labels.into_boxed_slice());
        let mut procedure = procedure;
        if let Some(block_signature_table) = parsed.block_signature_table {
            procedure = procedure.with_block_signature_table(block_signature_table);
        }
        if let Some(root_map_table) = parsed.root_map_table {
            procedure = procedure.with_root_map_table(root_map_table);
        }
        let id = self.ensure_procedure_symbol(&parsed.name);
        *self.artifact.procedures.get_mut(id) = procedure;
        Ok(())
    }

    fn parse_optional_params(
        parts: &[String],
        idx: usize,
        parsed: &mut ProcedureHeader,
    ) -> AssemblyResult<usize> {
        if parts.get(idx).map(String::as_str) != Some("params") {
            return Ok(idx);
        }
        parsed.params = must_get(parts.get(idx + 1), "procedure params")?
            .parse()
            .map_err(|_| text_invalid_operand("params count", parts[idx + 1].as_str()))?;
        Ok(idx + 2)
    }

    fn parse_optional_param_types(
        &mut self,
        parts: &[String],
        mut idx: usize,
        parsed: &mut ProcedureHeader,
    ) -> AssemblyResult<usize> {
        if parts.get(idx).map(String::as_str) == Some("param_types") {
            idx += 1;
            (parsed.param_tys, idx) = self.parse_procedure_type_list(
                parts,
                idx,
                "procedure parameter types",
                "procedure parameter type",
            )?;
        }
        Ok(idx)
    }

    fn parse_required_locals(
        parts: &[String],
        idx: usize,
        parsed: &mut ProcedureHeader,
    ) -> AssemblyResult<usize> {
        if parts.get(idx).map(String::as_str) != Some("locals") {
            return Err(text_expected_form(
                ".procedure $Name [params <count>] [param_types [$Type ...]] locals <count> [local_types [$Type ...]] [result [$Type ...]] entry <label> body <id> callconv <kind> visibility <kind> [export] [hot] [cold]",
            ));
        }
        parsed.locals = must_get(parts.get(idx + 1), "locals count")?
            .parse()
            .map_err(|_| text_invalid_operand("locals count", parts[idx + 1].as_str()))?;
        Ok(idx + 2)
    }

    fn parse_optional_local_types(
        &mut self,
        parts: &[String],
        mut idx: usize,
        parsed: &mut ProcedureHeader,
    ) -> AssemblyResult<usize> {
        if parts.get(idx).map(String::as_str) == Some("local_types") {
            idx += 1;
            (parsed.local_tys, idx) = self.parse_procedure_type_list(
                parts,
                idx,
                "procedure local types",
                "procedure local type",
            )?;
        }
        Ok(idx)
    }

    fn parse_optional_result_types(
        &mut self,
        parts: &[String],
        mut idx: usize,
        parsed: &mut ProcedureHeader,
    ) -> AssemblyResult<usize> {
        if parts.get(idx).map(String::as_str) == Some("result") {
            idx += 1;
            (parsed.result_tys, idx) = self.parse_procedure_type_list(
                parts,
                idx,
                "procedure result types",
                "procedure result type",
            )?;
        }
        Ok(idx)
    }

    fn parse_optional_entry_label(
        parts: &[String],
        idx: usize,
        parsed: &mut ProcedureHeader,
    ) -> AssemblyResult<usize> {
        if parts.get(idx).map(String::as_str) != Some("entry") {
            return Ok(idx);
        }
        parsed.entry_label = must_get(parts.get(idx + 1), "procedure entry label")?
            .parse()
            .map_err(|_| text_invalid_operand("procedure entry label", parts[idx + 1].as_str()))?;
        Ok(idx + 2)
    }

    fn parse_optional_bytecode_body(
        parts: &[String],
        idx: usize,
        parsed: &mut ProcedureHeader,
    ) -> AssemblyResult<usize> {
        if parts.get(idx).map(String::as_str) != Some("body") {
            return Ok(idx);
        }
        parsed.bytecode_body = must_get(parts.get(idx + 1), "procedure bytecode body")?
            .parse()
            .map_err(|_| {
                text_invalid_operand("procedure bytecode body", parts[idx + 1].as_str())
            })?;
        Ok(idx + 2)
    }

    fn parse_optional_block_signature_table(
        parts: &[String],
        idx: usize,
        parsed: &mut ProcedureHeader,
    ) -> AssemblyResult<usize> {
        if parts.get(idx).map(String::as_str) != Some("block_table") {
            return Ok(idx);
        }
        let raw = must_get(parts.get(idx + 1), "procedure block table")?
            .parse()
            .map_err(|_| text_invalid_operand("procedure block table", parts[idx + 1].as_str()))?;
        parsed.block_signature_table = Some(Idx::from_raw(raw));
        Ok(idx + 2)
    }

    fn parse_optional_root_map_table(
        parts: &[String],
        idx: usize,
        parsed: &mut ProcedureHeader,
    ) -> AssemblyResult<usize> {
        if parts.get(idx).map(String::as_str) != Some("root_map") {
            return Ok(idx);
        }
        let raw = must_get(parts.get(idx + 1), "procedure root map")?
            .parse()
            .map_err(|_| text_invalid_operand("procedure root map", parts[idx + 1].as_str()))?;
        parsed.root_map_table = Some(Idx::from_raw(raw));
        Ok(idx + 2)
    }

    fn parse_optional_domains(
        &mut self,
        parts: &[String],
        mut idx: usize,
        parsed: &mut ProcedureHeader,
    ) -> AssemblyResult<usize> {
        if parts.get(idx).map(String::as_str) != Some("domains") {
            return Ok(idx);
        }
        idx += 1;
        let Some(first_token) = parts.get(idx).map(String::as_str) else {
            return Err(text_missing_operand("procedure domains"));
        };
        if first_token != "[" {
            return Err(text_expected_form("domains [\"domain\" ...]"));
        }
        idx += 1;
        loop {
            let Some(token) = parts.get(idx) else {
                return Err(text_missing_operand("procedure domains"));
            };
            if token == "]" {
                idx += 1;
                break;
            }
            parsed
                .domain_requirements
                .push(self.intern_string(&parse_quoted(token)?));
            idx += 1;
        }
        if parsed.domain_requirements.is_empty() {
            return Err(text_missing_operand("procedure domain"));
        }
        Ok(idx)
    }

    fn parse_optional_calling_convention(
        parts: &[String],
        idx: usize,
        parsed: &mut ProcedureHeader,
    ) -> AssemblyResult<usize> {
        if parts.get(idx).map(String::as_str) != Some("callconv") {
            return Ok(idx);
        }
        let token = must_get(parts.get(idx + 1), "procedure calling convention")?;
        parsed.calling_convention = token
            .parse::<ProcedureCallingConvention>()
            .map_err(|()| text_invalid_operand("procedure calling convention", token))?;
        Ok(idx + 2)
    }

    fn parse_optional_visibility(
        parts: &[String],
        idx: usize,
        parsed: &mut ProcedureHeader,
    ) -> AssemblyResult<usize> {
        if parts.get(idx).map(String::as_str) != Some("visibility") {
            return Ok(idx);
        }
        let token = must_get(parts.get(idx + 1), "procedure visibility")?;
        parsed.visibility = token
            .parse::<ProcedureVisibility>()
            .map_err(|()| text_invalid_operand("procedure visibility", token))?;
        Ok(idx + 2)
    }

    fn parse_flags(parts: &[String], idx: usize, parsed: &mut ProcedureHeader) -> AssemblyResult {
        for token in parts.iter().skip(idx) {
            match token.as_str() {
                "export" => parsed.export = true,
                "hot" => parsed.hot = true,
                "cold" => parsed.cold = true,
                _ => return Err(text_invalid_operand("procedure flag", token.as_str())),
            }
        }
        Ok(())
    }

    fn parse_procedure_type_list(
        &mut self,
        parts: &[String],
        mut idx: usize,
        missing_list: &'static str,
        missing_item: &'static str,
    ) -> AssemblyResult<(Vec<TypeId>, usize)> {
        let Some(first_token) = parts.get(idx).map(String::as_str) else {
            return Err(text_missing_operand(missing_list));
        };
        let mut out = Vec::new();
        let mut opened_inline = false;
        if first_token == "[" {
            idx += 1;
        } else if let Some(inline) = first_token.strip_prefix('[') {
            opened_inline = true;
            if let Some(single) = inline.strip_suffix(']') {
                if single.is_empty() {
                    return Err(text_missing_operand(missing_item));
                }
                let type_name = parse_symbol(single)?;
                out.push(self.ensure_type_symbol(&type_name, &type_name));
            } else if !inline.is_empty() {
                let type_name = parse_symbol(inline)?;
                out.push(self.ensure_type_symbol(&type_name, &type_name));
            }
            idx += 1;
        } else {
            return Err(text_expected_form(
                ".procedure $Name [params <count>] [param_types [$Type ...]] locals <count> [result [$Type ...]] [export] [hot] [cold]",
            ));
        }
        if !opened_inline && parts.get(idx).map(String::as_str) == Some("]") {
            return Err(text_missing_operand(missing_item));
        }
        if !(opened_inline
            && parts
                .get(idx.saturating_sub(1))
                .is_some_and(|token| token.ends_with(']')))
        {
            loop {
                let Some(token) = parts.get(idx) else {
                    return Err(text_missing_operand(missing_list));
                };
                if token == "]" {
                    idx += 1;
                    break;
                }
                if let Some(last_type) = token.strip_suffix(']') {
                    if last_type.is_empty() {
                        return Err(text_missing_operand(missing_item));
                    }
                    let type_name = parse_symbol(last_type)?;
                    out.push(self.ensure_type_symbol(&type_name, &type_name));
                    idx += 1;
                    break;
                }
                let type_name = parse_symbol(token)?;
                out.push(self.ensure_type_symbol(&type_name, &type_name));
                idx += 1;
            }
        }
        if out.is_empty() {
            return Err(text_missing_operand(missing_item));
        }
        Ok((out, idx))
    }

    fn parse_instruction(
        &mut self,
        line: &str,
        labels: &mut Vec<StringId>,
        label_ids: &mut LabelIdMap,
    ) -> AssemblyResult<Instruction> {
        let parts = tokenize(line)?;
        let Some(opcode_text) = parts.first() else {
            return Err(text_missing_operand("opcode"));
        };
        let Some(opcode) = Opcode::from_mnemonic(opcode_text) else {
            return Err(text_unknown_opcode(opcode_text));
        };
        let operand = self.parse_operand(opcode.operand_shape(), &parts, labels, label_ids)?;
        Ok(Instruction::new(opcode, operand))
    }
}
