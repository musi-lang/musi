use super::symbols::{ensure_label, must_get, parse_symbol, tokenize};
use super::*;

impl TextBuilder {
    pub(crate) fn parse_procedure(&mut self, header: &str, lines: &[&str]) -> AssemblyResult {
        let parts = tokenize(header)?;
        if parts.len() < 4 {
            return Err(AssemblyError::TextParseFailed(
                "expected `.procedure $Name [params <count>] locals <count> [export]`".into(),
            ));
        }
        let name = parse_symbol(&parts[1])?;
        let mut params = 0_u16;
        let mut idx = 2;
        if parts.get(idx).map(String::as_str) == Some("params") {
            params = must_get(parts.get(idx + 1), "procedure params")?
                .parse()
                .map_err(|_| AssemblyError::TextParseFailed("invalid params count".into()))?;
            idx += 2;
        }
        if parts.get(idx).map(String::as_str) != Some("locals") {
            return Err(AssemblyError::TextParseFailed(
                "expected `.procedure $Name [params <count>] locals <count> [export]`".into(),
            ));
        }
        let locals = must_get(parts.get(idx + 1), "locals count")?
            .parse()
            .map_err(|_| AssemblyError::TextParseFailed("invalid locals count".into()))?;
        let export = parts.iter().skip(idx + 2).any(|part| part == "export");
        let hot = parts.iter().skip(idx + 2).any(|part| part == "hot");
        let cold = parts.iter().skip(idx + 2).any(|part| part == "cold");

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

        let procedure = ProcedureDescriptor::new(
            self.intern_string(&name),
            params,
            locals,
            code.into_boxed_slice(),
        )
        .with_export(export)
        .with_hot(hot)
        .with_cold(cold)
        .with_labels(labels.into_boxed_slice());
        let id = self.ensure_procedure_symbol(&name);
        *self.artifact.procedures.get_mut(id) = procedure;
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
}
