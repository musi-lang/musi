use super::symbols::{ensure_label, must_get, parse_symbol, tokenize};
use super::*;

impl TextBuilder {
    pub(crate) fn parse_procedure(&mut self, header: &str, lines: &[&str]) -> AssemblyResult {
        let parts = tokenize(header)?;
        if parts.len() < 4 {
            return Err(text_expected_form(
                ".procedure $Name [params <count>] locals <count> [export]",
            ));
        }
        let name = parse_symbol(&parts[1])?;
        let mut params = 0_u16;
        let mut idx = 2;
        if parts.get(idx).map(String::as_str) == Some("params") {
            params = must_get(parts.get(idx + 1), "procedure params")?
                .parse()
                .map_err(|_| text_invalid_operand("params count", parts[3].as_str()))?;
            idx += 2;
        }
        if parts.get(idx).map(String::as_str) != Some("locals") {
            return Err(text_expected_form(
                ".procedure $Name [params <count>] locals <count> [export]",
            ));
        }
        let locals = must_get(parts.get(idx + 1), "locals count")?
            .parse()
            .map_err(|_| text_invalid_operand("locals count", parts[idx + 1].as_str()))?;
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
                    String::from(label_name),
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
            return Err(text_missing_operand("opcode"));
        };
        let Some(opcode) = Opcode::from_mnemonic(opcode_text) else {
            return Err(text_unknown_opcode(opcode_text));
        };
        let operand = self.parse_operand(opcode.operand_shape(), &parts, labels, label_ids)?;
        Ok(Instruction::new(opcode, operand))
    }
}
