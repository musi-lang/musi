use super::symbols::{ensure_label, must_get, parse_local, parse_quoted, parse_symbol};
use super::*;

impl TextBuilder {
    pub(crate) fn parse_operand(
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
            OperandShape::Procedure => self.parse_procedure_operand(parts),
            OperandShape::WideProcedureCaptures => {
                self.parse_wide_procedure_captures_operand(parts)
            }
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

    fn parse_procedure_operand(&mut self, parts: &[String]) -> AssemblyResult<Operand> {
        let name = parse_symbol(must_get(parts.get(1), "procedure")?)?;
        Ok(Operand::Procedure(self.ensure_procedure_symbol(&name)))
    }

    fn parse_wide_procedure_captures_operand(
        &mut self,
        parts: &[String],
    ) -> AssemblyResult<Operand> {
        let name = parse_symbol(must_get(parts.get(1), "procedure")?)?;
        let procedure = self.ensure_procedure_symbol(&name);
        let captures = must_get(parts.get(2), "capture count")?
            .parse::<u8>()
            .map_err(|_| AssemblyError::TextParseFailed("invalid capture count".into()))?;
        Ok(Operand::WideProcedureCaptures {
            procedure,
            captures,
        })
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
}
