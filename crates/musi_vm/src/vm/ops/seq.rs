use std::rc::Rc;
use std::slice::from_ref;

use music_seam::{Instruction, Opcode, Operand};

use crate::VmValueKind;

use super::{StepOutcome, Value, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    pub(crate) fn exec_seq(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::SeqNew => self.exec_seq_new(instruction),
            Opcode::SeqGet => self.exec_seq_get(),
            Opcode::SeqGetN => self.exec_seq_get_n(instruction),
            Opcode::SeqSet => self.exec_seq_set(),
            Opcode::SeqSetN => self.exec_seq_set_n(instruction),
            Opcode::SeqCat => self.exec_seq_cat(),
            Opcode::RangeNew => self.exec_range_new(instruction),
            Opcode::RangeContains => self.exec_range_contains(),
            Opcode::RangeMaterialize => self.exec_range_materialize(),
            Opcode::SeqHas => self.exec_seq_has(),
            _ => Err(Self::invalid_dispatch(instruction, "sequence")),
        }
    }

    fn exec_seq_new(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        let Operand::TypeLen { ty, len } = instruction.operand else {
            return Err(Self::invalid_operand(instruction));
        };
        let items = self.pop_args(usize::from(len))?;
        self.push_value(Value::sequence(ty, items))?;
        Ok(StepOutcome::Continue)
    }

    fn exec_seq_get(&mut self) -> VmResult<StepOutcome> {
        let index_value = self.pop_value()?;
        let index = Self::expect_int(&index_value)?;
        let seq_value = self.pop_value()?;
        let seq = Self::expect_seq(seq_value)?;
        let seq_ref = seq.borrow();
        let slot = usize::try_from(index).unwrap_or(usize::MAX);
        let value = seq_ref.items.get(slot).cloned().ok_or_else(|| {
            VmError::new(VmErrorKind::InvalidSequenceIndex {
                index,
                len: seq_ref.items.len(),
            })
        })?;
        drop(seq_ref);
        self.push_value(value)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_seq_get_n(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        let Operand::I16(len) = instruction.operand else {
            return Err(Self::invalid_operand(instruction));
        };
        let indices = self.pop_index_list(len)?;
        let seq_value = self.pop_value()?;
        let seq = Self::expect_seq(seq_value)?;
        let value = Self::get_nested_seq(seq, &indices)?;
        self.push_value(value)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_seq_set(&mut self) -> VmResult<StepOutcome> {
        let value = self.pop_value()?;
        let index_value = self.pop_value()?;
        let index = Self::expect_int(&index_value)?;
        let seq_value = self.pop_value()?;
        let seq = Self::expect_seq(seq_value)?;
        {
            let mut seq_mut = seq.borrow_mut();
            let len = seq_mut.items.len();
            let slot = usize::try_from(index).unwrap_or(usize::MAX);
            let item = seq_mut
                .items
                .get_mut(slot)
                .ok_or_else(|| VmError::new(VmErrorKind::InvalidSequenceIndex { index, len }))?;
            *item = value;
        }
        self.push_value(Value::Seq(seq))?;
        Ok(StepOutcome::Continue)
    }

    fn exec_seq_set_n(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        let Operand::I16(len) = instruction.operand else {
            return Err(Self::invalid_operand(instruction));
        };
        let value = self.pop_value()?;
        let indices = self.pop_index_list(len)?;
        let seq_value = self.pop_value()?;
        let seq = Self::expect_seq(seq_value)?;
        Self::set_nested_seq(Rc::clone(&seq), &indices, value)?;
        self.push_value(Value::Seq(seq))?;
        Ok(StepOutcome::Continue)
    }

    fn exec_seq_cat(&mut self) -> VmResult<StepOutcome> {
        let right_value = self.pop_value()?;
        let right_items = Self::expect_seq_items(right_value)?;
        let left_value = self.pop_value()?;
        let left = Self::expect_seq(left_value)?;
        let left = left.borrow();
        let mut items = left.items.clone();
        items.extend(right_items);
        self.push_value(Value::sequence(left.ty, items))?;
        Ok(StepOutcome::Continue)
    }

    fn exec_range_new(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        let Operand::TypeLen { ty, len } = instruction.operand else {
            return Err(Self::invalid_operand(instruction));
        };
        let end = self.pop_value()?;
        let start = self.pop_value()?;
        let module_slot = self.current_module_slot()?;
        let end_bound = self.range_end_bound_value(module_slot, len)?;
        self.push_value(Value::data(ty, 0, [start, end, end_bound]))?;
        Ok(StepOutcome::Continue)
    }

    fn exec_range_contains(&mut self) -> VmResult<StepOutcome> {
        let needle = self.pop_value()?;
        let range = self.pop_value()?;
        let evidence = self.pop_value()?;
        let items = self.materialize_range_items(range, evidence)?;
        let module_slot = self.current_module_slot()?;
        self.push_value(self.bool_value(module_slot, items.iter().any(|item| item == &needle))?)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_range_materialize(&mut self) -> VmResult<StepOutcome> {
        let range = self.pop_value()?;
        let evidence = self.pop_value()?;
        let ty = self.range_sequence_type(&range)?;
        let items = self.materialize_range_items(range, evidence)?;
        self.push_value(Value::sequence(ty, items))?;
        Ok(StepOutcome::Continue)
    }

    fn exec_seq_has(&mut self) -> VmResult<StepOutcome> {
        let needle = self.pop_value()?;
        let seq_value = self.pop_value()?;
        let contains = Self::expect_seq_items(seq_value)?
            .iter()
            .any(|item| item == &needle);
        let module_slot = self.current_module_slot()?;
        self.push_value(self.bool_value(module_slot, contains)?)?;
        Ok(StepOutcome::Continue)
    }

    fn expect_seq_items(value: Value) -> VmResult<Vec<Value>> {
        match value {
            Value::Seq(seq) => Ok(seq.borrow().items.iter().cloned().collect()),
            other => Err(Self::invalid_value_kind(VmValueKind::Seq, &other)),
        }
    }

    fn range_end_bound_value(&self, module_slot: usize, flags: u16) -> VmResult<Value> {
        let bound_ty = self.named_type_id(module_slot, "Bound").ok_or_else(|| {
            VmError::new(VmErrorKind::InvalidValueKind {
                expected: VmValueKind::Data,
                found: VmValueKind::Unit,
            })
        })?;
        let tag = i64::from(flags != 0);
        Ok(Value::data(bound_ty, tag, []))
    }

    fn bound_tag(value: Value) -> VmResult<i64> {
        let data = Self::expect_data(value)?;
        Ok(data.borrow().tag)
    }

    fn materialize_range_items(&mut self, range: Value, evidence: Value) -> VmResult<Vec<Value>> {
        let (start, end, inclusive_end) = self.range_parts(range)?;
        if self.is_builtin_rangeable_int_evidence(&evidence) {
            return Self::materialize_builtin_int_range(&start, &end, inclusive_end);
        }
        let (compare, next, prev) = Self::range_dictionary(evidence)?;
        let direction = self.compare_range_values(&compare, &start, &end)?;
        let is_ascending = direction <= 0;
        let step = if is_ascending { next } else { prev };
        let mut current = start;
        let mut items = Vec::new();
        loop {
            let cmp_to_end = self.compare_range_values(&compare, &current, &end)?;
            let before_end = if is_ascending {
                cmp_to_end < 0
            } else {
                cmp_to_end > 0
            };
            let at_end = cmp_to_end == 0;
            if !(before_end || inclusive_end && at_end) {
                break;
            }
            items.push(current.clone());
            if at_end {
                break;
            }
            let stepped = self.step_range_value(&step, &current, is_ascending, &compare)?;
            current = stepped;
        }
        Ok(items)
    }

    fn range_parts(&self, range: Value) -> VmResult<(Value, Value, bool)> {
        let data = Self::expect_data(range)?;
        let data = data.borrow();
        if !self.is_range_type(data.ty) || data.fields.len() != 3 {
            return Err(VmError::new(VmErrorKind::InvalidValueKind {
                expected: VmValueKind::Data,
                found: VmValueKind::Data,
            }));
        }
        let start = data.fields.first().cloned().unwrap_or(Value::Unit);
        let end = data.fields.get(1).cloned().unwrap_or(Value::Unit);
        let end_bound = data.fields.get(2).cloned().unwrap_or(Value::Unit);
        Ok((start, end, Self::bound_tag(end_bound)? == 0))
    }

    fn range_dictionary(evidence: Value) -> VmResult<(Value, Value, Value)> {
        let found = evidence.kind();
        let data = Self::expect_data(evidence)
            .map_err(|_| VmError::new(VmErrorKind::InvalidRangeEvidence { found }))?;
        let data = data.borrow();
        if data.fields.len() < 3 {
            return Err(VmError::new(VmErrorKind::InvalidRangeStep {
                detail: "range dictionary field count".into(),
            }));
        }
        Ok((
            data.fields[0].clone(),
            data.fields[1].clone(),
            data.fields[2].clone(),
        ))
    }

    fn is_builtin_rangeable_int_evidence(&self, evidence: &Value) -> bool {
        let Value::Data(data) = evidence else {
            return false;
        };
        let data = data.borrow();
        data.fields.is_empty() && self.is_named_type(data.ty, "__builtin_dict__Rangeable_Int_Dict")
    }

    fn range_sequence_type(&self, range: &Value) -> VmResult<music_seam::TypeId> {
        let Value::Data(data) = range else {
            return Err(Self::invalid_value_kind(VmValueKind::Data, range));
        };
        let range_ty = data.borrow().ty;
        let range_name = self
            .loaded_modules
            .iter()
            .find_map(|module| {
                let name = module.program.type_name(range_ty);
                name.contains("Range[").then_some(name)
            })
            .ok_or_else(|| {
                VmError::new(VmErrorKind::InvalidValueKind {
                    expected: VmValueKind::Seq,
                    found: VmValueKind::Unit,
                })
            })?;
        let item_name = range_name
            .rsplit_once("Range[")
            .map_or(range_name, |(_, tail)| tail)
            .strip_suffix(']')
            .unwrap_or(range_name);
        let seq_name = format!("[]{item_name}");
        let module_slot = self.current_module_slot()?;
        self.named_type_id(module_slot, &seq_name).ok_or_else(|| {
            VmError::new(VmErrorKind::InvalidValueKind {
                expected: VmValueKind::Seq,
                found: VmValueKind::Unit,
            })
        })
    }

    fn materialize_builtin_int_range(
        start: &Value,
        end: &Value,
        inclusive_end: bool,
    ) -> VmResult<Vec<Value>> {
        let (Some(start), Some(end)) = (Self::int_like_value(start), Self::int_like_value(end))
        else {
            return Err(VmError::new(VmErrorKind::InvalidRangeBounds {
                start: start.kind(),
                end: end.kind(),
            }));
        };
        Ok(build_range_items(start, end, inclusive_end))
    }

    const fn int_like_value(value: &Value) -> Option<i64> {
        match value {
            Value::Int(value) => Some(*value),
            _ => None,
        }
    }

    fn compare_range_values(
        &mut self,
        compare: &Value,
        left: &Value,
        right: &Value,
    ) -> VmResult<i64> {
        let compared = self.call_value(compare.clone(), &[left.clone(), right.clone()])?;
        Self::expect_int(&compared).map_err(|_| {
            VmError::new(VmErrorKind::InvalidRangeStep {
                detail: "compare must return Int".into(),
            })
        })
    }

    fn step_range_value(
        &mut self,
        step: &Value,
        current: &Value,
        is_ascending: bool,
        compare: &Value,
    ) -> VmResult<Value> {
        let stepped = self.call_value(step.clone(), from_ref(current))?;
        let Some(next) = self.option_payload(stepped)? else {
            return Err(VmError::new(VmErrorKind::InvalidRangeStep {
                detail: "step returned .None before terminal".into(),
            }));
        };
        let progress = self.compare_range_values(compare, &next, current)?;
        let makes_progress = if is_ascending {
            progress > 0
        } else {
            progress < 0
        };
        if !makes_progress {
            return Err(VmError::new(VmErrorKind::InvalidRangeStep {
                detail: "step did not make progress".into(),
            }));
        }
        Ok(next)
    }

    fn option_payload(&self, value: Value) -> VmResult<Option<Value>> {
        let data = Self::expect_data(value).map_err(|_| {
            VmError::new(VmErrorKind::InvalidRangeStep {
                detail: "step must return Option-like data".into(),
            })
        })?;
        let data = data.borrow();
        let Some(layout) = self
            .loaded_modules
            .iter()
            .find_map(|module| module.program.type_data_layout(data.ty))
        else {
            return Err(VmError::new(VmErrorKind::InvalidRangeStep {
                detail: "step result data layout missing".into(),
            }));
        };
        let variant = layout
            .variants
            .get(usize::try_from(data.tag).unwrap_or(usize::MAX))
            .ok_or_else(|| {
                VmError::new(VmErrorKind::InvalidRangeStep {
                    detail: "step result tag invalid".into(),
                })
            })?;
        match variant.name.as_ref() {
            "None" => Ok(None),
            "Some" => Ok(data.fields.first().cloned()),
            _ => Err(VmError::new(VmErrorKind::InvalidRangeStep {
                detail: "step result must use None/Some variants".into(),
            })),
        }
    }
}

fn build_range_items(start: i64, end: i64, inclusive_end: bool) -> Vec<Value> {
    let mut current = start;
    let mut items = Vec::<Value>::new();
    if start <= end {
        while current < end || (inclusive_end && current == end) {
            items.push(Value::Int(current));
            current = current.saturating_add(1);
        }
    } else {
        while current > end || (inclusive_end && current == end) {
            items.push(Value::Int(current));
            current = current.saturating_sub(1);
        }
    }
    items
}
