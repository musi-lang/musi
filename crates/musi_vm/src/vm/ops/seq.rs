use std::slice::from_ref;

use music_seam::{Instruction, Opcode, Operand, TypeId};

use super::{
    GcRef, RuntimeInstruction, RuntimeOperand, StepOutcome, Value, ValueList, Vm, VmError,
    VmErrorKind, VmResult,
};
use crate::VmValueKind;
use crate::value::DataValue;

const MAX_RANGE_MATERIALIZE_LEN: usize = 1_000_000;

#[derive(Clone, Copy)]
enum RuntimeRangeKind {
    Open,
    Closed,
    From,
    UpTo,
    Thru,
}

impl Vm {
    pub(crate) fn exec_seq(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::SeqNew => {
                let Operand::TypeLen { ty, len } = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                self.exec_seq_new(ty, len)
            }
            Opcode::SeqGet => self.exec_seq_get(),
            Opcode::SeqGetN => {
                let Operand::I16(len) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                self.exec_seq_get_n(len)
            }
            Opcode::SeqSet => self.exec_seq_set(),
            Opcode::SeqSetN => {
                let Operand::I16(len) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                self.exec_seq_set_n(len)
            }
            Opcode::SeqCat => self.exec_seq_cat(),
            Opcode::SeqLen => self.exec_seq_len(),
            Opcode::RangeNew => self.exec_range_new(instruction),
            Opcode::RangeContains => self.exec_range_contains(),
            Opcode::RangeMaterialize => self.exec_range_materialize(),
            Opcode::SeqHas => self.exec_seq_has(),
            _ => Err(Self::invalid_dispatch(instruction, "sequence")),
        }
    }

    pub(crate) fn exec_fast_seq(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        match runtime.opcode {
            Opcode::SeqNew => {
                let RuntimeOperand::TypeLen { ty, len } = runtime.operand else {
                    let instruction = self.current_raw_instruction(runtime.raw_index)?;
                    return Err(Self::invalid_operand(&instruction));
                };
                self.exec_seq_new(ty, len)
            }
            Opcode::SeqGet => self.exec_seq_get(),
            Opcode::SeqGetN => {
                let RuntimeOperand::I16(len) = runtime.operand else {
                    let instruction = self.current_raw_instruction(runtime.raw_index)?;
                    return Err(Self::invalid_operand(&instruction));
                };
                self.exec_seq_get_n(len)
            }
            Opcode::SeqSet => self.exec_seq_set(),
            Opcode::SeqSetN => {
                let RuntimeOperand::I16(len) = runtime.operand else {
                    let instruction = self.current_raw_instruction(runtime.raw_index)?;
                    return Err(Self::invalid_operand(&instruction));
                };
                self.exec_seq_set_n(len)
            }
            _ => {
                let instruction = self.current_raw_instruction(runtime.raw_index)?;
                Err(Self::invalid_dispatch(&instruction, "sequence"))
            }
        }
    }

    fn exec_seq_new(&mut self, ty: TypeId, len: u16) -> VmResult<StepOutcome> {
        let items = self.pop_args(usize::from(len))?;
        let value = self.alloc_sequence_owned(ty, items)?;
        self.push_value(value)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_seq_get(&mut self) -> VmResult<StepOutcome> {
        let index_value = self.pop_value()?;
        let index = Self::expect_int(&index_value)?;
        let seq_value = self.pop_value()?;
        let seq = Self::expect_seq(seq_value)?;
        let slot = usize::try_from(index).unwrap_or(usize::MAX);
        let len = self.heap.sequence_len(seq)?;
        let value = self
            .heap
            .sequence_get_cloned(seq, slot)
            .map_err(|_| VmError::new(VmErrorKind::InvalidSequenceIndex { index, len }))?;
        self.push_value(value)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_seq_get_n(&mut self, len: i16) -> VmResult<StepOutcome> {
        let value = if len == 2 {
            let second = self.pop_int_index()?;
            let first = self.pop_int_index()?;
            let seq_value = self.pop_value()?;
            let seq = Self::expect_seq(seq_value)?;
            self.get_nested_seq2(seq, first, second)?
        } else {
            let indices = self.pop_index_list(len)?;
            let seq_value = self.pop_value()?;
            let seq = Self::expect_seq(seq_value)?;
            self.get_nested_seq(seq, &indices)?
        };
        self.push_value(value)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_seq_set(&mut self) -> VmResult<StepOutcome> {
        let value = self.pop_value()?;
        let index_value = self.pop_value()?;
        let index = Self::expect_int(&index_value)?;
        let seq_value = self.pop_value()?;
        let seq = Self::expect_seq(seq_value)?;
        let len = self.heap.sequence_len(seq)?;
        let slot = usize::try_from(index).unwrap_or(usize::MAX);
        self.heap
            .sequence_set(seq, slot, value)
            .map_err(|_| VmError::new(VmErrorKind::InvalidSequenceIndex { index, len }))?;
        self.push_value(Value::Seq(seq))?;
        Ok(StepOutcome::Continue)
    }

    fn exec_seq_set_n(&mut self, len: i16) -> VmResult<StepOutcome> {
        let value = self.pop_value()?;
        let seq = if len == 2 {
            let second = self.pop_int_index()?;
            let first = self.pop_int_index()?;
            let seq_value = self.pop_value()?;
            let seq = Self::expect_seq(seq_value)?;
            self.set_nested_seq2(seq, first, second, value)?;
            seq
        } else {
            let indices = self.pop_index_list(len)?;
            let seq_value = self.pop_value()?;
            let seq = Self::expect_seq(seq_value)?;
            self.set_nested_seq(seq, &indices, value)?;
            seq
        };
        self.push_value(Value::Seq(seq))?;
        Ok(StepOutcome::Continue)
    }

    fn exec_seq_cat(&mut self) -> VmResult<StepOutcome> {
        let right_value = self.pop_value()?;
        let right_items = self.expect_seq_items(right_value)?;
        let left_value = self.pop_value()?;
        let left = Self::expect_seq(left_value)?;
        let mut items: ValueList = self.heap.sequence_items_cloned(left)?.into_iter().collect();
        items.extend(right_items);
        let value = self.alloc_sequence(self.heap.sequence_ty(left)?, items)?;
        self.push_value(value)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_seq_len(&mut self) -> VmResult<StepOutcome> {
        let seq_value = self.pop_value()?;
        let seq = Self::expect_seq(seq_value)?;
        let len = i64::try_from(self.heap.sequence_len(seq)?).unwrap_or(i64::MAX);
        self.push_value(Value::Int(len))?;
        Ok(StepOutcome::Continue)
    }

    fn exec_range_new(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        let Operand::TypeLen { ty, len } = instruction.operand else {
            return Err(Self::invalid_operand(instruction));
        };
        let kind = Self::decode_range_kind(len)?;
        let upper = self.pop_value()?;
        let lower = self.pop_value()?;
        let fields = match kind {
            RuntimeRangeKind::Open | RuntimeRangeKind::Closed => vec![lower, upper],
            RuntimeRangeKind::From => {
                let evidence = self.pop_value()?;
                let (_, default_upper) = self.range_bounds_dictionary(evidence)?;
                vec![self.call_value(default_upper, &[])?, lower]
            }
            RuntimeRangeKind::UpTo | RuntimeRangeKind::Thru => {
                let evidence = self.pop_value()?;
                let (default_lower, _) = self.range_bounds_dictionary(evidence)?;
                vec![self.call_value(default_lower, &[])?, upper]
            }
        };
        let value = self.alloc_data(ty, 0, fields)?;
        self.push_value(value)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_range_contains(&mut self) -> VmResult<StepOutcome> {
        let needle = self.pop_value()?;
        let range = self.pop_value()?;
        let evidence = self.pop_value()?;
        let module_slot = self.current_module_slot()?;
        let contains = self.range_contains_value(range, evidence, &needle)?;
        let value = self.bool_value(module_slot, contains)?;
        self.push_value(value)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_range_materialize(&mut self) -> VmResult<StepOutcome> {
        let range = self.pop_value()?;
        let evidence = self.pop_value()?;
        let ty = self.range_sequence_type(&range)?;
        let items = self.materialize_range_items(range, evidence)?;
        let value = self.alloc_sequence(ty, items)?;
        self.push_value(value)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_seq_has(&mut self) -> VmResult<StepOutcome> {
        let needle = self.pop_value()?;
        let seq_value = self.pop_value()?;
        let contains = match seq_value {
            Value::Seq(seq) => self
                .heap
                .sequence_items_cloned(seq)?
                .iter()
                .any(|item| self.values_equal(item, &needle)),
            other => return Err(Self::invalid_value_kind(VmValueKind::Seq, &other)),
        };
        let module_slot = self.current_module_slot()?;
        let value = self.bool_value(module_slot, contains)?;
        self.push_value(value)?;
        Ok(StepOutcome::Continue)
    }

    fn pop_int_index(&mut self) -> VmResult<i64> {
        let index_value = self.pop_value()?;
        Self::expect_int(&index_value)
    }

    fn get_nested_seq2(&self, seq: GcRef, first: i64, second: i64) -> VmResult<Value> {
        let row = self.nested_row_seq(seq, first)?;
        let len = self.heap.sequence_len(row)?;
        let slot = usize::try_from(second).unwrap_or(usize::MAX);
        self.heap
            .sequence_get_cloned(row, slot)
            .map_err(|_| VmError::new(VmErrorKind::InvalidSequenceIndex { index: second, len }))
    }

    fn set_nested_seq2(&mut self, seq: GcRef, first: i64, second: i64, value: Value) -> VmResult {
        let row = self.nested_row_seq(seq, first)?;
        let len = self.heap.sequence_len(row)?;
        let slot = usize::try_from(second).unwrap_or(usize::MAX);
        self.heap
            .sequence_set(row, slot, value)
            .map_err(|_| VmError::new(VmErrorKind::InvalidSequenceIndex { len, index: second }))
    }

    fn nested_row_seq(&self, seq: GcRef, index: i64) -> VmResult<GcRef> {
        let len = self.heap.sequence_len(seq)?;
        let slot = usize::try_from(index).unwrap_or(usize::MAX);
        let Ok(value) = self.heap.sequence_get_cloned(seq, slot) else {
            return Err(VmError::new(VmErrorKind::InvalidSequenceIndex {
                index,
                len,
            }));
        };
        match &value {
            Value::Seq(row) => Ok(*row),
            other => Err(Self::invalid_value_kind(VmValueKind::Seq, other)),
        }
    }

    fn expect_seq_items(&self, value: Value) -> VmResult<Vec<Value>> {
        match value {
            Value::Seq(seq) => self.heap.sequence_items_cloned(seq),
            other => Err(Self::invalid_value_kind(VmValueKind::Seq, &other)),
        }
    }
}

impl Vm {
    fn decode_range_kind(flags: u16) -> VmResult<RuntimeRangeKind> {
        match flags {
            0 => Ok(RuntimeRangeKind::Open),
            1 => Ok(RuntimeRangeKind::Closed),
            2 => Ok(RuntimeRangeKind::From),
            3 => Ok(RuntimeRangeKind::UpTo),
            4 => Ok(RuntimeRangeKind::Thru),
            _ => Err(VmError::new(VmErrorKind::InvalidRangeStep {
                detail: "unknown range kind".into(),
            })),
        }
    }

    fn materialize_range_items(&mut self, range: Value, evidence: Value) -> VmResult<Vec<Value>> {
        let (start, end, kind) = self.range_parts(range)?;
        let (compare, next, prev) = self.range_dictionary(evidence)?;
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
            let include_end = matches!(kind, RuntimeRangeKind::Closed | RuntimeRangeKind::Thru);
            if !(before_end || include_end && at_end) {
                break;
            }
            if items.len() >= MAX_RANGE_MATERIALIZE_LEN {
                return Err(VmError::new(VmErrorKind::RangeMaterializeTooLarge {
                    len: items.len().saturating_add(1),
                    limit: MAX_RANGE_MATERIALIZE_LEN,
                }));
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

    fn range_contains_value(
        &mut self,
        range: Value,
        evidence: Value,
        needle: &Value,
    ) -> VmResult<bool> {
        let (start, end, kind) = self.range_parts(range)?;
        let (compare, next, prev) = self.range_dictionary(evidence)?;
        let direction = self.compare_range_values(&compare, &start, &end)?;
        let is_ascending = direction <= 0;
        let step = if is_ascending { next } else { prev };
        let mut current = start;
        let mut visited = 0usize;
        loop {
            let cmp_to_end = self.compare_range_values(&compare, &current, &end)?;
            let before_end = if is_ascending {
                cmp_to_end < 0
            } else {
                cmp_to_end > 0
            };
            let at_end = cmp_to_end == 0;
            let include_end = matches!(kind, RuntimeRangeKind::Closed | RuntimeRangeKind::Thru);
            if !(before_end || include_end && at_end) {
                return Ok(false);
            }
            if visited >= MAX_RANGE_MATERIALIZE_LEN {
                return Err(VmError::new(VmErrorKind::RangeMaterializeTooLarge {
                    len: visited.saturating_add(1),
                    limit: MAX_RANGE_MATERIALIZE_LEN,
                }));
            }
            if self.values_equal(&current, needle) {
                return Ok(true);
            }
            visited = visited.saturating_add(1);
            if at_end {
                return Ok(false);
            }
            current = self.step_range_value(&step, &current, is_ascending, &compare)?;
        }
    }

    fn range_parts(&self, range: Value) -> VmResult<(Value, Value, RuntimeRangeKind)> {
        let data = Self::expect_data(range)?;
        let data = self.heap.data(data)?;
        let Some(kind) = self.range_type_kind(data.ty) else {
            return Err(VmError::new(VmErrorKind::InvalidValueKind {
                expected: VmValueKind::Data,
                found: VmValueKind::Data,
            }));
        };
        let (start, end) = match kind {
            RuntimeRangeKind::From => (
                data.fields.get(1).cloned().unwrap_or(Value::Unit),
                data.fields.first().cloned().unwrap_or(Value::Unit),
            ),
            RuntimeRangeKind::Open
            | RuntimeRangeKind::Closed
            | RuntimeRangeKind::UpTo
            | RuntimeRangeKind::Thru => (
                data.fields.first().cloned().unwrap_or(Value::Unit),
                data.fields.get(1).cloned().unwrap_or(Value::Unit),
            ),
        };
        Ok((start, end, kind))
    }

    fn range_dictionary(&self, evidence: Value) -> VmResult<(Value, Value, Value)> {
        let data = self.range_evidence_data(evidence, 3, "range dictionary field count")?;
        Ok((
            data.fields[0].clone(),
            data.fields[1].clone(),
            data.fields[2].clone(),
        ))
    }

    fn range_bounds_dictionary(&self, evidence: Value) -> VmResult<(Value, Value)> {
        let data = self.range_evidence_data(evidence, 2, "range bounds dictionary field count")?;
        Ok((data.fields[0].clone(), data.fields[1].clone()))
    }

    fn range_evidence_data(
        &self,
        evidence: Value,
        min_fields: usize,
        detail: &'static str,
    ) -> VmResult<&DataValue> {
        let found = evidence.kind();
        let data = Self::expect_data(evidence)
            .map_err(|_| VmError::new(VmErrorKind::InvalidRangeEvidence { found }))?;
        let data = self.heap.data(data)?;
        if data.fields.len() < min_fields {
            return Err(VmError::new(VmErrorKind::InvalidRangeStep {
                detail: detail.into(),
            }));
        }
        Ok(data)
    }

    fn range_sequence_type(&self, range: &Value) -> VmResult<TypeId> {
        let Value::Data(data) = range else {
            return Err(Self::invalid_value_kind(VmValueKind::Data, range));
        };
        let range_ty = self.heap.data(*data)?.ty;
        let range_name = self
            .loaded_modules
            .iter()
            .find_map(|module| {
                let name = module.program.type_name(range_ty);
                Self::range_item_name(name).is_some().then_some(name)
            })
            .ok_or_else(|| {
                VmError::new(VmErrorKind::InvalidValueKind {
                    expected: VmValueKind::Seq,
                    found: VmValueKind::Unit,
                })
            })?;
        let item_name = Self::range_item_name(range_name).unwrap_or(range_name);
        let seq_name = format!("[]{item_name}");
        let module_slot = self.current_module_slot()?;
        self.named_type_id(module_slot, &seq_name).ok_or_else(|| {
            VmError::new(VmErrorKind::InvalidValueKind {
                expected: VmValueKind::Seq,
                found: VmValueKind::Unit,
            })
        })
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
        let data = self.heap.data(data)?;
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
            .iter()
            .find(|variant| variant.tag == data.tag)
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

    fn range_type_kind(&self, ty: TypeId) -> Option<RuntimeRangeKind> {
        match self.named_type_tail(ty)? {
            tail if tail.starts_with("Range[") => Some(RuntimeRangeKind::Open),
            tail if tail.starts_with("ClosedRange[") => Some(RuntimeRangeKind::Closed),
            tail if tail.starts_with("PartialRangeFrom[") => Some(RuntimeRangeKind::From),
            tail if tail.starts_with("PartialRangeUpTo[") => Some(RuntimeRangeKind::UpTo),
            tail if tail.starts_with("PartialRangeThru[") => Some(RuntimeRangeKind::Thru),
            _ => None,
        }
    }

    fn range_item_name(range_name: &str) -> Option<&str> {
        [
            "Range[",
            "ClosedRange[",
            "PartialRangeFrom[",
            "PartialRangeUpTo[",
            "PartialRangeThru[",
        ]
        .into_iter()
        .find_map(|prefix| {
            range_name
                .rsplit_once(prefix)
                .map(|(_, tail)| tail.strip_suffix(']').unwrap_or(tail))
        })
    }
}
