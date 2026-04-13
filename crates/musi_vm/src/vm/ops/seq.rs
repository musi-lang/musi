use std::rc::Rc;
use std::slice::from_ref;

use music_seam::{Instruction, Opcode, Operand, TypeId};

use crate::VmValueKind;
use crate::value::DataValuePtr;

use super::{StepOutcome, Value, Vm, VmError, VmErrorKind, VmResult};

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
        let kind = Self::decode_range_kind(len)?;
        let upper = self.pop_value()?;
        let lower = self.pop_value()?;
        let fields = match kind {
            RuntimeRangeKind::Open | RuntimeRangeKind::Closed => vec![lower, upper],
            RuntimeRangeKind::From => {
                let evidence = self.pop_value()?;
                let (_, default_upper) = Self::range_bounds_dictionary(evidence)?;
                vec![self.call_value(default_upper, &[])?, lower]
            }
            RuntimeRangeKind::UpTo | RuntimeRangeKind::Thru => {
                let evidence = self.pop_value()?;
                let (default_lower, _) = Self::range_bounds_dictionary(evidence)?;
                vec![self.call_value(default_lower, &[])?, upper]
            }
        };
        self.push_value(Value::data(ty, 0, fields))?;
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

    fn range_parts(&self, range: Value) -> VmResult<(Value, Value, RuntimeRangeKind)> {
        let data = Self::expect_data(range)?;
        let data = data.borrow();
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

    fn range_dictionary(evidence: Value) -> VmResult<(Value, Value, Value)> {
        let data = Self::range_evidence_data(evidence, 3, "range dictionary field count")?;
        let data = data.borrow();
        Ok((
            data.fields[0].clone(),
            data.fields[1].clone(),
            data.fields[2].clone(),
        ))
    }

    fn range_bounds_dictionary(evidence: Value) -> VmResult<(Value, Value)> {
        let data = Self::range_evidence_data(evidence, 2, "range bounds dictionary field count")?;
        let data = data.borrow();
        Ok((data.fields[0].clone(), data.fields[1].clone()))
    }

    fn range_evidence_data(
        evidence: Value,
        min_fields: usize,
        detail: &'static str,
    ) -> VmResult<DataValuePtr> {
        let found = evidence.kind();
        let data = Self::expect_data(evidence)
            .map_err(|_| VmError::new(VmErrorKind::InvalidRangeEvidence { found }))?;
        if data.borrow().fields.len() < min_fields {
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
        let range_ty = data.borrow().ty;
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
