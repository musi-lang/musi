use music_seam::TypeId;
use music_seam::descriptor::ConstantValue;
use music_term::SyntaxTerm;

use super::{GcRef, Value, VmError, VmErrorKind, VmResult, VmValueKind};

use super::Vm;

impl Vm {
    pub(crate) fn constant_value(
        &mut self,
        module_slot: usize,
        value: &ConstantValue,
    ) -> VmResult<Value> {
        Ok(match value {
            ConstantValue::Int(value) => Value::Int(*value),
            ConstantValue::Float(value) => Value::Float(*value),
            ConstantValue::Bool(value) => self.bool_value(module_slot, *value)?,
            ConstantValue::String(id) => {
                let text = self
                    .module(module_slot)
                    .map_or("", |module| module.program.string_text(*id))
                    .to_owned();
                self.alloc_string(text)?
            }
            ConstantValue::Syntax { shape, text } => {
                let text = self
                    .module(module_slot)
                    .map_or("", |module| module.program.string_text(*text));
                let term = SyntaxTerm::parse(*shape, text).map_err(|detail| {
                    VmError::new(VmErrorKind::InvalidSyntaxConstant {
                        detail: detail.to_string().into(),
                    })
                })?;
                self.alloc_syntax(term)?
            }
        })
    }

    pub(crate) fn expect_int(value: &Value) -> VmResult<i64> {
        match value {
            Value::Int(value) => Ok(*value),
            Value::Nat(number) => i64::try_from(*number)
                .map_err(|_| Self::invalid_value_kind(VmValueKind::Int, value)),
            _ => Err(Self::invalid_value_kind(VmValueKind::Int, value)),
        }
    }

    pub(crate) const fn expect_float(value: &Value) -> VmResult<f64> {
        match value {
            Value::Float(value) => Ok(*value),
            _ => Err(Self::invalid_value_kind(VmValueKind::Float, value)),
        }
    }

    pub(crate) fn expect_string_value(&self, value: Value) -> VmResult<Box<str>> {
        match value {
            Value::String(text) => Ok(self.heap.string(text)?.into()),
            other => Err(Self::invalid_value_kind(VmValueKind::String, &other)),
        }
    }

    pub(crate) fn expect_seq(value: Value) -> VmResult<GcRef> {
        match value {
            Value::Seq(seq) => Ok(seq),
            other => Err(Self::invalid_value_kind(VmValueKind::Seq, &other)),
        }
    }

    pub(crate) fn expect_data(value: Value) -> VmResult<GcRef> {
        match value {
            Value::Data(data) => Ok(data),
            other => Err(Self::invalid_value_kind(VmValueKind::Data, &other)),
        }
    }

    pub(crate) fn get_nested_seq(&self, seq: GcRef, indices: &[i64]) -> VmResult<Value> {
        let mut current = seq;
        for (index_pos, index) in indices.iter().enumerate() {
            let next = {
                let current_ref = self.heap.sequence(current)?;
                let slot = usize::try_from(*index).unwrap_or(usize::MAX);
                current_ref.items.get(slot).cloned().ok_or_else(|| {
                    VmError::new(VmErrorKind::InvalidSequenceIndex {
                        index: *index,
                        len: current_ref.items.len(),
                    })
                })?
            };
            if index_pos + 1 == indices.len() {
                return Ok(next);
            }
            current = Self::expect_seq(next)?;
        }
        Err(VmError::new(VmErrorKind::EmptySequenceIndexList))
    }

    pub(crate) fn set_nested_seq(&mut self, seq: GcRef, indices: &[i64], value: Value) -> VmResult {
        let Some((last, prefix)) = indices.split_last() else {
            return Err(VmError::new(VmErrorKind::EmptySequenceIndexList));
        };
        let mut current = seq;
        for index in prefix {
            let next = {
                let current_ref = self.heap.sequence(current)?;
                let slot = usize::try_from(*index).unwrap_or(usize::MAX);
                current_ref.items.get(slot).cloned().ok_or_else(|| {
                    VmError::new(VmErrorKind::InvalidSequenceIndex {
                        index: *index,
                        len: current_ref.items.len(),
                    })
                })?
            };
            current = Self::expect_seq(next)?;
        }
        {
            let current_ref = self.heap.sequence_mut(current)?;
            let len = current_ref.items.len();
            let slot_index = usize::try_from(*last).unwrap_or(usize::MAX);
            let slot = current_ref.items.get_mut(slot_index).ok_or_else(|| {
                VmError::new(VmErrorKind::InvalidSequenceIndex { index: *last, len })
            })?;
            *slot = value;
        }
        self.heap.refresh_allocation(current)?;
        Ok(())
    }

    pub(crate) const fn invalid_value_kind(expected: VmValueKind, value: &Value) -> VmError {
        VmError::new(VmErrorKind::InvalidValueKind {
            expected,
            found: value.kind(),
        })
    }

    pub(crate) fn bool_value(&mut self, module_slot: usize, value: bool) -> VmResult<Value> {
        let bool_ty = self.named_type_id(module_slot, "Bool").ok_or_else(|| {
            VmError::new(VmErrorKind::InvalidValueKind {
                expected: VmValueKind::Bool,
                found: VmValueKind::Unit,
            })
        })?;
        self.alloc_data(bool_ty, i64::from(value), [])
    }

    pub(crate) fn bool_flag(&self, value: &Value) -> Option<bool> {
        let Value::Data(data) = value else {
            return None;
        };
        let data = self.heap.data(*data).ok()?;
        (data.fields.is_empty() && self.is_named_type(data.ty, "Bool")).then_some(data.tag != 0)
    }

    pub(crate) fn values_equal(&self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Float(left), Value::Float(right)) => left.to_bits() == right.to_bits(),
            (Value::String(left), Value::String(right)) => {
                self.heap.string(*left).ok() == self.heap.string(*right).ok()
            }
            (Value::Seq(left), Value::Seq(right)) => self
                .heap
                .sequence(*left)
                .ok()
                .zip(self.heap.sequence(*right).ok())
                .is_some_and(|(left, right)| {
                    left.items.len() == right.items.len()
                        && left
                            .items
                            .iter()
                            .zip(right.items.iter())
                            .all(|(left, right)| self.values_equal(left, right))
                }),
            (Value::Data(left), Value::Data(right)) => self
                .heap
                .data(*left)
                .ok()
                .zip(self.heap.data(*right).ok())
                .is_some_and(|(left, right)| {
                    left.ty == right.ty
                        && left.tag == right.tag
                        && left.fields.len() == right.fields.len()
                        && left
                            .fields
                            .iter()
                            .zip(right.fields.iter())
                            .all(|(left, right)| self.values_equal(left, right))
                }),
            (Value::Closure(left), Value::Closure(right))
            | (Value::Continuation(left), Value::Continuation(right)) => left == right,
            (Value::Module(left), Value::Module(right)) => self
                .heap
                .module(*left)
                .ok()
                .zip(self.heap.module(*right).ok())
                .is_some_and(|(left, right)| left.slot == right.slot),
            _ => left == right,
        }
    }

    pub(crate) fn named_type_id(&self, module_slot: usize, name: &str) -> Option<TypeId> {
        let module = self.module(module_slot).ok()?;
        module.program.artifact().types.iter().find_map(|(id, _)| {
            let ty_name = module.program.type_name(id);
            let tail = ty_name.rsplit_once("::").map_or(ty_name, |(_, tail)| tail);
            (tail == name).then_some(id)
        })
    }

    pub(crate) fn is_named_type(&self, ty: TypeId, expected: &str) -> bool {
        self.named_type_tail(ty) == Some(expected)
    }

    pub(crate) fn named_type_tail(&self, ty: TypeId) -> Option<&str> {
        self.loaded_modules.iter().find_map(|module| {
            let ty_name = module.program.type_name(ty);
            let tail = ty_name.rsplit_once("::").map_or(ty_name, |(_, tail)| tail);
            (!tail.is_empty()).then_some(tail)
        })
    }
}
