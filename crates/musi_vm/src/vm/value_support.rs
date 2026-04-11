use std::rc::Rc;

use music_seam::descriptor::ConstantValue;
use music_term::SyntaxTerm;

use super::{
    ClosureValuePtr, DataValuePtr, SeqValuePtr, Value, VmError, VmErrorKind, VmResult, VmValueKind,
};

use super::Vm;

impl Vm {
    pub(crate) fn constant_value(&self, module_slot: usize, value: &ConstantValue) -> Value {
        match value {
            ConstantValue::Int(value) => Value::Int(*value),
            ConstantValue::Float(value) => Value::Float(*value),
            ConstantValue::Bool(value) => Value::Bool(*value),
            ConstantValue::String(id) => Value::string(
                self.module(module_slot)
                    .map_or("", |module| module.program.string_text(*id)),
            ),
            ConstantValue::Syntax { shape, text } => {
                let text = self
                    .module(module_slot)
                    .map_or("", |module| module.program.string_text(*text));
                let term = SyntaxTerm::parse(*shape, text)
                    .expect("artifact syntax constants must carry valid syntax fragments");
                Value::syntax(term)
            }
        }
    }

    pub(crate) const fn expect_int(value: &Value) -> VmResult<i64> {
        match value {
            Value::Int(value) => Ok(*value),
            _ => Err(Self::invalid_value_kind(VmValueKind::Int, value)),
        }
    }

    pub(crate) const fn expect_float(value: &Value) -> VmResult<f64> {
        match value {
            Value::Float(value) => Ok(*value),
            _ => Err(Self::invalid_value_kind(VmValueKind::Float, value)),
        }
    }

    pub(crate) fn expect_string_value(value: Value) -> VmResult<Rc<str>> {
        match value {
            Value::String(text) => Ok(text),
            other => Err(Self::invalid_value_kind(VmValueKind::String, &other)),
        }
    }

    pub(crate) fn expect_seq(value: Value) -> VmResult<SeqValuePtr> {
        match value {
            Value::Seq(seq) => Ok(seq),
            other => Err(Self::invalid_value_kind(VmValueKind::Seq, &other)),
        }
    }

    pub(crate) fn expect_data(value: Value) -> VmResult<DataValuePtr> {
        match value {
            Value::Data(data) => Ok(data),
            other => Err(Self::invalid_value_kind(VmValueKind::Data, &other)),
        }
    }

    pub(crate) fn expect_closure(value: Value) -> VmResult<ClosureValuePtr> {
        match value {
            Value::Closure(closure) => Ok(closure),
            other => Err(Self::invalid_value_kind(VmValueKind::Closure, &other)),
        }
    }

    pub(crate) fn get_nested_seq(seq: SeqValuePtr, indices: &[i64]) -> VmResult<Value> {
        let mut current = seq;
        for (index_pos, index) in indices.iter().enumerate() {
            let next = {
                let current = current.borrow();
                let slot = usize::try_from(*index).unwrap_or(usize::MAX);
                current.items.get(slot).cloned().ok_or_else(|| {
                    VmError::new(VmErrorKind::InvalidSequenceIndex {
                        index: *index,
                        len: current.items.len(),
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

    pub(crate) fn set_nested_seq(seq: SeqValuePtr, indices: &[i64], value: Value) -> VmResult {
        let Some((last, prefix)) = indices.split_last() else {
            return Err(VmError::new(VmErrorKind::EmptySequenceIndexList));
        };
        let mut current = seq;
        for index in prefix {
            let next = {
                let current_ref = current.borrow();
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
        let mut current = current.borrow_mut();
        let len = current.items.len();
        let slot_index = usize::try_from(*last).unwrap_or(usize::MAX);
        let slot = current
            .items
            .get_mut(slot_index)
            .ok_or_else(|| VmError::new(VmErrorKind::InvalidSequenceIndex { index: *last, len }))?;
        *slot = value;
        Ok(())
    }

    pub(crate) const fn invalid_value_kind(expected: VmValueKind, value: &Value) -> VmError {
        VmError::new(VmErrorKind::InvalidValueKind {
            expected,
            found: value.kind(),
        })
    }
}
