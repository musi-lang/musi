use std::rc::Rc;

use music_seam::descriptor::ConstantValue;
use music_term::SyntaxTerm;

use super::{
    ClosureValuePtr, DataValuePtr, SeqValuePtr, Value, VmError, VmErrorKind, VmResult, VmValueKind,
};

use super::Vm;

impl Vm {
    pub(crate) fn constant_value(
        &self,
        module_slot: usize,
        value: &ConstantValue,
    ) -> VmResult<Value> {
        Ok(match value {
            ConstantValue::Int(value) => Value::Int(*value),
            ConstantValue::Float(value) => Value::Float(*value),
            ConstantValue::Bool(value) => self.bool_value(module_slot, *value)?,
            ConstantValue::String(id) => Value::string(
                self.module(module_slot)
                    .map_or("", |module| module.program.string_text(*id)),
            ),
            ConstantValue::Syntax { shape, text } => {
                let text = self
                    .module(module_slot)
                    .map_or("", |module| module.program.string_text(*text));
                let term = SyntaxTerm::parse(*shape, text).map_err(|detail| {
                    VmError::new(VmErrorKind::SyntaxConstantInvalid {
                        detail: detail.to_string().into(),
                    })
                })?;
                Value::syntax(term)
            }
        })
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

    pub(crate) fn bool_value(&self, module_slot: usize, value: bool) -> VmResult<Value> {
        let bool_ty = self.named_type_id(module_slot, "Bool").ok_or_else(|| {
            VmError::new(VmErrorKind::InvalidValueKind {
                expected: VmValueKind::Bool,
                found: VmValueKind::Unit,
            })
        })?;
        Ok(Value::data(bool_ty, i64::from(value), []))
    }

    pub(crate) fn bool_flag(&self, value: &Value) -> Option<bool> {
        let Value::Data(data) = value else {
            return None;
        };
        let data = data.borrow();
        (data.fields.is_empty() && self.is_named_type(data.ty, "Bool")).then_some(data.tag != 0)
    }

    pub(crate) fn named_type_id(
        &self,
        module_slot: usize,
        name: &str,
    ) -> Option<music_seam::TypeId> {
        let module = self.module(module_slot).ok()?;
        module.program.artifact().types.iter().find_map(|(id, _)| {
            let ty_name = module.program.type_name(id);
            let tail = ty_name.rsplit_once("::").map_or(ty_name, |(_, tail)| tail);
            (tail == name).then_some(id)
        })
    }

    pub(crate) fn is_range_type(&self, ty: music_seam::TypeId) -> bool {
        self.named_type_tail(ty)
            .is_some_and(|tail| tail.starts_with("Range["))
    }

    pub(crate) fn is_named_type(&self, ty: music_seam::TypeId, expected: &str) -> bool {
        self.named_type_tail(ty) == Some(expected)
    }

    fn named_type_tail(&self, ty: music_seam::TypeId) -> Option<&str> {
        self.loaded_modules.iter().find_map(|module| {
            let ty_name = module.program.type_name(ty);
            let tail = ty_name.rsplit_once("::").map_or(ty_name, |(_, tail)| tail);
            (!tail.is_empty()).then_some(tail)
        })
    }
}
