use std::mem::size_of;

use music_term::SyntaxTerm;

use crate::value::{
    ClosureValue, ContinuationValue, DataValue, GcRef, I64ArrayValue, ModuleValue, SequenceValue,
    Value,
};

const VALUE_BYTES: usize = size_of::<Value>();
const OBJECT_HEADER_BYTES: usize = 16;

#[derive(Debug, Clone)]
pub(super) enum HeapObject {
    String(Box<str>),
    Syntax(SyntaxTerm),
    Seq(SequenceValue),
    I64Array(I64ArrayValue),
    Data(DataValue),
    Closure(ClosureValue),
    Module(ModuleValue),
    Continuation(Box<ContinuationValue>),
}

impl HeapObject {
    pub(super) const fn sequence_i64_array_bytes() -> usize {
        OBJECT_HEADER_BYTES + size_of::<GcRef>() + size_of::<usize>()
    }

    pub(super) fn bytes(&self) -> usize {
        match self {
            Self::String(text) => OBJECT_HEADER_BYTES.saturating_add(text.len()),
            Self::Syntax(term) => OBJECT_HEADER_BYTES.saturating_add(term.text().len()),
            Self::Seq(value) => OBJECT_HEADER_BYTES.saturating_add(value.inline_bytes()),
            Self::I64Array(value) => OBJECT_HEADER_BYTES.saturating_add(value.bytes()),
            Self::Data(value) => {
                OBJECT_HEADER_BYTES.saturating_add(value.fields.len().saturating_mul(VALUE_BYTES))
            }
            Self::Closure(value) => {
                OBJECT_HEADER_BYTES.saturating_add(value.captures.len().saturating_mul(VALUE_BYTES))
            }
            Self::Module(value) => OBJECT_HEADER_BYTES.saturating_add(value.spec.len()),
            Self::Continuation(value) => {
                let frame_values = value
                    .frames
                    .iter()
                    .map(|frame| frame.locals.len().saturating_add(frame.stack.len()))
                    .sum::<usize>();
                let value_count = frame_values.saturating_add(value.handlers.len());
                OBJECT_HEADER_BYTES.saturating_add(value_count.saturating_mul(VALUE_BYTES))
            }
        }
    }

    pub(super) fn visit_children(&self, mut visit: impl FnMut(GcRef)) {
        match self {
            Self::Seq(value) => value.visit_heap_children(&mut visit),
            Self::Data(value) => {
                for child in value.fields.iter().filter_map(Value::gc_ref) {
                    visit(child);
                }
            }
            Self::Closure(value) => {
                for child in value.captures.iter().filter_map(Value::gc_ref) {
                    visit(child);
                }
            }
            Self::Continuation(value) => {
                for frame in &value.frames {
                    for child in frame
                        .locals
                        .iter()
                        .chain(frame.stack.iter())
                        .filter_map(Value::gc_ref)
                    {
                        visit(child);
                    }
                }
                for handler in &value.handlers {
                    if let Some(reference) = handler.handler.gc_ref() {
                        visit(reference);
                    }
                }
            }
            Self::String(_) | Self::Syntax(_) | Self::I64Array(_) | Self::Module(_) => {}
        }
    }

    pub(super) fn has_children(&self) -> bool {
        match self {
            Self::Seq(value) => value.has_heap_children(),
            Self::Data(value) => value.fields.iter().any(|value| value.gc_ref().is_some()),
            Self::Closure(value) => value.captures.iter().any(|value| value.gc_ref().is_some()),
            Self::Continuation(value) => {
                value.frames.iter().any(|frame| {
                    frame
                        .locals
                        .iter()
                        .chain(frame.stack.iter())
                        .any(|value| value.gc_ref().is_some())
                }) || value
                    .handlers
                    .iter()
                    .any(|handler| handler.handler.gc_ref().is_some())
            }
            Self::String(_) | Self::Syntax(_) | Self::I64Array(_) | Self::Module(_) => false,
        }
    }

    pub(super) fn break_edges(&mut self) {
        match self {
            Self::Seq(value) => value.clear_edges(),
            Self::Data(value) => value.fields.clear(),
            Self::Closure(value) => value.captures.clear(),
            Self::Continuation(value) => {
                value.frames.clear();
                value.handlers.clear();
            }
            Self::String(_) | Self::Syntax(_) | Self::I64Array(_) | Self::Module(_) => {}
        }
    }
}
