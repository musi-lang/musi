use std::mem::size_of;

use music_term::SyntaxTerm;

use crate::value::{ClosureValue, ContinuationValue, DataValue, ModuleValue, SequenceValue, Value};

const VALUE_BYTES: usize = size_of::<Value>();
const OBJECT_HEADER_BYTES: usize = 16;

#[derive(Debug, Clone)]
pub(super) enum HeapObject {
    String(Box<str>),
    Syntax(SyntaxTerm),
    Seq(SequenceValue),
    Data(DataValue),
    Closure(ClosureValue),
    Module(ModuleValue),
    Continuation(Box<ContinuationValue>),
}

impl HeapObject {
    pub(super) fn bytes(&self) -> usize {
        match self {
            Self::String(text) => OBJECT_HEADER_BYTES.saturating_add(text.len()),
            Self::Syntax(term) => OBJECT_HEADER_BYTES.saturating_add(term.text().len()),
            Self::Seq(value) => {
                OBJECT_HEADER_BYTES.saturating_add(value.items.len().saturating_mul(VALUE_BYTES))
            }
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

    pub(super) fn children(&self) -> Vec<Value> {
        match self {
            Self::Seq(value) => value.items.iter().cloned().collect(),
            Self::Data(value) => value.fields.iter().cloned().collect(),
            Self::Closure(value) => value.captures.iter().cloned().collect(),
            Self::Continuation(value) => value
                .frames
                .iter()
                .flat_map(|frame| frame.locals.iter().chain(frame.stack.iter()))
                .chain(value.handlers.iter().map(|handler| &handler.handler))
                .cloned()
                .collect(),
            Self::String(_) | Self::Syntax(_) | Self::Module(_) => Vec::new(),
        }
    }

    pub(super) fn break_edges(&mut self) {
        match self {
            Self::Seq(value) => value.items.clear(),
            Self::Data(value) => value.fields.clear(),
            Self::Closure(value) => value.captures.clear(),
            Self::Continuation(value) => {
                value.frames.clear();
                value.handlers.clear();
            }
            Self::String(_) | Self::Syntax(_) | Self::Module(_) => {}
        }
    }
}
