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

    pub(super) fn visit_children(&self, mut visit: impl FnMut(&Value)) {
        match self {
            Self::Seq(value) => {
                for child in &value.items {
                    visit(child);
                }
            }
            Self::Data(value) => {
                for child in &value.fields {
                    visit(child);
                }
            }
            Self::Closure(value) => {
                for child in &value.captures {
                    visit(child);
                }
            }
            Self::Continuation(value) => {
                for frame in &value.frames {
                    for child in frame.locals.iter().chain(frame.stack.iter()) {
                        visit(child);
                    }
                }
                for handler in &value.handlers {
                    visit(&handler.handler);
                }
            }
            Self::String(_) | Self::Syntax(_) | Self::Module(_) => {}
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
