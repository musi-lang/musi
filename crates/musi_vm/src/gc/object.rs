use std::mem::size_of;

use music_seam::TypeId;
use music_term::SyntaxTerm;

use crate::value::{
    ClosureValue, ContinuationValue, DataValue, GcRef, IsolateId, ModuleValue, SequenceValue, Value,
};

const VALUE_BYTES: usize = size_of::<Value>();
const OBJECT_HEADER_BYTES: usize = 16;
const I64_BYTES: usize = size_of::<i64>();

#[derive(Debug, Clone, Copy)]
pub(super) struct PackedSeq2x2 {
    pub ty: TypeId,
    pub cells: [[i64; 2]; 2],
    pub row_refs: [GcRef; 2],
    pub row_map: [u8; 2],
}

impl PackedSeq2x2 {
    #[must_use]
    pub(super) fn cell_for(self, logical_row: u8, column: usize) -> i64 {
        let physical_row = usize::from(self.row_map[usize::from(logical_row)]);
        self.cells[physical_row][column]
    }

    pub(super) fn set_cell_for(&mut self, logical_row: u8, column: usize, value: i64) {
        let physical_row = usize::from(self.row_map[usize::from(logical_row)]);
        self.cells[physical_row][column] = value;
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct PackedSeq2x2Row {
    pub ty: TypeId,
    pub grid: GcRef,
    pub logical_row: u8,
}

#[derive(Debug, Clone)]
pub(super) enum HeapObject {
    String(Box<str>),
    Syntax(SyntaxTerm),
    Seq(SequenceValue),
    Data(DataValue),
    Closure(ClosureValue),
    Module(ModuleValue),
    Continuation(Box<ContinuationValue>),
    PackedSeq2x2(PackedSeq2x2),
    PackedSeq2x2Row(PackedSeq2x2Row),
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
            Self::PackedSeq2x2(_) => OBJECT_HEADER_BYTES.saturating_add(4 * I64_BYTES),
            Self::PackedSeq2x2Row(_) => OBJECT_HEADER_BYTES.saturating_add(VALUE_BYTES),
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
            Self::PackedSeq2x2(value) => {
                for row in value.row_refs {
                    visit(&Value::Seq(row));
                }
            }
            Self::PackedSeq2x2Row(value) => visit(&Value::Seq(value.grid)),
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
            Self::PackedSeq2x2(value) => {
                let dead = GcRef::new(IsolateId::new(0), usize::MAX, u32::MAX);
                value.row_refs = [dead; 2];
            }
            Self::PackedSeq2x2Row(value) => {
                value.grid = GcRef::new(IsolateId::new(0), usize::MAX, u32::MAX);
            }
            Self::String(_) | Self::Syntax(_) | Self::Module(_) => {}
        }
    }
}
