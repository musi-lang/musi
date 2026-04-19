use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::mem::size_of;
use std::ptr::from_ref;
use std::rc::{Rc, Weak};

use music_term::SyntaxTerm;

use crate::error::{VmError, VmErrorKind};
use crate::types::VmResult;
use crate::value::{ClosureValue, ContinuationValue, DataValue, ModuleValue, SequenceValue, Value};

const VALUE_BYTES: usize = size_of::<Value>();
const OBJECT_HEADER_BYTES: usize = 16;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeapOptions {
    pub max_object_bytes: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum HeapObjectKey {
    String { address: usize, len: usize },
    Syntax { address: usize },
    Seq { address: usize },
    Data { address: usize },
    Closure { address: usize },
    Module { address: usize },
    Continuation { address: usize },
}

#[derive(Debug, Clone)]
enum HeapObjectRecord {
    String {
        bytes: usize,
    },
    Syntax {
        bytes: usize,
        inner: Weak<SyntaxTerm>,
    },
    Seq {
        bytes: usize,
        inner: Weak<RefCell<SequenceValue>>,
    },
    Data {
        bytes: usize,
        inner: Weak<RefCell<DataValue>>,
    },
    Closure {
        bytes: usize,
        inner: Weak<RefCell<ClosureValue>>,
    },
    Module {
        bytes: usize,
        inner: Weak<ModuleValue>,
    },
    Continuation {
        bytes: usize,
        inner: Weak<ContinuationValue>,
    },
}

impl HeapObjectRecord {
    const fn bytes(&self) -> usize {
        match self {
            Self::String { bytes }
            | Self::Syntax { bytes, .. }
            | Self::Seq { bytes, .. }
            | Self::Data { bytes, .. }
            | Self::Closure { bytes, .. }
            | Self::Module { bytes, .. }
            | Self::Continuation { bytes, .. } => *bytes,
        }
    }

    fn break_edges(&self) {
        match self {
            Self::Seq { inner, .. } => {
                if let Some(inner) = inner.upgrade() {
                    inner.borrow_mut().items.clear();
                }
            }
            Self::Data { inner, .. } => {
                if let Some(inner) = inner.upgrade() {
                    inner.borrow_mut().fields.clear();
                }
            }
            Self::Closure { inner, .. } => {
                if let Some(inner) = inner.upgrade() {
                    inner.borrow_mut().captures.clear();
                }
            }
            Self::String { .. }
            | Self::Syntax { .. }
            | Self::Module { .. }
            | Self::Continuation { .. } => {}
        }
    }

    fn is_alive_by_rust_refs(&self) -> bool {
        match self {
            Self::String { .. } => true,
            Self::Syntax { inner, .. } => inner.strong_count() > 0,
            Self::Seq { inner, .. } => inner.strong_count() > 0,
            Self::Data { inner, .. } => inner.strong_count() > 0,
            Self::Closure { inner, .. } => inner.strong_count() > 0,
            Self::Module { inner, .. } => inner.strong_count() > 0,
            Self::Continuation { inner, .. } => inner.strong_count() > 0,
        }
    }
}

#[derive(Debug, Default)]
pub struct RuntimeHeap {
    records: HashMap<HeapObjectKey, HeapObjectRecord>,
    allocated_bytes: usize,
}

impl RuntimeHeap {
    #[must_use]
    pub(super) const fn allocated_bytes(&self) -> usize {
        self.allocated_bytes
    }

    pub(super) fn observe_value(&mut self, source: &Value, options: &HeapOptions) -> VmResult {
        self.observe_value_inner(source, options, &mut HashSet::new())
    }

    pub(super) fn collect_from_roots<'a>(
        &mut self,
        roots: impl IntoIterator<Item = &'a Value>,
    ) -> HeapCollectionStats {
        let mut marked = HashSet::new();
        for root in roots {
            mark_value(root, &mut marked);
        }

        let before_bytes = self.allocated_bytes;
        let before_objects = self.records.len();
        let mut reclaimed_bytes = 0usize;
        self.records.retain(|key, record| {
            let keep = marked.contains(key) && record.is_alive_by_rust_refs();
            if keep {
                true
            } else {
                record.break_edges();
                reclaimed_bytes = reclaimed_bytes.saturating_add(record.bytes());
                false
            }
        });
        self.allocated_bytes = self.allocated_bytes.saturating_sub(reclaimed_bytes);
        HeapCollectionStats {
            before_bytes,
            after_bytes: self.allocated_bytes,
            before_objects,
            after_objects: self.records.len(),
        }
    }

    fn observe_value_inner(
        &mut self,
        source: &Value,
        options: &HeapOptions,
        seen: &mut HashSet<HeapObjectKey>,
    ) -> VmResult {
        let Some((key, record)) = heap_record(source) else {
            return Ok(());
        };
        if !seen.insert(key) {
            return Ok(());
        }

        let bytes = record.bytes();
        if options.max_object_bytes.is_some_and(|limit| bytes > limit) {
            return Err(VmError::new(VmErrorKind::HeapObjectTooLarge {
                bytes,
                limit: options.max_object_bytes.unwrap_or_default(),
            }));
        }
        if self.records.insert(key, record).is_none() {
            self.allocated_bytes = self.allocated_bytes.saturating_add(bytes);
        }

        trace_children(source, |child| {
            self.observe_value_inner(child, options, seen)
        })?;
        Ok(())
    }
}

/// Runtime heap collection counters.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeapCollectionStats {
    /// Estimated live heap bytes before collection.
    pub before_bytes: usize,
    /// Estimated live heap bytes after collection.
    pub after_bytes: usize,
    /// Tracked heap objects before collection.
    pub before_objects: usize,
    /// Tracked heap objects after collection.
    pub after_objects: usize,
}

fn heap_record(source: &Value) -> Option<(HeapObjectKey, HeapObjectRecord)> {
    match source {
        Value::String(text) => {
            let address = text.as_ptr().addr();
            let len = text.len();
            let bytes = OBJECT_HEADER_BYTES.saturating_add(len);
            Some((
                HeapObjectKey::String { address, len },
                HeapObjectRecord::String { bytes },
            ))
        }
        Value::Syntax(inner) => {
            let address = from_ref::<SyntaxTerm>(inner.as_ref()).addr();
            let bytes = OBJECT_HEADER_BYTES.saturating_add(inner.text().len());
            Some((
                HeapObjectKey::Syntax { address },
                HeapObjectRecord::Syntax {
                    bytes,
                    inner: Rc::downgrade(inner),
                },
            ))
        }
        Value::Seq(inner) => {
            let address = inner.as_ptr().addr();
            let bytes = OBJECT_HEADER_BYTES
                .saturating_add(inner.borrow().items.len().saturating_mul(VALUE_BYTES));
            Some((
                HeapObjectKey::Seq { address },
                HeapObjectRecord::Seq {
                    bytes,
                    inner: Rc::downgrade(inner),
                },
            ))
        }
        Value::Data(inner) => {
            let address = inner.as_ptr().addr();
            let bytes = OBJECT_HEADER_BYTES
                .saturating_add(inner.borrow().fields.len().saturating_mul(VALUE_BYTES));
            Some((
                HeapObjectKey::Data { address },
                HeapObjectRecord::Data {
                    bytes,
                    inner: Rc::downgrade(inner),
                },
            ))
        }
        Value::Closure(inner) => {
            let address = inner.as_ptr().addr();
            let bytes = OBJECT_HEADER_BYTES
                .saturating_add(inner.borrow().captures.len().saturating_mul(VALUE_BYTES));
            Some((
                HeapObjectKey::Closure { address },
                HeapObjectRecord::Closure {
                    bytes,
                    inner: Rc::downgrade(inner),
                },
            ))
        }
        Value::Module(inner) => {
            let address = from_ref::<ModuleValue>(inner.as_ref()).addr();
            let bytes = OBJECT_HEADER_BYTES.saturating_add(inner.spec.len());
            Some((
                HeapObjectKey::Module { address },
                HeapObjectRecord::Module {
                    bytes,
                    inner: Rc::downgrade(inner),
                },
            ))
        }
        Value::Continuation(inner) => {
            let address = from_ref::<ContinuationValue>(inner.as_ref()).addr();
            let frame_values = inner
                .frames
                .iter()
                .map(|frame| frame.locals.len().saturating_add(frame.stack.len()))
                .sum::<usize>();
            let handler_values = inner.handlers.len();
            let bytes = OBJECT_HEADER_BYTES.saturating_add(
                frame_values
                    .saturating_add(handler_values)
                    .saturating_mul(VALUE_BYTES),
            );
            Some((
                HeapObjectKey::Continuation { address },
                HeapObjectRecord::Continuation {
                    bytes,
                    inner: Rc::downgrade(inner),
                },
            ))
        }
        Value::Unit
        | Value::Int(_)
        | Value::Nat(_)
        | Value::Float(_)
        | Value::CPtr(_)
        | Value::Type(_)
        | Value::Foreign(_)
        | Value::Effect(_)
        | Value::Class(_) => None,
    }
}

fn mark_value(source: &Value, marked: &mut HashSet<HeapObjectKey>) {
    let Some((key, _record)) = heap_record(source) else {
        return;
    };
    if !marked.insert(key) {
        return;
    }
    trace_children(source, |child| {
        mark_value(child, marked);
        Ok(())
    })
    .expect("marking children should not fail");
}

fn trace_children(source: &Value, mut visit: impl FnMut(&Value) -> VmResult) -> VmResult {
    match source {
        Value::Seq(inner) => {
            for child in &inner.borrow().items {
                visit(child)?;
            }
        }
        Value::Data(inner) => {
            for child in &inner.borrow().fields {
                visit(child)?;
            }
        }
        Value::Closure(inner) => {
            for child in &inner.borrow().captures {
                visit(child)?;
            }
        }
        Value::Continuation(inner) => {
            for frame in &inner.frames {
                for child in &frame.locals {
                    visit(child)?;
                }
                for child in &frame.stack {
                    visit(child)?;
                }
            }
            for handler in &inner.handlers {
                visit(&handler.handler)?;
            }
        }
        Value::Unit
        | Value::Int(_)
        | Value::Nat(_)
        | Value::Float(_)
        | Value::String(_)
        | Value::CPtr(_)
        | Value::Syntax(_)
        | Value::Module(_)
        | Value::Foreign(_)
        | Value::Type(_)
        | Value::Effect(_)
        | Value::Class(_) => {}
    }
    Ok(())
}
