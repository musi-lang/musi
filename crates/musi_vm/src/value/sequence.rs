use std::mem::size_of;

use music_seam::TypeId;

use crate::value::ids::GcRef;
use crate::value::scalar::{Value, ValueList};

const MIN_I64_ARRAY_BUFFER_LEN: usize = 16;

#[derive(Debug, Clone)]
pub struct I64ArrayValue {
    cells: Box<[i64]>,
    is_shared: bool,
}

impl I64ArrayValue {
    #[must_use]
    pub(crate) fn new(cells: impl Into<Box<[i64]>>) -> Self {
        Self {
            cells: cells.into(),
            is_shared: false,
        }
    }

    #[must_use]
    pub(crate) fn shared(cells: impl Into<Box<[i64]>>) -> Self {
        Self {
            cells: cells.into(),
            is_shared: true,
        }
    }

    #[must_use]
    pub(crate) fn len(&self) -> usize {
        self.cells.len()
    }

    #[must_use]
    pub(crate) fn bytes(&self) -> usize {
        self.cells.len().saturating_mul(size_of::<i64>())
    }

    #[must_use]
    pub(crate) fn get(&self, index: usize) -> Option<i64> {
        self.cells.get(index).copied()
    }

    pub(crate) fn set(&mut self, index: usize, value: i64) -> Option<()> {
        if self.is_shared {
            return None;
        }
        *self.cells.get_mut(index)? = value;
        Some(())
    }

    #[must_use]
    pub(crate) fn values(&self) -> &[i64] {
        &self.cells
    }

    #[must_use]
    pub(crate) const fn is_shared(&self) -> bool {
        self.is_shared
    }
}

#[derive(Debug, Clone)]
pub struct SequenceValue {
    pub(crate) ty: TypeId,
    storage: SequenceStorage,
    layout_version: u32,
}

#[derive(Debug, Clone)]
enum SequenceStorage {
    Values(Box<ValueList>),
    IntPair([i64; 2]),
    IntArray { buffer: GcRef, len: usize },
}

impl SequenceValue {
    #[must_use]
    pub fn new(ty: TypeId, items: ValueList) -> Self {
        let storage = int_pair_storage(&items).map_or_else(
            || SequenceStorage::Values(Box::new(items)),
            SequenceStorage::IntPair,
        );
        Self {
            ty,
            storage,
            layout_version: 0,
        }
    }

    #[must_use]
    pub(crate) const fn from_i64_array(ty: TypeId, buffer: GcRef, len: usize) -> Self {
        Self {
            ty,
            storage: SequenceStorage::IntArray { buffer, len },
            layout_version: 0,
        }
    }

    #[must_use]
    pub(crate) fn len(&self) -> usize {
        match &self.storage {
            SequenceStorage::Values(items) => items.len(),
            SequenceStorage::IntPair(_) => 2,
            SequenceStorage::IntArray { len, .. } => *len,
        }
    }

    #[must_use]
    pub(crate) fn inline_bytes(&self) -> usize {
        match &self.storage {
            SequenceStorage::Values(items) => items.len().saturating_mul(size_of::<Value>()),
            SequenceStorage::IntPair(_) => 2_usize.saturating_mul(size_of::<i64>()),
            SequenceStorage::IntArray { .. } => {
                size_of::<GcRef>().saturating_add(size_of::<usize>())
            }
        }
    }

    #[must_use]
    pub(crate) const fn layout_version(&self) -> u32 {
        self.layout_version
    }

    #[must_use]
    pub(crate) const fn i64_array(&self) -> Option<(GcRef, usize)> {
        match &self.storage {
            SequenceStorage::IntArray { buffer, len } => Some((*buffer, *len)),
            SequenceStorage::Values(_) | SequenceStorage::IntPair(_) => None,
        }
    }

    pub(crate) fn visit_heap_children(&self, mut visit: impl FnMut(GcRef)) {
        match &self.storage {
            SequenceStorage::Values(items) => {
                for child in items.iter().filter_map(Value::gc_ref) {
                    visit(child);
                }
            }
            SequenceStorage::IntArray { buffer, .. } => visit(*buffer),
            SequenceStorage::IntPair(_) => {}
        }
    }

    pub(crate) fn has_heap_children(&self) -> bool {
        match &self.storage {
            SequenceStorage::Values(items) => items.iter().any(|value| value.gc_ref().is_some()),
            SequenceStorage::IntArray { .. } => true,
            SequenceStorage::IntPair(_) => false,
        }
    }

    #[must_use]
    pub(crate) fn get_cloned(&self, index: usize) -> Option<Value> {
        match &self.storage {
            SequenceStorage::Values(items) => items.get(index).cloned(),
            SequenceStorage::IntPair(cells) => cells.get(index).copied().map(Value::Int),
            SequenceStorage::IntArray { .. } => None,
        }
    }

    pub(crate) fn set(&mut self, index: usize, value: Value) -> Option<()> {
        match (&mut self.storage, value) {
            (SequenceStorage::Values(items), value) => {
                *items.as_mut().get_mut(index)? = value;
                self.layout_version = self.layout_version.wrapping_add(1);
                Some(())
            }
            (SequenceStorage::IntPair(cells), Value::Int(value)) => {
                *cells.get_mut(index)? = value;
                Some(())
            }
            (SequenceStorage::IntPair(cells), value) => {
                let mut items: ValueList = cells.iter().copied().map(Value::Int).collect();
                *items.get_mut(index)? = value;
                self.storage = SequenceStorage::Values(Box::new(items));
                self.layout_version = self.layout_version.wrapping_add(1);
                Some(())
            }
            (SequenceStorage::IntArray { .. }, _) => None,
        }
    }

    pub(crate) fn replace_with_values(&mut self, items: ValueList) {
        self.storage = SequenceStorage::Values(Box::new(items));
        self.layout_version = self.layout_version.wrapping_add(1);
    }

    #[must_use]
    pub(crate) const fn int_pair(&self) -> Option<[i64; 2]> {
        match &self.storage {
            SequenceStorage::IntPair(cells) => Some(*cells),
            SequenceStorage::Values(_) | SequenceStorage::IntArray { .. } => None,
        }
    }

    pub(crate) const fn int_pair_mut(&mut self) -> Option<&mut [i64; 2]> {
        let SequenceStorage::IntPair(cells) = &mut self.storage else {
            return None;
        };
        Some(cells)
    }

    pub(crate) fn values_mut(&mut self) -> Option<&mut ValueList> {
        match &mut self.storage {
            SequenceStorage::Values(items) => Some(items.as_mut()),
            SequenceStorage::IntPair(_) | SequenceStorage::IntArray { .. } => None,
        }
    }

    #[cfg(test)]
    pub(crate) fn push_value(&mut self, value: Value) {
        if let SequenceStorage::IntPair(cells) = self.storage {
            self.storage =
                SequenceStorage::Values(Box::new(cells.iter().copied().map(Value::Int).collect()));
            self.layout_version = self.layout_version.wrapping_add(1);
        }
        if let SequenceStorage::Values(items) = &mut self.storage {
            items.as_mut().push(value);
        }
    }

    pub(crate) fn clear_edges(&mut self) {
        match &mut self.storage {
            SequenceStorage::Values(items) => items.as_mut().clear(),
            SequenceStorage::IntArray { len, .. } => *len = 0,
            SequenceStorage::IntPair(_) => {}
        }
    }

    pub(crate) fn take_i64_array_cells(self) -> Result<(TypeId, Vec<i64>), Self> {
        let Self {
            ty,
            storage,
            layout_version,
        } = self;
        let SequenceStorage::Values(items) = storage else {
            return Err(Self {
                ty,
                storage,
                layout_version,
            });
        };
        if items.len() < MIN_I64_ARRAY_BUFFER_LEN
            || !items.iter().all(|value| matches!(value, Value::Int(_)))
        {
            return Err(Self {
                ty,
                storage: SequenceStorage::Values(items),
                layout_version,
            });
        }
        let mut cells = Vec::with_capacity(items.len());
        for value in items.iter() {
            if let Value::Int(value) = value {
                cells.push(*value);
            }
        }
        Ok((ty, cells))
    }
}

fn int_pair_storage(items: &ValueList) -> Option<[i64; 2]> {
    let [Value::Int(first), Value::Int(second)] = items.as_slice() else {
        return None;
    };
    Some([*first, *second])
}
