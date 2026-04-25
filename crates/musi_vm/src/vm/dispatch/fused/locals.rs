use super::super::frame::local_index_error;
use super::super::int::local_int_from_frame;
use super::super::sequence_index::sequence_index;
use super::super::*;

impl Vm {
    pub(super) fn local_value(&self, local: u16) -> VmResult<Value> {
        let frame = self.current_frame()?;
        let index = usize::from(local);
        let Some(value) = frame.locals.get(index) else {
            return Err(local_index_error(local, frame.locals.len()));
        };
        Ok(value.clone())
    }

    pub(super) fn fast_local_int(&self, slot: u16) -> VmResult<i64> {
        let frame = self.current_frame()?;
        local_int_from_frame(frame, slot)
    }
    pub(super) fn local_seq(&self, local: u16) -> VmResult<GcRef> {
        let frame = self.current_frame()?;
        let index = usize::from(local);
        let Some(value) = frame.locals.get(index) else {
            return Err(local_index_error(local, frame.locals.len()));
        };
        Self::expect_seq(value.clone())
    }

    pub(super) fn seq2_int(&self, seq: GcRef, first: i64, second: i64) -> VmResult<i64> {
        let row = self.seq2_row(seq, first)?;
        let len = self.heap.sequence_len(row)?;
        let slot = sequence_index(second, len)?;
        self.heap.sequence_int_at(row, slot)
    }

    pub(super) fn set_seq2(
        &mut self,
        seq: GcRef,
        first: i64,
        second: i64,
        value: Value,
    ) -> VmResult {
        let row = self.seq2_row(seq, first)?;
        let len = self.heap.sequence_len(row)?;
        let slot = sequence_index(second, len)?;
        self.heap.sequence_set(row, slot, value)
    }

    pub(super) fn seq2_row(&self, seq: GcRef, first: i64) -> VmResult<GcRef> {
        let len = self.heap.sequence_len(seq)?;
        let slot = usize::try_from(first).unwrap_or(usize::MAX);
        let Ok(value) = self.heap.sequence_get_cloned(seq, slot) else {
            return Err(VmError::new(VmErrorKind::InvalidSequenceIndex {
                index: first,
                len,
            }));
        };
        match value {
            Value::Seq(row) => Ok(row),
            other => Err(Self::invalid_value_kind(VmValueKind::Seq, &other)),
        }
    }

    pub(super) fn store_local(&mut self, local: u16, value: Value) -> VmResult {
        let frame = self.current_frame_mut()?;
        let index = usize::from(local);
        let len = frame.locals.len();
        let Some(slot) = frame.locals.get_mut(index) else {
            return Err(local_index_error(local, len));
        };
        *slot = value;
        Ok(())
    }
}
