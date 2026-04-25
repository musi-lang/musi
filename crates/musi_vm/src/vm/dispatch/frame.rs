use super::*;

impl Vm {
    pub(super) fn current_frame(&self) -> VmResult<&CallFrame> {
        self.frames.last().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })
    }

    pub(super) fn current_frame_mut(&mut self) -> VmResult<&mut CallFrame> {
        self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })
    }
}

#[inline]
pub(super) fn local_index_error(slot: u16, len: usize) -> VmError {
    VmError::new(VmErrorKind::IndexOutOfBounds {
        space: VmIndexSpace::Local,
        owner: None,
        index: i64::from(slot),
        len,
    })
}
