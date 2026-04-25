use super::*;

#[inline]
pub(super) fn sequence_index(index: i64, len: usize) -> VmResult<usize> {
    let Ok(slot) = usize::try_from(index) else {
        return Err(sequence_index_error(index, len));
    };
    if slot >= len {
        return Err(sequence_index_error(index, len));
    }
    Ok(slot)
}

#[inline]
const fn sequence_index_error(index: i64, len: usize) -> VmError {
    VmError::new(VmErrorKind::InvalidSequenceIndex { index, len })
}
