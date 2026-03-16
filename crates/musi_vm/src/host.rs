//! Host function trait for FFI dispatch.
//!
//! The VM itself is pure — it delegates foreign calls to an external
//! `HostFunctions` implementation provided by the embedder.

use crate::error::VmError;
use crate::heap::Heap;
use crate::value::Value;

/// Trait for dispatching `INV_FFI` opcodes to external code.
pub trait HostFunctions {
    /// Call the foreign function at index `idx` with the given arguments.
    ///
    /// # Errors
    ///
    /// Returns `VmError` if the call fails.
    fn call_foreign(&mut self, idx: u32, args: &[Value], heap: &mut Heap)
    -> Result<Value, VmError>;
}
