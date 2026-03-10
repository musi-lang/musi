//! Host function trait for FFI dispatch.
//!
//! The VM itself is pure — it delegates foreign calls to an external
//! `HostFunctions` implementation provided by the embedder.

use crate::error::VmError;
use crate::heap::Heap;
use crate::value::Value;

/// Trait for dispatching `INV_FFI` opcodes to external code.
///
/// The VM holds an optional `Box<dyn HostFunctions>` and calls
/// [`call_foreign`](Self::call_foreign) when it encounters an `INV_FFI`
/// instruction.
pub trait HostFunctions {
    /// Call the foreign function at index `idx` with the given arguments.
    ///
    /// The `heap` parameter is provided so the implementation can read
    /// string data from heap objects (e.g. for C string marshaling).
    ///
    /// # Errors
    ///
    /// Returns `VmError` if the call fails (e.g. symbol not found,
    /// type mismatch, or the foreign function itself fails).
    fn call_foreign(&mut self, idx: u32, args: &[Value], heap: &Heap) -> Result<Value, VmError>;
}
