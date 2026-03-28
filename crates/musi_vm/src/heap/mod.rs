#![allow(unsafe_code)]

mod alloc;
mod collect;
mod layout;
mod mark;
mod objects;

pub use objects::{Closure, Continuation, HeapObject, VmArray, VmCPtr, VmCell, VmSlice, VmString};

use layout::{Block, BumpState, GcState, LargeObjectSpace};

pub struct Heap {
    blocks: Vec<Block>,
    free_blocks: Vec<usize>,
    recyclable_blocks: Vec<usize>,
    bump: BumpState,
    large: LargeObjectSpace,
    gc: GcState,
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
