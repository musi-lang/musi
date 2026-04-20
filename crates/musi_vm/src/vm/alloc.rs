use music_seam::{ProcedureId, TypeId};
use music_term::SyntaxTerm;

use super::super::value::{ContinuationFrameList, ContinuationHandlerList};
use super::{
    ClosureValue, ContinuationValue, DataValue, ModuleValue, SequenceValue, Value, ValueList, Vm,
    VmResult,
};

impl Vm {
    /// Allocates one VM-owned string.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when heap limits reject the allocation.
    pub fn alloc_string(&mut self, text: impl Into<Box<str>>) -> VmResult<Value> {
        let value = self.heap.alloc_string(text, &self.heap_options())?;
        self.after_heap_allocation(&value)?;
        Ok(value)
    }

    /// Allocates one VM-owned syntax term.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when heap limits reject the allocation.
    pub fn alloc_syntax(&mut self, term: SyntaxTerm) -> VmResult<Value> {
        let value = self.heap.alloc_syntax(term, &self.heap_options())?;
        self.after_heap_allocation(&value)?;
        Ok(value)
    }

    /// Allocates one VM-owned sequence.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when heap limits reject the allocation.
    pub fn alloc_sequence<Items>(&mut self, ty: TypeId, items: Items) -> VmResult<Value>
    where
        Items: IntoIterator<Item = Value>,
    {
        self.alloc_sequence_owned(ty, items.into_iter().collect())
    }

    /// Allocates one two-element VM-owned sequence.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when heap limits reject the allocation.
    pub fn alloc_pair_sequence(
        &mut self,
        ty: TypeId,
        first: Value,
        second: Value,
    ) -> VmResult<Value> {
        let value = self
            .heap
            .alloc_pair_sequence(ty, first, second, &self.heap_options())?;
        self.after_heap_allocation(&value)?;
        Ok(value)
    }

    pub(crate) fn alloc_sequence_owned(&mut self, ty: TypeId, items: ValueList) -> VmResult<Value> {
        let value = self
            .heap
            .alloc_sequence(SequenceValue::new(ty, items), &self.heap_options())?;
        self.after_heap_allocation(&value)?;
        Ok(value)
    }

    /// Allocates one VM-owned data value.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when heap limits reject the allocation.
    pub fn alloc_data<Fields>(&mut self, ty: TypeId, tag: i64, fields: Fields) -> VmResult<Value>
    where
        Fields: IntoIterator<Item = Value>,
    {
        self.alloc_data_owned(ty, tag, fields.into_iter().collect())
    }

    pub(crate) fn alloc_data_owned(
        &mut self,
        ty: TypeId,
        tag: i64,
        fields: ValueList,
    ) -> VmResult<Value> {
        let value = self
            .heap
            .alloc_data(DataValue::new(ty, tag, fields), &self.heap_options())?;
        self.after_heap_allocation(&value)?;
        Ok(value)
    }

    pub(crate) fn alloc_closure_owned(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        captures: ValueList,
    ) -> VmResult<Value> {
        let loaded = self
            .module(module_slot)?
            .program
            .loaded_procedure(procedure)?;
        let closure = ClosureValue::new(module_slot, procedure, captures)
            .with_shape(loaded.params, loaded.locals);
        let value = self.heap.alloc_closure(closure, &self.heap_options())?;
        self.after_heap_allocation(&value)?;
        Ok(value)
    }

    pub(crate) fn alloc_module(
        &mut self,
        spec: impl Into<Box<str>>,
        slot: usize,
    ) -> VmResult<Value> {
        let value = self
            .heap
            .alloc_module(ModuleValue::new(spec, slot), &self.heap_options())?;
        self.after_heap_allocation(&value)?;
        Ok(value)
    }

    pub(crate) fn alloc_continuation(
        &mut self,
        frames: ContinuationFrameList,
        handlers: ContinuationHandlerList,
    ) -> VmResult<Value> {
        let value = self.heap.alloc_continuation(
            ContinuationValue::new(frames, handlers),
            &self.heap_options(),
        )?;
        self.after_heap_allocation(&value)?;
        Ok(value)
    }
}
