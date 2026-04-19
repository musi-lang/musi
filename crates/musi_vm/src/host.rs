use music_seam::{EffectId, ForeignId, TypeId};
use music_term::TypeTerm;

use super::gc::{HeapOptions, RuntimeHeap};
use super::value::{DataValue, RecordView, StringView, SyntaxView};
use super::{
    Program, ProgramDataLayout, ProgramTypeAbiKind, Value, VmError, VmErrorKind, VmResult,
};

#[derive(Debug, Clone)]
pub struct ForeignCall {
    pub(crate) program: Program,
    pub(crate) module: Box<str>,
    pub(crate) foreign: ForeignId,
    pub(crate) name: Box<str>,
    pub(crate) abi: Box<str>,
    pub(crate) symbol: Box<str>,
    pub(crate) link: Option<Box<str>>,
    pub(crate) param_tys: Box<[TypeId]>,
    pub(crate) result_ty: TypeId,
}

impl ForeignCall {
    #[must_use]
    pub fn module(&self) -> &str {
        &self.module
    }

    #[must_use]
    pub const fn foreign(&self) -> ForeignId {
        self.foreign
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn abi(&self) -> &str {
        &self.abi
    }

    #[must_use]
    pub fn symbol(&self) -> &str {
        &self.symbol
    }

    #[must_use]
    pub fn link(&self) -> Option<&str> {
        self.link.as_deref()
    }

    #[must_use]
    pub fn param_tys(&self) -> &[TypeId] {
        &self.param_tys
    }

    #[must_use]
    pub const fn result_ty(&self) -> TypeId {
        self.result_ty
    }

    #[must_use]
    pub fn param_ty_name(&self, index: usize) -> Option<&str> {
        self.param_tys
            .get(index)
            .map(|ty| self.program.type_name(*ty))
    }

    #[must_use]
    pub fn result_ty_name(&self) -> &str {
        self.program.type_name(self.result_ty)
    }

    #[must_use]
    pub fn type_name(&self, ty: TypeId) -> &str {
        self.program.type_name(ty)
    }

    #[must_use]
    pub fn type_term(&self, ty: TypeId) -> TypeTerm {
        self.program.type_term(ty)
    }

    #[must_use]
    pub fn type_data_layout(&self, ty: TypeId) -> Option<&ProgramDataLayout> {
        self.program.type_data_layout(ty)
    }

    #[must_use]
    pub fn type_abi_kind(&self, ty: TypeId) -> ProgramTypeAbiKind {
        self.program.type_abi_kind(ty)
    }

    #[must_use]
    pub fn param_data_layout(&self, index: usize) -> Option<&ProgramDataLayout> {
        let ty = *self.param_tys.get(index)?;
        self.type_data_layout(ty)
    }

    #[must_use]
    pub fn param_abi_kind(&self, index: usize) -> Option<ProgramTypeAbiKind> {
        let ty = *self.param_tys.get(index)?;
        Some(self.type_abi_kind(ty))
    }

    #[must_use]
    pub fn result_data_layout(&self) -> Option<&ProgramDataLayout> {
        self.type_data_layout(self.result_ty)
    }

    #[must_use]
    pub fn result_abi_kind(&self) -> ProgramTypeAbiKind {
        self.type_abi_kind(self.result_ty)
    }
}

#[derive(Debug, Clone)]
pub struct EffectCall {
    pub(crate) program: Program,
    pub(crate) effect: EffectId,
    pub(crate) module: Box<str>,
    pub(crate) effect_name: Box<str>,
    pub(crate) op: u16,
    pub(crate) op_name: Box<str>,
    pub(crate) param_tys: Box<[TypeId]>,
    pub(crate) result_ty: TypeId,
    pub(crate) is_comptime_safe: bool,
}

impl EffectCall {
    #[must_use]
    pub fn module(&self) -> &str {
        &self.module
    }

    #[must_use]
    pub const fn effect(&self) -> EffectId {
        self.effect
    }

    #[must_use]
    pub fn effect_name(&self) -> &str {
        &self.effect_name
    }

    #[must_use]
    pub const fn op(&self) -> u16 {
        self.op
    }

    #[must_use]
    pub fn op_name(&self) -> &str {
        &self.op_name
    }

    #[must_use]
    pub fn param_tys(&self) -> &[TypeId] {
        &self.param_tys
    }

    #[must_use]
    pub const fn result_ty(&self) -> TypeId {
        self.result_ty
    }

    #[must_use]
    pub const fn is_comptime_safe(&self) -> bool {
        self.is_comptime_safe
    }

    #[must_use]
    pub fn param_ty_name(&self, index: usize) -> Option<&str> {
        self.param_tys
            .get(index)
            .map(|ty| self.program.type_name(*ty))
    }

    #[must_use]
    pub fn result_ty_name(&self) -> &str {
        self.program.type_name(self.result_ty)
    }

    #[must_use]
    pub fn type_name(&self, ty: TypeId) -> &str {
        self.program.type_name(ty)
    }

    #[must_use]
    pub fn type_term(&self, ty: TypeId) -> TypeTerm {
        self.program.type_term(ty)
    }

    #[must_use]
    pub fn type_data_layout(&self, ty: TypeId) -> Option<&ProgramDataLayout> {
        self.program.type_data_layout(ty)
    }

    #[must_use]
    pub fn type_abi_kind(&self, ty: TypeId) -> ProgramTypeAbiKind {
        self.program.type_abi_kind(ty)
    }

    #[must_use]
    pub fn param_data_layout(&self, index: usize) -> Option<&ProgramDataLayout> {
        let ty = *self.param_tys.get(index)?;
        self.type_data_layout(ty)
    }

    #[must_use]
    pub fn param_abi_kind(&self, index: usize) -> Option<ProgramTypeAbiKind> {
        let ty = *self.param_tys.get(index)?;
        Some(self.type_abi_kind(ty))
    }

    #[must_use]
    pub fn result_data_layout(&self) -> Option<&ProgramDataLayout> {
        self.type_data_layout(self.result_ty)
    }

    #[must_use]
    pub fn result_abi_kind(&self) -> ProgramTypeAbiKind {
        self.type_abi_kind(self.result_ty)
    }
}

pub trait VmHost {
    /// Handles one runtime foreign call.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when host foreign execution rejects or fails.
    fn call_foreign(
        &mut self,
        ctx: VmHostCallContext<'_, '_>,
        foreign: &ForeignCall,
        args: &[Value],
    ) -> VmResult<Value>;

    /// Handles one unhandled runtime effect invocation.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when host effect handling rejects or fails.
    fn handle_effect(
        &mut self,
        ctx: VmHostCallContext<'_, '_>,
        effect: &EffectCall,
        args: &[Value],
    ) -> VmResult<Value>;
}

/// Borrowed VM context passed to host callbacks.
pub type VmHostCallContext<'call, 'vm> = &'call mut VmHostContext<'vm>;

pub struct VmHostContext<'a> {
    pub(crate) heap: &'a mut RuntimeHeap,
    pub(crate) options: HeapOptions,
}

impl<'a> VmHostContext<'a> {
    pub(crate) const fn new(heap: &'a mut RuntimeHeap, options: HeapOptions) -> Self {
        Self { heap, options }
    }

    /// Allocates one VM-owned string.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when heap limits reject the allocation.
    pub fn alloc_string(&mut self, text: impl Into<Box<str>>) -> VmResult<Value> {
        self.heap.alloc_string(text, &self.options)
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
        self.heap.alloc_data(
            DataValue::new(ty, tag, fields.into_iter().collect()),
            &self.options,
        )
    }

    pub fn string<'b>(&'b self, value: &'b Value) -> Option<StringView<'b>> {
        let Value::String(reference) = value else {
            return None;
        };
        self.heap.string(*reference).ok().map(StringView::new)
    }

    pub fn syntax<'b>(&'b self, value: &'b Value) -> Option<SyntaxView<'b>> {
        let Value::Syntax(reference) = value else {
            return None;
        };
        self.heap.syntax(*reference).ok().map(SyntaxView::new)
    }

    pub fn record<'b>(&'b self, value: &'b Value) -> Option<RecordView<'b>> {
        let Value::Data(reference) = value else {
            return None;
        };
        self.heap.data(*reference).ok().map(RecordView::new)
    }

    #[must_use]
    pub fn bool_flag(&self, value: &Value) -> Option<bool> {
        let record = self.record(value)?;
        (record.is_empty() && (record.tag() == 0 || record.tag() == 1)).then_some(record.tag() != 0)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct RejectingHost;

impl VmHost for RejectingHost {
    fn call_foreign(
        &mut self,
        _ctx: &mut VmHostContext<'_>,
        foreign: &ForeignCall,
        _args: &[Value],
    ) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::ForeignCallRejected {
            foreign: foreign.name().into(),
        }))
    }

    fn handle_effect(
        &mut self,
        _ctx: &mut VmHostContext<'_>,
        effect: &EffectCall,
        _args: &[Value],
    ) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::EffectRejected {
            effect: effect.effect_name().into(),
            op: Some(effect.op_name().into()),
            reason: "native host rejected runtime effect".into(),
        }))
    }
}
