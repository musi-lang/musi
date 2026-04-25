use music_seam::{EffectId, ForeignId, ProcedureId, ShapeId, TypeId};
use music_term::SyntaxTerm;

use crate::gc::RuntimeHeap;
use crate::value::ids::GcRef;
use crate::value::objects::{ClosureValue, DataValue, ProcedureValue};
use crate::value::scalar::{BitsValue, Value};

#[derive(Debug)]
pub enum ValueView<'a> {
    Unit,
    Int(i64),
    Nat(u64),
    Float(f64),
    Bits(&'a BitsValue),
    Bool(bool),
    String(StringView<'a>),
    CPtr(usize),
    Syntax(SyntaxView<'a>),
    Seq(SeqView<'a>),
    Record(RecordView<'a>),
    Data(RecordView<'a>),
    Closure(ClosureView<'a>),
    Procedure(ProcedureValue),
    Continuation,
    Type(TypeId),
    Module(ModuleView<'a>),
    Foreign(ForeignId),
    Effect(EffectId),
    Shape(ShapeId),
}

#[must_use]
pub fn render_value_view(view: ValueView<'_>) -> Option<String> {
    match view {
        ValueView::Unit => None,
        ValueView::Int(value) => Some(value.to_string()),
        ValueView::Nat(value) => Some(value.to_string()),
        ValueView::Float(value) => Some(value.to_string()),
        ValueView::Bits(value) => Some(format!(
            "<bits:{}:0x{:x}>",
            value.width(),
            value.to_u64().unwrap_or(0)
        )),
        ValueView::Bool(value) => Some(if value { ".True" } else { ".False" }.to_owned()),
        ValueView::String(text) => Some(text.as_str().to_owned()),
        ValueView::Syntax(term) => Some(term.term().text().to_owned()),
        ValueView::Seq(seq) => Some(format!("<seq:{}>", seq.len())),
        ValueView::Record(record) => Some(format!("<record:{}>", record.len())),
        ValueView::Data(record) => Some(format!("<data:{}:{}>", record.tag(), record.len())),
        ValueView::Closure(_) => Some("<closure>".to_owned()),
        ValueView::Procedure(procedure) => Some(format!(
            "<procedure:{}:{}>",
            procedure.module_slot(),
            procedure.procedure().raw()
        )),
        ValueView::Continuation => Some("<continuation>".to_owned()),
        ValueView::Type(ty) => Some(format!("<type:{}>", ty.raw())),
        ValueView::Module(module) => Some(format!("<module:{}>", module.spec())),
        ValueView::Foreign(foreign) => Some(format!("<foreign:{}>", foreign.raw())),
        ValueView::Effect(effect) => Some(format!("<effect:{}>", effect.raw())),
        ValueView::Shape(shape) => Some(format!("<capability:{}>", shape.raw())),
        ValueView::CPtr(addr) => Some(format!("<cptr:0x{addr:x}>")),
    }
}

#[derive(Debug)]
pub struct StringView<'a> {
    pub(crate) text: &'a str,
}

impl<'a> StringView<'a> {
    #[must_use]
    pub const fn new(text: &'a str) -> Self {
        Self { text }
    }

    #[must_use]
    pub const fn as_str(&self) -> &'a str {
        self.text
    }
}

#[derive(Debug)]
pub struct SyntaxView<'a> {
    pub(crate) inner: &'a SyntaxTerm,
}

impl<'a> SyntaxView<'a> {
    #[must_use]
    pub const fn new(inner: &'a SyntaxTerm) -> Self {
        Self { inner }
    }

    #[must_use]
    pub const fn term(&self) -> &'a SyntaxTerm {
        self.inner
    }
}

#[derive(Debug)]
pub struct ClosureView<'a> {
    pub(crate) inner: &'a ClosureValue,
}

impl<'a> ClosureView<'a> {
    #[must_use]
    pub const fn new(inner: &'a ClosureValue) -> Self {
        Self { inner }
    }

    #[must_use]
    pub const fn module_slot(&self) -> usize {
        self.inner.module_slot
    }

    #[must_use]
    pub const fn procedure(&self) -> ProcedureId {
        self.inner.procedure
    }

    #[must_use]
    pub fn captures(&self) -> &[Value] {
        &self.inner.captures
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ModuleView<'a> {
    pub(crate) spec: &'a str,
    pub(crate) slot: usize,
}

impl<'a> ModuleView<'a> {
    #[must_use]
    pub const fn new(spec: &'a str, slot: usize) -> Self {
        Self { spec, slot }
    }

    #[must_use]
    pub const fn spec(self) -> &'a str {
        self.spec
    }

    #[must_use]
    pub const fn slot(self) -> usize {
        self.slot
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ForeignView<'a> {
    pub(crate) module_slot: usize,
    pub(crate) foreign: ForeignId,
    pub(crate) type_args: &'a [TypeId],
}

impl<'a> ForeignView<'a> {
    #[must_use]
    pub const fn new(module_slot: usize, foreign: ForeignId, type_args: &'a [TypeId]) -> Self {
        Self {
            module_slot,
            foreign,
            type_args,
        }
    }

    #[must_use]
    pub const fn module_slot(self) -> usize {
        self.module_slot
    }

    #[must_use]
    pub const fn foreign(self) -> ForeignId {
        self.foreign
    }

    #[must_use]
    pub const fn type_args(self) -> &'a [TypeId] {
        self.type_args
    }
}

#[derive(Debug)]
pub struct SeqView<'a> {
    pub(crate) heap: &'a RuntimeHeap,
    pub(crate) reference: GcRef,
}

impl<'a> SeqView<'a> {
    #[must_use]
    pub const fn new(heap: &'a RuntimeHeap, reference: GcRef) -> Self {
        Self { heap, reference }
    }

    #[must_use]
    ///
    /// # Panics
    ///
    /// Panics when sequence reference is stale or collected.
    pub fn ty(&self) -> TypeId {
        self.heap
            .sequence_ty(self.reference)
            .expect("live sequence")
    }

    #[must_use]
    ///
    /// # Panics
    ///
    /// Panics when sequence reference is stale or collected.
    pub fn len(&self) -> usize {
        self.heap
            .sequence_len(self.reference)
            .expect("live sequence")
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[must_use]
    pub fn get(&self, index: usize) -> Option<Value> {
        self.heap.sequence_get_cloned(self.reference, index).ok()
    }
}

#[derive(Debug)]
pub struct RecordView<'a> {
    pub(crate) inner: &'a DataValue,
}

impl<'a> RecordView<'a> {
    #[must_use]
    pub const fn new(inner: &'a DataValue) -> Self {
        Self { inner }
    }

    #[must_use]
    pub const fn ty(&self) -> TypeId {
        self.inner.ty
    }

    #[must_use]
    pub const fn tag(&self) -> i64 {
        self.inner.tag
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.inner.fields.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.inner.fields.is_empty()
    }

    #[must_use]
    pub fn get(&self, index: usize) -> Option<&Value> {
        self.inner.fields.get(index)
    }
}
