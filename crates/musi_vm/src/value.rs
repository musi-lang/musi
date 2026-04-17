use std::cell::{Ref, RefCell};
use std::rc::Rc;

use music_seam::{ClassId, EffectId, ForeignId, MethodId, TypeId};
use music_term::SyntaxTerm;
use smallvec::SmallVec;

use super::VmValueKind;

pub type ValueList = SmallVec<[Value; 8]>;
pub type ContinuationFrameList = SmallVec<[ContinuationFrame; 4]>;
pub type ContinuationHandlerList = SmallVec<[ContinuationHandler; 4]>;
pub type SyntaxValuePtr = Rc<SyntaxTerm>;

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Int(i64),
    Nat(u64),
    Float(f64),
    String(Rc<str>),
    CPtr(usize),
    Syntax(SyntaxValuePtr),
    Seq(SeqValuePtr),
    Data(DataValuePtr),
    Closure(ClosureValuePtr),
    Continuation(ContinuationValuePtr),
    Type(TypeId),
    Module(ModuleValuePtr),
    Foreign(ForeignValue),
    Effect(EffectId),
    Class(ClassId),
}

#[derive(Debug, Clone)]
pub struct SequenceValue {
    pub(crate) ty: TypeId,
    pub(crate) items: ValueList,
}

impl SequenceValue {
    #[must_use]
    pub const fn new(ty: TypeId, items: ValueList) -> Self {
        Self { ty, items }
    }
}

#[derive(Debug, Clone)]
pub struct DataValue {
    pub(crate) ty: TypeId,
    pub(crate) tag: i64,
    pub(crate) fields: ValueList,
}

impl DataValue {
    #[must_use]
    pub const fn new(ty: TypeId, tag: i64, fields: ValueList) -> Self {
        Self { ty, tag, fields }
    }
}

#[derive(Debug, Clone)]
pub struct ClosureValue {
    pub(crate) module_slot: usize,
    pub(crate) method: MethodId,
    pub(crate) captures: ValueList,
}

impl ClosureValue {
    #[must_use]
    pub const fn new(module_slot: usize, method: MethodId, captures: ValueList) -> Self {
        Self {
            module_slot,
            method,
            captures,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleValue {
    pub(crate) spec: Box<str>,
    pub(crate) slot: usize,
}

impl ModuleValue {
    #[must_use]
    pub fn new(spec: impl Into<Box<str>>, slot: usize) -> Self {
        Self {
            spec: spec.into(),
            slot,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForeignValue {
    pub(crate) module_slot: usize,
    pub(crate) foreign: ForeignId,
    pub(crate) type_args: Box<[TypeId]>,
}

impl ForeignValue {
    #[must_use]
    pub fn new(module_slot: usize, foreign: ForeignId) -> Self {
        Self {
            module_slot,
            foreign,
            type_args: Box::default(),
        }
    }

    #[must_use]
    pub fn with_type_args(mut self, type_args: impl Into<Box<[TypeId]>>) -> Self {
        self.type_args = type_args.into();
        self
    }
}

#[derive(Debug, Clone)]
pub struct ContinuationFrame {
    pub(crate) module_slot: usize,
    pub(crate) method: MethodId,
    pub(crate) ip: usize,
    pub(crate) locals: ValueList,
    pub(crate) stack: ValueList,
}

impl ContinuationFrame {
    #[must_use]
    pub const fn new(
        module_slot: usize,
        method: MethodId,
        locals: ValueList,
        stack: ValueList,
    ) -> Self {
        Self {
            module_slot,
            method,
            ip: 0,
            locals,
            stack,
        }
    }

    #[must_use]
    pub const fn with_ip(mut self, ip: usize) -> Self {
        self.ip = ip;
        self
    }
}

#[derive(Debug, Clone)]
pub struct ContinuationHandler {
    pub(crate) handler_id: u64,
    pub(crate) effect: EffectId,
    pub(crate) handler: Value,
    pub(crate) frame_depth: usize,
    pub(crate) stack_depth: usize,
    pub(crate) pop_ip: usize,
}

impl ContinuationHandler {
    #[must_use]
    pub const fn new(handler_id: u64, effect: EffectId, handler: Value) -> Self {
        Self {
            handler_id,
            effect,
            handler,
            frame_depth: 0,
            stack_depth: 0,
            pop_ip: 0,
        }
    }

    #[must_use]
    pub const fn with_stack_state(
        mut self,
        frame_depth: usize,
        stack_depth: usize,
        pop_ip: usize,
    ) -> Self {
        self.frame_depth = frame_depth;
        self.stack_depth = stack_depth;
        self.pop_ip = pop_ip;
        self
    }
}

#[derive(Debug, Clone)]
pub struct ContinuationValue {
    pub(crate) frames: ContinuationFrameList,
    pub(crate) handlers: ContinuationHandlerList,
}

impl ContinuationValue {
    #[must_use]
    pub const fn new(frames: ContinuationFrameList, handlers: ContinuationHandlerList) -> Self {
        Self { frames, handlers }
    }
}

pub type SeqValuePtr = Rc<RefCell<SequenceValue>>;
pub type DataValuePtr = Rc<RefCell<DataValue>>;
pub type ClosureValuePtr = Rc<RefCell<ClosureValue>>;
pub type ModuleValuePtr = Rc<ModuleValue>;
pub type ContinuationValuePtr = Rc<ContinuationValue>;

#[derive(Debug)]
pub enum ValueView<'a> {
    Unit,
    Int(i64),
    Nat(u64),
    Float(f64),
    Bool(bool),
    String(StringView<'a>),
    CPtr(usize),
    Syntax(SyntaxView<'a>),
    Seq(SeqView<'a>),
    Record(RecordView<'a>),
    Data(RecordView<'a>),
    Closure,
    Continuation,
    Type(TypeId),
    Module(&'a str),
    Foreign(ForeignId),
    Effect(EffectId),
    Class(ClassId),
}

#[must_use]
pub fn render_value_view(view: ValueView<'_>) -> Option<String> {
    match view {
        ValueView::Unit => None,
        ValueView::Int(value) => Some(value.to_string()),
        ValueView::Nat(value) => Some(value.to_string()),
        ValueView::Float(value) => Some(value.to_string()),
        ValueView::Bool(value) => Some(if value { ".True" } else { ".False" }.to_owned()),
        ValueView::String(text) => Some(text.as_str().to_owned()),
        ValueView::Syntax(term) => Some(term.term().text().to_owned()),
        ValueView::Seq(seq) => Some(format!("<seq:{}>", seq.len())),
        ValueView::Record(record) => Some(format!("<record:{}>", record.len())),
        ValueView::Data(record) => Some(format!("<data:{}:{}>", record.tag(), record.len())),
        ValueView::Closure => Some("<closure>".to_owned()),
        ValueView::Continuation => Some("<continuation>".to_owned()),
        ValueView::Type(ty) => Some(format!("<type:{}>", ty.raw())),
        ValueView::Module(spec) => Some(format!("<module:{spec}>")),
        ValueView::Foreign(foreign) => Some(format!("<foreign:{}>", foreign.raw())),
        ValueView::Effect(effect) => Some(format!("<effect:{}>", effect.raw())),
        ValueView::Class(class) => Some(format!("<class:{}>", class.raw())),
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
}

#[derive(Debug)]
pub struct SyntaxView<'a> {
    pub(crate) inner: &'a SyntaxTerm,
}

#[derive(Debug)]
pub struct ClosureView<'a> {
    pub(crate) inner: Ref<'a, ClosureValue>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ModuleView<'a> {
    pub(crate) spec: &'a str,
    pub(crate) slot: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ForeignView<'a> {
    pub(crate) module_slot: usize,
    pub(crate) foreign: ForeignId,
    pub(crate) type_args: &'a [TypeId],
}

impl<'a> SyntaxView<'a> {
    #[must_use]
    pub const fn new(inner: &'a SyntaxTerm) -> Self {
        Self { inner }
    }
}

#[derive(Debug)]
pub struct SeqView<'a> {
    pub(crate) inner: Ref<'a, SequenceValue>,
}

impl<'a> SeqView<'a> {
    #[must_use]
    pub const fn new(inner: Ref<'a, SequenceValue>) -> Self {
        Self { inner }
    }
}

#[derive(Debug)]
pub struct RecordView<'a> {
    pub(crate) inner: Ref<'a, DataValue>,
}

impl<'a> RecordView<'a> {
    #[must_use]
    pub const fn new(inner: Ref<'a, DataValue>) -> Self {
        Self { inner }
    }
}

impl Value {
    #[must_use]
    pub fn string(text: impl Into<Rc<str>>) -> Self {
        Self::String(text.into())
    }

    #[must_use]
    pub fn syntax(term: SyntaxTerm) -> Self {
        Self::Syntax(Rc::new(term))
    }

    #[must_use]
    pub const fn c_ptr(address: usize) -> Self {
        Self::CPtr(address)
    }

    #[must_use]
    pub fn sequence<Items>(ty: TypeId, items: Items) -> Self
    where
        Items: IntoIterator<Item = Self>,
    {
        Self::Seq(Rc::new(RefCell::new(SequenceValue::new(
            ty,
            items.into_iter().collect(),
        ))))
    }

    #[must_use]
    pub fn data<Fields>(ty: TypeId, tag: i64, fields: Fields) -> Self
    where
        Fields: IntoIterator<Item = Self>,
    {
        Self::Data(Rc::new(RefCell::new(DataValue::new(
            ty,
            tag,
            fields.into_iter().collect(),
        ))))
    }

    #[must_use]
    pub fn closure<Captures>(module_slot: usize, method: MethodId, captures: Captures) -> Self
    where
        Captures: IntoIterator<Item = Self>,
    {
        Self::Closure(Rc::new(RefCell::new(ClosureValue::new(
            module_slot,
            method,
            captures.into_iter().collect(),
        ))))
    }

    #[must_use]
    pub fn module(spec: impl Into<Box<str>>, slot: usize) -> Self {
        Self::Module(Rc::new(ModuleValue::new(spec, slot)))
    }

    #[must_use]
    pub fn foreign(module_slot: usize, foreign: ForeignId) -> Self {
        Self::Foreign(ForeignValue::new(module_slot, foreign))
    }

    #[must_use]
    pub fn continuation(frames: ContinuationFrameList, handlers: ContinuationHandlerList) -> Self {
        Self::Continuation(Rc::new(ContinuationValue::new(frames, handlers)))
    }

    #[must_use]
    pub const fn kind(&self) -> VmValueKind {
        match self {
            Self::Unit => VmValueKind::Unit,
            Self::Int(_) => VmValueKind::Int,
            Self::Nat(_) => VmValueKind::Nat,
            Self::Float(_) => VmValueKind::Float,
            Self::String(_) => VmValueKind::String,
            Self::CPtr(_) => VmValueKind::CPtr,
            Self::Syntax(_) => VmValueKind::Syntax,
            Self::Seq(_) => VmValueKind::Seq,
            Self::Data(_) => VmValueKind::Data,
            Self::Closure(_) => VmValueKind::Closure,
            Self::Continuation(_) => VmValueKind::Continuation,
            Self::Type(_) => VmValueKind::Type,
            Self::Module(_) => VmValueKind::Module,
            Self::Foreign(_) => VmValueKind::Foreign,
            Self::Effect(_) => VmValueKind::Effect,
            Self::Class(_) => VmValueKind::Class,
        }
    }

    #[must_use]
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::String(text) => Some(text.as_ref()),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_record(&self) -> Option<RecordView<'_>> {
        match self {
            Self::Data(value) => Some(RecordView::new(value.borrow())),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_seq(&self) -> Option<SeqView<'_>> {
        match self {
            Self::Seq(value) => Some(SeqView::new(value.borrow())),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_closure(&self) -> Option<ClosureView<'_>> {
        match self {
            Self::Closure(value) => Some(ClosureView::new(value.borrow())),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_module(&self) -> Option<ModuleView<'_>> {
        match self {
            Self::Module(value) => Some(ModuleView::new(value.spec.as_ref(), value.slot)),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_foreign(&self) -> Option<ForeignView<'_>> {
        match self {
            Self::Foreign(value) => Some(ForeignView::new(
                value.module_slot,
                value.foreign,
                &value.type_args,
            )),
            _ => None,
        }
    }
}

impl<'a> StringView<'a> {
    #[must_use]
    pub const fn as_str(&self) -> &'a str {
        self.text
    }
}

impl<'a> SyntaxView<'a> {
    #[must_use]
    pub const fn term(&self) -> &'a SyntaxTerm {
        self.inner
    }
}

impl<'a> ClosureView<'a> {
    #[must_use]
    pub const fn new(inner: Ref<'a, ClosureValue>) -> Self {
        Self { inner }
    }

    #[must_use]
    pub fn module_slot(&self) -> usize {
        self.inner.module_slot
    }

    #[must_use]
    pub fn method(&self) -> MethodId {
        self.inner.method
    }

    #[must_use]
    pub fn captures(&self) -> &[Value] {
        &self.inner.captures
    }
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

impl SeqView<'_> {
    #[must_use]
    pub fn ty(&self) -> TypeId {
        self.inner.ty
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.inner.items.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.inner.items.is_empty()
    }

    #[must_use]
    pub fn get(&self, index: usize) -> Option<&Value> {
        self.inner.items.get(index)
    }
}

impl RecordView<'_> {
    #[must_use]
    pub fn ty(&self) -> TypeId {
        self.inner.ty
    }

    #[must_use]
    pub fn tag(&self) -> i64 {
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

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unit, Self::Unit) => true,
            (Self::Int(left), Self::Int(right)) => left == right,
            (Self::Float(left), Self::Float(right)) => left.to_bits() == right.to_bits(),
            (Self::String(left), Self::String(right)) => left == right,
            (Self::CPtr(left), Self::CPtr(right)) => left == right,
            (Self::Seq(left), Self::Seq(right)) => {
                let left = left.borrow();
                let right = right.borrow();
                left.items == right.items
            }
            (Self::Data(left), Self::Data(right)) => {
                let left = left.borrow();
                let right = right.borrow();
                left.ty == right.ty && left.tag == right.tag && left.fields == right.fields
            }
            (Self::Closure(left), Self::Closure(right)) => {
                let left = left.borrow();
                let right = right.borrow();
                left.module_slot == right.module_slot
                    && left.method == right.method
                    && left.captures == right.captures
            }
            (Self::Continuation(left), Self::Continuation(right)) => Rc::ptr_eq(left, right),
            (Self::Type(left), Self::Type(right)) => left == right,
            (Self::Module(left), Self::Module(right)) => left.slot == right.slot,
            (Self::Foreign(left), Self::Foreign(right)) => left == right,
            (Self::Effect(left), Self::Effect(right)) => left == right,
            (Self::Class(left), Self::Class(right)) => left == right,
            _ => false,
        }
    }
}

impl Eq for Value {}
