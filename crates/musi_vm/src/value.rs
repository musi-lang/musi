use std::cell::{Ref, RefCell};
use std::rc::Rc;

use music_bc::{ClassId, EffectId, ForeignId, MethodId, TypeId};
use smallvec::SmallVec;

use super::VmValueKind;

pub type ValueList = SmallVec<[Value; 8]>;
pub type ContinuationFrameList = SmallVec<[ContinuationFrame; 4]>;
pub type ContinuationHandlerList = SmallVec<[ContinuationHandler; 4]>;

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Bool(bool),
    String(Rc<str>),
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

#[derive(Debug, Clone)]
pub struct DataValue {
    pub(crate) ty: TypeId,
    pub(crate) tag: i64,
    pub(crate) fields: ValueList,
}

#[derive(Debug, Clone)]
pub struct ClosureValue {
    pub(crate) module_slot: usize,
    pub(crate) method: MethodId,
    pub(crate) captures: ValueList,
}

#[derive(Debug, Clone)]
pub struct ModuleValue {
    pub(crate) spec: Box<str>,
    pub(crate) slot: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForeignValue {
    pub(crate) module_slot: usize,
    pub(crate) foreign: ForeignId,
}

#[derive(Debug, Clone)]
pub struct ContinuationFrame {
    pub(crate) module_slot: usize,
    pub(crate) method: MethodId,
    pub(crate) ip: usize,
    pub(crate) locals: ValueList,
    pub(crate) stack: ValueList,
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

#[derive(Debug, Clone)]
pub struct ContinuationValue {
    pub(crate) frames: ContinuationFrameList,
    pub(crate) handlers: ContinuationHandlerList,
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
    Float(f64),
    Bool(bool),
    String(StringView<'a>),
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

#[derive(Debug)]
pub struct StringView<'a> {
    pub(crate) text: &'a str,
}

#[derive(Debug)]
pub struct SeqView<'a> {
    pub(crate) inner: Ref<'a, SequenceValue>,
}

#[derive(Debug)]
pub struct RecordView<'a> {
    pub(crate) inner: Ref<'a, DataValue>,
}

impl Value {
    #[must_use]
    pub fn string(text: impl Into<Rc<str>>) -> Self {
        Self::String(text.into())
    }

    #[must_use]
    pub fn sequence(ty: TypeId, items: impl IntoIterator<Item = Self>) -> Self {
        Self::Seq(Rc::new(RefCell::new(SequenceValue {
            ty,
            items: items.into_iter().collect(),
        })))
    }

    #[must_use]
    pub fn data(ty: TypeId, tag: i64, fields: impl IntoIterator<Item = Self>) -> Self {
        Self::Data(Rc::new(RefCell::new(DataValue {
            ty,
            tag,
            fields: fields.into_iter().collect(),
        })))
    }

    #[must_use]
    pub fn closure(
        module_slot: usize,
        method: MethodId,
        captures: impl IntoIterator<Item = Self>,
    ) -> Self {
        Self::Closure(Rc::new(RefCell::new(ClosureValue {
            module_slot,
            method,
            captures: captures.into_iter().collect(),
        })))
    }

    #[must_use]
    pub fn module(spec: impl Into<Box<str>>, slot: usize) -> Self {
        Self::Module(Rc::new(ModuleValue {
            spec: spec.into(),
            slot,
        }))
    }

    #[must_use]
    pub const fn foreign(module_slot: usize, foreign: ForeignId) -> Self {
        Self::Foreign(ForeignValue {
            module_slot,
            foreign,
        })
    }

    #[must_use]
    pub fn continuation(frames: ContinuationFrameList, handlers: ContinuationHandlerList) -> Self {
        Self::Continuation(Rc::new(ContinuationValue { frames, handlers }))
    }

    #[must_use]
    pub const fn kind(&self) -> VmValueKind {
        match self {
            Self::Unit => VmValueKind::Unit,
            Self::Int(_) => VmValueKind::Int,
            Self::Float(_) => VmValueKind::Float,
            Self::Bool(_) => VmValueKind::Bool,
            Self::String(_) => VmValueKind::String,
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
}

impl<'a> StringView<'a> {
    #[must_use]
    pub const fn as_str(&self) -> &'a str {
        self.text
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
            (Self::Bool(left), Self::Bool(right)) => left == right,
            (Self::String(left), Self::String(right)) => left == right,
            (Self::Seq(left), Self::Seq(right)) => {
                let left = left.borrow();
                let right = right.borrow();
                left.ty == right.ty && left.items == right.items
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
