use music_seam::{EffectId, ForeignId, ProcedureId, ShapeId, TypeId};
use smallvec::SmallVec;

use crate::value::ids::GcRef;
use crate::value::objects::{ForeignValue, ProcedureValue};
use crate::value::views::ForeignView;

use crate::VmValueKind;

pub type ValueList = SmallVec<[Value; 8]>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HeapValueKind {
    String,
    Syntax,
    Seq,
    Data,
    Closure,
    Continuation,
    Module,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit,
    Int(i64),
    Nat(u64),
    Float(f64),
    Bits(BitsValue),
    String(GcRef),
    CPtr(usize),
    Syntax(GcRef),
    Seq(GcRef),
    Data(GcRef),
    Closure(GcRef),
    Procedure(ProcedureValue),
    Continuation(GcRef),
    Type(TypeId),
    Module(GcRef),
    Foreign(ForeignValue),
    Effect(EffectId),
    Shape(ShapeId),
}

impl Eq for Value {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BitsValue {
    width: u32,
    words: Box<[u64]>,
}

impl BitsValue {
    #[must_use]
    pub fn from_u64(width: u32, value: u64) -> Self {
        let mut words = vec![0; words_for_bits(width)];
        if let Some(first) = words.first_mut() {
            *first = value;
        }
        Self::new(width, words)
    }

    #[must_use]
    pub fn new(width: u32, mut words: Vec<u64>) -> Self {
        words.resize(words_for_bits(width), 0);
        if let Some(last) = words.last_mut() {
            *last &= last_word_mask(width);
        }
        Self {
            width,
            words: words.into_boxed_slice(),
        }
    }

    #[must_use]
    pub const fn width(&self) -> u32 {
        self.width
    }

    #[must_use]
    pub fn to_u64(&self) -> Option<u64> {
        if self.width > 64 {
            return None;
        }
        Some(self.words.first().copied().unwrap_or(0))
    }

    #[must_use]
    pub fn and(&self, other: &Self) -> Option<Self> {
        self.zip_words(other, |left, right| left & right)
    }

    #[must_use]
    pub fn or(&self, other: &Self) -> Option<Self> {
        self.zip_words(other, |left, right| left | right)
    }

    #[must_use]
    pub fn xor(&self, other: &Self) -> Option<Self> {
        self.zip_words(other, |left, right| left ^ right)
    }

    #[must_use]
    pub fn not(&self) -> Self {
        Self::new(self.width, self.words.iter().map(|word| !word).collect())
    }

    fn zip_words(&self, other: &Self, op: impl Fn(u64, u64) -> u64) -> Option<Self> {
        (self.width == other.width).then(|| {
            Self::new(
                self.width,
                self.words
                    .iter()
                    .zip(other.words.iter())
                    .map(|(left, right)| op(*left, *right))
                    .collect(),
            )
        })
    }
}

fn words_for_bits(width: u32) -> usize {
    usize::try_from(width.saturating_add(63) / 64).unwrap_or(usize::MAX)
}

const fn last_word_mask(width: u32) -> u64 {
    let used = width % 64;
    if used == 0 {
        u64::MAX
    } else {
        (1_u64 << used) - 1
    }
}

impl Value {
    #[must_use]
    pub const fn c_ptr(address: usize) -> Self {
        Self::CPtr(address)
    }

    #[must_use]
    pub fn foreign(module_slot: usize, foreign: ForeignId) -> Self {
        Self::Foreign(ForeignValue::new(module_slot, foreign))
    }

    #[must_use]
    pub const fn procedure(
        module_slot: usize,
        procedure: ProcedureId,
        params: u16,
        locals: u16,
    ) -> Self {
        Self::Procedure(ProcedureValue::new(module_slot, procedure, params, locals))
    }

    #[must_use]
    pub const fn kind(&self) -> VmValueKind {
        match self {
            Self::Unit => VmValueKind::Unit,
            Self::Int(_) => VmValueKind::Int,
            Self::Nat(_) => VmValueKind::Nat,
            Self::Float(_) => VmValueKind::Float,
            Self::Bits(_) => VmValueKind::Bits,
            Self::String(_) => VmValueKind::String,
            Self::CPtr(_) => VmValueKind::CPtr,
            Self::Syntax(_) => VmValueKind::Syntax,
            Self::Seq(_) => VmValueKind::Seq,
            Self::Data(_) => VmValueKind::Data,
            Self::Closure(_) => VmValueKind::Closure,
            Self::Procedure(_) => VmValueKind::Procedure,
            Self::Continuation(_) => VmValueKind::Continuation,
            Self::Type(_) => VmValueKind::Type,
            Self::Module(_) => VmValueKind::Module,
            Self::Foreign(_) => VmValueKind::Foreign,
            Self::Effect(_) => VmValueKind::Effect,
            Self::Shape(_) => VmValueKind::Shape,
        }
    }

    #[must_use]
    pub const fn gc_ref(&self) -> Option<GcRef> {
        match self {
            Self::String(reference)
            | Self::Syntax(reference)
            | Self::Seq(reference)
            | Self::Data(reference)
            | Self::Closure(reference)
            | Self::Continuation(reference)
            | Self::Module(reference) => Some(*reference),
            Self::Unit
            | Self::Int(_)
            | Self::Nat(_)
            | Self::Float(_)
            | Self::Bits(_)
            | Self::CPtr(_)
            | Self::Procedure(_)
            | Self::Type(_)
            | Self::Foreign(_)
            | Self::Effect(_)
            | Self::Shape(_) => None,
        }
    }

    #[must_use]
    pub const fn heap_kind(&self) -> Option<HeapValueKind> {
        match self {
            Self::String(_) => Some(HeapValueKind::String),
            Self::Syntax(_) => Some(HeapValueKind::Syntax),
            Self::Seq(_) => Some(HeapValueKind::Seq),
            Self::Data(_) => Some(HeapValueKind::Data),
            Self::Closure(_) => Some(HeapValueKind::Closure),
            Self::Continuation(_) => Some(HeapValueKind::Continuation),
            Self::Module(_) => Some(HeapValueKind::Module),
            Self::Unit
            | Self::Int(_)
            | Self::Nat(_)
            | Self::Float(_)
            | Self::Bits(_)
            | Self::CPtr(_)
            | Self::Procedure(_)
            | Self::Type(_)
            | Self::Foreign(_)
            | Self::Effect(_)
            | Self::Shape(_) => None,
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
