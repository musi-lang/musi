use std::mem::size_of;

use music_seam::{EffectId, ForeignId, ProcedureId, ShapeId, TypeId};
use music_term::SyntaxTerm;
use smallvec::SmallVec;

use crate::gc::RuntimeHeap;

use super::VmValueKind;

pub type ValueList = SmallVec<[Value; 8]>;
pub type ContinuationFrameList = SmallVec<[ContinuationFrame; 4]>;
pub type ContinuationHandlerList = SmallVec<[ContinuationHandler; 4]>;

const MIN_I64_ARRAY_BUFFER_LEN: usize = 16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IsolateId(u64);

impl IsolateId {
    #[must_use]
    pub(crate) const fn new(raw: u64) -> Self {
        Self(raw)
    }

    #[must_use]
    pub const fn raw(self) -> u64 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GcRef {
    pub(crate) isolate: IsolateId,
    pub(crate) slot: usize,
    pub(crate) generation: u32,
}

impl GcRef {
    #[must_use]
    pub(crate) const fn new(isolate: IsolateId, slot: usize, generation: u32) -> Self {
        Self {
            isolate,
            slot,
            generation,
        }
    }

    #[must_use]
    pub const fn isolate(self) -> IsolateId {
        self.isolate
    }

    #[must_use]
    pub const fn slot(self) -> usize {
        self.slot
    }

    #[must_use]
    pub const fn generation(self) -> u32 {
        self.generation
    }
}

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

#[derive(Debug, Clone)]
pub struct I64ArrayValue {
    cells: Box<[i64]>,
    is_shared: bool,
}

impl I64ArrayValue {
    #[must_use]
    pub(crate) fn new(cells: impl Into<Box<[i64]>>) -> Self {
        Self {
            cells: cells.into(),
            is_shared: false,
        }
    }

    #[must_use]
    pub(crate) fn shared(cells: impl Into<Box<[i64]>>) -> Self {
        Self {
            cells: cells.into(),
            is_shared: true,
        }
    }

    #[must_use]
    pub(crate) fn len(&self) -> usize {
        self.cells.len()
    }

    #[must_use]
    pub(crate) fn bytes(&self) -> usize {
        self.cells.len().saturating_mul(size_of::<i64>())
    }

    #[must_use]
    pub(crate) fn get(&self, index: usize) -> Option<i64> {
        self.cells.get(index).copied()
    }

    pub(crate) fn set(&mut self, index: usize, value: i64) -> Option<()> {
        if self.is_shared {
            return None;
        }
        *self.cells.get_mut(index)? = value;
        Some(())
    }

    #[must_use]
    pub(crate) fn values(&self) -> &[i64] {
        &self.cells
    }

    #[must_use]
    pub(crate) const fn is_shared(&self) -> bool {
        self.is_shared
    }
}

#[derive(Debug, Clone)]
pub struct SequenceValue {
    pub(crate) ty: TypeId,
    storage: SequenceStorage,
    layout_version: u32,
}

#[derive(Debug, Clone)]
enum SequenceStorage {
    Values(Box<ValueList>),
    IntPair([i64; 2]),
    IntArray { buffer: GcRef, len: usize },
}

impl SequenceValue {
    #[must_use]
    pub fn new(ty: TypeId, items: ValueList) -> Self {
        let storage = int_pair_storage(&items).map_or_else(
            || SequenceStorage::Values(Box::new(items)),
            SequenceStorage::IntPair,
        );
        Self {
            ty,
            storage,
            layout_version: 0,
        }
    }

    #[must_use]
    pub(crate) const fn from_i64_array(ty: TypeId, buffer: GcRef, len: usize) -> Self {
        Self {
            ty,
            storage: SequenceStorage::IntArray { buffer, len },
            layout_version: 0,
        }
    }

    #[must_use]
    pub(crate) fn len(&self) -> usize {
        match &self.storage {
            SequenceStorage::Values(items) => items.len(),
            SequenceStorage::IntPair(_) => 2,
            SequenceStorage::IntArray { len, .. } => *len,
        }
    }

    #[must_use]
    pub(crate) fn inline_bytes(&self) -> usize {
        match &self.storage {
            SequenceStorage::Values(items) => items.len().saturating_mul(size_of::<Value>()),
            SequenceStorage::IntPair(_) => 2_usize.saturating_mul(size_of::<i64>()),
            SequenceStorage::IntArray { .. } => {
                size_of::<GcRef>().saturating_add(size_of::<usize>())
            }
        }
    }

    #[must_use]
    pub(crate) const fn layout_version(&self) -> u32 {
        self.layout_version
    }

    #[must_use]
    pub(crate) const fn i64_array(&self) -> Option<(GcRef, usize)> {
        match &self.storage {
            SequenceStorage::IntArray { buffer, len } => Some((*buffer, *len)),
            SequenceStorage::Values(_) | SequenceStorage::IntPair(_) => None,
        }
    }

    pub(crate) fn visit_heap_children(&self, mut visit: impl FnMut(GcRef)) {
        match &self.storage {
            SequenceStorage::Values(items) => {
                for child in items.iter().filter_map(Value::gc_ref) {
                    visit(child);
                }
            }
            SequenceStorage::IntArray { buffer, .. } => visit(*buffer),
            SequenceStorage::IntPair(_) => {}
        }
    }

    pub(crate) fn has_heap_children(&self) -> bool {
        match &self.storage {
            SequenceStorage::Values(items) => items.iter().any(|value| value.gc_ref().is_some()),
            SequenceStorage::IntArray { .. } => true,
            SequenceStorage::IntPair(_) => false,
        }
    }

    #[must_use]
    pub(crate) fn get_cloned(&self, index: usize) -> Option<Value> {
        match &self.storage {
            SequenceStorage::Values(items) => items.get(index).cloned(),
            SequenceStorage::IntPair(cells) => cells.get(index).copied().map(Value::Int),
            SequenceStorage::IntArray { .. } => None,
        }
    }

    pub(crate) fn set(&mut self, index: usize, value: Value) -> Option<()> {
        match (&mut self.storage, value) {
            (SequenceStorage::Values(items), value) => {
                *items.as_mut().get_mut(index)? = value;
                self.layout_version = self.layout_version.wrapping_add(1);
                Some(())
            }
            (SequenceStorage::IntPair(cells), Value::Int(value)) => {
                *cells.get_mut(index)? = value;
                Some(())
            }
            (SequenceStorage::IntPair(cells), value) => {
                let mut items: ValueList = cells.iter().copied().map(Value::Int).collect();
                *items.get_mut(index)? = value;
                self.storage = SequenceStorage::Values(Box::new(items));
                self.layout_version = self.layout_version.wrapping_add(1);
                Some(())
            }
            (SequenceStorage::IntArray { .. }, _) => None,
        }
    }

    pub(crate) fn replace_with_values(&mut self, items: ValueList) {
        self.storage = SequenceStorage::Values(Box::new(items));
        self.layout_version = self.layout_version.wrapping_add(1);
    }

    #[must_use]
    pub(crate) const fn int_pair(&self) -> Option<[i64; 2]> {
        match &self.storage {
            SequenceStorage::IntPair(cells) => Some(*cells),
            SequenceStorage::Values(_) | SequenceStorage::IntArray { .. } => None,
        }
    }

    pub(crate) const fn int_pair_mut(&mut self) -> Option<&mut [i64; 2]> {
        let SequenceStorage::IntPair(cells) = &mut self.storage else {
            return None;
        };
        Some(cells)
    }

    pub(crate) fn values_mut(&mut self) -> Option<&mut ValueList> {
        match &mut self.storage {
            SequenceStorage::Values(items) => Some(items.as_mut()),
            SequenceStorage::IntPair(_) | SequenceStorage::IntArray { .. } => None,
        }
    }

    #[cfg(test)]
    pub(crate) fn push_value(&mut self, value: Value) {
        if let SequenceStorage::IntPair(cells) = self.storage {
            self.storage =
                SequenceStorage::Values(Box::new(cells.iter().copied().map(Value::Int).collect()));
            self.layout_version = self.layout_version.wrapping_add(1);
        }
        if let SequenceStorage::Values(items) = &mut self.storage {
            items.as_mut().push(value);
        }
    }

    pub(crate) fn clear_edges(&mut self) {
        match &mut self.storage {
            SequenceStorage::Values(items) => items.as_mut().clear(),
            SequenceStorage::IntArray { len, .. } => *len = 0,
            SequenceStorage::IntPair(_) => {}
        }
    }

    pub(crate) fn take_i64_array_cells(self) -> Result<(TypeId, Vec<i64>), Self> {
        let Self {
            ty,
            storage,
            layout_version,
        } = self;
        let SequenceStorage::Values(items) = storage else {
            return Err(Self {
                ty,
                storage,
                layout_version,
            });
        };
        if items.len() < MIN_I64_ARRAY_BUFFER_LEN
            || !items.iter().all(|value| matches!(value, Value::Int(_)))
        {
            return Err(Self {
                ty,
                storage: SequenceStorage::Values(items),
                layout_version,
            });
        }
        let mut cells = Vec::with_capacity(items.len());
        for value in items.iter() {
            if let Value::Int(value) = value {
                cells.push(*value);
            }
        }
        Ok((ty, cells))
    }
}

fn int_pair_storage(items: &ValueList) -> Option<[i64; 2]> {
    let [Value::Int(first), Value::Int(second)] = items.as_slice() else {
        return None;
    };
    Some([*first, *second])
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
    pub(crate) procedure: ProcedureId,
    pub(crate) params: u16,
    pub(crate) locals: u16,
    pub(crate) captures: ValueList,
}

impl ClosureValue {
    #[must_use]
    pub const fn new(module_slot: usize, procedure: ProcedureId, captures: ValueList) -> Self {
        Self {
            module_slot,
            procedure,
            params: 0,
            locals: 0,
            captures,
        }
    }

    #[must_use]
    pub const fn with_shape(mut self, params: u16, locals: u16) -> Self {
        self.params = params;
        self.locals = locals;
        self
    }

    #[must_use]
    pub fn param_count(&self) -> usize {
        usize::from(self.params)
    }

    #[must_use]
    pub fn local_count(&self) -> usize {
        usize::from(self.locals.max(self.params))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProcedureValue {
    pub(crate) module_slot: usize,
    pub(crate) procedure: ProcedureId,
    pub(crate) params: u16,
    pub(crate) locals: u16,
}

impl ProcedureValue {
    #[must_use]
    pub const fn new(module_slot: usize, procedure: ProcedureId, params: u16, locals: u16) -> Self {
        Self {
            module_slot,
            procedure,
            params,
            locals,
        }
    }

    #[must_use]
    pub const fn module_slot(self) -> usize {
        self.module_slot
    }

    #[must_use]
    pub const fn procedure(self) -> ProcedureId {
        self.procedure
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
    pub(crate) procedure: ProcedureId,
    pub(crate) ip: usize,
    pub(crate) locals: ValueList,
    pub(crate) stack: ValueList,
}

impl ContinuationFrame {
    #[must_use]
    pub const fn new(
        module_slot: usize,
        procedure: ProcedureId,
        locals: ValueList,
        stack: ValueList,
    ) -> Self {
        Self {
            module_slot,
            procedure,
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
