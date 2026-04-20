use super::{
    ClosureView, ModuleView, RecordView, SeqView, StringView, SyntaxView, Value, ValueView, Vm,
};

impl Vm {
    /// Inspects one value through this VM heap.
    ///
    /// # Panics
    ///
    /// Panics when given a stale heap reference that did not come from this VM or was collected.
    #[must_use]
    pub fn inspect<'a>(&'a self, value: &'a Value) -> ValueView<'a> {
        match value {
            Value::Unit => ValueView::Unit,
            Value::Int(value) => ValueView::Int(*value),
            Value::Nat(value) => ValueView::Nat(*value),
            Value::Float(value) => ValueView::Float(*value),
            Value::String(text) => ValueView::String(StringView::new(
                self.heap.string(*text).expect("live string"),
            )),
            Value::CPtr(address) => ValueView::CPtr(*address),
            Value::Syntax(term) => ValueView::Syntax(SyntaxView::new(
                self.heap.syntax(*term).expect("live syntax"),
            )),
            Value::Seq(seq) => ValueView::Seq(SeqView::new(&self.heap, *seq)),
            Value::Data(data) => {
                let inner = self.heap.data(*data).expect("live data");
                if inner.fields.is_empty() && self.is_named_type(inner.ty, "Bool") {
                    ValueView::Bool(inner.tag != 0)
                } else if inner.tag == 0 {
                    ValueView::Record(RecordView::new(inner))
                } else {
                    ValueView::Data(RecordView::new(inner))
                }
            }
            Value::Closure(closure) => ValueView::Closure(ClosureView::new(
                self.heap.closure(*closure).expect("live closure"),
            )),
            Value::Procedure(procedure) => ValueView::Procedure(*procedure),
            Value::Continuation(_) => ValueView::Continuation,
            Value::Type(ty) => ValueView::Type(*ty),
            Value::Module(module) => {
                let module = self.heap.module(*module).expect("live module");
                ValueView::Module(ModuleView::new(&module.spec, module.slot))
            }
            Value::Foreign(foreign) => ValueView::Foreign(foreign.foreign),
            Value::Effect(effect) => ValueView::Effect(*effect),
            Value::Class(class) => ValueView::Class(*class),
        }
    }
}
