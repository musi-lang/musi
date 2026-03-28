use crate::value::Value;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArrayValue {
    pub tag: Value,
    pub elements: Vec<Value>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ValueView {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    Tag(u16),
    String(String),
    Array(ArrayValue),
    Slice { length: usize },
    Cell(Value),
    CPtr,
    Closure,
    Continuation,
}
