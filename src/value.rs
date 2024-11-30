#[derive(Debug)]
pub enum Value {
    Integer(i64),
    Natural(u64),
    Real(f64),
    Boolean(bool),

    Array(Vec<Value>), // [v, v, v, ...]

    String(Vec<char>),
    Function {
        name: Box<str>,
        code: Vec<u8>,
        arity: usize,
    },
    Reference(Box<Value>),

    Unit,
}
