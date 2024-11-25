pub enum Value {
    Integer(i64),
    Natural(u64),
    Real(f64),
    Boolean(bool),

    String(Box<str>),
    Function {
        name: Box<str>,
        code: Vec<u8>,
        arity: usize,
    },
    Reference(Box<Value>),

    Nothing,
}
