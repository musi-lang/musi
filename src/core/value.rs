#[derive(Debug)]
#[non_exhaustive]
pub enum Value {
    Integer(i64),
    Natural(u64),
    Real(f64),
    Boolean(bool),

    String(Vec<u8>),
    Character(u8),

    Function {
        name: Box<str>,
        code: Vec<u8>,
        arity: usize,
    },
    Reference(Box<Value>),

    Unit,
}
