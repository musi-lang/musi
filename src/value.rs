#[derive(Debug)]
pub enum Value {
    Integer(i64),
    Natural(u64),
    Real(f64),
    Boolean(bool),

    String(Vec<char>),
    Character(char),

    Function {
        name: Box<str>,
        code: Vec<u8>,
        arity: usize,
    },
    Reference(Box<Value>),

    Unit,
}
