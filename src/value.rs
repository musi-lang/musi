use std::sync::Arc;

#[derive(Clone, Debug)]
pub enum Value {
    Integer(i64),
    Natural(u64),
    Real(f64),
    Boolean(bool),
    String(Arc<String>),
    Nothing,
    Function {
        name: Arc<String>,
        arity: usize,
        code: Arc<[u8]>,
    },
}
