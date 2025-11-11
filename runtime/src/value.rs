use alloc::rc::Rc;
use alloc::string::String;
use alloc::vec::Vec;

#[derive(Clone)]
pub enum Value {
    Int64(i64),
    Bin64(f64),
    Bool(bool),
    Str(Rc<String>),
    Unit,
}

pub type ValueList = Vec<Value>;
