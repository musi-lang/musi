use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

use serde::{Deserialize, Serialize};

use crate::errors::{MusiError, MusiResult};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Value {
    Unit,
    Bool(bool),
    Rune(char),
    Str(String),

    Int(isize),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Int128(i128),

    Nat(usize),
    Nat8(u8),
    Nat16(u16),
    Nat32(u32),
    Nat64(u64),
    Nat128(u128),

    Bin32(f32),
    Bin64(f64),

    ArrayList(Vec<Self>),
    Array { elems: Vec<Self>, size: usize },
    Tuple(Vec<Self>),
    Record(HashMap<String, Self>),
    Callable(Callable),

    Option(Option<Box<Self>>),
    Expect(Box<ExpectValue>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExpectValue {
    Pass(Value),
    Fail(Value),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Callable {
    pub name: String,
    pub params: Vec<String>,
    pub arity: usize,
    pub closure: Option<HashMap<String, Value>>, // for closures
    // TODO: reference bytecode or Rust function
    pub bc_offset: Option<usize>,
}

impl Value {
    pub const fn show(&self) -> &'static str {
        match self {
            Self::Unit => "Unit",
            Self::Bool(_) => "Bool",
            Self::Rune(_) => "Rune",
            Self::Str(_) => "Str",
            Self::Int(_) => "Int",
            Self::Nat(_) => "Nat",
            Self::Int8(_) => "Int8",
            Self::Int16(_) => "Int16",
            Self::Int32(_) => "Int32",
            Self::Int64(_) => "Int64",
            Self::Int128(_) => "Int128",
            Self::Nat8(_) => "Nat8",
            Self::Nat16(_) => "Nat16",
            Self::Nat32(_) => "Nat32",
            Self::Nat64(_) => "Nat64",
            Self::Nat128(_) => "Nat128",
            Self::Bin32(_) => "Bin32",
            Self::Bin64(_) => "Bin64",
            Self::ArrayList(_) => "ArrayList",
            Self::Array { .. } => "Array",
            Self::Tuple(_) => "Tuple",
            Self::Record(_) => "Record",
            Self::Callable(_) => "Callable",
            Self::Option(_) => "Option",
            Self::Expect(_) => "Expect",
        }
    }

    pub const fn is_numeric(&self) -> bool {
        matches!(
            self,
            Self::Int(_)
                | Self::Nat(_)
                | Self::Int8(_)
                | Self::Int16(_)
                | Self::Int32(_)
                | Self::Int64(_)
                | Self::Int128(_)
                | Self::Nat8(_)
                | Self::Nat16(_)
                | Self::Nat32(_)
                | Self::Nat64(_)
                | Self::Nat128(_)
                | Self::Bin32(_)
                | Self::Bin64(_)
        )
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Bool(b) => *b,
            Self::Int(i) => *i != 0,
            Self::Nat(n) => *n != 0,
            Self::Int8(i) => *i != 0,
            Self::Int16(i) => *i != 0,
            Self::Int32(i) => *i != 0,
            Self::Int64(i) => *i != 0,
            Self::Int128(i) => *i != 0,
            Self::Nat8(n) => *n != 0,
            Self::Nat16(n) => *n != 0,
            Self::Nat32(n) => *n != 0,
            Self::Nat64(n) => *n != 0,
            Self::Nat128(n) => *n != 0,
            Self::Bin32(f) => *f != 0.0,
            Self::Bin64(f) => *f != 0.0,
            Self::Unit => false,
            Self::Option(None) => false,
            Self::Option(Some(_)) => true,
            Self::Str(s) => !s.is_empty(),
            Self::ArrayList(arr) => !arr.is_empty(),
            Self::Array { elems, .. } => !elems.is_empty(),
            Self::Tuple(tup) => !tup.is_empty(),
            Self::Record(rec) => !rec.is_empty(),
            Self::Callable(_) => true,
            Self::Expect(exp) => matches!(exp.as_ref(), ExpectValue::Pass(_)),
            Self::Rune(c) => *c != '\0',
        }
    }

    /// Generic helper for integer conversion to signed integers
    fn as_int_variant<T>(&self) -> MusiResult<T>
    where
        T: TryFrom<isize> + TryFrom<usize>,
    {
        match self {
            Self::Int(i) => {
                T::try_from(*i).map_err(|_| MusiError::invalid_cast(self.show(), "Int"))
            }
            Self::Nat(n) => {
                T::try_from(*n).map_err(|_| MusiError::invalid_cast(self.show(), "Int"))
            }
            Self::Int8(i) => {
                T::try_from(*i as isize).map_err(|_| MusiError::invalid_cast(self.show(), "Int"))
            }
            Self::Int16(i) => {
                T::try_from(*i as isize).map_err(|_| MusiError::invalid_cast(self.show(), "Int"))
            }
            Self::Int32(i) => {
                T::try_from(*i as isize).map_err(|_| MusiError::invalid_cast(self.show(), "Int"))
            }
            Self::Int64(i) => {
                T::try_from(*i as isize).map_err(|_| MusiError::invalid_cast(self.show(), "Int"))
            }
            Self::Int128(i) => {
                T::try_from(*i as isize).map_err(|_| MusiError::invalid_cast(self.show(), "Int"))
            }
            Self::Nat8(n) => {
                T::try_from(*n as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Int"))
            }
            Self::Nat16(n) => {
                T::try_from(*n as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Int"))
            }
            Self::Nat32(n) => {
                T::try_from(*n as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Int"))
            }
            Self::Nat64(n) => {
                T::try_from(*n as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Int"))
            }
            Self::Nat128(n) => {
                T::try_from(*n as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Int"))
            }
            _ => Err(MusiError::invalid_cast(self.show(), "Int")),
        }
    }

    /// Generic helper for integer conversion to unsigned integers
    fn as_nat_variant<T>(&self) -> MusiResult<T>
    where
        T: TryFrom<usize> + TryFrom<isize>,
    {
        match self {
            Self::Nat(n) => {
                T::try_from(*n).map_err(|_| MusiError::invalid_cast(self.show(), "Nat"))
            }
            Self::Int(i) if *i >= 0 => {
                T::try_from(*i as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Nat"))
            }
            Self::Int8(i) if *i >= 0 => {
                T::try_from(*i as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Nat"))
            }
            Self::Int16(i) if *i >= 0 => {
                T::try_from(*i as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Nat"))
            }
            Self::Int32(i) if *i >= 0 => {
                T::try_from(*i as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Nat"))
            }
            Self::Int64(i) if *i >= 0 => {
                T::try_from(*i as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Nat"))
            }
            Self::Int128(i) if *i >= 0 => {
                T::try_from(*i as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Nat"))
            }
            Self::Nat8(n) => {
                T::try_from(*n as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Nat"))
            }
            Self::Nat16(n) => {
                T::try_from(*n as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Nat"))
            }
            Self::Nat32(n) => {
                T::try_from(*n as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Nat"))
            }
            Self::Nat64(n) => {
                T::try_from(*n as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Nat"))
            }
            Self::Nat128(n) => {
                T::try_from(*n as usize).map_err(|_| MusiError::invalid_cast(self.show(), "Nat"))
            }
            _ => Err(MusiError::invalid_cast(self.show(), "Nat")),
        }
    }

    pub fn as_int(&self) -> MusiResult<isize> {
        self.as_int_variant()
    }

    pub fn as_nat(&self) -> MusiResult<usize> {
        self.as_nat_variant()
    }

    pub fn as_str(&self) -> MusiResult<String> {
        match self {
            Self::Str(s) => Ok(s.clone()),
            _ => Err(MusiError::invalid_cast(self.show(), "Str")),
        }
    }

    pub fn as_bool(&self) -> MusiResult<bool> {
        match self {
            Self::Bool(b) => Ok(*b),
            _ => Err(MusiError::invalid_cast(self.show(), "Bool")),
        }
    }

    pub fn as_array(&self) -> MusiResult<&Vec<Value>> {
        match self {
            Self::ArrayList(arr) => Ok(arr),
            Self::Array { elems, .. } => Ok(elems),
            _ => Err(MusiError::invalid_cast(self.show(), "Array")),
        }
    }

    pub fn as_array_mut(&mut self) -> MusiResult<&mut Vec<Value>> {
        match self {
            Self::ArrayList(arr) => Ok(arr),
            Self::Array { elems, .. } => Ok(elems),
            _ => Err(MusiError::invalid_cast(self.show(), "Array")),
        }
    }

    pub fn as_tuple(&self) -> MusiResult<&Vec<Value>> {
        match self {
            Self::Tuple(tuple) => Ok(tuple),
            _ => Err(MusiError::invalid_cast(self.show(), "Tuple")),
        }
    }

    pub fn as_record(&self) -> MusiResult<&HashMap<String, Value>> {
        match self {
            Self::Record(record) => Ok(record),
            _ => Err(MusiError::invalid_cast(self.show(), "Record")),
        }
    }

    pub fn as_record_mut(&mut self) -> MusiResult<&mut HashMap<String, Value>> {
        match self {
            Self::Record(record) => Ok(record),
            _ => Err(MusiError::invalid_cast(self.show(), "Record")),
        }
    }

    pub const fn none() -> Self {
        Self::Option(None)
    }

    pub fn some(value: Self) -> Self {
        Self::Option(Some(Box::new(value)))
    }

    pub fn pass(value: Self) -> Self {
        Self::Expect(Box::new(ExpectValue::Pass(value)))
    }
    pub fn fail(value: Self) -> Self {
        Self::Expect(Box::new(ExpectValue::Fail(value)))
    }

    pub fn equals(&self, other: &Self) -> MusiResult<bool> {
        match (self, other) {
            (Self::Unit, Self::Unit) => Ok(true),
            (Self::Bool(a), Self::Bool(b)) => Ok(a == b),
            (Self::Rune(a), Self::Rune(b)) => Ok(a == b),
            (Self::Str(a), Self::Str(b)) => Ok(a == b),
            (Self::Int(a), Self::Int(b)) => Ok(a == b),
            (Self::Nat(a), Self::Nat(b)) => Ok(a == b),
            (Self::Int8(a), Self::Int8(b)) => Ok(a == b),
            (Self::Int16(a), Self::Int16(b)) => Ok(a == b),
            (Self::Int32(a), Self::Int32(b)) => Ok(a == b),
            (Self::Int64(a), Self::Int64(b)) => Ok(a == b),
            (Self::Int128(a), Self::Int128(b)) => Ok(a == b),
            (Self::Nat8(a), Self::Nat8(b)) => Ok(a == b),
            (Self::Nat16(a), Self::Nat16(b)) => Ok(a == b),
            (Self::Nat32(a), Self::Nat32(b)) => Ok(a == b),
            (Self::Nat64(a), Self::Nat64(b)) => Ok(a == b),
            (Self::Nat128(a), Self::Nat128(b)) => Ok(a == b),
            (Self::Bin32(a), Self::Bin32(b)) => Ok(a == b),
            (Self::Bin64(a), Self::Bin64(b)) => Ok(a == b),
            (Self::Option(a), Self::Option(b)) => Ok(a == b),
            (Self::Expect(a), Self::Expect(b)) => match (a.as_ref(), b.as_ref()) {
                (ExpectValue::Pass(av), ExpectValue::Pass(bv)) => av.equals(bv),
                (ExpectValue::Fail(av), ExpectValue::Fail(bv)) => av.equals(bv),
                _ => Ok(false),
            },
            (Self::ArrayList(a), Self::ArrayList(b)) => {
                if a.len() != b.len() {
                    return Ok(false);
                }
                for (i, av) in a.iter().enumerate() {
                    if !av.equals(&b[i])? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (
                Self::Array {
                    elems: a,
                    size: size_a,
                },
                Self::Array {
                    elems: b,
                    size: size_b,
                },
            ) => {
                if size_a != size_b || a.len() != b.len() {
                    return Ok(false);
                }
                for (i, av) in a.iter().enumerate() {
                    if !av.equals(&b[i])? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (Self::Tuple(a), Self::Tuple(b)) => {
                if a.len() != b.len() {
                    return Ok(false);
                }
                for (i, av) in a.iter().enumerate() {
                    if !av.equals(&b[i])? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (Self::Record(a), Self::Record(b)) => {
                if a.len() != b.len() {
                    return Ok(false);
                }
                for (key, av) in a {
                    if let Some(bv) = b.get(key) {
                        if !av.equals(bv)? {
                            return Ok(false);
                        }
                    } else {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    pub fn hash<H: Hasher>(&self, hasher: &mut H) -> MusiResult<()> {
        match self {
            Self::Unit => {
                0u8.hash(hasher);
            }
            Self::Bool(b) => {
                b.hash(hasher);
            }
            Self::Rune(c) => {
                c.hash(hasher);
            }
            Self::Str(s) => {
                s.hash(hasher);
            }
            Self::Int(i) => {
                i.hash(hasher);
            }
            Self::Nat(n) => {
                n.hash(hasher);
            }
            Self::Int8(i) => {
                i.hash(hasher);
            }
            Self::Int16(i) => {
                i.hash(hasher);
            }
            Self::Int32(i) => {
                i.hash(hasher);
            }
            Self::Int64(i) => {
                i.hash(hasher);
            }
            Self::Int128(i) => {
                i.hash(hasher);
            }
            Self::Nat8(n) => {
                n.hash(hasher);
            }
            Self::Nat16(n) => {
                n.hash(hasher);
            }
            Self::Nat32(n) => {
                n.hash(hasher);
            }
            Self::Nat64(n) => {
                n.hash(hasher);
            }
            Self::Nat128(n) => {
                n.hash(hasher);
            }
            Self::Bin32(f) => {
                f.to_bits().hash(hasher);
            }
            Self::Bin64(f) => {
                f.to_bits().hash(hasher);
            }
            // TODO: need recursive hashing
            Self::Option(opt) => match opt {
                None => {
                    0u8.hash(hasher);
                }
                Some(v) => {
                    1u8.hash(hasher);
                    v.hash(hasher);
                }
            },
            Self::Expect(result) => match result.as_ref() {
                ExpectValue::Pass(v) => {
                    0u8.hash(hasher);
                    let _ = v.hash(hasher);
                }
                ExpectValue::Fail(e) => {
                    1u8.hash(hasher);
                    let _ = e.hash(hasher);
                }
            },
            _ => {
                // TODO: need more sophisticated hashing
                self.show().hash(hasher);
            }
        }
        Ok(())
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Rune(c) => write!(f, "'{c}'"),
            Self::Str(s) => write!(f, "\"{s}\""),
            Self::Int(i) => write!(f, "{i}"),
            Self::Nat(n) => write!(f, "{n}"),
            Self::Int8(i) => write!(f, "{i}i8"),
            Self::Int16(i) => write!(f, "{i}i16"),
            Self::Int32(i) => write!(f, "{i}i32"),
            Self::Int64(i) => write!(f, "{i}i64"),
            Self::Int128(i) => write!(f, "{i}i128"),
            Self::Nat8(n) => write!(f, "{n}n8"),
            Self::Nat16(n) => write!(f, "{n}n16"),
            Self::Nat32(n) => write!(f, "{n}n32"),
            Self::Nat64(n) => write!(f, "{n}n64"),
            Self::Nat128(n) => write!(f, "{n}n128"),
            Self::Bin32(fl) => write!(f, "{fl}b32"),
            Self::Bin64(fl) => write!(f, "{fl}b64"),
            Self::ArrayList(arr) => {
                write!(f, "[")?;
                for (i, val) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{val}")?;
                }
                write!(f, "]")
            }
            Self::Array { elems, size } => {
                write!(f, "[")?;
                for (i, val) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{val}")?;
                }
                write!(f, "; {size}]")
            }
            Self::Tuple(tup) => {
                write!(f, "(")?;
                for (i, val) in tup.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{val}")?;
                }
                write!(f, ")")
            }
            Self::Record(record) => {
                write!(f, "{{")?;
                for (i, (key, val)) in record.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, ".{key} := {val}")?;
                }
                write!(f, "}}")
            }
            Self::Callable(proc) => write!(f, "<proc {}>", proc.name),
            Self::Option(None) => write!(f, "none"),
            Self::Option(Some(val)) => write!(f, "some({val})"),
            Self::Expect(exp) => match exp.as_ref() {
                ExpectValue::Pass(val) => write!(f, "pass({val})"),
                ExpectValue::Fail(err) => write!(f, "fail({err})"),
            },
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.equals(other).unwrap_or(false)
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash(state).unwrap();
    }
}

// From implementations for common types
impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Self::Bool(b)
    }
}

impl From<char> for Value {
    fn from(c: char) -> Self {
        Self::Rune(c)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Self::Str(s)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Self::Str(s.to_string())
    }
}

impl From<isize> for Value {
    fn from(i: isize) -> Self {
        Self::Int(i)
    }
}

impl From<usize> for Value {
    fn from(n: usize) -> Self {
        Self::Nat(n)
    }
}

impl From<i8> for Value {
    fn from(i: i8) -> Self {
        Self::Int8(i)
    }
}

impl From<u8> for Value {
    fn from(n: u8) -> Self {
        Self::Nat8(n)
    }
}

impl From<i16> for Value {
    fn from(i: i16) -> Self {
        Self::Int16(i)
    }
}

impl From<u16> for Value {
    fn from(n: u16) -> Self {
        Self::Nat16(n)
    }
}

impl From<i32> for Value {
    fn from(i: i32) -> Self {
        Self::Int32(i)
    }
}

impl From<u32> for Value {
    fn from(n: u32) -> Self {
        Self::Nat32(n)
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Self::Int64(i)
    }
}

impl From<u64> for Value {
    fn from(n: u64) -> Self {
        Self::Nat64(n)
    }
}

impl From<i128> for Value {
    fn from(i: i128) -> Self {
        Self::Int128(i)
    }
}

impl From<u128> for Value {
    fn from(n: u128) -> Self {
        Self::Nat128(n)
    }
}

impl From<f32> for Value {
    fn from(f: f32) -> Self {
        Self::Bin32(f)
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Self::Bin64(f)
    }
}

impl From<Vec<Value>> for Value {
    fn from(v: Vec<Value>) -> Self {
        Self::ArrayList(v)
    }
}

impl Value {
    pub const fn unit() -> Self {
        Self::Unit
    }
    pub const fn bool(b: bool) -> Self {
        Self::Bool(b)
    }
    pub const fn rune(c: char) -> Self {
        Self::Rune(c)
    }
    pub fn str(s: impl Into<String>) -> Self {
        Self::Str(s.into())
    }
    pub const fn int(i: isize) -> Self {
        Self::Int(i)
    }
    pub const fn nat(n: usize) -> Self {
        Self::Nat(n)
    }

    pub fn array<T: Into<Value>>(items: Vec<T>) -> Self {
        Self::ArrayList(items.into_iter().map(|v| v.into()).collect())
    }

    pub fn array_list<T: Into<Value>>(items: Vec<T>) -> Self {
        let elems: Vec<Value> = items.into_iter().map(|v| v.into()).collect();
        let size = elems.len();
        Self::Array { elems, size }
    }

    pub fn tuple<T: Into<Value>>(items: Vec<T>) -> Self {
        Self::Tuple(items.into_iter().map(|v| v.into()).collect())
    }

    pub fn record(pairs: Vec<(impl Into<String>, impl Into<Value>)>) -> Self {
        let mut record = HashMap::new();
        for (key, value) in pairs {
            record.insert(key.into(), value.into());
        }
        Self::Record(record)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_creation() {
        let unit = Value::unit();
        let bool_val = Value::bool(true);
        let int_val = Value::int(42);
        let str_val = Value::str("hello");

        assert_eq!(unit.show(), "Unit");
        assert_eq!(bool_val.show(), "Bool");
        assert_eq!(int_val.show(), "Int");
        assert_eq!(str_val.show(), "Str");
    }

    #[test]
    fn test_array_creation() {
        let arr = Value::array(vec![Value::int(1), Value::int(2), Value::int(3)]);
        assert_eq!(arr.show(), "ArrayList");

        let arr_ref = arr.as_array().unwrap();
        assert_eq!(arr_ref.len(), 3);
    }

    #[test]
    fn test_record_creation() {
        let rec = Value::record(vec![("x", Value::int(42)), ("y", Value::int(24))]);

        assert_eq!(rec.show(), "Record");
        let rec_ref = rec.as_record().unwrap();
        assert_eq!(rec_ref.get("x").unwrap(), &Value::int(42));
        assert_eq!(rec_ref.get("y").unwrap(), &Value::int(24));
    }

    #[test]
    fn test_truthy_values() {
        assert!(!Value::unit().is_truthy());
        assert!(!Value::bool(false).is_truthy());
        assert!(Value::bool(true).is_truthy());
        assert!(!Value::int(0).is_truthy());
        assert!(Value::int(42).is_truthy());
        assert!(!Value::str("").is_truthy());
        assert!(Value::str("hello").is_truthy());
    }

    #[test]
    fn test_type_conversions() {
        let int_val = Value::int(42);
        assert_eq!(int_val.as_int().unwrap(), 42);
        assert_eq!(int_val.as_nat().unwrap(), 42);

        let str_val = Value::str("test");
        assert!(str_val.as_str().is_ok());
        assert!(str_val.as_int().is_err());
    }

    #[test]
    fn test_option_expect() {
        let some_val = Value::some(Value::int(42));
        let none_val = Value::none();
        let pass_val = Value::pass(Value::str("success"));
        let fail_val = Value::fail(Value::str("failure"));

        assert_eq!(some_val.show(), "Option");
        assert_eq!(none_val.show(), "Option");
        assert_eq!(pass_val.show(), "Expect");
        assert_eq!(fail_val.show(), "Expect");
    }
}
