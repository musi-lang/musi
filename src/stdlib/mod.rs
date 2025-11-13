pub mod _assert;
pub mod _io;
pub mod _lang;
pub mod _math;

use std::collections::HashMap;

use crate::errors::MusiResult;
use crate::value::Value;

pub type BuiltinFunction = fn(&[Value]) -> MusiResult<Value>;

pub struct BuiltinRegistry {
    procs: HashMap<String, BuiltinFunction>,
}

impl BuiltinRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            procs: HashMap::new(),
        };
        registry.register_core_builtins();
        registry
    }

    pub fn register(&mut self, name: String, func: BuiltinFunction) {
        self.procs.insert(name, func);
    }

    pub fn get(&self, name: &str) -> Option<BuiltinFunction> {
        self.procs.get(name).copied()
    }

    pub fn contains(&self, name: &str) -> bool {
        self.procs.contains_key(name)
    }

    pub fn list_procs(&self) -> Vec<String> {
        self.procs.keys().cloned().collect()
    }

    pub fn register_core_builtins(&mut self) {
        self.register_lang_builtins();
        self.register_io_builtins();
        self.register_math_builtins();
        self.register_assert_builtins();
    }

    fn register_lang_builtins(&mut self) {
        self.register("int_to_str".to_string(), _lang::int_to_str);
        self.register("str_len".to_string(), _lang::str_len);
        self.register("array_len".to_string(), _lang::array_len);
        self.register("array_get".to_string(), _lang::array_get);
        self.register("array_set".to_string(), _lang::array_set);
    }

    fn register_io_builtins(&mut self) {
        self.register("write".to_string(), _io::write);
        self.register("writeln".to_string(), _io::writeln);
    }

    fn register_math_builtins(&mut self) {
        self.register("abs".to_string(), _math::abs);
        self.register("min".to_string(), _math::min);
        self.register("max".to_string(), _math::max);
        self.register("sqrt".to_string(), _math::sqrt);
    }

    fn register_assert_builtins(&mut self) {
        self.register("assert".to_string(), _assert::assert);
        self.register("assert_eq".to_string(), _assert::assert_eq);
        self.register("assert_neq".to_string(), _assert::assert_neq);
    }
}

impl Default for BuiltinRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_builtin_registry() {
        let registry = BuiltinRegistry::new();

        assert!(registry.contains("int_to_str"));
        assert!(registry.contains("writeln"));
        assert!(registry.contains("assert"));
        assert!(!registry.contains("nonexistent"));
    }

    #[test]
    fn test_int_to_str() {
        let result = _lang::int_to_str(&[Value::Int(42)]).unwrap();
        assert_eq!(result, Value::Str("42".to_string()));
    }

    #[test]
    fn test_str_len() {
        let result = _lang::str_len(&[Value::Str("hello".to_string())]).unwrap();
        assert_eq!(result, Value::Nat(5));
    }

    #[test]
    fn test_array_ops() {
        let arr = Value::array(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);

        let len = _lang::array_len(&[arr.clone()]).unwrap();
        assert_eq!(len, Value::Nat(3));

        let elem = _lang::array_get(&[arr.clone(), Value::Nat(1)]).unwrap();
        assert_eq!(elem, Value::Int(2));
    }

    #[test]
    fn test_math_builtins() {
        let abs_result = _math::abs(&[Value::Int(-5)]).unwrap();
        assert_eq!(abs_result, Value::Int(5));

        let min_result = _math::min(&[Value::Int(3), Value::Int(7)]).unwrap();
        assert_eq!(min_result, Value::Int(3));
        let max_result = _math::max(&[Value::Int(3), Value::Int(7)]).unwrap();
        assert_eq!(max_result, Value::Int(7));
    }
}
