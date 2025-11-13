use crate::MAX_STACK_SIZE;
use crate::errors::{MusiError, MusiResult};
use crate::value::Value;

pub struct OperandStack {
    data: Vec<Value>,
}

impl OperandStack {
    pub fn new() -> Self {
        Self {
            data: Vec::with_capacity(1024),
        }
    }

    pub fn push(&mut self, value: Value) -> MusiResult<()> {
        if self.data.len() >= MAX_STACK_SIZE {
            return Err(MusiError::StackOverflow(MAX_STACK_SIZE));
        }
        self.data.push(value);
        Ok(())
    }

    pub fn pop(&mut self) -> MusiResult<Value> {
        self.data.pop().ok_or(MusiError::StackUnderflow)
    }

    fn check_bounds(&self, offset: usize) -> MusiResult<usize> {
        if offset >= self.data.len() {
            return Err(MusiError::StackUnderflow);
        }
        Ok(self.data.len() - 1 - offset)
    }

    pub fn dup(&mut self) -> MusiResult<()> {
        let top = self.peek(0)?;
        self.push(top.clone())
    }

    pub fn peek(&self, offset: usize) -> MusiResult<&Value> {
        let idx = self.check_bounds(offset)?;
        Ok(&self.data[idx])
    }

    pub fn poke(&mut self, offset: usize, value: Value) -> MusiResult<()> {
        let idx = self.check_bounds(offset)?;
        self.data[idx] = value;
        Ok(())
    }

    pub fn size(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }

    pub fn truncate(&mut self, new_size: usize) -> MusiResult<()> {
        if new_size > self.data.len() {
            return Err(MusiError::StackUnderflow);
        }
        self.data.truncate(new_size);
        Ok(())
    }

    pub fn pop_n(&mut self, count: usize) -> MusiResult<Vec<Value>> {
        if count > self.data.len() {
            return Err(MusiError::StackUnderflow);
        }

        let start = self.data.len() - count;
        let result = self.data.split_off(start);
        Ok(result)
    }

    pub fn push_n(&mut self, values: Vec<Value>) -> MusiResult<()> {
        if self.data.len() + values.len() > MAX_STACK_SIZE {
            return Err(MusiError::StackOverflow(MAX_STACK_SIZE));
        }
        self.data.extend(values);
        Ok(())
    }

    pub fn swap(&mut self, offset1: usize, offset2: usize) -> MusiResult<()> {
        let idx1 = self.check_bounds(offset1)?;
        let idx2 = self.check_bounds(offset2)?;
        self.data.swap(idx1, idx2);
        Ok(())
    }

    pub fn rotate(&mut self, n: usize) -> MusiResult<()> {
        if n > self.data.len() {
            return Err(MusiError::StackUnderflow);
        }
        if n > 1 {
            let start = self.data.len() - n;
            self.data[start..].rotate_left(1);
        }
        Ok(())
    }

    pub fn debug_dump(&self) -> String {
        if self.data.is_empty() {
            return "(empty)".to_string();
        }
        format!(
            "OperandStack (size={})\n{}",
            self.data.len(),
            self.data
                .iter()
                .enumerate()
                .map(|(i, v)| format!("  [{i}] {v}"))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

impl Default for OperandStack {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for OperandStack {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_pop() {
        let mut stack = OperandStack::new();
        assert!(stack.is_empty());

        stack.push(Value::Int(42)).unwrap();
        assert_eq!(stack.size(), 1);
        assert!(!stack.is_empty());

        let value = stack.pop().unwrap();
        assert_eq!(value, Value::Int(42));
        assert!(stack.is_empty());
    }

    #[test]
    fn test_stack_underflow() {
        let mut stack = OperandStack::new();
        let result = stack.pop();
        assert!(matches!(result, Err(MusiError::StackUnderflow)));
    }

    #[test]
    fn test_dup() {
        let mut stack = OperandStack::new();
        stack.push(Value::Int(42)).unwrap();
        stack.dup().unwrap();

        assert_eq!(stack.size(), 2);
        let top = stack.pop().unwrap();
        assert_eq!(top, Value::Int(42));
        let second = stack.pop().unwrap();
        assert_eq!(second, Value::Int(42));
    }

    #[test]
    fn test_peek() {
        let mut stack = OperandStack::new();
        stack.push(Value::Int(1)).unwrap();
        stack.push(Value::Int(2)).unwrap();
        stack.push(Value::Int(3)).unwrap();

        assert_eq!(*stack.peek(0).unwrap(), Value::Int(3));
        assert_eq!(*stack.peek(1).unwrap(), Value::Int(2));
        assert_eq!(*stack.peek(2).unwrap(), Value::Int(1));
        assert!(stack.peek(3).is_err());
    }

    #[test]
    fn test_swap() {
        let mut stack = OperandStack::new();
        stack.push(Value::Int(1)).unwrap();
        stack.push(Value::Int(2)).unwrap();

        stack.swap(0, 1).unwrap();
        assert_eq!(*stack.peek(0).unwrap(), Value::Int(1));
        assert_eq!(*stack.peek(1).unwrap(), Value::Int(2));
    }

    #[test]
    fn test_rotate() {
        let mut stack = OperandStack::new();
        stack.push(Value::Int(1)).unwrap();
        stack.push(Value::Int(2)).unwrap();
        stack.push(Value::Int(3)).unwrap();

        stack.rotate(3).unwrap();
        assert_eq!(*stack.peek(0).unwrap(), Value::Int(1));
        assert_eq!(*stack.peek(1).unwrap(), Value::Int(3));
        assert_eq!(*stack.peek(2).unwrap(), Value::Int(2));
    }

    #[test]
    fn test_pop_n_push_n() {
        let mut stack = OperandStack::new();
        stack.push(Value::Int(1)).unwrap();
        stack.push(Value::Int(2)).unwrap();
        stack.push(Value::Int(3)).unwrap();

        let values = stack.pop_n(2).unwrap();
        assert_eq!(values, vec![Value::Int(2), Value::Int(3)]);
        assert_eq!(stack.size(), 1);

        stack.push_n(vec![Value::Int(4), Value::Int(5)]).unwrap();
        assert_eq!(stack.size(), 3);
    }
}
