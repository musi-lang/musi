use crate::MAX_CALL_DEPTH;
use crate::errors::{MusiError, MusiResult};
use crate::value::Value;

#[derive(Debug, Clone)]
pub struct CallFrame {
    ret_addr: usize,
    proc_name: String,
    locals: Vec<Value>,
    arg_count: usize,
    stack_size_before: usize,
}

impl CallFrame {
    pub fn new(
        ret_addr: usize,
        proc_name: String,
        local_count: usize,
        arg_count: usize,
        stack_size_before: usize,
    ) -> Self {
        let mut locals = Vec::with_capacity(local_count);
        locals.resize_with(local_count, || Value::Unit);

        Self {
            ret_addr,
            proc_name,
            locals,
            arg_count,
            stack_size_before,
        }
    }

    pub fn ret_addr(&self) -> usize {
        self.ret_addr
    }

    pub fn proc_name(&self) -> &str {
        &self.proc_name
    }

    pub fn arg_count(&self) -> usize {
        self.arg_count
    }

    pub fn stack_size_before(&self) -> usize {
        self.stack_size_before
    }

    pub fn local_count(&self) -> usize {
        self.locals.len()
    }

    fn check_bounds(&self, idx: usize, max: usize) -> MusiResult<()> {
        if idx >= max {
            return Err(MusiError::IndexOutOfBounds { idx, len: max });
        }
        Ok(())
    }

    pub fn get_local(&self, index: usize) -> MusiResult<&Value> {
        self.check_bounds(index, self.locals.len())?;
        Ok(&self.locals[index])
    }

    pub fn set_loc(&mut self, index: usize, value: Value) -> MusiResult<()> {
        self.check_bounds(index, self.locals.len())?;
        self.locals[index] = value;
        Ok(())
    }

    pub fn get_arg(&self, index: usize) -> MusiResult<&Value> {
        self.check_bounds(index, self.arg_count)?;
        Ok(&self.locals[index])
    }

    pub fn set_arg(&mut self, index: usize, value: Value) -> MusiResult<()> {
        self.check_bounds(index, self.arg_count)?;
        self.locals[index] = value;
        Ok(())
    }

    pub fn locals(&self) -> &[Value] {
        &self.locals
    }

    pub fn locals_mut(&mut self) -> &mut [Value] {
        &mut self.locals
    }

    pub fn debug_dump(&self) -> String {
        let mut result = format!("at {}+0x{:04X}\n", self.proc_name, self.ret_addr);

        if !self.locals.is_empty() {
            result.push_str("  Locals: ");
            for (i, local) in self.locals.iter().enumerate() {
                if i > 0 {
                    result.push(' ');
                }
                result.push_str(&format!("[{i}]={local}"));
            }
        }

        result
    }
}

pub struct CallStack {
    frames: Vec<CallFrame>,
    max_depth: usize,
}

impl CallStack {
    pub fn new() -> Self {
        Self {
            frames: Vec::with_capacity(128),
            max_depth: MAX_CALL_DEPTH,
        }
    }

    pub fn push(&mut self, frame: CallFrame) -> MusiResult<()> {
        if self.frames.len() >= self.max_depth {
            return Err(MusiError::CallDepthExceeded(self.max_depth));
        }
        self.frames.push(frame);
        Ok(())
    }

    pub fn pop(&mut self) -> MusiResult<CallFrame> {
        self.frames
            .pop()
            .ok_or(MusiError::InvalidVmState("Empty call stack".to_string()))
    }

    pub fn peek(&self) -> MusiResult<&CallFrame> {
        self.frames
            .last()
            .ok_or(MusiError::InvalidVmState("Empty call stack".to_string()))
    }

    pub fn peek_mut(&mut self) -> MusiResult<&mut CallFrame> {
        self.frames
            .last_mut()
            .ok_or(MusiError::InvalidVmState("Empty call stack".to_string()))
    }

    pub fn depth(&self) -> usize {
        self.frames.len()
    }

    pub fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }

    pub fn clear(&mut self) {
        self.frames.clear();
    }

    fn frame_index(&self, depth: usize) -> MusiResult<usize> {
        if depth >= self.frames.len() {
            return Err(MusiError::IndexOutOfBounds {
                idx: depth,
                len: self.frames.len(),
            });
        }
        Ok(self.frames.len() - 1 - depth)
    }

    pub fn get_frame(&self, depth: usize) -> MusiResult<&CallFrame> {
        let idx = self.frame_index(depth)?;
        Ok(&self.frames[idx])
    }

    pub fn get_frame_mut(&mut self, depth: usize) -> MusiResult<&mut CallFrame> {
        let idx = self.frame_index(depth)?;
        Ok(&mut self.frames[idx])
    }

    pub fn debug_dump(&self) -> String {
        if self.frames.is_empty() {
            return "(empty)".to_string();
        }

        let mut result = format!("CallStack (depth={})\n", self.frames.len());
        let last_idx = self.frames.len() - 1;

        for (i, frame) in self.frames.iter().rev().enumerate() {
            let connector = if i == last_idx { "└─" } else { "├─" };
            let frame_dump = frame.debug_dump();
            let lines: Vec<&str> = frame_dump.lines().collect();

            for (j, line) in lines.iter().enumerate() {
                if j == 0 {
                    result.push_str(&format!("{connector} {line}"));
                } else {
                    let prefix = if i == last_idx { "   " } else { "│  " };
                    result.push_str(&format!("{prefix}  {line}"));
                }
                result.push('\n');
            }
        }

        result
    }

    pub fn frames(&self) -> &[CallFrame] {
        &self.frames
    }
}

impl Default for CallStack {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for CallStack {
    fn clone(&self) -> Self {
        Self {
            frames: self.frames.clone(),
            max_depth: self.max_depth,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn test_call_frame_creation() {
        let frame = CallFrame::new(100, "test_proc".to_string(), 3, 2, 5);

        assert_eq!(frame.ret_addr(), 100);
        assert_eq!(frame.proc_name(), "test_proc");
        assert_eq!(frame.local_count(), 3);
        assert_eq!(frame.arg_count(), 2);
        assert_eq!(frame.stack_size_before(), 5);
    }

    #[test]
    fn test_call_frame_locals() {
        let mut frame = CallFrame::new(0, "test".to_string(), 2, 0, 0);

        assert_eq!(frame.get_local(0).unwrap(), &Value::Unit);

        frame.set_loc(0, Value::Int(42)).unwrap();
        assert_eq!(frame.get_local(0).unwrap(), &Value::Int(42));

        let result = frame.get_local(5);
        assert!(matches!(result, Err(MusiError::IndexOutOfBounds { .. })));
    }

    #[test]
    fn test_call_frame_args() {
        let mut frame = CallFrame::new(0, "test".to_string(), 3, 2, 0);

        frame.set_arg(0, Value::Int(10)).unwrap();
        frame.set_arg(1, Value::Int(20)).unwrap();

        assert_eq!(frame.get_arg(0).unwrap(), &Value::Int(10));
        assert_eq!(frame.get_arg(1).unwrap(), &Value::Int(20));

        let result = frame.get_arg(2);
        assert!(matches!(result, Err(MusiError::IndexOutOfBounds { .. })));
    }

    #[test]
    fn test_call_stack_operations() {
        let mut call_stack = CallStack::new();
        assert!(call_stack.is_empty());

        let frame = CallFrame::new(0, "main".to_string(), 0, 0, 0);

        call_stack.push(frame).unwrap();
        assert_eq!(call_stack.depth(), 1);
        assert!(!call_stack.is_empty());

        let popped = call_stack.pop().unwrap();
        assert_eq!(popped.proc_name(), "main");
        assert!(call_stack.is_empty());
    }

    #[test]
    fn test_call_stack_depth_limit() {
        let mut call_stack = CallStack::new();

        for i in 0..MAX_CALL_DEPTH {
            call_stack
                .push(CallFrame::new(0, format!("f{}", i), 0, 0, 0))
                .unwrap();
        }

        let result = call_stack.push(CallFrame::new(0, "overflow".to_string(), 0, 0, 0));
        assert!(matches!(result, Err(MusiError::CallDepthExceeded(_))));
    }

    #[test]
    fn test_call_stack_peek() {
        let mut call_stack = CallStack::new();

        let frame1 = CallFrame::new(100, "f1".to_string(), 0, 0, 0);
        let frame2 = CallFrame::new(200, "f2".to_string(), 0, 0, 0);

        call_stack.push(frame1).unwrap();
        call_stack.push(frame2).unwrap();

        let top = call_stack.peek().unwrap();
        assert_eq!(top.proc_name(), "f2");
        assert_eq!(top.ret_addr(), 200);
    }

    #[test]
    fn test_call_stack_frame_access() {
        let mut call_stack = CallStack::new();

        call_stack
            .push(CallFrame::new(0, "main".to_string(), 0, 0, 0))
            .unwrap();
        call_stack
            .push(CallFrame::new(100, "helper".to_string(), 0, 0, 0))
            .unwrap();

        let top_frame = call_stack.get_frame(0).unwrap();
        assert_eq!(top_frame.proc_name(), "helper");

        let next_frame = call_stack.get_frame(1).unwrap();
        assert_eq!(next_frame.proc_name(), "main");
    }
}
