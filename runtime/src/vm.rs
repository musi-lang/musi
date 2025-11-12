use crate::frame::Frames;
use crate::instr::Instr;
use crate::loader::Bytecode;
use crate::stack::Stack;
use crate::value::Value;

pub struct VM {
    pub stack: Stack,
    pub frames: Frames,
    pub ip: usize,
    pub bc: Bytecode,
}

impl VM {
    pub fn new(bc: Bytecode) -> Self {
        Self {
            stack: Stack::new(),
            frames: Frames::new(),
            ip: 0,
            bc,
        }
    }

    pub fn exec(&mut self) {
        loop {
            let instr = crate::instr::decode(&self.bc.code, &mut self.ip);
            match instr {
                Instr::Nop => {}
                Instr::Pop => {
                    self.stack.pop();
                }
                Instr::Dup => self.stack.dup(),
                Instr::LdC(idx) => self.exec_ldc(idx),
                Instr::LdCI4(n) => self.stack.push(Value::Int64(n as i64)),
                Instr::LdCStr(idx) => self.exec_ldc(idx),
                Instr::LdLoc(slot) => self.exec_ldloc(slot),
                Instr::StLoc(slot) => self.exec_stloc(slot),
                Instr::LdArg(slot) => self.exec_ldarg(slot),
                Instr::Add => self.binop(|a, b| a + b),
                Instr::Sub => self.binop(|a, b| a - b),
                Instr::Mul => self.binop(|a, b| a * b),
                Instr::Div => self.binop(|a, b| {
                    if b == 0 {
                        panic!("division by zero");
                    }
                    a / b
                }),
                Instr::Mod => self.binop(|a, b| {
                    if b == 0 {
                        panic!("Euclidean division by zero");
                    }
                    a.rem_euclid(b)
                }),
                Instr::Neg => self.unop(|a| -a),
                Instr::And => self.bitop(|a, b| a & b),
                Instr::Or => self.bitop(|a, b| a | b),
                Instr::Xor => self.bitop(|a, b| a ^ b),
                Instr::Not => self.unop(|a| !a),
                Instr::Shl => self.bitop(|a, b| a << b),
                Instr::Shr => self.bitop(|a, b| a >> b),
                Instr::CmpEq => self.cmpop(|a, b| a == b),
                Instr::CmpNe => self.cmpop(|a, b| a != b),
                Instr::CmpLt => self.cmpop(|a, b| a < b),
                Instr::CmpGt => self.cmpop(|a, b| a > b),
                Instr::CmpLe => self.cmpop(|a, b| a <= b),
                Instr::CmpGe => self.cmpop(|a, b| a >= b),
                Instr::Br(offset) => self.ip = (self.ip as i32 + offset) as usize,
                Instr::BrTrue(offset) => self.exec_br_cond(offset, true),
                Instr::BrFalse(offset) => self.exec_br_cond(offset, false),
                Instr::Call(proc_name) => {
                    if proc_name.starts_with("__builtin_") {
                        self.exec_builtin(&proc_name);
                    } else {
                        let proc_id =
                            self.bc
                                .procs
                                .iter()
                                .position(|proc| proc.name == proc_name)
                                .expect("procedure not found") as u32;
                        self.exec_call(proc_id);
                    }
                }
                Instr::Ret => self.exec_ret(),
                _ => panic!("unimplemented instruction: {instr:?}"),
            }
        }
    }

    fn exec_ldc(&mut self, idx: u32) {
        let val = self.bc.consts[idx as usize].clone();
        self.stack.push(val);
    }

    fn exec_ldloc(&mut self, slot: u32) {
        let val = self.frames.curr_locals()[slot as usize].clone();
        self.stack.push(val);
    }

    fn exec_stloc(&mut self, slot: u32) {
        let val = self.stack.pop();
        let locals = self.frames.curr_locals();
        if slot as usize >= locals.len() {
            locals.resize(slot as usize + 1, Value::Unit);
        }
        locals[slot as usize] = val;
    }

    fn exec_ldarg(&mut self, slot: u32) {
        let val = self.frames.curr_locals()[slot as usize].clone();
        self.stack.push(val);
    }

    fn binop<F>(&mut self, f: F)
    where
        F: Fn(i64, i64) -> i64,
    {
        let (b, a) = (self.stack.pop(), self.stack.pop());
        let result = match (a, b) {
            (Value::Int64(x), Value::Int64(y)) => Value::Int64(f(x, y)),
            _ => panic!("type error"),
        };
        self.stack.push(result);
    }

    fn unop<F>(&mut self, f: F)
    where
        F: Fn(i64) -> i64,
    {
        let a = self.stack.pop();
        let result = match a {
            Value::Int64(x) => Value::Int64(f(x)),
            _ => panic!("type error"),
        };
        self.stack.push(result);
    }

    fn bitop<F>(&mut self, f: F)
    where
        F: Fn(i64, i64) -> i64,
    {
        let (b, a) = (self.stack.pop(), self.stack.pop());
        let result = match (a, b) {
            (Value::Int64(x), Value::Int64(y)) => Value::Int64(f(x, y)),
            _ => panic!("type error"),
        };
        self.stack.push(result);
    }

    fn cmpop<F>(&mut self, f: F)
    where
        F: Fn(i64, i64) -> bool,
    {
        let (b, a) = (self.stack.pop(), self.stack.pop());
        let result = match (a, b) {
            (Value::Int64(x), Value::Int64(y)) => Value::Bool(f(x, y)),
            _ => panic!("type error"),
        };
        self.stack.push(result);
    }

    fn exec_br_cond(&mut self, offset: i32, expected: bool) {
        let cond = self.stack.pop();
        if let Value::Bool(val) = cond {
            if val == expected {
                self.ip = (self.ip as i32 + offset) as usize;
            }
        } else {
            panic!("expected type 'Bool' for branch");
        }
    }

    fn exec_call(&mut self, proc_id: u32) {
        use crate::frame::Frame;
        let proc = &self.bc.procs[proc_id as usize];
        let frame = Frame {
            ret_addr: self.ip as u32,
            stack_base: self.stack.data.len() - proc.param_count as usize,
        };
        self.frames.push(frame, proc.local_count as usize);
        let locals = self.frames.curr_locals();
        locals.resize(proc.param_count as usize, Value::Unit);
        for i in (0..proc.param_count).rev() {
            locals[i as usize] = self.stack.pop();
        }
        self.ip = proc.code_offset as usize;
    }

    fn exec_ret(&mut self) {
        if self.frames.data.is_empty() {
            std::process::exit(0);
        }
        let result = self.stack.pop();
        let frame = self.frames.pop();
        while self.stack.data.len() > frame.stack_base {
            self.stack.pop();
        }
        self.stack.push(result);
        self.ip = frame.ret_addr as usize;
    }

    fn exec_builtin(&mut self, name: &str) {
        match name {
            "__builtin_write" => {
                let mut args = Vec::new();
                args.push(self.stack.pop());
                args.push(self.stack.pop());
                args.reverse();
                match (&args[0], &args[1]) {
                    (Value::Int64(fd_val), Value::Str(s)) => {
                        if *fd_val == 1 {
                            print!("{s}");
                        }
                        self.stack.push(Value::Int64(s.len() as i64));
                    }
                    _ => panic!("type error in '__builtin_write'"),
                }
            }
            _ => panic!("unknown builtin: {name}"),
        }
    }
}
