use crate::bytecode::BytecodeModule;
use crate::errors::{MusiError, MusiResult};
use crate::frames::{CallFrame, CallStack};
use crate::opcode::*;
use crate::stack::OperandStack;
use crate::value::Value;

pub struct Executor {
    stack: OperandStack,
    call_stack: CallStack,
    pc: usize,
    module: BytecodeModule,
}

impl Executor {
    pub fn new(module: BytecodeModule) -> Self {
        Self {
            stack: OperandStack::new(),
            call_stack: CallStack::new(),
            pc: 0,
            module,
        }
    }

    pub fn exec_proc(&mut self, proc_name: &str) -> MusiResult<Value> {
        let proc = self
            .module
            .get_proc(proc_name)
            .ok_or_else(|| MusiError::ProcedureNotFound(proc_name.to_string()))?;

        let code = self
            .module
            .get_proc_code(proc)
            .ok_or_else(|| MusiError::InvalidBytecode("procedure code not found".to_string()))?
            .to_vec();

        let frame = CallFrame::new(
            usize::MAX,
            proc_name.to_string(),
            proc.local_count as usize,
            proc.param_count as usize,
            self.stack.size(),
        );

        self.call_stack.push(frame)?;
        self.pc = 0;

        let result = self.exec_code(&code);
        self.call_stack.pop()?;
        result
    }

    fn exec_code(&mut self, code: &[u8]) -> MusiResult<Value> {
        while self.pc < code.len() {
            let opcode = code[self.pc];
            self.pc += 1;

            if opcode == 0x7A {
                self.exec_2byte_op(code)?;
                continue;
            }

            match opcode {
                NOP => {}
                DUP => self.stack.dup()?,
                POP => {
                    self.stack.pop()?;
                }
                RET => break,
                BR => self.br(code)?,
                BRTRUE => self.br_cond(code, true)?,
                BRFALSE => self.br_cond(code, false)?,
                BEQ => self.br_cmp(code, |l, r| l.equals(r))?,
                BGE => self.br_cmp(code, |l, r| Ok(cmp(l, r)? >= 0))?,
                BGT => self.br_cmp(code, |l, r| Ok(cmp(l, r)? > 0))?,
                BLE => self.br_cmp(code, |l, r| Ok(cmp(l, r)? <= 0))?,
                BLT => self.br_cmp(code, |l, r| Ok(cmp(l, r)? < 0))?,
                BNE => self.br_cmp(code, |l, r| Ok(!l.equals(r)?))?,
                ADD => self.bin_op(add)?,
                SUB => self.bin_op(sub)?,
                MUL => self.bin_op(mul)?,
                DIV => self.bin_op(div)?,
                REM => self.bin_op(rem)?,
                REM_UN => self.bin_op(rem_un)?,
                AND => self.bin_op(and)?,
                OR => self.bin_op(or)?,
                XOR => self.bin_op(xor)?,
                SHL => self.bin_op(shl)?,
                SHR => self.bin_op(shr)?,
                NEG => self.un_op(neg)?,
                CONV_I8 => self.un_op(|v| conv(v, |n| Value::Int8(n as i8)))?,
                CONV_I16 => self.un_op(|v| conv(v, |n| Value::Int16(n as i16)))?,
                CONV_I32 => self.un_op(|v| conv(v, |n| Value::Int32(n as i32)))?,
                CONV_I64 => self.un_op(|v| conv(v, |n| Value::Int64(n)))?,
                CEQ => self.bin_op(|l, r| Ok(Value::Bool(l.equals(r)?)))?,
                CGT => self.bin_op(|l, r| Ok(Value::Bool(cmp(l, r)? > 0)))?,
                CLT => self.bin_op(|l, r| Ok(Value::Bool(cmp(l, r)? < 0)))?,
                LDNULL => self.stack.push(Value::Unit)?,
                LDC_I4 => self.ldc_i4(code)?,
                LDLOC => self.ld_loc(code)?,
                STLOC => self.st_loc(code)?,
                LDARG => self.ld_arg(code)?,
                STARG => self.st_arg(code)?,
                _ => {
                    return Err(MusiError::UnknownOpcode {
                        opcode,
                        offset: self.pc - 1,
                    });
                }
            }
        }
        Ok(Value::Unit)
    }

    fn exec_2byte_op(&mut self, code: &[u8]) -> MusiResult<()> {
        if self.pc >= code.len() {
            return Err(MusiError::UnexpectedEndOfBytecode(self.pc));
        }
        let sub_op = code[self.pc];
        self.pc += 1;
        let opcode_u16 = 0x7A00 | (sub_op as u16);

        match opcode_u16 {
            THROW => Err(MusiError::UnhandledException("throw".to_string())),
            RETHROW => Err(MusiError::RethrowWithoutException),
            _ => Err(MusiError::UnknownOpcode {
                opcode: sub_op,
                offset: self.pc - 1,
            }),
        }
    }

    fn read_i32(&mut self, code: &[u8]) -> MusiResult<i32> {
        if self.pc + 4 > code.len() {
            return Err(MusiError::UnexpectedEndOfBytecode(self.pc));
        }
        let val = i32::from_le_bytes([
            code[self.pc],
            code[self.pc + 1],
            code[self.pc + 2],
            code[self.pc + 3],
        ]);
        self.pc += 4;
        Ok(val)
    }

    fn read_u8(&mut self, code: &[u8]) -> MusiResult<usize> {
        if self.pc >= code.len() {
            return Err(MusiError::UnexpectedEndOfBytecode(self.pc));
        }
        let val = code[self.pc] as usize;
        self.pc += 1;
        Ok(val)
    }

    fn br(&mut self, code: &[u8]) -> MusiResult<()> {
        let offset = self.read_i32(code)?;
        self.jump(code, offset)
    }

    fn br_cond(&mut self, code: &[u8], expect_true: bool) -> MusiResult<()> {
        let cond = self.stack.pop()?.is_truthy();
        let offset = self.read_i32(code)?;
        if cond == expect_true {
            self.jump(code, offset)?;
        }
        Ok(())
    }

    fn br_cmp<F>(&mut self, code: &[u8], cmp: F) -> MusiResult<()>
    where
        F: FnOnce(&Value, &Value) -> MusiResult<bool>,
    {
        let r = self.stack.pop()?;
        let l = self.stack.pop()?;
        let offset = self.read_i32(code)?;
        if cmp(&l, &r)? {
            self.jump(code, offset)?;
        }
        Ok(())
    }

    fn jump(&mut self, code: &[u8], offset: i32) -> MusiResult<()> {
        let new_pc = (self.pc as isize + offset as isize) as usize;
        if new_pc >= code.len() {
            return Err(MusiError::InvalidBranchOffset(offset));
        }
        self.pc = new_pc;
        Ok(())
    }

    fn bin_op<F>(&mut self, op: F) -> MusiResult<()>
    where
        F: FnOnce(&Value, &Value) -> MusiResult<Value>,
    {
        let r = self.stack.pop()?;
        let l = self.stack.pop()?;
        self.stack.push(op(&l, &r)?)
    }

    fn un_op<F>(&mut self, op: F) -> MusiResult<()>
    where
        F: FnOnce(&Value) -> MusiResult<Value>,
    {
        let v = self.stack.pop()?;
        self.stack.push(op(&v)?)
    }

    fn ldc_i4(&mut self, code: &[u8]) -> MusiResult<()> {
        let val = self.read_i32(code)?;
        self.stack.push(Value::Int32(val))
    }

    fn ld_loc(&mut self, code: &[u8]) -> MusiResult<()> {
        let idx = self.read_u8(code)?;
        let val = self.call_stack.peek()?.get_local(idx)?.clone();
        self.stack.push(val)
    }

    fn st_loc(&mut self, code: &[u8]) -> MusiResult<()> {
        let idx = self.read_u8(code)?;
        let val = self.stack.pop()?;
        self.call_stack.peek_mut()?.set_loc(idx, val)
    }

    fn ld_arg(&mut self, code: &[u8]) -> MusiResult<()> {
        let idx = self.read_u8(code)?;
        let val = self.call_stack.peek()?.get_arg(idx)?.clone();
        self.stack.push(val)
    }

    fn st_arg(&mut self, code: &[u8]) -> MusiResult<()> {
        let idx = self.read_u8(code)?;
        let val = self.stack.pop()?;
        self.call_stack.peek_mut()?.set_arg(idx, val)
    }

    pub fn debug_dump_state(&self) -> String {
        format!(
            "pc={}\nstack={}\ncall_stack={}",
            self.pc,
            self.stack.debug_dump(),
            self.call_stack.debug_dump()
        )
    }
}

fn cmp(l: &Value, r: &Value) -> MusiResult<i8> {
    use Value::*;
    match (l, r) {
        (Int(a), Int(b)) => Ok(a.cmp(b) as i8),
        (Nat(a), Nat(b)) => Ok(a.cmp(b) as i8),
        (Int32(a), Int32(b)) => Ok(a.cmp(b) as i8),
        (Nat32(a), Nat32(b)) => Ok(a.cmp(b) as i8),
        (Int64(a), Int64(b)) => Ok(a.cmp(b) as i8),
        (Nat64(a), Nat64(b)) => Ok(a.cmp(b) as i8),
        _ => Err(MusiError::TypeMismatch),
    }
}

fn arith<F>(l: &Value, r: &Value, name: &str, op: F) -> MusiResult<Value>
where
    F: Fn(i64, i64) -> Option<i64>,
{
    use Value::*;
    let err = || MusiError::IntegerOverflow {
        op: name.to_string(),
    };
    match (l, r) {
        (Int(a), Int(b)) => op(*a as i64, *b as i64)
            .map(|v| Int(v as isize))
            .ok_or_else(err),
        (Nat(a), Nat(b)) => op(*a as i64, *b as i64)
            .map(|v| Nat(v as usize))
            .ok_or_else(err),
        (Int32(a), Int32(b)) => op(*a as i64, *b as i64)
            .map(|v| Int32(v as i32))
            .ok_or_else(err),
        (Nat32(a), Nat32(b)) => op(*a as i64, *b as i64)
            .map(|v| Nat32(v as u32))
            .ok_or_else(err),
        _ => Err(MusiError::TypeMismatch),
    }
}

fn add(l: &Value, r: &Value) -> MusiResult<Value> {
    arith(l, r, "add", |a, b| a.checked_add(b))
}

fn sub(l: &Value, r: &Value) -> MusiResult<Value> {
    arith(l, r, "sub", |a, b| a.checked_sub(b))
}

fn mul(l: &Value, r: &Value) -> MusiResult<Value> {
    arith(l, r, "mul", |a, b| a.checked_mul(b))
}

fn div(l: &Value, r: &Value) -> MusiResult<Value> {
    use Value::*;
    match (l, r) {
        (Int(a), Int(b)) if *b != 0 => Ok(Int(a / b)),
        (Nat(a), Nat(b)) if *b != 0 => Ok(Nat(a / b)),
        (Int32(a), Int32(b)) if *b != 0 => Ok(Int32(a / b)),
        (Nat32(a), Nat32(b)) if *b != 0 => Ok(Nat32(a / b)),
        _ if matches!(
            (l, r),
            (
                Int(_) | Nat(_) | Int32(_) | Nat32(_),
                Int(_) | Nat(_) | Int32(_) | Nat32(_)
            )
        ) =>
        {
            Err(MusiError::DivisionByZero)
        }
        _ => Err(MusiError::TypeMismatch),
    }
}

fn rem_generic<F>(l: &Value, r: &Value, op: F, err: MusiError) -> MusiResult<Value>
where
    F: Fn(i64, i64) -> i64,
{
    use Value::*;
    match (l, r) {
        (Int(a), Int(b)) if *b != 0 => Ok(Int(op(*a as i64, *b as i64) as isize)),
        (Nat(a), Nat(b)) if *b != 0 => Ok(Nat(op(*a as i64, *b as i64) as usize)),
        (Int32(a), Int32(b)) if *b != 0 => Ok(Int32(op(*a as i64, *b as i64) as i32)),
        (Nat32(a), Nat32(b)) if *b != 0 => Ok(Nat32(op(*a as i64, *b as i64) as u32)),
        _ if matches!(
            (l, r),
            (
                Int(_) | Nat(_) | Int32(_) | Nat32(_),
                Int(_) | Nat(_) | Int32(_) | Nat32(_)
            )
        ) =>
        {
            Err(err)
        }
        _ => Err(MusiError::TypeMismatch),
    }
}

fn rem(l: &Value, r: &Value) -> MusiResult<Value> {
    rem_generic(l, r, |a, b| a % b, MusiError::ModuloByZero)
}

fn rem_un(l: &Value, r: &Value) -> MusiResult<Value> {
    rem_generic(l, r, |a, b| a.rem_euclid(b), MusiError::ModuloByZero)
}

fn bit<F>(l: &Value, r: &Value, op: F) -> MusiResult<Value>
where
    F: Fn(i64, i64) -> i64,
{
    use Value::*;
    match (l, r) {
        (Int(a), Int(b)) => Ok(Int(op(*a as i64, *b as i64) as isize)),
        (Nat(a), Nat(b)) => Ok(Nat(op(*a as i64, *b as i64) as usize)),
        (Int32(a), Int32(b)) => Ok(Int32(op(*a as i64, *b as i64) as i32)),
        (Nat32(a), Nat32(b)) => Ok(Nat32(op(*a as i64, *b as i64) as u32)),
        (Bool(a), Bool(b)) => Ok(Bool(op(*a as i64, *b as i64) != 0)),
        _ => Err(MusiError::TypeMismatch),
    }
}

fn and(l: &Value, r: &Value) -> MusiResult<Value> {
    bit(l, r, |a, b| a & b)
}

fn or(l: &Value, r: &Value) -> MusiResult<Value> {
    bit(l, r, |a, b| a | b)
}

fn xor(l: &Value, r: &Value) -> MusiResult<Value> {
    bit(l, r, |a, b| a ^ b)
}

fn sh<F>(l: &Value, r: &Value, op: F) -> MusiResult<Value>
where
    F: Fn(i64, u32) -> Option<i64>,
{
    use Value::*;
    match (l, r) {
        (Int(a), Int(b)) => Ok(Int(op(*a as i64, *b as u32).unwrap_or(0) as isize)),
        (Nat(a), Nat(b)) => Ok(Nat(op(*a as i64, *b as u32).unwrap_or(0) as usize)),
        (Int32(a), Int32(b)) => Ok(Int32(op(*a as i64, *b as u32).unwrap_or(0) as i32)),
        (Nat32(a), Nat32(b)) => Ok(Nat32(op(*a as i64, *b as u32).unwrap_or(0) as u32)),
        _ => Err(MusiError::TypeMismatch),
    }
}

fn shl(l: &Value, r: &Value) -> MusiResult<Value> {
    sh(l, r, |a, b| a.checked_shl(b))
}

fn shr(l: &Value, r: &Value) -> MusiResult<Value> {
    sh(l, r, |a, b| a.checked_shr(b))
}

fn neg(v: &Value) -> MusiResult<Value> {
    use Value::*;
    let err = || MusiError::IntegerOverflow {
        op: "neg".to_string(),
    };
    match v {
        Int(n) => Ok(Int(-n)),
        Int32(n) => n.checked_neg().map(Int32).ok_or_else(err),
        Int64(n) => n.checked_neg().map(Int64).ok_or_else(err),
        Bin32(f) => Ok(Bin32(-f)),
        Bin64(f) => Ok(Bin64(-f)),
        _ => Err(MusiError::TypeMismatch),
    }
}

fn conv<F>(v: &Value, f: F) -> MusiResult<Value>
where
    F: Fn(i64) -> Value,
{
    use Value::*;
    match v {
        Int(n) => Ok(f(*n as i64)),
        Nat(n) => Ok(f(*n as i64)),
        Int32(n) => Ok(f(*n as i64)),
        Nat32(n) => Ok(f(*n as i64)),
        Int64(n) => Ok(f(*n)),
        Nat64(n) => Ok(f(*n as i64)),
        _ => Err(MusiError::TypeMismatch),
    }
}
