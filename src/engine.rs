use std::collections::HashMap;
use std::fs::File;
use std::io::{Cursor, Read};
use std::path::Path;

use crate::bytecode::BytecodeModule;
use crate::errors::{MusiError, MusiResult};
use crate::executor::Executor;
use crate::memmgr::MemoryManager;
use crate::opcode::*;
use crate::value::Value;

pub struct VM {
    modules: HashMap<String, BytecodeModule>,
    memmgr: MemoryManager,
    debugging: bool,
}

impl VM {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            memmgr: MemoryManager::new(),
            debugging: false,
        }
    }

    pub fn with_debug(debug: bool) -> Self {
        Self {
            modules: HashMap::new(),
            memmgr: MemoryManager::new(),
            debugging: debug,
        }
    }

    pub fn load_module<P: AsRef<Path>>(&mut self, path: P) -> MusiResult<()> {
        let path_ref = path.as_ref();
        let mut file = File::open(path_ref)
            .map_err(|why| MusiError::IoError(format!("open {}: {why}", path_ref.display())))?;

        let mut bytes = vec![];
        file.read_to_end(&mut bytes)
            .map_err(|why| MusiError::IoError(format!("read {}: {why}", path_ref.display())))?;

        let module = BytecodeModule::read(&mut Cursor::new(bytes))?;
        module.check_instr()?;

        let module_name = path_ref
            .file_stem()
            .and_then(|s| s.to_str())
            .ok_or_else(|| MusiError::InvalidBytecode("invalid module name".to_string()))?
            .to_string();

        self.modules.insert(module_name, module);
        Ok(())
    }

    pub fn add_module(&mut self, name: String, module: BytecodeModule) {
        self.modules.insert(name, module);
    }

    pub fn get_module(&self, name: &str) -> Option<&BytecodeModule> {
        self.modules.get(name)
    }

    pub fn list_modules(&self) -> Vec<String> {
        self.modules.keys().cloned().collect()
    }

    fn get_module_or_err(&self, name: &str) -> MusiResult<&BytecodeModule> {
        self.modules
            .get(name)
            .ok_or_else(|| MusiError::ModuleNotFound(name.to_string()))
    }

    pub fn exec_proc(&mut self, module_name: &str, proc_name: &str) -> MusiResult<Value> {
        let module = self.get_module_or_err(module_name)?;
        let mut executor = Executor::new(module.clone());

        if self.debugging {
            println!("executing {module_name}.{proc_name}");
        }

        let result = executor.exec_proc(proc_name)?;

        if self.debugging {
            println!("result: {result}");
        }

        Ok(result)
    }

    pub fn exec_main(&mut self, module_name: &str) -> MusiResult<Value> {
        self.exec_proc(module_name, "main")
    }

    pub fn call_entrypoint(&mut self, module_name: &str, entrypoint: &str) -> MusiResult<Value> {
        self.exec_proc(module_name, entrypoint)
    }

    pub fn check_module(&mut self, module_name: &str) -> MusiResult<()> {
        self.get_module_or_err(module_name)?.check_instr()
    }

    pub fn check_all_modules(&self) -> MusiResult<()> {
        for (name, module) in &self.modules {
            module
                .check_instr()
                .map_err(|why| MusiError::InvalidBytecode(format!("module '{name}': {why}")))?;
        }
        Ok(())
    }

    pub fn get_module_info(&self, module_name: &str) -> MusiResult<String> {
        let module = self.get_module_or_err(module_name)?;
        let mut info = format!(".package '{module_name}'\n\n");
        info.push_str(&format!(".import count: {}\n", module.imports.len()));
        info.push_str(&format!(".proc count: {}\n", module.procs.len()));
        info.push_str(&format!(".const int count: {}\n", module.consts.ints.len()));
        info.push_str(&format!(
            ".const string count: {}\n",
            module.consts.strs.len()
        ));
        info.push_str(&format!(".code size: {} bytes\n\n", module.code.len()));

        for (name, proc) in &module.procs {
            info.push_str(&format!(
                ".proc {name} (params: {}, locals: {}, size: {} bytes)\n",
                proc.param_count, proc.local_count, proc.code_size
            ));
        }

        Ok(info)
    }

    pub fn list_procs(&self, module_name: &str) -> MusiResult<Vec<String>> {
        Ok(self
            .get_module_or_err(module_name)?
            .procs
            .keys()
            .cloned()
            .collect())
    }

    pub fn has_proc(&self, module_name: &str, proc_name: &str) -> bool {
        self.modules
            .get(module_name)
            .map_or(false, |m| m.procs.contains_key(proc_name))
    }

    pub fn get_const_info(&self, module_name: &str) -> MusiResult<String> {
        let module = self.get_module_or_err(module_name)?;
        let mut info = format!(".package '{module_name}'\n\n");

        for (i, &val) in module.consts.ints.iter().enumerate() {
            info.push_str(&format!(
                ".field static literal int32 [{i}] := int32({val})\n"
            ));
        }

        if !module.consts.ints.is_empty() && !module.consts.strs.is_empty() {
            info.push('\n');
        }

        for (i, val) in module.consts.strs.iter().enumerate() {
            info.push_str(&format!(
                ".field static literal string [{i}] := \"{val}\"\n"
            ));
        }

        Ok(info)
    }

    pub fn disass_module(&self, module_name: &str) -> MusiResult<String> {
        let module = self.get_module_or_err(module_name)?;
        let mut result = format!(".package '{module_name}'\n\n");

        for (name, proc) in &module.procs {
            result.push_str(&format!("\n.proc export {name}() -> () msil managed\n{{\n"));
            result.push_str("  .maxstack 8\n");

            if proc.local_count > 0 {
                result.push_str("  .locals init (");
                for i in 0..proc.local_count {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&format!("[{i}] int32"));
                }
                result.push_str(")\n");
            }
            result.push_str("\n");

            if let Some(code) = module.get_proc_code(proc) {
                let disasm_str = disasm(code)?
                    .lines()
                    .map(|line| format!("  {line}"))
                    .collect::<Vec<_>>()
                    .join("\n");
                result.push_str(&disasm_str);
                result.push('\n');
            }

            result.push_str("}\n");
        }

        Ok(result)
    }

    pub fn set_debugging(&mut self, active: bool) {
        self.debugging = active;
    }

    pub fn is_debugging(&self) -> bool {
        self.debugging
    }

    pub fn clear_modules(&mut self) {
        self.modules.clear();
    }

    pub fn memory_stats(&self) -> (usize, usize, usize, usize) {
        self.memmgr.stats()
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

fn disasm(code: &[u8]) -> MusiResult<String> {
    let mut result = String::new();
    let mut pc = 0;

    while pc < code.len() {
        result.push_str(&format!("IL_{:04X}: ", pc));
        let op = code[pc];
        pc += 1;

        let instr = match op {
            NOP => "nop",
            DUP => "dup",
            POP => "pop",
            RET => "ret",
            LDNULL => "ldnull",
            ADD => "add",
            SUB => "sub",
            MUL => "mul",
            DIV => "div",
            REM => "rem",
            REM_UN => "rem.un",
            AND => "and",
            OR => "or",
            XOR => "xor",
            SHL => "shl",
            SHR => "shr",
            NEG => "neg",
            CONV_I8 => "conv.i8",
            CONV_I16 => "conv.i16",
            CONV_I32 => "conv.i32",
            CONV_I64 => "conv.i64",
            CEQ => "ceq",
            CGT => "cgt",
            CLT => "clt",
            LDLOC => {
                result.push_str(&format!("ldloc.{}\n", read_u8(code, &mut pc)?));
                continue;
            }
            STLOC => {
                result.push_str(&format!("stloc.{}\n", read_u8(code, &mut pc)?));
                continue;
            }
            LDARG => {
                result.push_str(&format!("ldarg.{}\n", read_u8(code, &mut pc)?));
                continue;
            }
            STARG => {
                result.push_str(&format!("starg.{}\n", read_u8(code, &mut pc)?));
                continue;
            }
            LDC_I4 => {
                result.push_str(&format!("ldc.i4 {}\n", read_i32(code, &mut pc)?));
                continue;
            }
            BR => {
                result.push_str(&format!("br {}\n", read_i32(code, &mut pc)?));
                continue;
            }
            BRTRUE => {
                result.push_str(&format!("brtrue {}\n", read_i32(code, &mut pc)?));
                continue;
            }
            BRFALSE => {
                result.push_str(&format!("brfalse {}\n", read_i32(code, &mut pc)?));
                continue;
            }
            _ => {
                result.push_str(&format!("unknown instruction: 0x{op:02X}\n"));
                continue;
            }
        };

        result.push_str(instr);
        result.push('\n');
    }

    Ok(result)
}

fn read_u8(code: &[u8], pc: &mut usize) -> MusiResult<u8> {
    if *pc >= code.len() {
        return Err(MusiError::UnexpectedEndOfBytecode(*pc));
    }
    let val = code[*pc];
    *pc += 1;
    Ok(val)
}

fn read_i32(code: &[u8], pc: &mut usize) -> MusiResult<i32> {
    if *pc + 4 > code.len() {
        return Err(MusiError::UnexpectedEndOfBytecode(*pc));
    }
    let val = i32::from_le_bytes([code[*pc], code[*pc + 1], code[*pc + 2], code[*pc + 3]]);
    *pc += 4;
    Ok(val)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vm_creation() {
        let vm = VM::new();
        assert_eq!(vm.list_modules().len(), 0);
        assert!(!vm.is_debugging());

        let vm_debug = VM::with_debug(true);
        assert!(vm_debug.is_debugging());
    }

    #[test]
    fn test_module_management() {
        let vm = VM::new();
        assert!(vm.get_module("test").is_none());
        assert!(vm.list_procs("test").is_err());
        assert!(!vm.has_proc("test", "main"));
    }

    #[test]
    fn test_debug_mode() {
        let mut vm = VM::new();
        assert!(!vm.is_debugging());

        vm.set_debugging(true);
        assert!(vm.is_debugging());

        vm.set_debugging(false);
        assert!(!vm.is_debugging());
    }
}
