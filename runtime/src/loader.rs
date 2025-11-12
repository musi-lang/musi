use alloc::collections::BTreeMap;
use alloc::rc::Rc;
use alloc::string::String;
use alloc::vec::Vec;

use crate::binary::{read_i64_le, read_u32_le, read_u64_le};
use crate::value::{Value, ValueList};

#[derive(Clone)]
pub struct ProcInfo {
    pub name: String,
    pub code_offset: u32,
    pub param_count: u16,
    pub local_count: u16,
}

pub type ProcTable = Vec<ProcInfo>;

pub struct Bytecode {
    pub consts: ValueList,
    pub procs: ProcTable,
    pub code: Vec<u8>,
}

struct Module {
    consts: ValueList,
    procs: ProcTable,
    code: Vec<u8>,
    imports: Vec<String>,
}

pub unsafe fn load(path: &str) -> Bytecode {
    let mut loaded = BTreeMap::new();
    let abs_path = resolve_path(path);
    load_module_recursive(&abs_path, &mut loaded);
    link_modules(loaded)
}

unsafe fn load_module_recursive(path: &str, loaded: &mut BTreeMap<String, Module>) {
    if loaded.contains_key(path) {
        return;
    }
    let data = read_file(path);
    let module = parse_module(&data);
    for import in &module.imports {
        let import_path = resolve_import(import);
        load_module_recursive(&import_path, loaded);
    }
    loaded.insert(path.to_string(), module);
}

fn resolve_path(path: &str) -> String {
    if path.ends_with(".msc") {
        path.to_string()
    } else {
        alloc::format!("{path}.msc")
    }
}

fn resolve_import(import: &str) -> String {
    if import.ends_with(".ms") {
        import.replace(".ms", ".msc")
    } else {
        alloc::format!("{import}.msc")
    }
}

fn link_modules(modules: BTreeMap<String, Module>) -> Bytecode {
    let mut consts = Vec::new();
    let mut procs = Vec::new();
    let mut code = Vec::new();
    let mut code_offset = 0u32;

    for (_, module) in modules {
        let Module {
            consts: mod_consts,
            procs: mod_procs,
            code: mut mod_code,
            imports: _,
        } = module;

        let const_offset = consts.len() as u32;
        consts.extend(mod_consts);
        
        remap_const_indices(&mut mod_code, const_offset);
        
        for mut proc in mod_procs {
            proc.code_offset += code_offset;
            procs.push(proc);
        }
        let code_len = mod_code.len() as u32;
        code.extend(mod_code);
        code_offset += code_len;
    }

    Bytecode {
        consts,
        procs,
        code,
    }
}

fn remap_const_indices(code: &mut [u8], offset: u32) {
    let mut pos = 0;
    while pos < code.len() {
        let op = code[pos];
        pos += 1;
        match op {
            0x10 | 0x12 => {
                let idx = u32::from_le_bytes([code[pos], code[pos+1], code[pos+2], code[pos+3]]);
                let new_idx = idx + offset;
                code[pos..pos+4].copy_from_slice(&new_idx.to_le_bytes());
                pos += 4;
            }
            0x11 | 0x20 | 0x21 | 0x22 | 0x60 | 0x61 | 0x62 | 0x80 | 0x81 | 0x82 | 0x90 | 0x91 => {
                pos += 4;
            }
            0x70 | 0x71 => {
                while pos < code.len() && code[pos] != 0 {
                    pos += 1;
                }
                pos += 1;
            }
            _ => {}
        }
    }
}

unsafe fn read_file(path: &str) -> Vec<u8> {
    use alloc::vec;
    let path_cstr = alloc::format!("{path}\0");
    let file = libc::fopen(
        path_cstr.as_ptr() as *const i8,
        b"rb\0".as_ptr() as *const i8,
    );
    if file.is_null() {
        panic!("failed to open file: {path}");
    }
    libc::fseek(file, 0, libc::SEEK_END);
    let size = libc::ftell(file) as usize;
    libc::fseek(file, 0, libc::SEEK_SET);
    let mut data = vec![0u8; size];
    libc::fread(data.as_mut_ptr() as *mut _, 1, size, file);
    libc::fclose(file);
    data
}

fn parse_module(data: &[u8]) -> Module {
    let mut pos = 0;

    if &data[0..4] != b"MUSI" {
        panic!("invalid magic");
    }
    pos += 4;

    let _version = read_u32_le(data, &mut pos);
    let import_offset = read_u32_le(data, &mut pos) as usize;
    let _import_size = read_u32_le(data, &mut pos);
    let const_offset = read_u32_le(data, &mut pos) as usize;
    let _const_size = read_u32_le(data, &mut pos);
    let proc_offset = read_u32_le(data, &mut pos) as usize;
    let _proc_size = read_u32_le(data, &mut pos);

    let imports = parse_imports(data, import_offset);
    let consts = parse_const_pool(data, const_offset);
    let procs = parse_proc_table(data, proc_offset);

    let code_offset = proc_offset + _proc_size as usize;
    let code_end = data.len();
    let code = data[code_offset..code_end].to_vec();

    Module {
        consts,
        procs,
        code,
        imports,
    }
}

fn parse_imports(data: &[u8], offset: usize) -> Vec<String> {
    let mut pos = offset;
    let count = read_u32_le(data, &mut pos) as usize;
    let mut imports = Vec::with_capacity(count);

    for _ in 0..count {
        let start = pos;
        while data[pos] != 0 {
            pos += 1;
        }
        let imp = String::from_utf8_lossy(&data[start..pos]).into_owned();
        pos += 1;
        imports.push(imp);
    }

    imports
}

fn parse_const_pool(data: &[u8], offset: usize) -> ValueList {
    let mut pos = offset;
    let count = read_u32_le(data, &mut pos) as usize;
    let mut consts = Vec::with_capacity(count);

    for _ in 0..count {
        let tag = data[pos];
        pos += 1;

        let val = match tag {
            0x01 => Value::Int64(read_i64_le(data, &mut pos)),
            0x02 => Value::Bin64(f64::from_bits(read_u64_le(data, &mut pos))),
            0x03 => {
                let start = pos;
                while data[pos] != 0 {
                    pos += 1;
                }
                let s = String::from_utf8_lossy(&data[start..pos]).into_owned();
                pos += 1;
                Value::Str(Rc::new(s))
            }
            0x04 => Value::Bool(true),
            0x05 => Value::Bool(false),
            0x00 => Value::Unit,
            _ => panic!("unknown const type: {tag:#x}"),
        };
        consts.push(val);
    }

    consts
}

fn parse_proc_table(data: &[u8], offset: usize) -> ProcTable {
    let mut pos = offset;
    let count = read_u32_le(data, &mut pos) as usize;
    let mut procs = Vec::with_capacity(count);

    for _ in 0..count {
        let name_start = pos;
        while data[pos] != 0 {
            pos += 1;
        }
        let name = String::from_utf8_lossy(&data[name_start..pos]).into_owned();
        pos += 1;

        let param_count = read_u32_le(data, &mut pos) as u16;
        let local_count = read_u32_le(data, &mut pos) as u16;
        let code_offset = read_u32_le(data, &mut pos);

        procs.push(ProcInfo {
            name,
            code_offset,
            param_count,
            local_count,
        });
    }

    procs
}
