use crate::binary::{read_i64_le, read_u32_le, read_u64_le};
use crate::value::{Value, ValueList};
use alloc::rc::Rc;
use alloc::string::String;
use alloc::vec::Vec;

#[derive(Clone, Copy)]
pub struct ProcInfo {
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

pub unsafe fn load(path: &str) -> Bytecode {
    let data = read_file(path);
    parse_bytecode(&data)
}

unsafe fn read_file(path: &str) -> Vec<u8> {
    use alloc::vec;
    let path_cstr = alloc::format!("{path}");
    let file = libc::fopen(
        path_cstr.as_ptr() as *const i8,
        b"rb\0".as_ptr() as *const i8,
    );
    if file.is_null() {
        panic!("failed to open file");
    }
    libc::fseek(file, 0, libc::SEEK_END);
    let size = libc::ftell(file) as usize;
    libc::fseek(file, 0, libc::SEEK_SET);
    let mut data = vec![0u8; size];
    libc::fread(data.as_mut_ptr() as *mut _, 1, size, file);
    libc::fclose(file);
    data
}

fn parse_bytecode(data: &[u8]) -> Bytecode {
    let mut pos = 0;

    let magic = read_u32_le(data, &mut pos);
    if magic != 0x4D555349
    /* 'MUSI' */
    {
        panic!("invalid magic: {magic:#x}");
    }

    let _version = read_u32_le(data, &mut pos);
    let const_offset = read_u32_le(data, &mut pos) as usize;
    let _const_size = read_u32_le(data, &mut pos);
    let proc_offset = read_u32_le(data, &mut pos) as usize;
    let _proc_size = read_u32_le(data, &mut pos);
    let bc_offset = read_u32_le(data, &mut pos) as usize;
    let bc_size = read_u32_le(data, &mut pos) as usize;

    let consts = parse_const_pool(data, const_offset);
    let procs = parse_proc_table(data, proc_offset);
    let code = data[bc_offset..bc_offset + bc_size].to_vec();

    Bytecode {
        consts,
        procs,
        code,
    }
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
                let len = read_u32_le(data, &mut pos) as usize;
                let s = String::from_utf8_lossy(&data[pos..pos + len]).into_owned();
                pos += len;
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
        while data[pos] != 0 {
            pos += 1;
        }
        pos += 1;

        let param_count = read_u32_le(data, &mut pos) as u16;
        let local_count = read_u32_le(data, &mut pos) as u16;
        let code_offset = read_u32_le(data, &mut pos);

        procs.push(ProcInfo {
            code_offset,
            param_count,
            local_count,
        });
    }

    procs
}
