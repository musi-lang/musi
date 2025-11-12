use crate::string::{string_from_bytes, string_new};
use crate::types::{ProcInfo, ProcTable, Value, ValueList};

pub const MAGIC_SIZE: usize = 4;
pub const TAG_UNIT: u8 = 0x00;
pub const TAG_INT64: u8 = 0x01;
pub const TAG_BIN64: u8 = 0x02;
pub const TAG_STR: u8 = 0x03;
pub const TAG_BOOL_TRUE: u8 = 0x04;
pub const TAG_BOOL_FALSE: u8 = 0x05;

pub unsafe fn read_le<T>(data: *const u8, pos: *mut usize, bytes: usize) -> T
where
    T: From<u8> + core::ops::BitOr<Output = T> + core::ops::Shl<usize, Output = T>,
{
    let current_pos = *pos;
    let mut val = T::from(*data.add(current_pos));
    for i in 1..bytes {
        val = val | (T::from(*data.add(current_pos + i)) << (i * 8));
    }
    *pos = current_pos + bytes;
    val
}

pub unsafe fn read_u32_le(data: *const u8, pos: *mut usize) -> u32 {
    read_le::<u32>(data, pos, 4)
}


pub unsafe fn read_i64_le(data: *const u8, pos: *mut usize) -> i64 {
    read_le::<u64>(data, pos, 8) as i64
}

pub unsafe fn read_u64_le(data: *const u8, pos: *mut usize) -> u64 {
    read_le::<u64>(data, pos, 8)
}

pub unsafe fn parse_module(data: &[u8]) -> (*mut Vec<Value>, *mut Vec<ProcInfo>, *mut Vec<u8>) {
    let mut pos = 0;

    if data.len() < MAGIC_SIZE || &data[0..MAGIC_SIZE] != b"MUSI" {
        panic!("invalid magic");
    }
    pos += MAGIC_SIZE;

    let _version = read_u32_le(data.as_ptr(), &mut pos);
    let _import_offset = read_u32_le(data.as_ptr(), &mut pos) as usize;
    let _import_size = read_u32_le(data.as_ptr(), &mut pos);
    let const_offset = read_u32_le(data.as_ptr(), &mut pos) as usize;
    let _const_size = read_u32_le(data.as_ptr(), &mut pos);
    let proc_offset = read_u32_le(data.as_ptr(), &mut pos) as usize;
    let _proc_size = read_u32_le(data.as_ptr(), &mut pos);

    let consts = parse_const_pool(data, const_offset);
    let procs = parse_proc_table(data, proc_offset);

    let code_offset = proc_offset + _proc_size as usize;
    let code_end = data.len();
    let code_vec = data[code_offset..code_end].to_vec();
    let code = Box::into_raw(Box::new(code_vec));

    (consts, procs, code)
}

pub unsafe fn parse_const_pool(data: &[u8], offset: usize) -> ValueList {
    let mut pos = offset;
    let count = read_u32_le(data.as_ptr(), &mut pos) as usize;
    let consts = Box::into_raw(Box::new(Vec::with_capacity(count)));

    for _ in 0..count {
        let tag = data[pos];
        pos += 1;

        let val = match tag {
            TAG_INT64 => Value::Int64(read_i64_le(data.as_ptr(), &mut pos)),
            TAG_BIN64 => Value::Bin64(f64::from_bits(read_u64_le(data.as_ptr(), &mut pos))),
            TAG_STR => {
                let start = pos;
                while data[pos] != 0 {
                    pos += 1;
                }
                let s = core::str::from_utf8_unchecked(&data[start..pos]);
                let string_ptr = string_new(s);
                pos += 1;
                Value::Str(string_ptr)
            }
            TAG_BOOL_TRUE => Value::Bool(true),
            TAG_BOOL_FALSE => Value::Bool(false),
            TAG_UNIT => Value::Unit,
            _ => panic!("unknown const type: 0x{tag:02x}"),
        };
        (*consts).push(val);
    }

    consts
}

pub unsafe fn parse_proc_table(data: &[u8], offset: usize) -> ProcTable {
    let mut pos = offset;
    let count = read_u32_le(data.as_ptr(), &mut pos) as usize;
    let procs = Box::into_raw(Box::new(Vec::with_capacity(count)));

    for _ in 0..count {
        let name_start = pos;
        while data[pos] != 0 {
            pos += 1;
        }
        let name = string_from_bytes(&data[name_start..pos]);
        pos += 1;

        let param_count = read_u32_le(data.as_ptr(), &mut pos) as u16;
        let local_count = read_u32_le(data.as_ptr(), &mut pos) as u16;
        let code_offset = read_u32_le(data.as_ptr(), &mut pos);

        (*procs).push(ProcInfo {
            name,
            code_offset,
            param_count,
            local_count,
        });
    }

    procs
}
