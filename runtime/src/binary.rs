pub fn read_u32_le(data: &[u8], pos: &mut usize) -> u32 {
    let val = u32::from_le_bytes([data[*pos], data[*pos + 1], data[*pos + 2], data[*pos + 3]]);
    *pos += 4;
    val
}

pub fn read_i32_le(data: &[u8], pos: &mut usize) -> i32 {
    let val = i32::from_le_bytes([data[*pos], data[*pos + 1], data[*pos + 2], data[*pos + 3]]);
    *pos += 4;
    val
}

pub fn read_i64_le(data: &[u8], pos: &mut usize) -> i64 {
    let val = i64::from_le_bytes([
        data[*pos],
        data[*pos + 1],
        data[*pos + 2],
        data[*pos + 3],
        data[*pos + 4],
        data[*pos + 5],
        data[*pos + 6],
        data[*pos + 7],
    ]);
    *pos += 8;
    val
}

pub fn read_u64_le(data: &[u8], pos: &mut usize) -> u64 {
    let val = u64::from_le_bytes([
        data[*pos],
        data[*pos + 1],
        data[*pos + 2],
        data[*pos + 3],
        data[*pos + 4],
        data[*pos + 5],
        data[*pos + 6],
        data[*pos + 7],
    ]);
    *pos += 8;
    val
}
