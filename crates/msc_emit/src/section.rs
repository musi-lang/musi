//! Section serialization: 4-byte tag + 4-byte BE length + payload.

pub fn write_section(buf: &mut Vec<u8>, tag: &[u8; 4], payload: &[u8]) {
    buf.extend_from_slice(tag);
    let len = payload.len() as u32;
    buf.extend_from_slice(&len.to_be_bytes());
    buf.extend_from_slice(payload);
}
