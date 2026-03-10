//! CRC-32/ISO-HDLC (standard Ethernet/ZIP CRC32).

/// Compute the CRC-32/ISO-HDLC checksum over `data`.
#[must_use]
pub fn crc32_slice(data: &[u8]) -> u32 {
    let mut crc: u32 = 0xFFFF_FFFF;
    for &byte in data {
        let low_byte = crc.to_le_bytes()[0];
        let table_idx = usize::from(low_byte ^ byte);
        crc = CRC32_TABLE[table_idx] ^ (crc >> 8);
    }
    crc ^ 0xFFFF_FFFF
}

/// Pre-computed CRC-32 lookup table (polynomial 0xEDB88320).
const CRC32_TABLE: [u32; 256] = {
    let poly: u32 = 0xEDB8_8320;
    let mut table = [0u32; 256];
    let mut i = 0usize;
    while i < 256 {
        let i_bytes = i.to_le_bytes();
        let mut crc = u32::from_le_bytes([i_bytes[0], i_bytes[1], i_bytes[2], i_bytes[3]]);
        let mut j = 0;
        while j < 8 {
            if crc & 1 != 0 {
                crc = (crc >> 1) ^ poly;
            } else {
                crc >>= 1;
            }
            j += 1;
        }
        table[i] = crc;
        i += 1;
    }
    table
};
