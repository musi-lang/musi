//! Instruction encoding functions for the SEAM bytecode format.
//!
//! All multi-byte operands are big-endian. The i24 signed offset for
//! `BR_LONG` is stored as three bytes in two's-complement big-endian order,
//! saturated to the [-8_388_608, 8_388_607] range.

use crate::Opcode;

/// Emit a format-0 (no operand) instruction.
pub fn encode_f0(buf: &mut Vec<u8>, op: Opcode) {
    buf.push(op.0);
}

/// Emit a format-I8 (single u8 operand) instruction.
pub fn encode_fi8(buf: &mut Vec<u8>, op: Opcode, arg: u8) {
    buf.push(op.0);
    buf.push(arg);
}

/// Emit a format-I16 (single u16 operand, big-endian) instruction.
pub fn encode_fi16(buf: &mut Vec<u8>, op: Opcode, arg: u16) {
    buf.push(op.0);
    let [hi, lo] = arg.to_be_bytes();
    buf.push(hi);
    buf.push(lo);
}

/// Emit a format-I8x2 (two independent u8 operands) instruction.
pub fn encode_fi8x2(buf: &mut Vec<u8>, op: Opcode, a: u8, b: u8) {
    buf.push(op.0);
    buf.push(a);
    buf.push(b);
}

/// Emit a format-I24 (signed 24-bit offset, big-endian) instruction.
///
/// The `offset` value must fit in [-8_388_608, 8_388_607]. Values outside
/// that range are clamped — callers are expected to stay within bounds.
pub fn encode_fi24(buf: &mut Vec<u8>, op: Opcode, offset: i32) {
    // Clamp to i24 range so the three bytes always round-trip correctly.
    let clamped = offset.clamp(-8_388_608, 8_388_607);
    // Keep only the low 24 bits of the two's-complement representation.
    let bits = (clamped as u32) & 0x00FF_FFFF;
    buf.push(op.0);
    buf.push((bits >> 16) as u8);
    buf.push((bits >> 8) as u8);
    buf.push(bits as u8);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Opcode;

    #[test]
    fn test_encode_f0_is_one_byte() {
        let mut buf = vec![];
        encode_f0(&mut buf, Opcode::NOP);
        assert_eq!(buf, vec![Opcode::NOP.0]);
    }

    #[test]
    fn test_encode_fi8_is_two_bytes() {
        let mut buf = vec![];
        encode_fi8(&mut buf, Opcode::LD_LOC, 42);
        assert_eq!(buf, vec![Opcode::LD_LOC.0, 42]);
    }

    #[test]
    fn test_encode_fi16_big_endian() {
        let mut buf = vec![];
        encode_fi16(&mut buf, Opcode::LD_CONST, 0x0102);
        assert_eq!(buf, vec![Opcode::LD_CONST.0, 0x01, 0x02]);
    }

    #[test]
    fn test_encode_fi16_roundtrip() {
        let mut buf = vec![];
        encode_fi16(&mut buf, Opcode::BR, 0xABCD);
        let decoded = u16::from_be_bytes([buf[1], buf[2]]);
        assert_eq!(decoded, 0xABCD);
    }

    #[test]
    fn test_encode_fi8x2_is_three_bytes() {
        let mut buf = vec![];
        encode_fi8x2(&mut buf, Opcode::REC_NEW, 3, 7);
        assert_eq!(buf, vec![Opcode::REC_NEW.0, 3, 7]);
    }

    #[test]
    fn test_encode_fi24_positive() {
        let mut buf = vec![];
        encode_fi24(&mut buf, Opcode::BR_LONG, 1000);
        assert_eq!(buf.len(), 4);
        assert_eq!(buf[0], Opcode::BR_LONG.0);
        // 1000 = 0x0003E8 → [0x00, 0x03, 0xE8]
        assert_eq!(&buf[1..], &[0x00, 0x03, 0xE8]);
    }

    #[test]
    fn test_encode_fi24_negative() {
        let mut buf = vec![];
        encode_fi24(&mut buf, Opcode::BR_LONG, -1);
        assert_eq!(buf.len(), 4);
        // -1 in i24 two's-complement = 0xFFFFFF
        assert_eq!(&buf[1..], &[0xFF, 0xFF, 0xFF]);
    }

    #[test]
    fn test_encode_fi24_roundtrip() {
        for &offset in &[0i32, 1, -1, 100, -100, 8_388_607, -8_388_608] {
            let mut buf = vec![];
            encode_fi24(&mut buf, Opcode::BR_LONG, offset);
            // Reconstruct signed i24 from the three bytes.
            let bits = (u32::from(buf[1]) << 16) | (u32::from(buf[2]) << 8) | u32::from(buf[3]);
            // Sign-extend from bit 23.
            let decoded = if bits & 0x80_0000 != 0 {
                (bits | 0xFF00_0000) as i32
            } else {
                bits as i32
            };
            assert_eq!(decoded, offset, "roundtrip failed for offset {offset}");
        }
    }

    #[test]
    fn test_encode_fi24_clamps_overflow() {
        let mut buf = vec![];
        encode_fi24(&mut buf, Opcode::BR_LONG, i32::MAX);
        // Should clamp to 8_388_607 = 0x7FFFFF
        assert_eq!(&buf[1..], &[0x7F, 0xFF, 0xFF]);
    }
}
