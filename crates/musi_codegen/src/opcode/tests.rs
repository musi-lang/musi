use super::*;

#[test]
fn nop_round_trip() {
    let op = Opcode::Nop;
    let mut buf = Vec::new();
    op.encode_into(&mut buf);
    assert_eq!(buf, [0x00]);
    let (decoded, size) = Opcode::decode(&buf, 0).expect("decode nop");
    assert_eq!(decoded, Opcode::Nop);
    assert_eq!(size, 1);
}

#[test]
fn ld_imm_i64_round_trip() {
    let op = Opcode::LdImmI64(-42_i64);
    let mut buf = Vec::new();
    op.encode_into(&mut buf);
    assert_eq!(buf.len(), 9);
    let (decoded, size) = Opcode::decode(&buf, 0).expect("decode i64");
    assert_eq!(decoded, Opcode::LdImmI64(-42));
    assert_eq!(size, 9);
}

#[test]
fn ld_imm_f64_round_trip() {
    let op = Opcode::LdImmF64(3.14_f64);
    let mut buf = Vec::new();
    op.encode_into(&mut buf);
    assert_eq!(buf.len(), 9);
    let (decoded, size) = Opcode::decode(&buf, 0).expect("decode f64");
    assert_eq!(decoded, Opcode::LdImmF64(3.14));
    assert_eq!(size, 9);
}

#[test]
fn call_round_trip() {
    let op = Opcode::Call(3);
    let mut buf = Vec::new();
    op.encode_into(&mut buf);
    assert_eq!(buf.len(), 3);
    let (decoded, size) = Opcode::decode(&buf, 0).expect("decode call");
    assert_eq!(decoded, Opcode::Call(3));
    assert_eq!(size, 3);
}

#[test]
fn bool_round_trip() {
    for v in [true, false] {
        let op = Opcode::LdImmBool(v);
        let mut buf = Vec::new();
        op.encode_into(&mut buf);
        assert_eq!(buf.len(), 2);
        let (decoded, size) = Opcode::decode(&buf, 0).expect("decode bool");
        assert_eq!(decoded, Opcode::LdImmBool(v));
        assert_eq!(size, 2);
    }
}

#[test]
fn encoded_len_matches_actual() {
    let ops = [
        Opcode::Nop,
        Opcode::Halt,
        Opcode::Ret,
        Opcode::Drop,
        Opcode::LdImmUnit,
        Opcode::LdImmI64(0),
        Opcode::LdImmF64(0.0),
        Opcode::LdImmBool(false),
        Opcode::LdConst(0),
        Opcode::LdLoc(0),
        Opcode::StLoc(0),
        Opcode::Call(0),
    ];
    for op in &ops {
        let mut buf = Vec::new();
        op.encode_into(&mut buf);
        assert_eq!(
            buf.len(),
            op.encoded_len(),
            "encoded_len mismatch for {op:?}"
        );
    }
}

#[test]
fn unknown_opcode_error() {
    let result = Opcode::decode(&[0xFF], 0);
    assert!(matches!(
        result,
        Err(DeserError::UnknownOpcode {
            tag: 0xFF,
            offset: 0
        })
    ));
}

#[test]
fn unexpected_eof_on_truncated_i64() {
    // LD_IMM_I64 tag present but payload truncated
    let result = Opcode::decode(&[0x10], 0);
    assert!(matches!(result, Err(DeserError::UnexpectedEof)));
}

#[test]
fn decode_at_nonzero_offset() {
    let mut buf = Vec::new();
    Opcode::Nop.encode_into(&mut buf);
    Opcode::LdImmI64(99).encode_into(&mut buf);
    let (decoded, size) = Opcode::decode(&buf, 1).expect("decode at offset 1");
    assert_eq!(decoded, Opcode::LdImmI64(99));
    assert_eq!(size, 9);
}
