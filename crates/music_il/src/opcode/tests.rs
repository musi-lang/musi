use std::collections::HashSet;

use super::*;

#[test]
fn all_78_opcodes_have_unique_byte_values() {
    let mut seen = HashSet::new();
    for &op in &ALL_OPCODES {
        let byte = op as u8;
        assert!(
            seen.insert(byte),
            "duplicate byte value {byte:#04x} for {op:?}"
        );
    }
    assert_eq!(seen.len(), OPCODE_COUNT);
}

#[test]
fn from_byte_round_trips_with_discriminant() {
    for &op in &ALL_OPCODES {
        let byte = op as u8;
        let decoded = Opcode::from_byte(byte).unwrap();
        assert_eq!(decoded, op, "round-trip failed for {byte:#04x}");
    }
}

#[test]
fn from_byte_returns_none_for_reserved() {
    let active: HashSet<u8> = ALL_OPCODES.iter().map(|op| *op as u8).collect();
    for byte in 0..=255u8 {
        if !active.contains(&byte) {
            assert!(
                Opcode::from_byte(byte).is_none(),
                "expected None for reserved byte {byte:#04x}"
            );
        }
    }
}

#[test]
fn mnemonic_returns_correct_string() {
    assert_eq!(Opcode::LdLoc.mnemonic(), "ld.loc");
    assert_eq!(Opcode::LdConst.mnemonic(), "ld.const");
    assert_eq!(Opcode::LdGlob.mnemonic(), "ld.glob");
    assert_eq!(Opcode::LdUpv.mnemonic(), "ld.upv");
    assert_eq!(Opcode::LdUnit.mnemonic(), "ld.unit");
    assert_eq!(Opcode::LdTru.mnemonic(), "ld.tru");
    assert_eq!(Opcode::LdFls.mnemonic(), "ld.fls");
    assert_eq!(Opcode::LdNil.mnemonic(), "ld.nil");
    assert_eq!(Opcode::LdOne.mnemonic(), "ld.one");
    assert_eq!(Opcode::LdSmi.mnemonic(), "ld.smi");
    assert_eq!(Opcode::StLoc.mnemonic(), "st.loc");
    assert_eq!(Opcode::StGlob.mnemonic(), "st.glob");
    assert_eq!(Opcode::StUpv.mnemonic(), "st.upv");
    assert_eq!(Opcode::LdLocW.mnemonic(), "ld.loc.w");
    assert_eq!(Opcode::StLocW.mnemonic(), "st.loc.w");
    assert_eq!(Opcode::Pop.mnemonic(), "pop");
    assert_eq!(Opcode::Dup.mnemonic(), "dup");
    assert_eq!(Opcode::Swap.mnemonic(), "swap");
    assert_eq!(Opcode::Rot.mnemonic(), "rot");
    assert_eq!(Opcode::IAdd.mnemonic(), "i.add");
    assert_eq!(Opcode::ISub.mnemonic(), "i.sub");
    assert_eq!(Opcode::IMul.mnemonic(), "i.mul");
    assert_eq!(Opcode::IDiv.mnemonic(), "i.div");
    assert_eq!(Opcode::IRem.mnemonic(), "i.rem");
    assert_eq!(Opcode::INeg.mnemonic(), "i.neg");
    assert_eq!(Opcode::FAdd.mnemonic(), "f.add");
    assert_eq!(Opcode::FSub.mnemonic(), "f.sub");
    assert_eq!(Opcode::FMul.mnemonic(), "f.mul");
    assert_eq!(Opcode::FDiv.mnemonic(), "f.div");
    assert_eq!(Opcode::FNeg.mnemonic(), "f.neg");
    assert_eq!(Opcode::And.mnemonic(), "and");
    assert_eq!(Opcode::Or.mnemonic(), "or");
    assert_eq!(Opcode::Not.mnemonic(), "not");
    assert_eq!(Opcode::Xor.mnemonic(), "xor");
    assert_eq!(Opcode::Shl.mnemonic(), "shl");
    assert_eq!(Opcode::Shr.mnemonic(), "shr");
    assert_eq!(Opcode::CmpEq.mnemonic(), "cmp.eq");
    assert_eq!(Opcode::CmpNeq.mnemonic(), "cmp.neq");
    assert_eq!(Opcode::CmpLt.mnemonic(), "cmp.lt");
    assert_eq!(Opcode::CmpGt.mnemonic(), "cmp.gt");
    assert_eq!(Opcode::CmpLeq.mnemonic(), "cmp.leq");
    assert_eq!(Opcode::CmpGeq.mnemonic(), "cmp.geq");
    assert_eq!(Opcode::BrTrue.mnemonic(), "br.true");
    assert_eq!(Opcode::BrFalse.mnemonic(), "br.false");
    assert_eq!(Opcode::BrJmp.mnemonic(), "br.jmp");
    assert_eq!(Opcode::BrTbl.mnemonic(), "br.tbl");
    assert_eq!(Opcode::BrBack.mnemonic(), "br.back");
    assert_eq!(Opcode::Call.mnemonic(), "call");
    assert_eq!(Opcode::CallTail.mnemonic(), "call.tail");
    assert_eq!(Opcode::Ret.mnemonic(), "ret");
    assert_eq!(Opcode::ClsNew.mnemonic(), "cls.new");
    assert_eq!(Opcode::ArrNew.mnemonic(), "arr.new");
    assert_eq!(Opcode::ArrGet.mnemonic(), "arr.get");
    assert_eq!(Opcode::ArrSet.mnemonic(), "arr.set");
    assert_eq!(Opcode::ArrLen.mnemonic(), "arr.len");
    assert_eq!(Opcode::ArrSlice.mnemonic(), "arr.slice");
    assert_eq!(Opcode::ArrFill.mnemonic(), "arr.fill");
    assert_eq!(Opcode::ArrCopy.mnemonic(), "arr.copy");
    assert_eq!(Opcode::ArrCaten.mnemonic(), "arr.caten");
    assert_eq!(Opcode::ArrGetI.mnemonic(), "arr.get.i");
    assert_eq!(Opcode::ArrSetI.mnemonic(), "arr.set.i");
    assert_eq!(Opcode::ArrTag.mnemonic(), "arr.tag");
    assert_eq!(Opcode::ArrNewT.mnemonic(), "arr.new.t");
    assert_eq!(Opcode::TyChk.mnemonic(), "ty.chk");
    assert_eq!(Opcode::TyCast.mnemonic(), "ty.cast");
    assert_eq!(Opcode::TyTag.mnemonic(), "ty.tag");
    assert_eq!(Opcode::EffHdlPush.mnemonic(), "eff.hdl.push");
    assert_eq!(Opcode::EffHdlPop.mnemonic(), "eff.hdl.pop");
    assert_eq!(Opcode::EffInvk.mnemonic(), "eff.invk");
    assert_eq!(Opcode::EffCont.mnemonic(), "eff.cont");
    assert_eq!(Opcode::TyclDict.mnemonic(), "tycl.dict");
    assert_eq!(Opcode::TyclCall.mnemonic(), "tycl.call");
    assert_eq!(Opcode::GcPin.mnemonic(), "gc.pin");
    assert_eq!(Opcode::GcUnpin.mnemonic(), "gc.unpin");
    assert_eq!(Opcode::FfiCall.mnemonic(), "ffi.call");
    assert_eq!(Opcode::Nop.mnemonic(), "nop");
    assert_eq!(Opcode::Panic.mnemonic(), "panic");
    assert_eq!(Opcode::Halt.mnemonic(), "halt");
}

#[test]
fn from_mnemonic_round_trips_with_mnemonic() {
    for &op in &ALL_OPCODES {
        let mn = op.mnemonic();
        let decoded = Opcode::from_mnemonic(mn).unwrap();
        assert_eq!(decoded, op, "round-trip failed for mnemonic {mn:?}");
    }
}

#[test]
fn from_mnemonic_returns_none_for_unknown() {
    assert!(Opcode::from_mnemonic("nope").is_none());
    assert!(Opcode::from_mnemonic("").is_none());
    assert!(Opcode::from_mnemonic("ld.loc.extra").is_none());
}

#[test]
fn opcode_count_matches_all_opcodes_len() {
    assert_eq!(ALL_OPCODES.len(), OPCODE_COUNT);
}
