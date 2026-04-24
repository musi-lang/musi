use super::*;
use std::collections::BTreeSet;

mod success {
    use super::*;

    #[test]
    fn opcode_catalog_has_unique_mnemonics_and_wire_codes() {
        let mut mnemonics = BTreeSet::new();
        let mut wire_codes = BTreeSet::new();
        for info in OPCODE_INFOS {
            let wire = match info.wire {
                OpcodeWire::Core(code) => u16::from(code),
                OpcodeWire::Extended(code) => code,
            };
            assert!(
                mnemonics.insert(info.mnemonic),
                "duplicate mnemonic `{}`",
                info.mnemonic
            );
            assert!(wire_codes.insert(wire), "duplicate wire code `{wire:#06x}`");
        }
        assert_eq!(mnemonics.len(), OPCODE_INFOS.len());
        assert_eq!(wire_codes.len(), OPCODE_INFOS.len());
    }

    #[test]
    fn opcode_mnemonics_fit_seam_style_constraints() {
        for info in OPCODE_INFOS {
            assert!(
                info.mnemonic
                    .chars()
                    .all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit() || ch == '.'),
                "mnemonic has invalid chars `{}`",
                info.mnemonic
            );
        }
    }
}

mod failure {}
