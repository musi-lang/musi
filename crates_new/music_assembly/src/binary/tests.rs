use music_il::{ConstantEntry, ConstantPool, Instruction, MethodEntry, MethodName, SeamArtifact};

use super::*;

#[test]
fn test_binary_codec_round_trips_minimal_module() {
    let mut constants = ConstantPool::new();
    let _ = constants.add(ConstantEntry::Str("hello".to_owned()));

    let module = SeamArtifact {
        constants,
        methods: vec![MethodEntry {
            name: MethodName::Entry,
            instructions: vec![
                Instruction::with_u16(music_il::Opcode::LdConst, 0),
                Instruction::simple(music_il::Opcode::Ret),
            ],
            locals_count: 1,
            absolute_global_loads: vec![],
        }],
        globals: vec![],
        types: vec![],
        effects: vec![],
        classes: vec![],
        foreigns: vec![],
    };

    let bytes = encode_binary(&module).expect("module should encode");
    let decoded = decode_binary(&bytes).expect("module should decode");

    assert_eq!(decoded.constants.entries().len(), 1);
    assert_eq!(decoded.methods.len(), 1);
}
