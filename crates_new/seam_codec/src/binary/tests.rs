use seam_ir::{ConstantEntry, ConstantPool, Instruction, MethodEntry, MethodName, SeamArtifact};

use super::{decode_binary, encode_binary};

#[test]
fn binary_codec_round_trips_minimal_module() {
    let mut constants = ConstantPool::new();
    let _ = constants.add(ConstantEntry::Str("hello".to_owned()));

    let module = SeamArtifact {
        constants,
        methods: vec![MethodEntry {
            name: MethodName::Entry,
            instructions: vec![
                Instruction::with_u16(seam_ir::Opcode::LdConst, 0),
                Instruction::simple(seam_ir::Opcode::Ret),
            ],
            locals_count: 1,
            absolute_global_loads: Vec::new(),
        }],
        globals: Vec::new(),
        types: Vec::new(),
        effects: Vec::new(),
        classes: Vec::new(),
        foreigns: Vec::new(),
    };

    let bytes = encode_binary(&module).expect("module should encode");
    let decoded = decode_binary(&bytes).expect("module should decode");

    assert_eq!(decoded.constants.entries().len(), 1);
    assert_eq!(decoded.methods.len(), 1);
}
