use music_il::{ConstantPool, MethodEntry, MethodName, SeamArtifact};

use super::*;

#[test]
fn test_validator_accepts_small_modules() {
    let module = SeamArtifact {
        constants: ConstantPool::new(),
        methods: vec![MethodEntry {
            name: MethodName::Entry,
            instructions: vec![],
            locals_count: 0,
            absolute_global_loads: vec![],
        }],
        globals: vec![],
        types: vec![],
        effects: vec![],
        classes: vec![],
        foreigns: vec![],
    };

    validate_module(&module).expect("small modules should validate");
}
