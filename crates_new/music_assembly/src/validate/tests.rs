use music_il::{ConstantPool, MethodEntry, MethodName, SeamArtifact};

use super::*;

#[test]
fn test_validator_accepts_small_modules() {
    let module = SeamArtifact {
        constants: ConstantPool::new(),
        methods: vec![MethodEntry {
            name: MethodName::Entry,
            instructions: Vec::new(),
            locals_count: 0,
            absolute_global_loads: Vec::new(),
        }],
        globals: Vec::new(),
        types: Vec::new(),
        effects: Vec::new(),
        classes: Vec::new(),
        foreigns: Vec::new(),
    };

    validate_module(&module).expect("small modules should validate");
}
