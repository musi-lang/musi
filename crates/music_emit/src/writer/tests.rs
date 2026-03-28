use musi_vm::Vm;
use music_il::format;
use music_il::format::{
    ClassDescriptor, ClassInstance, ClassMethod, EffectDescriptor, EffectOpDescriptor, FfiType,
    ForeignAbi, ForeignDescriptor, TypeDescriptor, TypeKind,
};
use music_il::instruction::Instruction;
use music_il::opcode::Opcode;
use music_shared::Symbol;

use crate::emitter::{GlobalEntry, MethodEntry, SeamModule};
use crate::pool::{ConstantEntry, ConstantPool};
use crate::writer::write_seam;

fn empty_module() -> SeamModule {
    SeamModule {
        constants: ConstantPool::new(),
        methods: Vec::new(),
        globals: Vec::new(),
        types: Vec::new(),
        effects: Vec::new(),
        classes: Vec::new(),
        foreigns: Vec::new(),
    }
}

#[test]
fn header_starts_with_magic() {
    let module = empty_module();
    let bytes = write_seam(&module);
    assert!(bytes.len() >= format::HEADER_SIZE);
    assert_eq!(&bytes[0..4], &format::MAGIC);
}

#[test]
fn header_version() {
    let module = empty_module();
    let bytes = write_seam(&module);
    assert_eq!(bytes[4], format::VERSION_MAJOR);
    assert_eq!(bytes[5], format::VERSION_MINOR);
}

#[test]
fn header_section_count_zero_for_empty() {
    let module = empty_module();
    let bytes = write_seam(&module);
    let section_count = u32::from_le_bytes([bytes[8], bytes[9], bytes[10], bytes[11]]);
    assert_eq!(section_count, 0);
}

#[test]
fn header_total_size_matches_buffer() {
    let module = empty_module();
    let bytes = write_seam(&module);
    let total_size = u32::from_le_bytes([bytes[12], bytes[13], bytes[14], bytes[15]]);
    assert_eq!(usize::try_from(total_size).unwrap(), bytes.len());
}

#[test]
fn constant_pool_section_present_when_nonempty() {
    let mut pool = ConstantPool::new();
    let _idx = pool.add(ConstantEntry::Int(42));

    let module = SeamModule {
        constants: pool,
        methods: Vec::new(),
        globals: Vec::new(),
        types: Vec::new(),
        effects: Vec::new(),
        classes: Vec::new(),
        foreigns: Vec::new(),
    };
    let bytes = write_seam(&module);

    let section_count = u32::from_le_bytes([bytes[8], bytes[9], bytes[10], bytes[11]]);
    assert!(section_count >= 1);

    let cnst_tag = &format::section::CNST;
    let found = bytes
        .windows(4)
        .skip(format::HEADER_SIZE)
        .any(|w| w == cnst_tag);
    assert!(found, "CNST section tag not found in output");
}

#[test]
fn roundtrip_string_in_string_table() {
    let mut pool = ConstantPool::new();
    let _idx = pool.add(ConstantEntry::Str(String::from("hello")));

    let module = SeamModule {
        constants: pool,
        methods: Vec::new(),
        globals: Vec::new(),
        types: Vec::new(),
        effects: Vec::new(),
        classes: Vec::new(),
        foreigns: Vec::new(),
    };
    let bytes = write_seam(&module);

    let strt_tag = &format::section::STRT;
    let tag_pos = bytes
        .windows(4)
        .position(|w| w == strt_tag)
        .expect("STRT section not found");
    let data_start = tag_pos + 4 + 4;
    let expected = b"hello\0";
    assert_eq!(&bytes[data_start..data_start + expected.len()], expected);
}

#[test]
fn roundtrip_tag_constant_supports_arrtag_compare() {
    let mut pool = ConstantPool::new();
    let tag_idx = pool.add(ConstantEntry::Tag(7));

    let module = SeamModule {
        constants: pool,
        methods: vec![crate::emitter::MethodEntry {
            name: None,
            source_name: None,
            locals_count: 0,
            absolute_global_loads: Vec::new(),
            instructions: vec![
                Instruction::with_type_tagged(
                    Opcode::ArrNewT,
                    format::BUILTIN_TYPE_ANY,
                    tag_idx as u8,
                    0,
                ),
                Instruction::simple(Opcode::ArrTag),
                Instruction::with_u16(Opcode::LdConst, tag_idx),
                Instruction::simple(Opcode::CmpEq),
                Instruction::simple(Opcode::Halt),
            ],
        }],
        globals: Vec::new(),
        types: Vec::new(),
        effects: Vec::new(),
        classes: Vec::new(),
        foreigns: Vec::new(),
    };

    let bytes = write_seam(&module);
    let loaded = musi_vm::load(&bytes).expect("load");
    let mut vm = Vm::new(loaded);
    let result = vm.run().expect("run");
    assert!(result.is_bool());
    assert!(result.as_bool());
}

#[test]
fn type_metadata_roundtrips_through_strt() {
    let module = SeamModule {
        constants: ConstantPool::new(),
        methods: Vec::new(),
        globals: Vec::new(),
        types: vec![TypeDescriptor {
            id: 0x0100,
            key: "/tmp/a.ms::Node".into(),
            kind: TypeKind::Choice,
            member_count: 1,
        }],
        effects: Vec::new(),
        classes: Vec::new(),
        foreigns: Vec::new(),
    };

    let loaded = musi_vm::load(&write_seam(&module)).expect("load");
    assert_eq!(loaded.types().len(), 1);
    assert_eq!(loaded.types()[0].key, "/tmp/a.ms::Node");
    assert_eq!(loaded.types()[0].kind, TypeKind::Choice);
}

#[test]
fn effect_metadata_roundtrips_through_strt() {
    let module = SeamModule {
        constants: ConstantPool::new(),
        methods: Vec::new(),
        globals: Vec::new(),
        types: Vec::new(),
        effects: vec![EffectDescriptor {
            id: 0,
            module_name: "musi:test".into(),
            name: "Test".into(),
            operations: vec![EffectOpDescriptor {
                id: 0,
                name: "emit".into(),
            }],
        }],
        classes: Vec::new(),
        foreigns: Vec::new(),
    };

    let loaded = musi_vm::load(&write_seam(&module)).expect("load");
    assert_eq!(loaded.effects().len(), 1);
    assert_eq!(loaded.effects()[0].module_name, "musi:test");
    assert_eq!(loaded.effects()[0].name, "Test");
    assert_eq!(loaded.effects()[0].operations[0].name, "emit");
}

#[test]
fn class_metadata_roundtrips_to_loaded_string_indices() {
    let mut pool = ConstantPool::new();
    let class_name = u32::from(pool.add(ConstantEntry::Str("Num".into())));
    let method_name = u32::from(pool.add(ConstantEntry::Str("add".into())));
    let module = SeamModule {
        constants: pool,
        methods: Vec::new(),
        globals: Vec::new(),
        types: Vec::new(),
        effects: Vec::new(),
        classes: vec![ClassDescriptor {
            id: 0,
            name_idx: class_name,
            method_count: 1,
            method_names: vec![method_name],
            instances: vec![ClassInstance {
                type_id: format::BUILTIN_TYPE_INT,
                methods: vec![ClassMethod {
                    name_idx: method_name,
                    method_idx: u16::MAX,
                }],
            }],
        }],
        foreigns: Vec::new(),
    };

    let loaded = musi_vm::load(&write_seam(&module)).expect("load");
    assert_eq!(loaded.strings()[0], "Num");
    assert_eq!(loaded.strings()[1], "add");
    assert_eq!(loaded.classes()[0].name_idx, 0);
    assert_eq!(loaded.classes()[0].method_names, vec![1]);
    assert_eq!(loaded.classes()[0].instances[0].methods[0].name_idx, 1);
}

#[test]
fn foreign_metadata_roundtrips_to_loaded_string_indices() {
    let mut pool = ConstantPool::new();
    let name_idx = u32::from(pool.add(ConstantEntry::Str("puts".into())));
    let symbol_idx = u32::from(pool.add(ConstantEntry::Str("puts".into())));
    let lib_idx = u32::from(pool.add(ConstantEntry::Str("libc.so.6".into())));
    let module = SeamModule {
        constants: pool,
        methods: Vec::new(),
        globals: Vec::new(),
        types: Vec::new(),
        effects: Vec::new(),
        classes: Vec::new(),
        foreigns: vec![ForeignDescriptor {
            name_idx,
            symbol_idx,
            lib_idx,
            abi: ForeignAbi::Default,
            arity: 1,
            exported: false,
            param_types: vec![FfiType::Str],
            return_type: FfiType::Int,
        }],
    };

    let loaded = musi_vm::load(&write_seam(&module)).expect("load");
    assert_eq!(loaded.strings()[0], "puts");
    assert_eq!(loaded.strings()[1], "libc.so.6");
    assert_eq!(loaded.foreigns()[0].name_idx, 0);
    assert_eq!(loaded.foreigns()[0].symbol_idx, 0);
    assert_eq!(loaded.foreigns()[0].lib_idx, 1);
}

#[test]
fn exported_global_name_roundtrips_through_strt_and_vm_export() {
    let module = SeamModule {
        constants: ConstantPool::new(),
        methods: vec![MethodEntry {
            name: None,
            source_name: None,
            locals_count: 0,
            absolute_global_loads: Vec::new(),
            instructions: vec![
                Instruction::with_i16(Opcode::LdSmi, 7),
                Instruction::with_u16(Opcode::StGlob, 0),
                Instruction::simple(Opcode::Halt),
            ],
        }],
        globals: vec![GlobalEntry {
            name: Symbol::synthetic(1),
            source_name: "test".into(),
            exported: true,
            opaque: false,
        }],
        types: Vec::new(),
        effects: Vec::new(),
        classes: Vec::new(),
        foreigns: Vec::new(),
    };

    let loaded = musi_vm::load(&write_seam(&module)).expect("load");
    assert_eq!(loaded.strings()[0], "test");

    let mut vm = Vm::new(loaded);
    let _ = vm.run().expect("run");
    let value = vm.export("test").expect("exported test");
    assert!(value.is_int());
    assert_eq!(value.as_int(), 7);
}
