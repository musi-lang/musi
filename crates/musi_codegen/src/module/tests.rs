// Tests for the parent module — extracted from the inline test block.
    use super::*;
    use crate::opcode::Opcode;

    /// Builds the canonical "Hello, world!" demo module:
    ///
    /// - const_pool\[0\] = `String("Hello, world!")`
    /// - symbol_table\[0\] = writeln (native, intrinsic_id=1, export)
    /// - symbol_table\[1\] = main (export)
    /// - function_table\[0\] = writeln (native, param_count=1, no code)
    /// - function_table\[1\] = main (code: `LdConst(0) Call(0) Halt`)
    fn hello_module() -> Module {
        let mut code = Vec::new();
        Opcode::LdConst(0).encode_into(&mut code);
        Opcode::Call(0).encode_into(&mut code);
        Opcode::Halt.encode_into(&mut code);
        let code_len = u32::try_from(code.len()).expect("fits");

        Module {
            const_pool: vec![ConstEntry::String("Hello, world!".into())],
            symbol_table: vec![
                SymbolEntry {
                    name: "writeln".into(),
                    flags: SymbolFlags::new(SymbolFlags::NATIVE | SymbolFlags::EXPORT),
                    intrinsic_id: 1,
                },
                SymbolEntry {
                    name: "main".into(),
                    flags: SymbolFlags::new(SymbolFlags::EXPORT),
                    intrinsic_id: 0xFFFF,
                },
            ],
            function_table: vec![
                // fn 0: writeln (native)
                FunctionEntry {
                    symbol_idx: 0,
                    param_count: 1,
                    local_count: 0,
                    code_offset: 0,
                    code_length: 0,
                },
                // fn 1: main
                FunctionEntry {
                    symbol_idx: 1,
                    param_count: 0,
                    local_count: 0,
                    code_offset: 0,
                    code_length: code_len,
                },
            ],
            code,
        }
    }

    #[test]
    fn serialize_deserialize_round_trip() {
        let original = hello_module();
        let bytes = original.serialize();
        let restored = Module::deserialize(&bytes).expect("deserialize");
        assert_eq!(original, restored);
    }

    #[test]
    fn header_magic_is_correct() {
        let bytes = hello_module().serialize();
        assert_eq!(&bytes[..4], b"MUSI");
    }

    #[test]
    fn invalid_magic_error() {
        let mut bytes = hello_module().serialize();
        bytes[0] = 0xFF;
        assert!(matches!(
            Module::deserialize(&bytes),
            Err(DeserError::InvalidMagic)
        ));
    }

    #[test]
    fn unsupported_version_error() {
        let mut bytes = hello_module().serialize();
        // version is at bytes[4..6] (little-endian)
        bytes[4] = 0x02;
        bytes[5] = 0x00;
        assert!(matches!(
            Module::deserialize(&bytes),
            Err(DeserError::UnsupportedVersion(2))
        ));
    }

    #[test]
    fn truncated_bytes_returns_eof() {
        let bytes = hello_module().serialize();
        let truncated = &bytes[..5];
        assert!(matches!(
            Module::deserialize(truncated),
            Err(DeserError::UnexpectedEof)
        ));
    }

    #[test]
    fn empty_module_round_trip() {
        let m = Module::new();
        let bytes = m.serialize();
        let restored = Module::deserialize(&bytes).expect("deserialize empty");
        assert_eq!(m, restored);
    }

    #[test]
    fn const_pool_all_variants() {
        let mut m = Module::new();
        m.const_pool.push(ConstEntry::Int(-1));
        m.const_pool.push(ConstEntry::Float(2.5));
        m.const_pool.push(ConstEntry::String("hi".into()));
        m.const_pool.push(ConstEntry::Bool(true));
        let bytes = m.serialize();
        let restored = Module::deserialize(&bytes).expect("deserialize");
        assert_eq!(m, restored);
    }
