    use super::*;
    use musi_codegen::{ConstEntry, FunctionEntry, Module, Opcode, SymbolEntry, SymbolFlags};

    use crate::error::VmError;
    use crate::native::NativeRegistry;
    use crate::value::Value;

    /// Builds the "Hello, world!" module:
    /// - fn 0: writeln  (native, param_count=1)
    /// - fn 1: main     (LdConst 0, Call 0, Halt)
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
                FunctionEntry {
                    symbol_idx: 0,
                    param_count: 1,
                    local_count: 0,
                    code_offset: 0,
                    code_length: 0,
                },
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
    fn hello_world_executes_without_error() {
        let module = hello_module();
        let natives = NativeRegistry::new();
        let mut vm = Vm::new(module, natives);
        // entry point = fn 1 (main)
        let result = vm.run(1).expect("vm run");
        assert_eq!(result, Value::Unit);
    }

    #[test]
    fn halt_returns_top_of_stack() {
        // Module: fn 0 = main: LdImmI64(99), Halt
        let mut code = Vec::new();
        Opcode::LdImmI64(99).encode_into(&mut code);
        Opcode::Halt.encode_into(&mut code);
        let code_len = u32::try_from(code.len()).expect("fits");

        let module = Module {
            const_pool: vec![],
            symbol_table: vec![SymbolEntry {
                name: "main".into(),
                flags: SymbolFlags::new(SymbolFlags::EXPORT),
                intrinsic_id: 0xFFFF,
            }],
            function_table: vec![FunctionEntry {
                symbol_idx: 0,
                param_count: 0,
                local_count: 0,
                code_offset: 0,
                code_length: code_len,
            }],
            code,
        };

        let mut vm = Vm::new(module, NativeRegistry::new());
        let result = vm.run(0).expect("vm run");
        assert_eq!(result, Value::Int(99));
    }

    #[test]
    fn local_store_and_load() {
        // main: LdImmI64(7), StLoc(0), LdLoc(0), Halt  (local_count=1)
        let mut code = Vec::new();
        Opcode::LdImmI64(7).encode_into(&mut code);
        Opcode::StLoc(0).encode_into(&mut code);
        Opcode::LdLoc(0).encode_into(&mut code);
        Opcode::Halt.encode_into(&mut code);
        let code_len = u32::try_from(code.len()).expect("fits");

        let module = Module {
            const_pool: vec![],
            symbol_table: vec![SymbolEntry {
                name: "main".into(),
                flags: SymbolFlags::new(SymbolFlags::EXPORT),
                intrinsic_id: 0xFFFF,
            }],
            function_table: vec![FunctionEntry {
                symbol_idx: 0,
                param_count: 0,
                local_count: 1,
                code_offset: 0,
                code_length: code_len,
            }],
            code,
        };

        let mut vm = Vm::new(module, NativeRegistry::new());
        let result = vm.run(0).expect("vm run");
        assert_eq!(result, Value::Int(7));
    }

    #[test]
    fn ret_returns_to_caller() {
        // fn 0 (callee): LdImmI64(42), Ret
        // fn 1 (main):   Call(0), Halt
        let mut callee_code = Vec::new();
        Opcode::LdImmI64(42).encode_into(&mut callee_code);
        Opcode::Ret.encode_into(&mut callee_code);

        let mut main_code = Vec::new();
        Opcode::Call(0).encode_into(&mut main_code);
        Opcode::Halt.encode_into(&mut main_code);

        let callee_len = u32::try_from(callee_code.len()).expect("fits");
        let callee_offset = 0u32;
        let main_offset = callee_len;
        let main_len = u32::try_from(main_code.len()).expect("fits");

        let mut code = callee_code;
        code.extend_from_slice(&main_code);

        let module = Module {
            const_pool: vec![],
            symbol_table: vec![
                SymbolEntry {
                    name: "callee".into(),
                    flags: SymbolFlags::new(0),
                    intrinsic_id: 0xFFFF,
                },
                SymbolEntry {
                    name: "main".into(),
                    flags: SymbolFlags::new(SymbolFlags::EXPORT),
                    intrinsic_id: 0xFFFF,
                },
            ],
            function_table: vec![
                FunctionEntry {
                    symbol_idx: 0,
                    param_count: 0,
                    local_count: 0,
                    code_offset: callee_offset,
                    code_length: callee_len,
                },
                FunctionEntry {
                    symbol_idx: 1,
                    param_count: 0,
                    local_count: 0,
                    code_offset: main_offset,
                    code_length: main_len,
                },
            ],
            code,
        };

        let mut vm = Vm::new(module, NativeRegistry::new());
        let result = vm.run(1).expect("vm run");
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn unknown_function_index_is_error() {
        let module = Module::new();
        let mut vm = Vm::new(module, NativeRegistry::new());
        let err = vm.run(0).expect_err("should fail");
        assert!(matches!(err, VmError::FunctionOutOfBounds(0)));
    }

    /// Tests that a while loop counting y from 0 to 10 terminates correctly.
    ///
    /// Equivalent to:
    ///   const x := 10;  (slot 0)
    ///   var y := 0;     (slot 1)
    ///   while y < x loop ( y <- y + 1; );
    ///   // result: y == 10
    #[test]
    fn while_loop_counts_to_10() {
        let mut code: Vec<u8> = Vec::new();

        // const x := 10  (slot 0)
        Opcode::LdImmI64(10).encode_into(&mut code); // 9 bytes  [0..9)
        Opcode::StLoc(0).encode_into(&mut code); // 3 bytes  [9..12)
        Opcode::LdImmUnit.encode_into(&mut code); // 1 byte   [12)
        Opcode::Drop.encode_into(&mut code); // 1 byte   [13)

        // var y := 0  (slot 1)
        Opcode::LdImmI64(0).encode_into(&mut code); // 9 bytes  [14..23)
        Opcode::StLoc(1).encode_into(&mut code); // 3 bytes  [23..26)
        Opcode::LdImmUnit.encode_into(&mut code); // 1 byte   [26)
        Opcode::Drop.encode_into(&mut code); // 1 byte   [27)

        // while y < x -- loop start at byte 28
        let loop_start: usize = 28;
        assert_eq!(code.len(), loop_start, "loop_start mismatch");

        Opcode::LdLoc(1).encode_into(&mut code); // 3 bytes  [28..31)  load y
        Opcode::LdLoc(0).encode_into(&mut code); // 3 bytes  [31..34)  load x
        Opcode::LtI64.encode_into(&mut code); // 1 byte   [34)      y < x

        // BrFalse placeholder -- will be patched
        let brfalse_pos = code.len(); // 35
        code.push(0x62); // BR_FALSE tag
        code.extend_from_slice(&i32::MAX.to_le_bytes()); // placeholder
        // code.len() == 40

        // loop body: y <- y + 1
        Opcode::LdLoc(1).encode_into(&mut code); // 3 bytes  [40..43)
        Opcode::LdImmI64(1).encode_into(&mut code); // 9 bytes  [43..52)
        Opcode::AddI64.encode_into(&mut code); // 1 byte   [52)
        Opcode::StLoc(1).encode_into(&mut code); // 3 bytes  [53..56)
        Opcode::LdImmUnit.encode_into(&mut code); // 1 byte   [56)  assign → Unit
        Opcode::Drop.encode_into(&mut code); // 1 byte   [57)  drop stmt
        Opcode::LdImmUnit.encode_into(&mut code); // 1 byte   [58)  block tail
        Opcode::Drop.encode_into(&mut code); // 1 byte   [59)  drop body

        // Br back to loop_start=28
        // after_instr = code.len() + 5 = 65; offset = 28 - 65 = -37
        let br_back_after = code.len() + 5; // 65
        let br_offset =
            i32::try_from(loop_start as isize - br_back_after as isize).expect("offset fits i32");
        assert_eq!(br_offset, -37, "br_back offset");
        Opcode::Br(br_offset).encode_into(&mut code); // 5 bytes  [60..65)

        // patch BrFalse: target = code.len() = 65
        let exit_pos = code.len(); // 65
        let brfalse_after = brfalse_pos + 5; // 40
        let brfalse_offset = i32::try_from(exit_pos as isize - brfalse_after as isize)
            .expect("brfalse offset fits i32");
        assert_eq!(brfalse_offset, 25, "brfalse offset");
        code[brfalse_pos + 1..brfalse_pos + 5].copy_from_slice(&brfalse_offset.to_le_bytes());

        // while result = Unit
        Opcode::LdImmUnit.encode_into(&mut code); // 1 byte   [65)
        Opcode::Drop.encode_into(&mut code); // 1 byte   [66)

        // load y and halt to inspect value
        Opcode::LdLoc(1).encode_into(&mut code); // 3 bytes  [67..70)
        Opcode::Halt.encode_into(&mut code); // 1 byte   [70)

        let code_len = u32::try_from(code.len()).expect("fits");

        let module = Module {
            const_pool: vec![],
            symbol_table: vec![SymbolEntry {
                name: "main".into(),
                flags: SymbolFlags::new(SymbolFlags::EXPORT),
                intrinsic_id: 0xFFFF,
            }],
            function_table: vec![FunctionEntry {
                symbol_idx: 0,
                param_count: 0,
                local_count: 2,
                code_offset: 0,
                code_length: code_len,
            }],
            code,
        };

        let mut vm = Vm::new(module, NativeRegistry::new());
        let result = vm.run(0).expect("while loop should terminate");
        assert_eq!(result, Value::Int(10), "y should equal 10 after loop");
    }
