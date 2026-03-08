use super::*;
use crate::native_registry::NativeRegistry;
use musi_codegen::{ConstEntry, FunctionEntry, Module, Opcode, SymbolEntry, SymbolFlags};

use crate::error::VmError;
use crate::value::Value;

use musi_codegen::emit;
use musi_lex::lex;
use musi_parse::parse;
use musi_shared::{DiagnosticBag, Interner, SourceDb};

fn run_src(src: &str) -> Value {
    let prelude_src = include_str!("../../../../std/prelude.ms");
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let mut diags = DiagnosticBag::new();

    let prelude_id = source_db.add("<prelude>", prelude_src);
    let prelude_lexed = lex(prelude_src, prelude_id, &mut interner, &mut diags);
    let prelude_module = parse(&prelude_lexed.tokens, prelude_id, &mut diags, &interner);

    let user_id = source_db.add("test.ms", src);
    let user_lexed = lex(src, user_id, &mut interner, &mut diags);
    let user_module = parse(&user_lexed.tokens, user_id, &mut diags, &interner);

    assert!(!diags.has_errors(), "parse errors");

    let module = emit(&prelude_module, &[], &[], &user_module, &interner).expect("emit failed");
    let main_fn_idx = u16::try_from(module.function_table.len() - 1).expect("fits");
    let mut vm = Vm::new(module, NativeRegistry::new(&[]));
    vm.run(main_fn_idx).expect("vm run failed")
}

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
                intrinsic_id: 0,
                abi: Box::from(""),
                link_lib: None,
                link_name: None,
            },
            SymbolEntry {
                name: "main".into(),
                flags: SymbolFlags::new(SymbolFlags::EXPORT),
                intrinsic_id: 0xFFFF,
                abi: Box::from(""),
                link_lib: None,
                link_name: None,
            },
        ],
        function_table: vec![
            FunctionEntry {
                symbol_idx: 0,
                param_count: 1,
                local_count: 0,
                code_offset: 0,
                code_length: 0,
                return_kind: musi_codegen::module::ReturnKind::Unknown,
            },
            FunctionEntry {
                symbol_idx: 1,
                param_count: 0,
                local_count: 0,
                code_offset: 0,
                code_length: code_len,
                return_kind: musi_codegen::module::ReturnKind::Unknown,
            },
        ],
        code,
        method_table: Vec::new(),
    }
}

#[test]
fn hello_world_executes_without_error() {
    let module = hello_module();
    let mut vm = Vm::new(module, NativeRegistry::new(&[]));
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
            abi: Box::from(""),
            link_lib: None,
            link_name: None,
        }],
        function_table: vec![FunctionEntry {
            symbol_idx: 0,
            param_count: 0,
            local_count: 0,
            code_offset: 0,
            code_length: code_len,
            return_kind: musi_codegen::module::ReturnKind::Unknown,
        }],
        code,
        method_table: Vec::new(),
    };

    let mut vm = Vm::new(module, NativeRegistry::new(&[]));
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
            abi: Box::from(""),
            link_lib: None,
            link_name: None,
        }],
        function_table: vec![FunctionEntry {
            symbol_idx: 0,
            param_count: 0,
            local_count: 1,
            code_offset: 0,
            code_length: code_len,
            return_kind: musi_codegen::module::ReturnKind::Unknown,
        }],
        code,
        method_table: Vec::new(),
    };

    let mut vm = Vm::new(module, NativeRegistry::new(&[]));
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
                abi: Box::from(""),
                link_lib: None,
                link_name: None,
            },
            SymbolEntry {
                name: "main".into(),
                flags: SymbolFlags::new(SymbolFlags::EXPORT),
                intrinsic_id: 0xFFFF,
                abi: Box::from(""),
                link_lib: None,
                link_name: None,
            },
        ],
        function_table: vec![
            FunctionEntry {
                symbol_idx: 0,
                param_count: 0,
                local_count: 0,
                code_offset: callee_offset,
                code_length: callee_len,
                return_kind: musi_codegen::module::ReturnKind::Unknown,
            },
            FunctionEntry {
                symbol_idx: 1,
                param_count: 0,
                local_count: 0,
                code_offset: main_offset,
                code_length: main_len,
                return_kind: musi_codegen::module::ReturnKind::Unknown,
            },
        ],
        code,
        method_table: Vec::new(),
    };

    let mut vm = Vm::new(module, NativeRegistry::new(&[]));
    let result = vm.run(1).expect("vm run");
    assert_eq!(result, Value::Int(42));
}

#[test]
fn unknown_function_index_is_error() {
    let module = Module::new();
    let mut vm = Vm::new(module, NativeRegistry::new(&[]));
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
    let brfalse_offset =
        i32::try_from(exit_pos as isize - brfalse_after as isize).expect("brfalse offset fits i32");
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
            abi: Box::from(""),
            link_lib: None,
            link_name: None,
        }],
        function_table: vec![FunctionEntry {
            symbol_idx: 0,
            param_count: 0,
            local_count: 2,
            code_offset: 0,
            code_length: code_len,
            return_kind: musi_codegen::module::ReturnKind::Unknown,
        }],
        code,
        method_table: Vec::new(),
    };

    let mut vm = Vm::new(module, NativeRegistry::new(&[]));
    let result = vm.run(0).expect("while loop should terminate");
    assert_eq!(result, Value::Int(10), "y should equal 10 after loop");
}

#[test]
fn factorial_compiles_and_runs() {
    let result = run_src(
        r#"
fn factorial(n: Int): Int => if n <= 1 then 1 else n * factorial(n - 1);
factorial(10);
"#,
    );
    assert_eq!(result, Value::Unit);
}

#[test]
fn lambda_double_compiles_and_runs() {
    let result = run_src(
        r#"
const f := fn(x: Int): Int => x * 2;
f(21);
"#,
    );
    assert_eq!(result, Value::Unit);
}

#[test]
fn choice_match_compiles_and_runs() {
    let _result = run_src(
        r#"
choice Option { Some(Int) | None };
const x := .Some(42);
match x with (
    .Some(v) => v
  | .None    => 0
);
"#,
    );
}

#[test]
fn record_construction_compiles_and_runs() {
    let _result = run_src(
        r#"
record Point { x: Int, y: Int };
const p := Point.{ x := 1, y := 2 };
p.x;
"#,
    );
}

#[test]
fn new_obj_and_ld_fld() {
    let mut code = Vec::new();
    Opcode::LdImmI64(10).encode_into(&mut code);
    Opcode::LdImmI64(20).encode_into(&mut code);
    Opcode::NewObj {
        type_tag: 0,
        field_count: 2,
    }
    .encode_into(&mut code);
    Opcode::LdFld(1).encode_into(&mut code);
    Opcode::Halt.encode_into(&mut code);
    let code_len = u32::try_from(code.len()).expect("fits");

    let module = Module {
        const_pool: vec![],
        symbol_table: vec![SymbolEntry {
            name: "main".into(),
            flags: SymbolFlags::new(0),
            intrinsic_id: 0xFFFF,
            abi: Box::from(""),
            link_lib: None,
            link_name: None,
        }],
        function_table: vec![FunctionEntry {
            symbol_idx: 0,
            param_count: 0,
            local_count: 0,
            code_offset: 0,
            code_length: code_len,
            return_kind: musi_codegen::module::ReturnKind::Unknown,
        }],
        code,
        method_table: Vec::new(),
    };
    let mut vm = Vm::new(module, NativeRegistry::new(&[]));
    let result = vm.run(0).expect("vm run");
    assert_eq!(result, Value::Int(20));
}

#[test]
fn ld_tag_reads_discriminant() {
    let mut code = Vec::new();
    Opcode::LdImmI64(42).encode_into(&mut code);
    Opcode::LdImmUnit.encode_into(&mut code);
    Opcode::NewObj {
        type_tag: 0,
        field_count: 2,
    }
    .encode_into(&mut code);
    Opcode::LdTag.encode_into(&mut code);
    Opcode::Halt.encode_into(&mut code);
    let code_len = u32::try_from(code.len()).expect("fits");

    let module = Module {
        const_pool: vec![],
        symbol_table: vec![SymbolEntry {
            name: "main".into(),
            flags: SymbolFlags::new(0),
            intrinsic_id: 0xFFFF,
            abi: Box::from(""),
            link_lib: None,
            link_name: None,
        }],
        function_table: vec![FunctionEntry {
            symbol_idx: 0,
            param_count: 0,
            local_count: 0,
            code_offset: 0,
            code_length: code_len,
            return_kind: musi_codegen::module::ReturnKind::Unknown,
        }],
        code,
        method_table: Vec::new(),
    };
    let mut vm = Vm::new(module, NativeRegistry::new(&[]));
    let result = vm.run(0).expect("vm run");
    assert_eq!(result, Value::Int(42));
}

#[test]
fn ld_fn_idx_and_call_dynamic() {
    // fn 0 (double): LdLoc(0), LdImmI64(2), MulI64, Ret
    let mut fn0_code = Vec::new();
    Opcode::LdLoc(0).encode_into(&mut fn0_code);
    Opcode::LdImmI64(2).encode_into(&mut fn0_code);
    Opcode::MulI64.encode_into(&mut fn0_code);
    Opcode::Ret.encode_into(&mut fn0_code);
    let fn0_len = u32::try_from(fn0_code.len()).expect("fits");

    // fn 1 (main): LdImmI64(21), LdFnIdx(0), CallDynamic, Halt
    let mut fn1_code = Vec::new();
    Opcode::LdImmI64(21).encode_into(&mut fn1_code);
    Opcode::LdFnIdx(0).encode_into(&mut fn1_code);
    Opcode::CallDynamic.encode_into(&mut fn1_code);
    Opcode::Halt.encode_into(&mut fn1_code);
    let fn1_offset = fn0_len;
    let fn1_len = u32::try_from(fn1_code.len()).expect("fits");

    let mut code = fn0_code;
    code.extend_from_slice(&fn1_code);

    let module = Module {
        const_pool: vec![],
        symbol_table: vec![
            SymbolEntry {
                name: "double".into(),
                flags: SymbolFlags::new(0),
                intrinsic_id: 0xFFFF,
                abi: Box::from(""),
                link_lib: None,
                link_name: None,
            },
            SymbolEntry {
                name: "main".into(),
                flags: SymbolFlags::new(0),
                intrinsic_id: 0xFFFF,
                abi: Box::from(""),
                link_lib: None,
                link_name: None,
            },
        ],
        function_table: vec![
            FunctionEntry {
                symbol_idx: 0,
                param_count: 1,
                local_count: 0,
                code_offset: 0,
                code_length: fn0_len,
                return_kind: musi_codegen::module::ReturnKind::Unknown,
            },
            FunctionEntry {
                symbol_idx: 1,
                param_count: 0,
                local_count: 0,
                code_offset: fn1_offset,
                code_length: fn1_len,
                return_kind: musi_codegen::module::ReturnKind::Unknown,
            },
        ],
        code,
        method_table: Vec::new(),
    };
    let mut vm = Vm::new(module, NativeRegistry::new(&[]));
    let result = vm.run(1).expect("vm run");
    assert_eq!(result, Value::Int(42));
}

#[test]
fn dup_clones_top_of_stack() {
    let mut code = Vec::new();
    Opcode::LdImmI64(7).encode_into(&mut code);
    Opcode::Dup.encode_into(&mut code);
    Opcode::AddI64.encode_into(&mut code);
    Opcode::Halt.encode_into(&mut code);
    let code_len = u32::try_from(code.len()).expect("fits");

    let module = Module {
        const_pool: vec![],
        symbol_table: vec![SymbolEntry {
            name: "main".into(),
            flags: SymbolFlags::new(0),
            intrinsic_id: 0xFFFF,
            abi: Box::from(""),
            link_lib: None,
            link_name: None,
        }],
        function_table: vec![FunctionEntry {
            symbol_idx: 0,
            param_count: 0,
            local_count: 0,
            code_offset: 0,
            code_length: code_len,
            return_kind: musi_codegen::module::ReturnKind::Unknown,
        }],
        code,
        method_table: Vec::new(),
    };
    let mut vm = Vm::new(module, NativeRegistry::new(&[]));
    let result = vm.run(0).expect("vm run");
    assert_eq!(result, Value::Int(14));
}

#[test]
fn ufcs_single_arg() {
    let result = run_src(
        r#"
fn double(n: Int): Int => n * 2;
const x := 5.double();
x;
"#,
    );
    assert_eq!(result, Value::Unit);
}

#[test]
fn ufcs_two_arg() {
    let result = run_src(
        r#"
fn my_add(a: Int, b: Int): Int => a + b;
const r := 3.my_add(4);
r;
"#,
    );
    assert_eq!(result, Value::Unit);
}

#[test]
fn class_given_dispatch() {
    let result = run_src(
        r#"
class Eq['T] {
    fn eq(a: 'T, b: 'T): Bool;
};

given Eq[Int] {
    fn eq(a: Int, b: Int): Bool => a = b;
};

const x := 5;
const y := 5;
const same := x.eq(y);
same;
"#,
    );
    assert_eq!(result, Value::Unit);
}

#[test]
fn user_type_operator_dispatch_via_given() {
    // given Add[Vec2] lets v1.add(v2) dispatch to user-defined method without panic
    let _result = run_src(
        r#"
record Vec2 { x: Int, y: Int };

given Add[Vec2] {
    fn add(a: Vec2, b: Vec2): Vec2 => Vec2.{ x := a.x + b.x, y := a.y + b.y };
};

const v1 := Vec2.{ x := 1, y := 2 };
const v2 := Vec2.{ x := 3, y := 4 };
const v3 := v1.add(v2);
writeln(int_to_string(v3.x));
"#,
    );
}

#[test]
fn match_arm_guard_filters_correctly() {
    let _r = run_src(
        r#"
choice Num { Small(Int) | Large(Int) };
const x := .Small(3);
const y := .Large(10);
const r1 := match x with (
    .Small(v) if v > 5 => 1
  | .Small(v) => 2
  | .Large(_) => 3
);
const r2 := match y with (
    .Small(v) if v > 5 => 1
  | .Small(v) => 2
  | .Large(_) => 3
);
writeln(int_to_string(r1));
writeln(int_to_string(r2));
"#,
    );
}

#[test]
fn dot_prefix_pattern_in_match() {
    let _r = run_src(
        r#"
const x := .Some(42);
const result := match x with (
    .Some(v) => v
  | .None => 0
);
writeln(int_to_string(result));
"#,
    );
}

#[test]
fn if_case_pattern_binding() {
    let _r = run_src(
        r#"
const x := .Some(7);
if case .Some(v) := x then
    writeln(int_to_string(v));
"#,
    );
}

#[test]
fn tuple_value_constructs() {
    let _r = run_src(
        r#"
const t := (10, 20);
writeln(int_to_string(10));
"#,
    );
}

#[test]
fn tuple_in_match() {
    let _r = run_src(
        r#"
const t := (3, 4);
const result := match t with (
    (a, b) => a + b
);
writeln(int_to_string(result));
"#,
    );
}

#[test]
fn bind_tuple_destructure() {
    let _r = run_src(
        r#"
const t := (10, 20);
const (a, b) := t;
writeln(int_to_string(a + b));
"#,
    );
}

#[test]
fn bind_record_destructure() {
    let _r = run_src(
        r#"
record Point { x: Int, y: Int };
const p := Point.{ x := 3, y := 4 };
const { x, y } := p;
writeln(int_to_string(x + y));
"#,
    );
}

#[test]
fn while_with_guard_skips() {
    // guard false exits the loop early: i reaches 5 (not 10) because guard i < 5 fails
    let _r = run_src(
        r#"
var i := 0;
while i < 10 if i < 5 loop (
    i <- i + 1;
);
writeln(int_to_string(i));
"#,
    );
}

#[test]
fn for_with_guard() {
    let _r = run_src(
        r#"
var evens := 0;
for x in [1, 2, 3, 4, 5, 6] if x % 2 = 0 loop (
    evens <- evens + x;
);
writeln(int_to_string(evens));
"#,
    );
}

#[test]
fn for_tuple_destructure() {
    let _r = run_src(
        r#"
const pairs := [(1, 2), (3, 4), (5, 6)];
var s := 0;
for (a, b) in pairs loop (
    s <- s + a + b;
);
writeln(int_to_string(s));
"#,
    );
}

#[test]
fn named_record_pattern_in_match() {
    let _r = run_src(
        r#"
record Point { x: Int, y: Int };
const p := Point.{ x := 3, y := 7 };
const result := match p with (
    Point { x, y } => x + y
);
writeln(int_to_string(result));
"#,
    );
}

#[test]
fn do_while_loop() {
    let _r = run_src(
        r#"
var i := 0;
loop (i <- i + 1) while i < 5;
writeln(int_to_string(i));
"#,
    );
}

#[test]
fn cycle_guard() {
    let _r = run_src(
        r#"
var i := 0;
var s := 0;
while i < 10 loop (
    i <- i + 1;
    cycle if i % 2 = 0;
    s <- s + i;
);
writeln(int_to_string(s));
"#,
    );
}
