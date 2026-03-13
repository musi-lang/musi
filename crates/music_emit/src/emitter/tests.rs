//! Integration tests: lex → parse → sema → emit → verify binary structure.

use musi_bc::Opcode;
use music_lex::lex;
use music_parse::parse;
use music_sema::analyze;
use music_shared::{DiagnosticBag, FileId, Interner};

use crate::{EmitOutput, emit};

fn compile(source: &str) -> Result<EmitOutput, String> {
    let mut interner = Interner::new();
    let mut diags = DiagnosticBag::new();
    let file_id = FileId(0);
    let lexed = lex(source, file_id, &mut interner, &mut diags);
    let parsed = parse(&lexed.tokens, file_id, &mut diags, &interner);
    let sema = analyze(&parsed, &mut interner, file_id, &mut diags);
    if diags.has_errors() {
        let msgs: Vec<String> = diags
            .iter()
            .map(|d| format!("{:?}: {}", d.severity, d.message))
            .collect();
        return Err(format!("errors:\n{}", msgs.join("\n")));
    }
    emit(&parsed, &sema, &mut interner, file_id).map_err(|e| e.to_string())
}

/// Like `compile` but ignores type errors so we can test emission of
/// constructs whose types require stdlib definitions (e.g. Result).
fn compile_lenient(source: &str) -> Result<EmitOutput, String> {
    let mut interner = Interner::new();
    let mut diags = DiagnosticBag::new();
    let file_id = FileId(0);
    let lexed = lex(source, file_id, &mut interner, &mut diags);
    let parsed = parse(&lexed.tokens, file_id, &mut diags, &interner);
    let sema = analyze(&parsed, &mut interner, file_id, &mut diags);
    emit(&parsed, &sema, &mut interner, file_id).map_err(|e| e.to_string())
}

/// Scan the function pool section (past the header) for the first occurrence of `op`.
fn find_opcode(bytes: &[u8], op: Opcode) -> Option<usize> {
    assert!(bytes.len() > 35, "bytes too short for header");
    // fn_off is at header bytes 32..36
    let fn_off = u32::from_le_bytes([bytes[32], bytes[33], bytes[34], bytes[35]]);
    let start = usize::try_from(fn_off).expect("fn_off fits usize");
    bytes
        .get(start..)
        .and_then(|slice| slice.iter().position(|&b| b == op.0))
        .map(|pos| pos + start)
}

#[test]
fn test_emit_header_valid() {
    let source = "#[entrypoint]\nlet main : () -> Int := () => 42;";
    let out = compile(source).expect("compile ok");
    assert!(out.bytes.len() >= 40, "output must have at least a header");
    assert_eq!(&out.bytes[..4], b"MUSI", "magic bytes");
}

#[test]
fn test_emit_simple_return_int() {
    let source = "#[entrypoint]\nlet main : () -> Int := () => 42;";
    let out = compile(source).expect("compile ok");
    let found = find_opcode(&out.bytes, Opcode::LD_CST)
        .or_else(|| find_opcode(&out.bytes, Opcode::LD_CST_W));
    assert!(found.is_some(), "expected LD_CST for integer literal 42");
}

#[test]
fn test_emit_binop_add() {
    let source = "#[entrypoint]\nlet add : (Int, Int) -> Int := (a, b) => a + b;";
    let out = compile(source).expect("compile ok");
    let found = find_opcode(&out.bytes, Opcode::I_ADD);
    assert!(found.is_some(), "expected I_ADD for integer addition");
}

#[test]
fn test_emit_piecewise() {
    let source = "#[entrypoint]\nlet f : (Int) -> Int := (x) => (42 if x > 0 | 0 if _);";
    let out = compile(source).expect("compile ok");
    let found = find_opcode(&out.bytes, Opcode::JMP_T_W);
    assert!(found.is_some(), "expected conditional jump in piecewise");
}

#[test]
fn test_emit_let_binding() {
    let source = "#[entrypoint]\nlet main : () -> Int := () => (let x := 10; x);";
    let out = compile(source).expect("compile ok");
    let found = find_opcode(&out.bytes, Opcode::ST_LOC)
        .or_else(|| find_opcode(&out.bytes, Opcode::ST_LOC_W));
    assert!(found.is_some(), "expected ST_LOC for let binding");
}

#[test]
fn test_emit_entry_fn_id_set() {
    let source = "#[entrypoint]\nlet go : () -> Int := () => 1;";
    let out = compile(source).expect("compile ok");
    // Entry fn id at header offset 12..16; 0xFFFFFFFF means no entry.
    let entry_id = u32::from_le_bytes([out.bytes[12], out.bytes[13], out.bytes[14], out.bytes[15]]);
    assert_ne!(entry_id, 0xFFFF_FFFF, "entry fn id should be set");
}

#[test]
fn test_emit_no_entrypoint_is_library() {
    let source = "let helper : (Int) -> Int := (x) => x + 1;";
    let out = compile(source).expect("compile ok");
    // Flags at offset 8..12; FLAG_IS_LIB = 1 << 2 = 4.
    let flags = u32::from_le_bytes([out.bytes[8], out.bytes[9], out.bytes[10], out.bytes[11]]);
    assert_eq!(flags & 4, 4, "no entrypoint => FLAG_IS_LIB");
}

#[test]
fn test_emit_range_inc() {
    let source = "#[entrypoint]\nlet f : (Int, Int) -> Int := (a, b) => a..b;";
    let out = compile(source);
    assert!(out.is_ok(), "range-inc should compile: {}", out.unwrap_err());
    let out = out.unwrap();
    let found = find_opcode(&out.bytes, Opcode::MK_PRD);
    assert!(found.is_some(), "expected MK_PRD for range construction");
}

#[test]
fn test_emit_range_exc() {
    let source = "#[entrypoint]\nlet f : (Int, Int) -> Int := (a, b) => a..<b;";
    let out = compile(source);
    assert!(out.is_ok(), "range-exc should compile: {}", out.unwrap_err());
    let out = out.unwrap();
    let found = find_opcode(&out.bytes, Opcode::MK_PRD);
    assert!(found.is_some(), "expected MK_PRD for range construction");
}

#[test]
fn test_emit_nil_coal() {
    let source = "#[entrypoint]\nlet f : (Int, Int) -> Int := (a, b) => a ?? b;";
    let out = compile(source);
    assert!(out.is_ok(), "nil-coalesce should compile: {}", out.unwrap_err());
    let out = out.unwrap();
    let found = find_opcode(&out.bytes, Opcode::CMP_TAG)
        .or_else(|| find_opcode(&out.bytes, Opcode::CMP_TAG_W));
    assert!(found.is_some(), "expected CMP_TAG for nil-coalesce");
}

#[test]
fn test_emit_cons() {
    let source = "#[entrypoint]\nlet f : (Int, [] Int) -> [] Int := (x, xs) => x :: xs;";
    let out = compile(source);
    assert!(out.is_ok(), "cons should compile: {}", out.unwrap_err());
    let out = out.unwrap();
    let found = find_opcode(&out.bytes, Opcode::MK_PRD);
    assert!(found.is_some(), "expected MK_PRD for cons cell");
}

#[test]
fn test_emit_try_expr() {
    let source = "#[entrypoint]\nlet f : (Int) -> Int := (x) => try x;";
    let out = compile(source);
    assert!(out.is_ok(), "try should compile: {}", out.unwrap_err());
    let out = out.unwrap();
    let found = find_opcode(&out.bytes, Opcode::MK_VAR)
        .or_else(|| find_opcode(&out.bytes, Opcode::MK_VAR_W));
    assert!(found.is_some(), "expected MK_VAR for try wrapping in Some");
}

#[test]
fn test_emit_err_coal() {
    // !! requires Result-typed left operand (unavailable without stdlib),
    // so use compile_lenient to skip type errors and verify emission.
    let source = "#[entrypoint]\nlet f : (Int, Int) -> Int := (a, b) => a !! b;";
    let out = compile_lenient(source);
    assert!(out.is_ok(), "err-coalesce should compile: {}", out.unwrap_err());
    let out = out.unwrap();
    let found = find_opcode(&out.bytes, Opcode::CMP_TAG)
        .or_else(|| find_opcode(&out.bytes, Opcode::CMP_TAG_W));
    assert!(found.is_some(), "expected CMP_TAG for err-coalesce");
}

#[test]
fn test_emit_defer() {
    let source = "#[entrypoint]\nlet f : () -> Int := () => (defer 1; 42);";
    let out = compile(source);
    assert!(out.is_ok(), "defer should compile: {}", out.unwrap_err());
}
