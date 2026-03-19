//! Integration tests: lex -> parse -> sema -> emit -> verify binary structure.

use std::str::from_utf8;

use msc_bc::Opcode;
use msc_lex::lex;
use msc_parse::parse;
use msc_sema::{SemaOptions, analyze};
use msc_shared::{DiagnosticBag, FileId, Interner};

use crate::{EmitOutput, emit};

fn compile_with(source: &str, script: bool, strict: bool) -> Result<EmitOutput, String> {
    let mut interner = Interner::new();
    let mut diags = DiagnosticBag::new();
    let file_id = FileId(0);
    let lexed = lex(source, file_id, &mut interner, &mut diags);
    let parsed = parse(&lexed.tokens, file_id, &mut diags, &mut interner);
    let sema = analyze(
        &parsed,
        &mut interner,
        file_id,
        &mut diags,
        &SemaOptions::default(),
    );
    if strict && diags.has_errors() {
        let msgs: Vec<String> = diags
            .iter()
            .map(|d| format!("{:?}: {}", d.severity, d.message))
            .collect();
        return Err(format!("errors:\n{}", msgs.join("\n")));
    }
    emit(&parsed, &sema, &mut interner, file_id, script, &[]).map_err(|e| e.to_string())
}

fn compile(source: &str) -> Result<EmitOutput, String> {
    compile_with(source, false, true)
}

fn compile_script(source: &str) -> Result<EmitOutput, String> {
    compile_with(source, true, true)
}

fn compile_lenient(source: &str) -> Result<EmitOutput, String> {
    compile_with(source, true, false)
}

/// Find the start+length of a tagged section in the binary.
/// Sections start at offset 16 (after the header) and are: 4-byte tag + 4-byte BE length + payload.
fn find_section(bytes: &[u8], tag: [u8; 4]) -> Option<(usize, usize)> {
    let mut pos = 16; // skip header
    while pos + 8 <= bytes.len() {
        let section_tag = &bytes[pos..pos + 4];
        let section_len = usize::try_from(u32::from_be_bytes([
            bytes[pos + 4],
            bytes[pos + 5],
            bytes[pos + 6],
            bytes[pos + 7],
        ]))
        .expect("section length fits in usize");
        if section_tag == tag {
            return Some((pos + 8, section_len));
        }
        pos += 8 + section_len;
    }
    None
}

/// Scan the METH section for the first occurrence of `op`.
fn find_opcode(bytes: &[u8], op: Opcode) -> Option<usize> {
    let (start, len) = find_section(bytes, *b"METH")?;
    let end = start + len;
    bytes
        .get(start..end)
        .and_then(|slice| slice.iter().position(|&b| b == op.0))
        .map(|p| p + start)
}

#[test]
fn test_emit_header_valid() {
    let source = "let main : () -> Int := () => 42;";
    let out = compile_script(source).expect("compile ok");
    assert!(
        out.bytes.len() >= 16,
        "output must have at least a 16-byte header"
    );
    assert_eq!(&out.bytes[..4], b"SEAM", "magic bytes");
    assert_eq!(out.bytes[4], 1, "major version");
    assert_eq!(out.bytes[5], 0, "minor version");
    assert_eq!(out.bytes[6], 0, "patch version");
}

#[test]
fn test_emit_sections_present() {
    let source = "let main : () -> Int := () => 42;";
    let out = compile_script(source).expect("compile ok");
    for tag in [
        *b"STRT", *b"TYPE", *b"CNST", *b"DEPS", *b"GLOB", *b"METH", *b"EFCT", *b"CLSS", *b"FRGN",
    ] {
        assert!(
            find_section(&out.bytes, tag).is_some(),
            "missing section {:?}",
            from_utf8(&tag).unwrap()
        );
    }
}

#[test]
fn test_emit_small_int_uses_ld_smi() {
    // 42 fits in i16 - the emitter should use LD_SMI for the literal.
    // Other LD_CONST emissions (e.g. function references) may still appear
    // in the same module, so we only assert LD_SMI is present.
    let source = "let main : () -> Int := () => 42;";
    let out = compile_script(source).expect("compile ok");
    assert!(
        find_opcode(&out.bytes, Opcode::LD_SMI).is_some(),
        "expected LD_SMI for small integer literal 42"
    );
}

#[test]
fn test_emit_large_int_uses_ld_const() {
    // 100000 does not fit in i16 - the emitter must fall back to LD_CONST.
    let source = "let main : () -> Int := () => 100000;";
    let out = compile_script(source).expect("compile ok");
    assert!(
        find_opcode(&out.bytes, Opcode::LD_CONST).is_some(),
        "expected LD_CONST for integer literal 100000 that exceeds i16 range"
    );
}

#[test]
fn test_emit_binop_add() {
    let source = "let add : (Int, Int) -> Int := (a, b) => a + b;";
    let out = compile_script(source).expect("compile ok");
    let found = find_opcode(&out.bytes, Opcode::ADD);
    assert!(found.is_some(), "expected ADD for integer addition");
}

#[test]
fn test_emit_piecewise() {
    let source = "let f : (Int) -> Int := (x) => (42 if x > 0 | 0 if _);";
    let out = compile_script(source).expect("compile ok");
    let found = find_opcode(&out.bytes, Opcode::BR_TRUE);
    assert!(found.is_some(), "expected conditional jump in piecewise");
}

#[test]
fn test_emit_let_binding() {
    let source = "let main : () -> Int := () => (let x := 10; x);";
    let out = compile_script(source).expect("compile ok");
    let found = find_opcode(&out.bytes, Opcode::ST_LOC);
    assert!(found.is_some(), "expected ST_LOC for let binding");
}

#[test]
fn test_emit_script_mode() {
    let source = "let go : () -> Int := () => 1;";
    let out = compile_script(source).expect("compile ok");
    // flags at byte 7; FLAG_SCRIPT = 1 << 2 = 4
    assert_eq!(out.bytes[7] & 4, 4, "script mode => FLAG_SCRIPT");
}

#[test]
fn test_emit_library_mode() {
    let source = "let helper : (Int) -> Int := (x) => x + 1;";
    let out = compile(source).expect("compile ok");
    // flags at byte 7; FLAG_LIBRARY = 1 << 1 = 2
    assert_eq!(out.bytes[7] & 2, 2, "library mode => FLAG_LIBRARY");
}

#[test]
fn test_emit_range_inc() {
    let source = "let f : (Int, Int) -> Int := (a, b) => a..b;";
    let out = compile_script(source);
    assert!(
        out.is_ok(),
        "range-inc should compile: {}",
        out.unwrap_err()
    );
    let out = out.unwrap();
    let found = find_opcode(&out.bytes, Opcode::TUP_NEW);
    assert!(found.is_some(), "expected TUP_NEW for range construction");
}

#[test]
fn test_emit_range_exc() {
    let source = "let f : (Int, Int) -> Int := (a, b) => a..<b;";
    let out = compile_script(source);
    assert!(
        out.is_ok(),
        "range-exc should compile: {}",
        out.unwrap_err()
    );
    let out = out.unwrap();
    let found = find_opcode(&out.bytes, Opcode::TUP_NEW);
    assert!(found.is_some(), "expected TUP_NEW for range construction");
}

#[test]
fn test_emit_nil_coal() {
    let source = "let f : (Int, Int) -> Int := (a, b) => a ?? b;";
    let out = compile_script(source);
    assert!(
        out.is_ok(),
        "nil-coalesce should compile: {}",
        out.unwrap_err()
    );
    let out = out.unwrap();
    let found = find_opcode(&out.bytes, Opcode::MAT_TAG);
    assert!(found.is_some(), "expected MAT_TAG for nil-coalesce");
}

#[test]
fn test_emit_cons() {
    let source = "let f : (Int, []Int) -> []Int := (x, xs) => x :: xs;";
    let out = compile_script(source);
    assert!(out.is_ok(), "cons should compile: {}", out.unwrap_err());
    let out = out.unwrap();
    let found = find_opcode(&out.bytes, Opcode::TUP_NEW);
    assert!(found.is_some(), "expected TUP_NEW for cons cell");
}

#[test]
fn test_emit_try_expr() {
    let source = "let f : (Int) -> Int := (x) => try x;";
    let out = compile_script(source);
    assert!(out.is_ok(), "try should compile: {}", out.unwrap_err());
    let out = out.unwrap();
    // After the rearchitecture, `try` wraps in a variant via REC_NEW (not OPT_SOME).
    let found = find_opcode(&out.bytes, Opcode::REC_NEW);
    assert!(
        found.is_some(),
        "expected REC_NEW for try wrapping in variant"
    );
}

#[test]
fn test_emit_err_coal() {
    let source = "let f : (Int, Int) -> Int := (a, b) => a !! b;";
    let out = compile_lenient(source);
    assert!(
        out.is_ok(),
        "err-coalesce should compile: {}",
        out.unwrap_err()
    );
    let out = out.unwrap();
    let found = find_opcode(&out.bytes, Opcode::MAT_TAG);
    assert!(found.is_some(), "expected MAT_TAG for err-coalesce");
}

#[test]
fn test_emit_defer() {
    let source = "let f : () -> Int := () => (defer 1; 42);";
    let out = compile_script(source);
    assert!(out.is_ok(), "defer should compile: {}", out.unwrap_err());
}

#[test]
fn test_emit_global_read_no_nop() {
    // Global reads should emit LD_LOC 0 + REC_GET, not NOP.
    let source = "let main : () -> Int := () => 42;";
    let out = compile_script(source).expect("compile ok");
    let (meth_start, meth_len) = find_section(&out.bytes, *b"METH").expect("METH section");
    let meth = &out.bytes[meth_start..meth_start + meth_len];
    // NOP should not appear in function code (it was the old global stub).
    assert!(
        !meth.contains(&Opcode::NOP.0),
        "no NOP opcodes should appear in METH section"
    );
}

#[test]
fn test_emit_variant_uses_tag() {
    // Variants should use REC_NEW with the correct tag, not OPT_SOME/OPT_NONE.
    let source = "let f : (Int) -> Int := (x) => try x;";
    let out = compile_script(source).expect("compile ok");
    let found_rec_new = find_opcode(&out.bytes, Opcode::REC_NEW);
    assert!(
        found_rec_new.is_some(),
        "expected REC_NEW for variant construction"
    );
    // OPT_SOME should NOT appear (we use REC_NEW with tag now).
    let found_opt_some = find_opcode(&out.bytes, Opcode::OPT_SOME);
    assert!(
        found_opt_some.is_none(),
        "OPT_SOME should not appear; variants use REC_NEW"
    );
}
