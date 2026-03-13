//! Expression parsing tests.

use music_ast::expr::{BinOp, Expr, UnaryOp};
use music_ast::lit::Lit;
use music_lex::lex as lex_source;
use music_lex::token::Token;
use music_shared::{DiagnosticBag, FileId, Interner, SourceDb};

use crate::parse;

fn lex(src: &str) -> (Vec<Token>, Interner, FileId) {
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let file_id = source_db.add("test.mu", src);
    let mut lex_diags = DiagnosticBag::new();
    let lexed = lex_source(src, file_id, &mut interner, &mut lex_diags);
    (lexed.tokens, interner, file_id)
}

fn parse_single_expr(src: &str) -> (Expr, DiagnosticBag) {
    let full = format!("{src};");
    let (tokens, interner, file_id) = lex(&full);
    let mut diags = DiagnosticBag::new();
    let module = parse(&tokens, file_id, &mut diags, &interner);
    assert_eq!(module.stmts.len(), 1, "expected exactly one statement");
    let expr = module.arenas.exprs[module.stmts[0].expr].clone();
    (expr, diags)
}

#[test]
fn test_parse_integer_literal() {
    let (expr, diags) = parse_single_expr("42");
    assert!(!diags.has_errors());
    assert!(matches!(
        expr,
        Expr::Lit {
            lit: Lit::Int { value: 42, .. },
            ..
        }
    ));
}

#[test]
fn test_parse_float_literal() {
    let (expr, diags) = parse_single_expr("3.14");
    assert!(!diags.has_errors());
    assert!(matches!(
        expr,
        Expr::Lit {
            lit: Lit::Float { .. },
            ..
        }
    ));
}

#[test]
fn test_parse_string_literal() {
    let (expr, diags) = parse_single_expr("\"hello\"");
    assert!(!diags.has_errors());
    assert!(matches!(
        expr,
        Expr::Lit {
            lit: Lit::Str { .. },
            ..
        }
    ));
}

#[test]
fn test_parse_name() {
    let (expr, diags) = parse_single_expr("foo");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Name { .. }));
}

#[test]
fn test_parse_addition() {
    let (expr, diags) = parse_single_expr("1 + 2");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::BinOp { op: BinOp::Add, .. }));
}

#[test]
fn test_parse_precedence_mul_over_add() {
    // 1 + 2 * 3 should parse as 1 + (2 * 3)
    let full = "1 + 2 * 3;";
    let (tokens, interner, file_id) = lex(full);
    let mut diags = DiagnosticBag::new();
    let module = parse(&tokens, file_id, &mut diags, &interner);
    assert!(!diags.has_errors());
    let top = &module.arenas.exprs[module.stmts[0].expr];
    assert!(
        matches!(top, Expr::BinOp { op: BinOp::Add, .. }),
        "expected BinOp::Add at top, got {top:?}"
    );
    let Expr::BinOp { right, .. } = top else {
        return;
    };
    let rhs = &module.arenas.exprs[*right];
    assert!(matches!(rhs, Expr::BinOp { op: BinOp::Mul, .. }));
}

#[test]
fn test_parse_left_associativity() {
    // 1 - 2 - 3 should parse as (1 - 2) - 3
    let full = "1 - 2 - 3;";
    let (tokens, interner, file_id) = lex(full);
    let mut diags = DiagnosticBag::new();
    let module = parse(&tokens, file_id, &mut diags, &interner);
    assert!(!diags.has_errors());
    let top = &module.arenas.exprs[module.stmts[0].expr];
    assert!(
        matches!(top, Expr::BinOp { op: BinOp::Sub, .. }),
        "expected BinOp::Sub at top"
    );
    let Expr::BinOp { left, .. } = top else {
        return;
    };
    let lhs = &module.arenas.exprs[*left];
    assert!(matches!(lhs, Expr::BinOp { op: BinOp::Sub, .. }));
}

#[test]
fn test_parse_cons_right_associativity() {
    // 1 :: 2 :: 3 should parse as 1 :: (2 :: 3)
    let full = "1 :: 2 :: 3;";
    let (tokens, interner, file_id) = lex(full);
    let mut diags = DiagnosticBag::new();
    let module = parse(&tokens, file_id, &mut diags, &interner);
    assert!(!diags.has_errors());
    let top = &module.arenas.exprs[module.stmts[0].expr];
    assert!(
        matches!(
            top,
            Expr::BinOp {
                op: BinOp::Cons,
                ..
            }
        ),
        "expected BinOp::Cons at top"
    );
    let Expr::BinOp { right, .. } = top else {
        return;
    };
    let rhs = &module.arenas.exprs[*right];
    assert!(matches!(
        rhs,
        Expr::BinOp {
            op: BinOp::Cons,
            ..
        }
    ));
}

#[test]
fn test_parse_negation() {
    let (expr, diags) = parse_single_expr("-42");
    assert!(!diags.has_errors());
    assert!(matches!(
        expr,
        Expr::UnaryOp {
            op: UnaryOp::Neg,
            ..
        }
    ));
}

#[test]
fn test_parse_not() {
    let (expr, diags) = parse_single_expr("not x");
    assert!(!diags.has_errors());
    assert!(matches!(
        expr,
        Expr::UnaryOp {
            op: UnaryOp::Not,
            ..
        }
    ));
}

#[test]
fn test_parse_paren_expr() {
    let (expr, diags) = parse_single_expr("(42)");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Paren { .. }));
}

#[test]
fn test_parse_unit_literal() {
    let (expr, diags) = parse_single_expr("()");
    assert!(!diags.has_errors());
    assert!(matches!(
        expr,
        Expr::Lit {
            lit: Lit::Unit { .. },
            ..
        }
    ));
}

#[test]
fn test_parse_tuple() {
    let (expr, diags) = parse_single_expr("(1, 2, 3)");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Tuple { .. }), "expected Tuple");
    let Expr::Tuple { elems, .. } = expr else {
        return;
    };
    assert_eq!(elems.len(), 3);
}

#[test]
fn test_parse_block() {
    let (expr, diags) = parse_single_expr("(1; 2; 3)");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Block { .. }), "expected Block");
    let Expr::Block { stmts, tail, .. } = expr else {
        return;
    };
    assert_eq!(stmts.len(), 2);
    assert!(tail.is_some());
}

#[test]
fn test_parse_call() {
    let (expr, diags) = parse_single_expr("f(1, 2)");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Call { .. }));
}

#[test]
fn test_parse_field_access() {
    let (expr, diags) = parse_single_expr("x.y");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Field { safe: false, .. }));
}

#[test]
fn test_parse_safe_navigation() {
    let (expr, diags) = parse_single_expr("x?.y");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Field { safe: true, .. }));
}

#[test]
fn test_parse_array_literal() {
    let (expr, diags) = parse_single_expr("[1, 2, 3]");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Array { .. }), "expected Array");
    let Expr::Array { elems, .. } = expr else {
        return;
    };
    assert_eq!(elems.len(), 3);
}

#[test]
fn test_parse_let_binding() {
    let (expr, diags) = parse_single_expr("let x := 42");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Let { .. }));
}

#[test]
fn test_parse_return_with_value() {
    let (expr, diags) = parse_single_expr("return 42");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Return { .. }), "expected Return");
    let Expr::Return { value, .. } = expr else {
        return;
    };
    assert!(value.is_some());
}

#[test]
fn test_parse_return_without_value() {
    let (expr, diags) = parse_single_expr("return");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Return { .. }), "expected Return");
    let Expr::Return { value, .. } = expr else {
        return;
    };
    assert!(value.is_none());
}

#[test]
fn test_parse_variant_no_args() {
    let (expr, diags) = parse_single_expr(".None");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Variant { .. }), "expected Variant");
    let Expr::Variant { args, .. } = expr else {
        return;
    };
    assert!(args.is_empty());
}

#[test]
fn test_parse_variant_with_args() {
    let (expr, diags) = parse_single_expr(".Some(42)");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Variant { .. }), "expected Variant");
    let Expr::Variant { args, .. } = expr else {
        return;
    };
    assert_eq!(args.len(), 1);
}

#[test]
fn test_parse_match_expr() {
    let (expr, diags) = parse_single_expr("match x (y => 1 | z => 2)");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Match { .. }), "expected Match");
    let Expr::Match { arms, .. } = expr else {
        return;
    };
    assert_eq!(arms.len(), 2);
}

#[test]
fn test_parse_anon_record() {
    let (expr, diags) = parse_single_expr(".{ x := 1, y := 2 }");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Record { .. }), "expected Record");
    let Expr::Record {
        ty_name, fields, ..
    } = expr
    else {
        return;
    };
    assert!(ty_name.is_none());
    assert_eq!(fields.len(), 2);
}

#[test]
fn test_parse_import() {
    let (expr, diags) = parse_single_expr("import \"std/io\"");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Import { .. }));
}

#[test]
fn test_parse_defer() {
    let (expr, diags) = parse_single_expr("defer cleanup()");
    assert!(!diags.has_errors());
    assert!(matches!(
        expr,
        Expr::UnaryOp {
            op: UnaryOp::Defer,
            ..
        }
    ));
}

#[test]
fn test_parse_await_is_identifier() {
    let (expr, diags) = parse_single_expr("await");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Name { .. }));
}
