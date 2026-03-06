use musi_lex::lex;
use musi_shared::{DiagnosticBag, FileId, Interner};

use super::*;
use crate::ast::{BinOp, Expr};

fn lex_and_parse(src: &str) -> (ParsedModule, DiagnosticBag) {
    let mut interner = Interner::new();
    let mut lex_diags = DiagnosticBag::new();
    let file_id = FileId(0);
    let lexed = lex(src, file_id, &mut interner, &mut lex_diags);
    let mut parse_diags = DiagnosticBag::new();
    let module = parse(&lexed.tokens, file_id, &mut parse_diags, &interner);
    // Merge lex errors into parse diags for uniform checking
    for d in lex_diags.iter() {
        parse_diags.push(d.clone());
    }
    (module, parse_diags)
}

fn parse_ok(src: &str) -> ParsedModule {
    let (module, diags) = lex_and_parse(src);
    assert!(
        !diags.has_errors(),
        "expected no errors for `{src}`, got: {:?}",
        diags.iter().collect::<Vec<_>>()
    );
    module
}

#[test]
fn parse_hello_world() {
    let module = parse_ok("writeln(\"Hello, world!\");");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(
            expr,
            Expr::Postfix {
                op: PostfixOp::Call { .. },
                ..
            }
        ),
        "expected a call expression, got: {expr:?}"
    );
}

// 2. fn add(a, b) (a + b) -- FnDef with 2 params and body
#[test]
fn parse_fn_def() {
    let module = parse_ok("fn add(a, b) (a + b);");
    assert_eq!(module.items.len(), 1);
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(matches!(expr, Expr::FnDef { params, body: Some(_), .. } if params.len() == 2));
}

// 3. const x := 42 -- Bind
#[test]
fn parse_const_bind() {
    let module = parse_ok("const x := 42;");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(
            expr,
            Expr::Bind {
                kind: BindKind::Const,
                init: Some(_),
                ..
            }
        ),
        "expected Bind Const, got: {expr:?}"
    );
}

// 4. if true then (1) else (2) -- If with else
#[test]
fn parse_if_then_else() {
    let module = parse_ok("if true then (1) else (2);");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(
            expr,
            Expr::If {
                else_body: Some(_),
                ..
            }
        ),
        "expected If with else, got: {expr:?}"
    );
}

// 5. match with 2 arms
#[test]
fn parse_match_expr() {
    let module = parse_ok("match x with case 0 => \"zero\" case _ => \"other\";");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(matches!(expr, Expr::Match { arms, .. } if arms.len() == 2));
}

// 6. choice Option { | Some(Int) | None }
#[test]
fn parse_choice_def() {
    let module = parse_ok("choice Option { | Some(Int) | None };");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(matches!(expr, Expr::Choice { variants, .. } if variants.len() == 2));
}

// 7. Broken input -- has_errors
#[test]
fn parse_broken_input_has_errors() {
    let (_module, diags) = lex_and_parse("fn foo(");
    assert!(diags.has_errors());
}

// 8. Binary operators produce correct BinOp variants
#[test]
fn parse_binary_ops() {
    let cases = [
        ("a + b", BinOp::Add),
        ("a - b", BinOp::Sub),
        ("a * b", BinOp::Mul),
        ("a / b", BinOp::Div),
        ("a % b", BinOp::Rem),
        ("a = b", BinOp::Eq),
        ("a /= b", BinOp::NotEq),
        ("a and b", BinOp::And),
        ("a or b", BinOp::Or),
    ];
    for (src, expected_op) in cases {
        let module = parse_ok(&format!("{src};"));
        let expr = module.ctx.exprs.get(module.items[0]);
        assert!(
            matches!(expr, Expr::Binary { op, .. } if *op == expected_op),
            "for `{src}`: expected {expected_op:?}, got: {expr:?}"
        );
    }
}

// 9. Pratt precedence: 1 + 2 * 3 -- Binary(Add, 1, Binary(Mul, 2, 3))
#[test]
fn pratt_precedence() {
    let module = parse_ok("1 + 2 * 3;");
    let top = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(top, Expr::Binary { op: BinOp::Add, .. }),
        "expected Add at top"
    );
    if let Expr::Binary { rhs, .. } = top {
        let rhs_expr = module.ctx.exprs.get(*rhs);
        assert!(
            matches!(rhs_expr, Expr::Binary { op: BinOp::Mul, .. }),
            "expected Mul on rhs, got: {rhs_expr:?}"
        );
    }
}

// 10. Lambda expression
#[test]
fn parse_lambda() {
    let module = parse_ok("fn (x) => x + 1;");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::Lambda { .. }),
        "expected Lambda, got: {expr:?}"
    );
}

// 11. Record definition
#[test]
fn parse_record_def() {
    let module = parse_ok("record Point { x: Int, y: Int };");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(matches!(expr, Expr::Record { fields, .. } if fields.len() == 2));
}

// 12. Array literal
#[test]
fn parse_array_literal() {
    let module = parse_ok("[1, 2, 3];");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(matches!(expr, Expr::Array { items, .. } if items.len() == 3));
}

// 13. Return expression
#[test]
fn parse_return_expr() {
    let module = parse_ok("return 42;");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::Return { value: Some(_), .. }),
        "expected Return with value, got: {expr:?}"
    );
}

// 14. Unit expression
#[test]
fn parse_unit() {
    let module = parse_ok("();");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::Unit { .. }),
        "expected Unit, got: {expr:?}"
    );
}

// 15. Tuple expression
#[test]
fn parse_tuple() {
    let module = parse_ok("(1, 2, 3);");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(matches!(expr, Expr::Tuple { elements, .. } if elements.len() == 3));
}

// 16. Block expression
#[test]
fn parse_block_expr() {
    let module = parse_ok("(const x := 1; x);");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::Block { stmts, tail: Some(_), .. } if stmts.len() == 1),
        "expected Block with 1 stmt and tail, got: {expr:?}"
    );
}

// 17. Prefix operators
#[test]
fn parse_prefix_neg() {
    let module = parse_ok("-42;");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(
            expr,
            Expr::Prefix {
                op: PrefixOp::Neg,
                ..
            }
        ),
        "expected Prefix Neg, got: {expr:?}"
    );
}

// 18. Assign expression
#[test]
fn parse_assign() {
    let module = parse_ok("x <- 42;");
    let expr = module.ctx.exprs.get(module.items[0]);
    assert!(
        matches!(expr, Expr::Assign { .. }),
        "expected Assign, got: {expr:?}"
    );
}
