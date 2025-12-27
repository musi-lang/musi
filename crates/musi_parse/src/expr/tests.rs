use crate::test_utils::TestContext;
use musi_ast::{CondKind, ExprKind, LitKind};
use musi_lex::token::TokenKind;

#[test]
fn test_expr_lit_int() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("42");
    assert!(matches!(ctx.expr(id).kind, ExprKind::Lit(LitKind::Int(_))));
}

#[test]
fn test_expr_lit_real() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("3.14");
    assert!(matches!(ctx.expr(id).kind, ExprKind::Lit(LitKind::Real(_))));
}

#[test]
fn test_expr_lit_string() {
    let mut ctx = TestContext::new();
    let str_id = ctx.intern("hello");
    let id = ctx.parse_expr(r#""hello""#);
    assert!(matches!(ctx.expr(id).kind, ExprKind::Lit(LitKind::String(s)) if s == str_id));
}

#[test]
fn test_expr_lit_rune() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("'x'");
    assert!(matches!(
        ctx.expr(id).kind,
        ExprKind::Lit(LitKind::Rune('x'))
    ));
}

#[test]
fn test_expr_lit_bool() {
    let mut ctx = TestContext::new();
    let id_true = ctx.parse_expr("true");
    let id_false = ctx.parse_expr("false");
    assert!(matches!(
        ctx.expr(id_true).kind,
        ExprKind::Lit(LitKind::Bool(true))
    ));
    assert!(matches!(
        ctx.expr(id_false).kind,
        ExprKind::Lit(LitKind::Bool(false))
    ));
}

#[test]
fn test_expr_ident() {
    let mut ctx = TestContext::new();
    let foo = ctx.intern("foo");
    let id = ctx.parse_expr("foo");
    assert!(matches!(ctx.expr(id).kind, ExprKind::Ident(i) if i == foo));
}

#[test]
fn test_expr_tuple_empty() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("()");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Tuple(elems) if elems.is_empty()));
}

#[test]
fn test_expr_tuple_single() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("(1,)");
    if let ExprKind::Tuple(elems) = &ctx.expr(id).kind {
        assert_eq!(elems.len(), 1);
    } else {
        panic!("expected tuple literal expression");
    }
}

#[test]
fn test_expr_tuple_multiple() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("(1, 2, 3)");
    if let ExprKind::Tuple(elems) = &ctx.expr(id).kind {
        assert_eq!(elems.len(), 3);
    } else {
        panic!("expected tuple literal expression");
    }
}

#[test]
fn test_expr_grouped() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("(42)");
    assert!(matches!(ctx.expr(id).kind, ExprKind::Lit(LitKind::Int(_))));
}

#[test]
fn test_expr_array_empty() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("[]");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Array(elems) if elems.is_empty()));
}

#[test]
fn test_expr_array_multiple() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("[1, 2, 3]");
    if let ExprKind::Array(elems) = &ctx.expr(id).kind {
        assert_eq!(elems.len(), 3);
    } else {
        panic!("expected array literal expression");
    }
}

#[test]
fn test_expr_record_anon() {
    let mut ctx = TestContext::new();
    let x = ctx.intern("x");
    let id = ctx.parse_expr(".{x := 1}");
    if let ExprKind::Record { base, fields } = &ctx.expr(id).kind {
        assert!(base.is_none());
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].name, x);
    } else {
        panic!("expected `record` expression without name");
    }
}

#[test]
fn test_expr_record_typed() {
    let mut ctx = TestContext::new();
    let point = ctx.intern("Point");
    let x = ctx.intern("x");
    let id = ctx.parse_expr("Point.{x := 1}");
    if let ExprKind::Record {
        base: Some(base_id),
        fields,
    } = &ctx.expr(id).kind
    {
        assert!(matches!(ctx.expr(*base_id).kind, ExprKind::Ident(i) if i == point));
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].name, x);
    } else {
        panic!("expected `record` expression");
    }
}

#[test]
fn test_expr_record_update() {
    let mut ctx = TestContext::new();
    let p = ctx.intern("p");
    let x = ctx.intern("x");
    let id = ctx.parse_expr(".{p with, x := 1}");
    if let ExprKind::Record {
        base: Some(base_id),
        fields,
    } = &ctx.expr(id).kind
    {
        assert!(matches!(ctx.expr(*base_id).kind, ExprKind::Ident(i) if i == p));
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].name, x);
    } else {
        panic!("expected record update expression");
    }
}

#[test]
fn test_expr_binary_add() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("1 + 2");
    if let ExprKind::Binary { op, .. } = &ctx.expr(id).kind {
        assert_eq!(*op, TokenKind::Plus);
    } else {
        panic!("expected binary `+` expression");
    }
}

#[test]
/// Should parse as `1 + (2 * 3)`.
fn test_expr_binary_precedence() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("1 + 2 * 3");
    if let ExprKind::Binary { op, rhs, .. } = &ctx.expr(id).kind {
        assert_eq!(*op, TokenKind::Plus);
        assert!(
            matches!(&ctx.expr(*rhs).kind, ExprKind::Binary { op, .. } if *op == TokenKind::Star)
        );
    } else {
        panic!("expected binary expression");
    }
}

#[test]
/// Should parse as `(1 + 2) + 3` (left-associative).
fn test_expr_binary_left_assoc() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("1 + 2 + 3");
    if let ExprKind::Binary { op, lhs, .. } = &ctx.expr(id).kind {
        assert_eq!(*op, TokenKind::Plus);
        assert!(
            matches!(&ctx.expr(*lhs).kind, ExprKind::Binary { op, .. } if *op == TokenKind::Plus)
        );
    } else {
        panic!("expected binary expression");
    }
}

#[test]
/// Should parse as `1 ** (2 ** 3)` (right-associative).
fn test_expr_binary_right_assoc() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("1 ** 2 ** 3");
    if let ExprKind::Binary { op, rhs, .. } = &ctx.expr(id).kind {
        assert_eq!(*op, TokenKind::StarStar);
        assert!(
            matches!(&ctx.expr(*rhs).kind, ExprKind::Binary { op, .. } if *op == TokenKind::StarStar)
        );
    } else {
        panic!("expected binary expression");
    }
}

#[test]
fn test_expr_unary_neg() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("-42");
    if let ExprKind::Unary { op, operand } = &ctx.expr(id).kind {
        assert_eq!(*op, TokenKind::Minus);
        assert!(matches!(
            ctx.expr(*operand).kind,
            ExprKind::Lit(LitKind::Int(_))
        ));
    } else {
        panic!("expected unary expression");
    }
}

#[test]
fn test_expr_unary_not() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("not true");
    if let ExprKind::Unary { op, operand } = &ctx.expr(id).kind {
        assert_eq!(*op, TokenKind::KwNot);
        assert!(matches!(
            ctx.expr(*operand).kind,
            ExprKind::Lit(LitKind::Bool(true))
        ));
    } else {
        panic!("expected unary expression");
    }
}

#[test]
fn test_expr_call_no_args() {
    let mut ctx = TestContext::new();
    let foo = ctx.intern("foo");
    let id = ctx.parse_expr("foo()");
    if let ExprKind::Call { callee, args } = &ctx.expr(id).kind {
        assert!(matches!(ctx.expr(*callee).kind, ExprKind::Ident(i) if i == foo));
        assert!(args.is_empty());
    } else {
        panic!("expected call expression");
    }
}

#[test]
fn test_expr_call_with_args() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("foo(1, 2)");
    if let ExprKind::Call { args, .. } = &ctx.expr(id).kind {
        assert_eq!(args.len(), 2);
    } else {
        panic!("expected call expression");
    }
}

#[test]
fn test_expr_index() {
    let mut ctx = TestContext::new();
    let arr = ctx.intern("arr");
    let id = ctx.parse_expr("arr.[0]");
    if let ExprKind::Index { base, index } = &ctx.expr(id).kind {
        assert!(matches!(ctx.expr(*base).kind, ExprKind::Ident(i) if i == arr));
        assert!(matches!(
            ctx.expr(*index).kind,
            ExprKind::Lit(LitKind::Int(_))
        ));
    } else {
        panic!("expected index expression");
    }
}

#[test]
fn test_expr_field() {
    let mut ctx = TestContext::new();
    let point = ctx.intern("point");
    let x = ctx.intern("x");
    let id = ctx.parse_expr("point.x");
    if let ExprKind::Field { base, field } = &ctx.expr(id).kind {
        assert!(matches!(ctx.expr(*base).kind, ExprKind::Ident(i) if i == point));
        assert_eq!(*field, x);
    } else {
        panic!("expected field expression");
    }
}

#[test]
fn test_expr_deref() {
    let mut ctx = TestContext::new();
    let ptr = ctx.intern("ptr");
    let id = ctx.parse_expr("ptr.^");
    if let ExprKind::Deref(base) = &ctx.expr(id).kind {
        assert!(matches!(ctx.expr(*base).kind, ExprKind::Ident(i) if i == ptr));
    } else {
        panic!("expected deref expression");
    }
}

#[test]
fn test_expr_block_empty() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("{}");
    if let ExprKind::Block { stmts, expr } = &ctx.expr(id).kind {
        assert!(stmts.is_empty());
        assert!(expr.is_none());
    } else {
        panic!("expected block expression");
    }
}

#[test]
fn test_expr_block_with_expr() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("{ 42 }");
    if let ExprKind::Block { stmts, expr } = &ctx.expr(id).kind {
        assert!(stmts.is_empty());
        assert!(expr.is_some());
    } else {
        panic!("expected block expression");
    }
}

#[test]
fn test_expr_block_with_stmts() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("{ 1; 2; 3 }");
    if let ExprKind::Block { stmts, expr } = &ctx.expr(id).kind {
        assert_eq!(stmts.len(), 2);
        assert!(expr.is_some());
    } else {
        panic!("expected block expression");
    }
}

#[test]
fn test_expr_if_simple() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("if true { 1 }");
    if let ExprKind::If {
        cond,
        then_br,
        else_br,
    } = &ctx.expr(id).kind
    {
        let cond_node = ctx.arena.conds.get(*cond);
        assert!(
            matches!(&cond_node.kind, CondKind::Expr(e) if matches!(ctx.expr(*e).kind, ExprKind::Lit(LitKind::Bool(true))))
        );
        assert!(matches!(&ctx.expr(*then_br).kind, ExprKind::Block { .. }));
        assert!(else_br.is_none());
    } else {
        panic!("expected if expression");
    }
}

#[test]
fn test_expr_if_else() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("if true { 1 } else { 2 }");
    if let ExprKind::If { else_br, .. } = &ctx.expr(id).kind {
        assert!(else_br.is_some());
    } else {
        panic!("expected if-else expression");
    }
}

#[test]
fn test_expr_if_else_if() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("if true { 1 } else if false { 2 } else { 3 }");
    if let ExprKind::If {
        else_br: Some(else_id),
        ..
    } = &ctx.expr(id).kind
    {
        assert!(matches!(&ctx.expr(*else_id).kind, ExprKind::If { .. }));
    } else {
        panic!("expected if-else-if expression");
    }
}

#[test]
fn test_expr_if_case() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("if case x := foo { x }");
    if let ExprKind::If { cond, .. } = &ctx.expr(id).kind {
        let cond_node = ctx.arena.conds.get(*cond);
        assert!(matches!(&cond_node.kind, CondKind::Case { .. }));
    } else {
        panic!("expected if-case expression");
    }
}

#[test]
fn test_expr_while_simple() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("while true { 1 }");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::While { .. }));
}

#[test]
fn test_expr_for() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("for x in items { x }");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::For { .. }));
}

#[test]
fn test_expr_match_simple() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("match x { case 1 => 2 }");
    if let ExprKind::Match { cases, .. } = &ctx.expr(id).kind {
        assert_eq!(cases.len(), 1);
    } else {
        panic!("expected match expression");
    }
}

#[test]
fn test_expr_match_multiple_cases() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("match x { case 1 => 2, case 3 => 4 }");
    if let ExprKind::Match { cases, .. } = &ctx.expr(id).kind {
        assert_eq!(cases.len(), 2);
    } else {
        panic!("expected match expression");
    }
}

#[test]
fn test_expr_match_with_guard() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("match x { case y if y > 0 => y }");
    if let ExprKind::Match { cases, .. } = &ctx.expr(id).kind {
        assert!(cases[0].guard.is_some());
    } else {
        panic!("expected match expression");
    }
}

#[test]
fn test_expr_return_empty() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("return");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Return(None)));
}

#[test]
fn test_expr_return_value() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("return 42");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Return(Some(_))));
}

#[test]
fn test_expr_defer() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("defer foo()");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Defer(_)));
}

#[test]
fn test_expr_break_empty() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("break");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Break(None)));
}

#[test]
fn test_expr_break_value() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("break 42");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Break(Some(_))));
}

#[test]
fn test_expr_cycle() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("cycle");
    assert!(matches!(ctx.expr(id).kind, ExprKind::Cycle));
}

#[test]
fn test_expr_unsafe() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("unsafe { 42 }");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Unsafe(_)));
}

#[test]
fn test_expr_import() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr(r#"import "foo""#);
    assert!(matches!(ctx.expr(id).kind, ExprKind::Import(_)));
}

#[test]
fn test_expr_record_def() {
    let mut ctx = TestContext::new();
    let point = ctx.intern("Point");
    let id = ctx.parse_expr("record Point { x: Int; y: Int }");
    if let ExprKind::RecordDef { name, fields, .. } = &ctx.expr(id).kind {
        assert_eq!(*name, Some(point));
        assert_eq!(fields.len(), 2);
    } else {
        panic!("expected record definition");
    }
}

#[test]
fn test_expr_choice_def() {
    let mut ctx = TestContext::new();
    let option = ctx.intern("Option");
    let id = ctx.parse_expr("choice Option { case Some(Int), case None }");
    if let ExprKind::ChoiceDef { name, cases, .. } = &ctx.expr(id).kind {
        assert_eq!(*name, Some(option));
        assert_eq!(cases.len(), 2);
    } else {
        panic!("expected choice definition");
    }
}

#[test]
fn test_expr_alias() {
    let mut ctx = TestContext::new();
    let my_int = ctx.intern("MyInt");
    let id = ctx.parse_expr("alias MyInt := Int");
    if let ExprKind::Alias { name, .. } = &ctx.expr(id).kind {
        assert_eq!(*name, my_int);
    } else {
        panic!("expected alias definition");
    }
}

#[test]
fn test_expr_fn() {
    let mut ctx = TestContext::new();
    let foo = ctx.intern("foo");
    let id = ctx.parse_expr("fn foo(x: Int): Int { x }");
    if let ExprKind::Fn { sig, .. } = &ctx.expr(id).kind {
        assert_eq!(sig.name, Some(foo));
        assert_eq!(sig.params.len(), 1);
    } else {
        panic!("expected function definition");
    }
}

#[test]
fn test_expr_fn_arrow() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("fn(x: Int): Int => x + 1");
    if let ExprKind::Fn { sig, body, .. } = &ctx.expr(id).kind {
        assert_eq!(sig.name, None);
        assert_eq!(sig.params.len(), 1);
        assert!(matches!(ctx.expr(*body).kind, ExprKind::Binary { .. }));
    } else {
        panic!("expected function expression with arrow body");
    }
}

#[test]
fn test_expr_bind_val() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("val x := 42");
    if let ExprKind::Bind { mutable, .. } = &ctx.expr(id).kind {
        assert!(!mutable);
    } else {
        panic!("expected bind expression");
    }
}

#[test]
fn test_expr_bind_var() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("var x := 42");
    if let ExprKind::Bind { mutable, .. } = &ctx.expr(id).kind {
        assert!(mutable);
    } else {
        panic!("expected bind expression");
    }
}

#[test]
fn test_expr_assign() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("x <- 42");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Assign { .. }));
}

#[test]
fn test_expr_range_inclusive() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("1..10");
    assert!(matches!(
        &ctx.expr(id).kind,
        ExprKind::Range {
            inclusive: true,
            ..
        }
    ));
}

#[test]
fn test_expr_range_exclusive() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("1..<10");
    assert!(matches!(
        &ctx.expr(id).kind,
        ExprKind::Range {
            inclusive: false,
            ..
        }
    ));
}

#[test]
fn test_expr_binary_not_in() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("x not in items");
    if let ExprKind::Unary { op, operand } = &ctx.expr(id).kind {
        assert_eq!(*op, TokenKind::KwNot);
        assert!(
            matches!(&ctx.expr(*operand).kind, ExprKind::Binary { op, .. } if *op == TokenKind::KwIn)
        );
    } else {
        panic!("expected binary expression");
    }
}

#[test]
fn test_expr_binary_pipe() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("x |> f");
    if let ExprKind::Binary { op, .. } = &ctx.expr(id).kind {
        assert_eq!(*op, TokenKind::BarGt);
    } else {
        panic!("expected binary expression");
    }
}

#[test]
fn test_expr_binary_coalesce() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("x ?? 0");
    if let ExprKind::Binary { op, .. } = &ctx.expr(id).kind {
        assert_eq!(*op, TokenKind::QuestionQuestion);
    } else {
        panic!("expected binary expression");
    }
}

#[test]
fn test_trailing_comma_tuple() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("(1, 2, 3,)");
    if let ExprKind::Tuple(elems) = &ctx.expr(id).kind {
        assert_eq!(elems.len(), 3);
    } else {
        panic!("expected tuple literal expression");
    }
}

#[test]
fn test_trailing_comma_array() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("[1, 2, 3,]");
    if let ExprKind::Array(elems) = &ctx.expr(id).kind {
        assert_eq!(elems.len(), 3);
    } else {
        panic!("expected array literal expression");
    }
}

#[test]
fn test_deeply_nested_blocks() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_expr("{ { { 42 } } }");
    if let ExprKind::Block {
        expr: Some(inner1), ..
    } = &ctx.expr(id).kind
    {
        if let ExprKind::Block {
            expr: Some(inner2), ..
        } = &ctx.expr(*inner1).kind
        {
            assert!(matches!(&ctx.expr(*inner2).kind, ExprKind::Block { .. }));
        } else {
            panic!("expected nested block expression");
        }
    } else {
        panic!("expected block expression");
    }
}
