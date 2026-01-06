use crate::TestCtx;
use musi_ast::{CondKind, ExprKind, LitKind};
use musi_lex::token::TokenKind;

#[test]
fn test_expr_lit_int() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("42");
    assert!(matches!(ctx.expr(id).kind, ExprKind::Lit(LitKind::Int(_))));
}

#[test]
fn test_expr_lit_real() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("3.14");
    assert!(matches!(ctx.expr(id).kind, ExprKind::Lit(LitKind::Real(_))));
}

#[test]
fn test_expr_lit_string() {
    let mut ctx = TestCtx::new();
    let str_id = ctx.intern("hello");
    let id = ctx.parse_expr(r#""hello""#);
    assert!(matches!(ctx.expr(id).kind, ExprKind::Lit(LitKind::String(s)) if s == str_id));
}

#[test]
fn test_expr_lit_rune() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("'x'");
    assert!(matches!(
        ctx.expr(id).kind,
        ExprKind::Lit(LitKind::Rune('x'))
    ));
}

#[test]
fn test_expr_lit_bool() {
    let mut ctx = TestCtx::new();
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
    let mut ctx = TestCtx::new();
    let foo = ctx.intern("foo");
    let id = ctx.parse_expr("foo");
    assert!(matches!(ctx.expr(id).kind, ExprKind::Ident(i) if i == foo));
}

#[test]
fn test_expr_tuple_empty() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("()");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Tuple(elems) if elems.is_empty()));
}

#[test]
fn test_expr_tuple_single() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("(1,)");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(kind, ExprKind::Tuple(elems) if elems.len() == 1));
}

#[test]
fn test_expr_tuple_multiple() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("(1, 2, 3)");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(kind, ExprKind::Tuple(elems) if elems.len() == 3));
}

#[test]
fn test_expr_grouped() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("(42)");
    assert!(matches!(ctx.expr(id).kind, ExprKind::Lit(LitKind::Int(_))));
}

#[test]
fn test_expr_array_empty() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("[]");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Array(elems) if elems.is_empty()));
}

#[test]
fn test_expr_array_multiple() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("[1, 2, 3]");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(kind, ExprKind::Array(elems) if elems.len() == 3));
}

#[test]
fn test_expr_record_anon() {
    let mut ctx = TestCtx::new();
    let x = ctx.intern("x");
    let id = ctx.parse_expr(".{x := 1}");
    let kind = &ctx.expr(id).kind;
    assert!(
        matches!(kind, ExprKind::Record { base: None, fields } if fields.len() == 1 && fields[0].name == x)
    );
}

#[test]
fn test_expr_record_typed() {
    let mut ctx = TestCtx::new();
    let point = ctx.intern("Point");
    let x = ctx.intern("x");
    let id = ctx.parse_expr("Point.{x := 1}");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::Record {
            base: Some(base_id),
            fields,
        } if matches!(ctx.expr(*base_id).kind, ExprKind::Ident(i) if i == point)
            && fields.len() == 1
            && fields[0].name == x
    ));
}

#[test]
fn test_expr_record_update() {
    let mut ctx = TestCtx::new();
    let p = ctx.intern("p");
    let x = ctx.intern("x");
    let id = ctx.parse_expr(".{p with x := 1}");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::Record {
            base: Some(base_id),
            fields,
        } if matches!(ctx.expr(*base_id).kind, ExprKind::Ident(i) if i == p)
            && fields.len() == 1
            && fields[0].name == x
    ));
}

#[test]
fn test_expr_binary_add() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("1 + 2");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::Binary {
            op: TokenKind::Plus,
            ..
        }
    ));
}

#[test]
/// Should parse as `1 + (2 * 3)`.
fn test_expr_binary_precedence() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("1 + 2 * 3");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::Binary {
            op: TokenKind::Plus,
            rhs,
            ..
        } if matches!(&ctx.expr(*rhs).kind, ExprKind::Binary { op: TokenKind::Star, .. })
    ));
}

#[test]
/// Should parse as `(1 + 2) + 3` (left-associative).
fn test_expr_binary_left_assoc() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("1 + 2 + 3");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::Binary {
            op: TokenKind::Plus,
            lhs,
            ..
        } if matches!(&ctx.expr(*lhs).kind, ExprKind::Binary { op: TokenKind::Plus, .. })
    ));
}

#[test]
/// Should parse as `1 ** (2 ** 3)` (right-associative).
fn test_expr_binary_right_assoc() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("1 ** 2 ** 3");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::Binary {
            op: TokenKind::StarStar,
            rhs,
            ..
        } if matches!(&ctx.expr(*rhs).kind, ExprKind::Binary { op: TokenKind::StarStar, .. })
    ));
}

#[test]
fn test_expr_unary_neg() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("-42");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::Unary {
            op: TokenKind::Minus,
            operand,
        } if matches!(ctx.expr(*operand).kind, ExprKind::Lit(LitKind::Int(_)))
    ));
}

#[test]
fn test_expr_unary_not() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("not true");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::Unary {
            op: TokenKind::KwNot,
            operand,
        } if matches!(ctx.expr(*operand).kind, ExprKind::Lit(LitKind::Bool(true)))
    ));
}

#[test]
fn test_expr_call_no_args() {
    let mut ctx = TestCtx::new();
    let foo = ctx.intern("foo");
    let id = ctx.parse_expr("foo()");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::Call { callee, args } if matches!(ctx.expr(*callee).kind, ExprKind::Ident(i) if i == foo)
            && args.is_empty()
    ));
}

#[test]
fn test_expr_call_with_args() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("foo(1, 2)");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(kind, ExprKind::Call { args, .. } if args.len() == 2));
}

#[test]
fn test_expr_index() {
    let mut ctx = TestCtx::new();
    let arr = ctx.intern("arr");
    let id = ctx.parse_expr("arr.[0]");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::Index { base, index } if matches!(ctx.expr(*base).kind, ExprKind::Ident(i) if i == arr)
            && matches!(ctx.expr(*index).kind, ExprKind::Lit(LitKind::Int(_)))
    ));
}

#[test]
fn test_expr_field() {
    let mut ctx = TestCtx::new();
    let point = ctx.intern("point");
    let x = ctx.intern("x");
    let id = ctx.parse_expr("point.x");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::Field { base, field } if matches!(ctx.expr(*base).kind, ExprKind::Ident(i) if i == point)
            && *field == x
    ));
}

#[test]
fn test_expr_deref() {
    let mut ctx = TestCtx::new();
    let ptr = ctx.intern("ptr");
    let id = ctx.parse_expr("ptr.^");
    let kind = &ctx.expr(id).kind;
    assert!(
        matches!(kind, ExprKind::Deref(base) if matches!(ctx.expr(*base).kind, ExprKind::Ident(i) if i == ptr))
    );
}

#[test]
fn test_expr_block_empty() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("{}");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(kind, ExprKind::Block { stmts, expr } if stmts.is_empty() && expr.is_none()));
}

#[test]
fn test_expr_block_with_expr() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("{ 42 }");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(kind, ExprKind::Block { stmts, expr } if stmts.is_empty() && expr.is_some()));
}

#[test]
fn test_expr_block_with_stmts() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("{ 1; 2; 3 }");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(kind, ExprKind::Block { stmts, expr } if stmts.len() == 2 && expr.is_some()));
}

#[test]
fn test_expr_if_simple() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("if true { 1 }");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::If {
            cond,
            then_br,
            else_br: None,
        } if matches!(&ctx.arena.conds.get(*cond).kind, CondKind::Expr(e) if matches!(ctx.expr(*e).kind, ExprKind::Lit(LitKind::Bool(true))))
            && matches!(&ctx.expr(*then_br).kind, ExprKind::Block { .. })
    ));
}

#[test]
fn test_expr_if_else() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("if true { 1 } else { 2 }");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::If {
            else_br: Some(_),
            ..
        }
    ));
}

#[test]
fn test_expr_if_else_if() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("if true { 1 } else if false { 2 } else { 3 }");
    let kind = &ctx.expr(id).kind;
    assert!(
        matches!(kind, ExprKind::If { else_br: Some(else_id), .. } if matches!(&ctx.expr(*else_id).kind, ExprKind::If { .. }))
    );
}

#[test]
fn test_expr_if_case() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("if case x := foo { x }");
    let kind = &ctx.expr(id).kind;
    assert!(
        matches!(kind, ExprKind::If { cond, .. } if matches!(&ctx.arena.conds.get(*cond).kind, CondKind::Case { .. }))
    );
}

#[test]
fn test_expr_while_simple() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("while true { 1 }");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::While { .. }));
}

#[test]
fn test_expr_for() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("for x in items { x }");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::For { .. }));
}

#[test]
fn test_expr_match_simple() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("match x { case 1 => 2 }");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(kind, ExprKind::Match { cases, .. } if cases.len() == 1));
}

#[test]
fn test_expr_match_multiple_cases() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("match x { case 1 => 2, case 3 => 4 }");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(kind, ExprKind::Match { cases, .. } if cases.len() == 2));
}

#[test]
fn test_expr_match_with_guard() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("match x { case y if y > 0 => y }");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(kind, ExprKind::Match { cases, .. } if cases[0].guard.is_some()));
}

#[test]
fn test_expr_return_empty() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("return");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Return(None)));
}

#[test]
fn test_expr_return_value() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("return 42");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Return(Some(_))));
}

#[test]
fn test_expr_defer() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("defer foo()");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Defer(_)));
}

#[test]
fn test_expr_break_empty() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("break");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Break(None)));
}

#[test]
fn test_expr_break_value() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("break 42");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Break(Some(_))));
}

#[test]
fn test_expr_cycle() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("cycle");
    assert!(matches!(ctx.expr(id).kind, ExprKind::Cycle));
}

#[test]
fn test_expr_unsafe() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("unsafe { 42 }");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Unsafe(_)));
}

#[test]
fn test_expr_import() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr(r#"import "foo""#);
    assert!(matches!(ctx.expr(id).kind, ExprKind::Import(_)));
}

#[test]
fn test_expr_record_def() {
    let mut ctx = TestCtx::new();
    let point = ctx.intern("Point");
    let id = ctx.parse_expr("record Point { x: Int; y: Int }");
    let kind = &ctx.expr(id).kind;
    assert!(
        matches!(kind, ExprKind::RecordDef { name, fields, .. } if *name == Some(point) && fields.len() == 2)
    );
}

#[test]
fn test_expr_choice_def() {
    let mut ctx = TestCtx::new();
    let option = ctx.intern("Option");
    let id = ctx.parse_expr("choice Option { case Some(Int), case None }");
    let kind = &ctx.expr(id).kind;
    assert!(
        matches!(kind, ExprKind::ChoiceDef { name, cases, .. } if *name == Some(option) && cases.len() == 2)
    );
}

#[test]
fn test_expr_alias() {
    let mut ctx = TestCtx::new();
    let my_int = ctx.intern("MyInt");
    let id = ctx.parse_expr("alias MyInt := Int");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(kind, ExprKind::TypeDef { name, .. } if *name == my_int));
}

#[test]
fn test_expr_fn() {
    let mut ctx = TestCtx::new();
    let foo = ctx.intern("foo");
    let id = ctx.parse_expr("fn foo(x: Int): Int { x }");
    let kind = &ctx.expr(id).kind;
    assert!(
        matches!(kind, ExprKind::Fn { sig, .. } if sig.name == Some(foo) && sig.params.len() == 1)
    );
}

#[test]
fn test_expr_fn_arrow() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("fn(x: Int): Int => x + 1");
    let kind = &ctx.expr(id).kind;
    assert!(
        matches!(kind, ExprKind::Fn { sig, body, .. } if sig.name.is_none() && sig.params.len() == 1 && matches!(ctx.expr(*body).kind, ExprKind::Binary { .. }))
    );
}

#[test]
fn test_expr_bind_val() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("val x := 42");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(kind, ExprKind::Bind { mutable: false, .. }));
}

#[test]
fn test_expr_bind_var() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("var x := 42");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(kind, ExprKind::Bind { mutable: true, .. }));
}

#[test]
fn test_expr_assign() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("x <- 42");
    assert!(matches!(&ctx.expr(id).kind, ExprKind::Assign { .. }));
}

#[test]
fn test_expr_range_inclusive() {
    let mut ctx = TestCtx::new();
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
    let mut ctx = TestCtx::new();
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
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("x not in items");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::Unary {
            op: TokenKind::KwNot,
            operand,
        } if matches!(&ctx.expr(*operand).kind, ExprKind::Binary { op: TokenKind::KwIn, .. })
    ));
}

#[test]
fn test_expr_binary_pipe() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("x |> f");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::Binary {
            op: TokenKind::BarGt,
            ..
        }
    ));
}

#[test]
fn test_expr_binary_coalesce() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("x ?? 0");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::Binary {
            op: TokenKind::QuestionQuestion,
            ..
        }
    ));
}

#[test]
fn test_trailing_comma_tuple() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("(1, 2, 3,)");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(kind, ExprKind::Tuple(elems) if elems.len() == 3));
}

#[test]
fn test_trailing_comma_array() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("[1, 2, 3,]");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(kind, ExprKind::Array(elems) if elems.len() == 3));
}

#[test]
fn test_deeply_nested_blocks() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("{ { { 42 } } }");
    let kind = &ctx.expr(id).kind;
    assert!(matches!(
        kind,
        ExprKind::Block {
            expr: Some(inner1),
            ..
        } if matches!(
            &ctx.expr(*inner1).kind,
            ExprKind::Block {
                expr: Some(inner2),
                ..
            } if matches!(&ctx.expr(*inner2).kind, ExprKind::Block { .. })
        )
    ));
}
