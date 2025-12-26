use musi_ast::{Cond, Expr, ExprKind, LitKind, Pat, PatKind, Typ, TypKind};
use musi_basic::{interner::Interner, source::SourceFile};
use musi_lex::{lexer::tokenize, token::TokenKind};

use crate::Parser;

struct TestContext {
    interner: Interner,
}

impl TestContext {
    fn new() -> Self {
        Self {
            interner: Interner::new(),
        }
    }

    fn tokenize(&mut self, input: &str) -> Vec<musi_lex::token::Token> {
        let source = SourceFile::new("test.ms".into(), input.into(), 0);
        let (tokens, _) = tokenize(&source, &mut self.interner);
        tokens
    }

    fn parse_expr(&mut self, input: &str) -> Expr {
        let tokens = self.tokenize(input);
        let mut parser = Parser::new(&tokens);
        parser.parse_expr().expect("parse failed")
    }

    fn parse_pat(&mut self, input: &str) -> Pat {
        let tokens = self.tokenize(input);
        let mut parser = Parser::new(&tokens);
        parser.parse_pat().expect("parse failed")
    }

    fn parse_typ(&mut self, input: &str) -> Typ {
        let tokens = self.tokenize(input);
        let mut parser = Parser::new(&tokens);
        parser.parse_typ().expect("parse failed")
    }

    fn intern(&mut self, s: &str) -> u32 {
        self.interner.intern(s)
    }
}

#[test]
fn test_expr_lit_int() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("42");
    assert!(matches!(expr.kind, ExprKind::Lit(LitKind::Int(_))));
}

#[test]
fn test_expr_lit_real() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("3.14");
    assert!(matches!(expr.kind, ExprKind::Lit(LitKind::Real(_))));
}

#[test]
fn test_expr_lit_string() {
    let mut ctx = TestContext::new();
    let id = ctx.intern("hello");
    let expr = ctx.parse_expr(r#""hello""#);
    assert!(matches!(expr.kind, ExprKind::Lit(LitKind::String(s)) if s == id));
}

#[test]
fn test_expr_lit_rune() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("'x'");
    assert!(matches!(expr.kind, ExprKind::Lit(LitKind::Rune('x'))));
}

#[test]
fn test_expr_lit_bool() {
    let mut ctx = TestContext::new();
    let expr_true = ctx.parse_expr("true");
    let expr_false = ctx.parse_expr("false");
    assert!(matches!(expr_true.kind, ExprKind::Lit(LitKind::Bool(true))));
    assert!(matches!(
        expr_false.kind,
        ExprKind::Lit(LitKind::Bool(false))
    ));
}

#[test]
fn test_expr_ident() {
    let mut ctx = TestContext::new();
    let id = ctx.intern("foo");
    let expr = ctx.parse_expr("foo");
    assert!(matches!(expr.kind, ExprKind::Ident(i) if i == id));
}

#[test]
fn test_expr_tuple_empty() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("()");
    assert!(matches!(expr.kind, ExprKind::Tuple(ref elems) if elems.is_empty()));
}

#[test]
fn test_expr_tuple_single() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("(1,)");
    if let ExprKind::Tuple(elems) = expr.kind {
        assert_eq!(elems.len(), 1);
    } else {
        panic!("expected tuple literal expression");
    }
}

#[test]
fn test_expr_tuple_multiple() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("(1, 2, 3)");
    if let ExprKind::Tuple(elems) = expr.kind {
        assert_eq!(elems.len(), 3);
    } else {
        panic!("expected tuple literal expression");
    }
}

#[test]
fn test_expr_grouped() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("(42)");
    assert!(matches!(expr.kind, ExprKind::Lit(LitKind::Int(_))));
}

#[test]
fn test_expr_array_empty() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("[]");
    assert!(matches!(expr.kind, ExprKind::Array(ref elems) if elems.is_empty()));
}

#[test]
fn test_expr_array_multiple() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("[1, 2, 3]");
    if let ExprKind::Array(elems) = expr.kind {
        assert_eq!(elems.len(), 3);
    } else {
        panic!("expected array literal expression");
    }
}

#[test]
fn test_expr_record_anon() {
    let mut ctx = TestContext::new();
    let x = ctx.intern("x");
    let expr = ctx.parse_expr(".{x := 1}");
    if let ExprKind::Record { ty, fields } = expr.kind {
        assert!(ty.is_none());
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
    let expr = ctx.parse_expr("Point.{x := 1}");
    if let ExprKind::Record {
        ty: Some(ty_expr),
        fields,
    } = expr.kind
    {
        assert!(matches!(ty_expr.kind, ExprKind::Ident(i) if i == point));
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].name, x);
    } else {
        panic!("expected `record` expression");
    }
}

#[test]
fn test_expr_binary_add() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("1 + 2");
    if let ExprKind::Binary { op, .. } = expr.kind {
        assert_eq!(op, TokenKind::Plus);
    } else {
        panic!("expected binary `+` expression");
    }
}

#[test]
/// Should parse as `1 + (2 * 3)`.
fn test_expr_binary_precedence() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("1 + 2 * 3");
    if let ExprKind::Binary { op, rhs, .. } = expr.kind {
        assert_eq!(op, TokenKind::Plus);
        assert!(matches!(
            rhs.kind,
            ExprKind::Binary {
                op: TokenKind::Star,
                ..
            }
        ));
    } else {
        panic!("expected binary `+` and `*` expression");
    }
}

#[test]
/// Should parse as `(1 - 2) - 3`. (left associative)
fn test_expr_binary_associativity() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("1 - 2 - 3");
    if let ExprKind::Binary { op, lhs, .. } = expr.kind {
        assert_eq!(op, TokenKind::Minus);
        assert!(matches!(
            lhs.kind,
            ExprKind::Binary {
                op: TokenKind::Minus,
                ..
            }
        ));
    } else {
        panic!("expected binary `-` expression");
    }
}

#[test]
fn test_expr_binary_comparison() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("1 < 2");
    if let ExprKind::Binary { op, .. } = expr.kind {
        assert_eq!(op, TokenKind::Lt);
    } else {
        panic!("expected binary `<` expression");
    }
}

#[test]
fn test_expr_binary_logical() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("true and false");
    if let ExprKind::Binary { op, .. } = expr.kind {
        assert_eq!(op, TokenKind::KwAnd);
    } else {
        panic!("expected binary `and` expression");
    }
}

#[test]
fn test_expr_unary_neg() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("-42");
    if let ExprKind::Unary { op, .. } = expr.kind {
        assert_eq!(op, TokenKind::Minus);
    } else {
        panic!("expected unary `-` expression");
    }
}

#[test]
fn test_expr_unary_not() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("not true");
    if let ExprKind::Unary { op, .. } = expr.kind {
        assert_eq!(op, TokenKind::KwNot);
    } else {
        panic!("expected unary `not` expression");
    }
}

#[test]
/// Should parse `not in` as `not(x in list)`
fn test_expr_not_in() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("x not in list");
    if let ExprKind::Unary {
        op: TokenKind::KwNot,
        operand,
    } = expr.kind
    {
        assert!(matches!(
            operand.kind,
            ExprKind::Binary {
                op: TokenKind::KwIn,
                ..
            }
        ));
    } else {
        panic!("expected binary `not in` expression");
    }
}

#[test]
fn test_expr_call() {
    let mut ctx = TestContext::new();
    let f = ctx.intern("f");
    let expr = ctx.parse_expr("f(1, 2)");
    if let ExprKind::Call { callee, args } = expr.kind {
        assert!(matches!(callee.kind, ExprKind::Ident(i) if i == f));
        assert_eq!(args.len(), 2);
    } else {
        panic!("expected call expression");
    }
}

#[test]
fn test_expr_index() {
    let mut ctx = TestContext::new();
    let arr = ctx.intern("arr");
    let expr = ctx.parse_expr("arr.[0]");
    if let ExprKind::Index { base, .. } = expr.kind {
        assert!(matches!(base.kind, ExprKind::Ident(i) if i == arr));
    } else {
        panic!("expected index expression");
    }
}

#[test]
fn test_expr_field() {
    let mut ctx = TestContext::new();
    let obj = ctx.intern("obj");
    let x = ctx.intern("x");
    let expr = ctx.parse_expr("obj.x");
    if let ExprKind::Field { base, field } = expr.kind {
        assert!(matches!(base.kind, ExprKind::Ident(i) if i == obj));
        assert_eq!(field, x);
    } else {
        panic!("expected field expression");
    }
}

#[test]
fn test_expr_deref() {
    let mut ctx = TestContext::new();
    let ptr = ctx.intern("ptr");
    let expr = ctx.parse_expr("ptr.^");
    if let ExprKind::Deref(base) = expr.kind {
        assert!(matches!(base.kind, ExprKind::Ident(i) if i == ptr));
    } else {
        panic!("expected deref expression");
    }
}

#[test]
fn test_expr_if() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("if true { 1 }");
    assert!(matches!(expr.kind, ExprKind::If { else_br: None, .. }));
}

#[test]
fn test_expr_if_else() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("if true { 1 } else { 2 }");
    assert!(matches!(
        expr.kind,
        ExprKind::If {
            else_br: Some(_),
            ..
        }
    ));
}

#[test]
fn test_expr_if_else_if() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("if true { 1 } else if false { 2 } else { 3 }");
    if let ExprKind::If {
        else_br: Some(else_expr),
        ..
    } = expr.kind
    {
        assert!(matches!(else_expr.kind, ExprKind::If { .. }));
    } else {
        panic!("expected '`if`-`else`-`if` expression");
    }
}

#[test]
fn test_expr_while() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("while true { 1 }");
    assert!(matches!(expr.kind, ExprKind::While { .. }));
}

#[test]
fn test_expr_if_case() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("if case Some(x) := opt { x }");
    if let ExprKind::If { cond, .. } = expr.kind {
        assert!(matches!(*cond, Cond::Case { .. }));
    } else {
        panic!("expected `if case` expression");
    }
}

#[test]
fn test_expr_while_case() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("while case Some(item) := get_next() { item }");
    if let ExprKind::While { cond, .. } = expr.kind {
        assert!(matches!(*cond, Cond::Case { .. }));
    } else {
        panic!("expected `while case` expression");
    }
}

#[test]
fn test_expr_if_case_multi() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("if case Some(x) := opt, x > 0 { x }");
    if let ExprKind::If { cond, .. } = expr.kind {
        if let Cond::Case { extra, .. } = *cond {
            assert_eq!(extra.len(), 1);
        } else {
            panic!("expected `case` condition");
        }
    } else {
        panic!("expected `if case` expression with extra condition");
    }
}

#[test]
fn test_expr_for() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("for x in items { x }");
    assert!(matches!(expr.kind, ExprKind::For { .. }));
}

#[test]
fn test_expr_match() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("match x { case 1 => 2 }");
    if let ExprKind::Match { cases, .. } = expr.kind {
        assert_eq!(cases.len(), 1);
    } else {
        panic!("expected `match` expression");
    }
}

#[test]
fn test_expr_match_guard() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("match x { case n if n > 0 => n }");
    if let ExprKind::Match { cases, .. } = expr.kind {
        assert!(cases[0].guard.is_some());
    } else {
        panic!("expected `match` expression with 'if' guard");
    }
}

#[test]
fn test_expr_range_inclusive() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("1..10");
    if let ExprKind::Range { inclusive, end, .. } = expr.kind {
        assert!(inclusive);
        assert!(end.is_some());
    } else {
        panic!("expected inclusive range expression");
    }
}

#[test]
fn test_expr_range_exclusive() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("1..<10");
    if let ExprKind::Range { inclusive, .. } = expr.kind {
        assert!(!inclusive);
    } else {
        panic!("expected exclusive range expression");
    }
}

#[test]
fn test_expr_range_unbounded() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("1..");
    if let ExprKind::Range { end, .. } = expr.kind {
        assert!(end.is_none());
    } else {
        panic!("expected unbounded range expression");
    }
}

#[test]
fn test_expr_assign() {
    let mut ctx = TestContext::new();
    let x = ctx.intern("x");
    let expr = ctx.parse_expr("x <- 42");
    if let ExprKind::Assign { target, .. } = expr.kind {
        assert!(matches!(target.kind, ExprKind::Ident(i) if i == x));
    } else {
        panic!("expected assign expression");
    }
}

#[test]
fn test_expr_return() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("return 42");
    if let ExprKind::Return(Some(val)) = expr.kind {
        assert!(matches!(val.kind, ExprKind::Lit(LitKind::Int(_))));
    } else {
        panic!("expected `return` expression");
    }
}

#[test]
fn test_expr_return_void() {
    let mut ctx = TestContext::new();
    let tokens = ctx.tokenize("return;");
    let mut parser = Parser::new(&tokens);
    let expr = parser.parse_expr().expect("parse failed");
    assert!(matches!(expr.kind, ExprKind::Return(None)));
}

#[test]
fn test_expr_break() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("break 1");
    assert!(matches!(expr.kind, ExprKind::Break(Some(_))));
}

#[test]
fn test_expr_cycle() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("cycle");
    assert!(matches!(expr.kind, ExprKind::Cycle));
}

#[test]
fn test_expr_defer() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("defer cleanup()");
    assert!(matches!(expr.kind, ExprKind::Defer(_)));
}

#[test]
fn test_expr_block_empty() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("{}");
    if let ExprKind::Block { stmts, expr } = expr.kind {
        assert!(stmts.is_empty());
        assert!(expr.is_none());
    } else {
        panic!("expected block expression");
    }
}

#[test]
fn test_expr_block_with_expr() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("{ 42 }");
    if let ExprKind::Block { stmts, expr } = expr.kind {
        assert!(stmts.is_empty());
        assert!(expr.is_some());
    } else {
        panic!("expected block expression");
    }
}

#[test]
fn test_expr_block_with_stmts() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("{ 1; 2; 3 }");
    if let ExprKind::Block { stmts, expr } = expr.kind {
        assert_eq!(stmts.len(), 2);
        assert!(expr.is_some());
    } else {
        panic!("expected block expression");
    }
}

#[test]
fn test_expr_val_bind() {
    let mut ctx = TestContext::new();
    let x = ctx.intern("x");
    let expr = ctx.parse_expr("val x := 42");
    if let ExprKind::Bind { mutable, pat, .. } = expr.kind {
        assert!(!mutable);
        assert!(matches!(pat.kind, PatKind::Ident(i) if i == x));
    } else {
        panic!("expected `val` binding expression");
    }
}

#[test]
fn test_expr_var_bind() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("var x := 42");
    if let ExprKind::Bind { mutable, .. } = expr.kind {
        assert!(mutable);
    } else {
        panic!("expected `var` binding expression");
    }
}

#[test]
fn test_expr_fn_def() {
    let mut ctx = TestContext::new();
    let add = ctx.intern("add");
    let expr = ctx.parse_expr("fn add(a, b) { a + b }");
    if let ExprKind::Fn { sig, .. } = expr.kind {
        assert_eq!(sig.name, Some(add));
        assert_eq!(sig.params.len(), 2);
    } else {
        panic!("expected `fn` expression");
    }
}

#[test]
fn test_expr_fn_anon() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("fn(x) { x }");
    if let ExprKind::Fn { sig, .. } = expr.kind {
        assert!(sig.name.is_none());
        assert_eq!(sig.params.len(), 1);
    } else {
        panic!("expected unnamed `fn` expression");
    }
}

#[test]
fn test_expr_record_def() {
    let mut ctx = TestContext::new();
    let point = ctx.intern("Point");
    let expr = ctx.parse_expr("record Point { x; y }");
    if let ExprKind::RecordDef { name, fields, .. } = expr.kind {
        assert_eq!(name, Some(point));
        assert_eq!(fields.len(), 2);
    } else {
        panic!("expected `record` typedef expression");
    }
}

#[test]
fn test_expr_sum_def() {
    let mut ctx = TestContext::new();
    let option = ctx.intern("Option");
    let expr = ctx.parse_expr("sum Option { case Some(x), case None }");
    if let ExprKind::SumDef { name, cases, .. } = expr.kind {
        assert_eq!(name, Some(option));
        assert_eq!(cases.len(), 2);
    } else {
        panic!("expected `sum` typedef expression");
    }
}

#[test]
fn test_expr_alias_def() {
    let mut ctx = TestContext::new();
    let myint = ctx.intern("MyInt");
    let expr = ctx.parse_expr("alias MyInt := Int");
    if let ExprKind::Alias { name, .. } = expr.kind {
        assert_eq!(name, myint);
    } else {
        panic!("expected `alias` expression");
    }
}

#[test]
fn test_pat_wild() {
    let mut ctx = TestContext::new();
    let pat = ctx.parse_pat("_");
    assert!(matches!(pat.kind, PatKind::Wild));
}

#[test]
fn test_pat_ident() {
    let mut ctx = TestContext::new();
    let x = ctx.intern("x");
    let pat = ctx.parse_pat("x");
    assert!(matches!(pat.kind, PatKind::Ident(i) if i == x));
}

#[test]
fn test_pat_lit_int() {
    let mut ctx = TestContext::new();
    let pat = ctx.parse_pat("42");
    assert!(matches!(pat.kind, PatKind::Lit(LitKind::Int(_))));
}

#[test]
fn test_pat_tuple() {
    let mut ctx = TestContext::new();
    let pat = ctx.parse_pat("(a, b, c)");
    if let PatKind::Tuple(elems) = pat.kind {
        assert_eq!(elems.len(), 3);
    } else {
        panic!("expected tuple pattern");
    }
}

#[test]
fn test_pat_array() {
    let mut ctx = TestContext::new();
    let pat = ctx.parse_pat("[a, b]");
    if let PatKind::Array(elems) = pat.kind {
        assert_eq!(elems.len(), 2);
    } else {
        panic!("expected array pattern");
    }
}

#[test]
fn test_pat_variant() {
    let mut ctx = TestContext::new();
    let some = ctx.intern("Some");
    let pat = ctx.parse_pat("Some(x)");
    if let PatKind::Variant { name, args, .. } = pat.kind {
        assert_eq!(name, some);
        assert_eq!(args.len(), 1);
    } else {
        panic!("expected variant pattern");
    }
}

#[test]
fn test_pat_record() {
    let mut ctx = TestContext::new();
    let point = ctx.intern("Point");
    let pat = ctx.parse_pat("Point.{x, y}");
    if let PatKind::Record {
        ty: Some(ty_expr),
        fields,
    } = pat.kind
    {
        assert!(matches!(ty_expr.kind, ExprKind::Ident(i) if i == point));
        assert_eq!(fields.len(), 2);
    } else {
        panic!("expected record pattern");
    }
}

#[test]
fn test_pat_or() {
    let mut ctx = TestContext::new();
    let pat = ctx.parse_pat("1 | 2 | 3");
    if let PatKind::Or(alts) = pat.kind {
        assert_eq!(alts.len(), 3);
    } else {
        panic!("expected or `|` pattern");
    }
}

#[test]
fn test_pat_cons() {
    let mut ctx = TestContext::new();
    let pat = ctx.parse_pat("head :: tail");
    if let PatKind::Cons(parts) = pat.kind {
        assert_eq!(parts.len(), 2);
    } else {
        panic!("expected cons `::` pattern");
    }
}

#[test]
fn test_typ_ident() {
    let mut ctx = TestContext::new();
    let int = ctx.intern("Int");
    let typ = ctx.parse_typ("Int");
    assert!(matches!(typ.kind, TypKind::Ident(i) if i == int));
}

#[test]
fn test_typ_optional() {
    let mut ctx = TestContext::new();
    let typ = ctx.parse_typ("?Int");
    assert!(matches!(typ.kind, TypKind::Optional(_)));
}

#[test]
fn test_typ_ptr() {
    let mut ctx = TestContext::new();
    let typ = ctx.parse_typ("^Int");
    assert!(matches!(typ.kind, TypKind::Ptr(_)));
}

#[test]
fn test_typ_array_unsized() {
    let mut ctx = TestContext::new();
    let typ = ctx.parse_typ("[]Int");
    if let TypKind::Array { size, .. } = typ.kind {
        assert!(size.is_none());
    } else {
        panic!("expected array type");
    }
}

#[test]
fn test_typ_array_sized() {
    let mut ctx = TestContext::new();
    let typ = ctx.parse_typ("[10]Int");
    if let TypKind::Array { size, .. } = typ.kind {
        assert!(size.is_some());
    } else {
        panic!("expected array type");
    }
}

#[test]
fn test_typ_fn() {
    let mut ctx = TestContext::new();
    let typ = ctx.parse_typ("Int -> String");
    assert!(matches!(typ.kind, TypKind::Fn { .. }));
}

#[test]
fn test_typ_tuple() {
    let mut ctx = TestContext::new();
    let typ = ctx.parse_typ("(Int, String)");
    if let TypKind::Tuple(elems) = typ.kind {
        assert_eq!(elems.len(), 2);
    } else {
        panic!("expected tuple type");
    }
}

#[test]
fn test_typ_app() {
    let mut ctx = TestContext::new();
    let list = ctx.intern("List");
    let typ = ctx.parse_typ("List[Int]");
    if let TypKind::App { base, args } = typ.kind {
        assert_eq!(base, list);
        assert_eq!(args.len(), 1);
    } else {
        panic!("expected type application");
    }
}

#[test]
/// Should parse as: `Fn(param: ?[10]^Int, ret: (Int, String))`
fn test_typ_complex() {
    let mut ctx = TestContext::new();
    let typ = ctx.parse_typ("?[10]^Int -> (Int, String)");
    assert!(matches!(typ.kind, TypKind::Fn { .. }));
}

#[test]
fn test_nested_calls() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("f(g(h(x)))");
    if let ExprKind::Call { callee, .. } = expr.kind {
        if let ExprKind::Ident(i) = callee.kind {
            assert_eq!(i, ctx.intern("f"));
        } else {
            panic!("expected identifier expression");
        }
    } else {
        panic!("expected call expression");
    }
}

#[test]
/// Should parse as `((obj.foo()).bar()).baz()``.
fn test_chained_method_calls() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("obj.foo().bar().baz()");
    assert!(matches!(expr.kind, ExprKind::Call { .. }));
}

#[test]
/// Should parse as `1 + (2 * (3**4))``
fn test_complex_precedence() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("1 + 2 * 3**4");
    if let ExprKind::Binary {
        op: TokenKind::Plus,
        rhs,
        ..
    } = expr.kind
    {
        if let ExprKind::Binary {
            op: TokenKind::Star,
            rhs: inner_rhs,
            ..
        } = rhs.kind
        {
            assert!(matches!(
                inner_rhs.kind,
                ExprKind::Binary {
                    op: TokenKind::StarStar,
                    ..
                }
            ));
        } else {
            panic!("expected binary `*` expression");
        }
    } else {
        panic!("expected binary `+` expression");
    }
}

#[test]
fn test_trailing_comma_tuple() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("(1, 2, 3,)");
    if let ExprKind::Tuple(elems) = expr.kind {
        assert_eq!(elems.len(), 3);
    } else {
        panic!("expected tuple literal expression");
    }
}

#[test]
fn test_trailing_comma_array() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("[1, 2, 3,]");
    if let ExprKind::Array(elems) = expr.kind {
        assert_eq!(elems.len(), 3);
    } else {
        panic!("expected array literal expression");
    }
}

#[test]
fn test_deeply_nested_blocks() {
    let mut ctx = TestContext::new();
    let expr = ctx.parse_expr("{ { { 42 } } }");
    if let ExprKind::Block {
        expr: Some(inner), ..
    } = expr.kind
    {
        if let ExprKind::Block {
            expr: Some(inner2), ..
        } = inner.kind
        {
            assert!(matches!(inner2.kind, ExprKind::Block { .. }));
        } else {
            panic!("expected nested block expression");
        }
    } else {
        panic!("expected block expression");
    }
}
