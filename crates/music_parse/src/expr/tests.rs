use music_ast::data::AstData;
use music_ast::expr::{AccessMode, BinOp, CompClause, ExprKind, ImportKind, PostfixOp, UnaryOp};
use music_lex::Lexer;
use music_shared::{Interner, Literal};

use crate::{ParseError, ParseErrorKind};

fn parse_from(source: &str) -> (AstData, Vec<ParseError>) {
    let (tokens, lex_errors) = Lexer::new(source).lex();
    assert!(
        lex_errors.is_empty(),
        "unexpected lex errors: {lex_errors:?}"
    );
    let mut interner = Interner::new();
    crate::parse(&tokens, source, &mut interner)
}

fn parse_expr(source: &str) -> (AstData, Vec<ParseError>) {
    let with_semi = String::from(source) + ";";
    parse_from(&with_semi)
}

fn root_kind(ast: &AstData) -> &ExprKind {
    assert_eq!(ast.root.len(), 1, "expected exactly one root node");
    &ast.exprs.get(ast.root[0]).kind
}

// ── Literals ──────────────────────────────────────────────────

#[test]
fn literal_int() {
    let (ast, errors) = parse_expr("42");
    assert!(errors.is_empty());
    assert_eq!(*root_kind(&ast), ExprKind::Lit(Literal::Int(42)));
}

#[test]
fn literal_float() {
    let (ast, errors) = parse_expr("2.72");
    assert!(errors.is_empty());
    assert!(
        matches!(root_kind(&ast), ExprKind::Lit(Literal::Float(f)) if (*f - 2.72).abs() < f64::EPSILON)
    );
}

#[test]
fn literal_string() {
    let (ast, errors) = parse_expr("\"hello\"");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::Lit(Literal::Str(s)) if s == "hello"));
}

#[test]
fn literal_rune() {
    let (ast, errors) = parse_expr("'a'");
    assert!(errors.is_empty());
    assert_eq!(*root_kind(&ast), ExprKind::Lit(Literal::Rune('a')));
}

// ── Identifiers ───────────────────────────────────────────────

#[test]
fn identifier() {
    let (ast, errors) = parse_expr("x");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::Var(_)));
}

#[test]
fn escaped_identifier() {
    let (ast, errors) = parse_expr("`escaped`");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::Var(_)));
}

// ── Binary operators ──────────────────────────────────────────

#[test]
fn binary_add() {
    let (ast, errors) = parse_expr("1 + 2");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::BinOp(BinOp::Add, _, _)));
}

#[test]
fn binary_and() {
    let (ast, errors) = parse_expr("a and b");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::BinOp(BinOp::And, _, _)));
}

#[test]
fn precedence_add_mul() {
    let (ast, errors) = parse_expr("1 + 2 * 3");
    assert!(errors.is_empty());
    // Top should be Add, right child should be Mul
    match root_kind(&ast) {
        ExprKind::BinOp(BinOp::Add, _, right) => {
            assert!(matches!(
                ast.exprs.get(*right).kind,
                ExprKind::BinOp(BinOp::Mul, _, _)
            ));
        }
        other => panic!("expected Add, got {other:?}"),
    }
}

// ── Unary operators ───────────────────────────────────────────

#[test]
fn unary_neg() {
    let (ast, errors) = parse_expr("-x");
    assert!(errors.is_empty());
    assert!(matches!(
        root_kind(&ast),
        ExprKind::UnaryOp(UnaryOp::Neg, _)
    ));
}

#[test]
fn unary_not() {
    let (ast, errors) = parse_expr("not b");
    assert!(errors.is_empty());
    assert!(matches!(
        root_kind(&ast),
        ExprKind::UnaryOp(UnaryOp::Not, _)
    ));
}

// ── Postfix ───────────────────────────────────────────────────

#[test]
fn call() {
    let (ast, errors) = parse_expr("f(x)");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::App(_, _)));
}

#[test]
fn field_access() {
    let (ast, errors) = parse_expr("point.x");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::Access { .. }));
}

#[test]
fn index() {
    let (ast, errors) = parse_expr("arr.[0]");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::Index { .. }));
}

#[test]
fn optional_chain() {
    let (ast, errors) = parse_expr("x?.field");
    assert!(errors.is_empty());
    assert!(matches!(
        root_kind(&ast),
        ExprKind::Access {
            mode: AccessMode::Optional,
            ..
        }
    ));
}

#[test]
fn force_unwrap() {
    let (ast, errors) = parse_expr("x!");
    assert!(errors.is_empty());
    assert!(matches!(
        root_kind(&ast),
        ExprKind::Postfix {
            op: PostfixOp::Force,
            ..
        }
    ));
}

#[test]
fn propagate() {
    let (ast, errors) = parse_expr("x?");
    assert!(errors.is_empty());
    assert!(matches!(
        root_kind(&ast),
        ExprKind::Postfix {
            op: PostfixOp::Propagate,
            ..
        }
    ));
}

// ── Paren forms ───────────────────────────────────────────────

#[test]
fn unit() {
    let (ast, errors) = parse_expr("()");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::TupleLit(items) if items.is_empty()));
}

#[test]
fn tuple() {
    let (ast, errors) = parse_expr("(a, b)");
    assert!(errors.is_empty());
    // Could be lambda params or tuple depending on whether => follows
    // Without =>, it's a tuple
    assert!(matches!(root_kind(&ast), ExprKind::TupleLit(items) if items.len() == 2));
}

#[test]
fn sequence() {
    let (ast, errors) = parse_expr("(a; b)");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::Seq(items) if items.len() == 2));
}

#[test]
fn piecewise() {
    let (ast, errors) = parse_expr("(x if cond | y if _)");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::Piecewise(arms) if arms.len() == 2));
}

#[test]
fn lambda() {
    let (ast, errors) = parse_expr("(x) => x + 1");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::Lambda { .. }));
}

// ── Arrays ────────────────────────────────────────────────────

#[test]
fn array_literal() {
    let (ast, errors) = parse_expr("[1, 2, 3]");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::ArrayLit(items) if items.len() == 3));
}

// ── Let binding ───────────────────────────────────────────────

#[test]
fn let_simple() {
    let (ast, errors) = parse_expr("let x := 42");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::Let(_)));
}

#[test]
fn let_with_sig() {
    let (ast, errors) = parse_expr("let f (x : Int) : Int := x + 1");
    assert!(errors.is_empty());
    match root_kind(&ast) {
        ExprKind::Let(binding) => {
            assert!(binding.sig.is_some());
            assert!(binding.value.is_some());
        }
        other => panic!("expected Let, got {other:?}"),
    }
}

// ── Case ──────────────────────────────────────────────────────

#[test]
fn case_expr() {
    let (ast, errors) = parse_expr("case x of (.Some(v) => v | .None => 0)");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::Case(data) if data.arms.len() == 2));
}

// ── Return / Resume / Need ────────────────────────────────────

#[test]
fn return_expr() {
    let (ast, errors) = parse_expr("return 42");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::Return(Some(_))));
}

#[test]
fn resume_expr() {
    let (ast, errors) = parse_expr("resume");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::Resume(None)));
}

#[test]
fn need_expr() {
    let (ast, errors) = parse_expr("need x");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::Need(_)));
}

// ── Import ────────────────────────────────────────────────────

#[test]
fn import_qualified() {
    let (ast, errors) = parse_expr("import \"path\" as name");
    assert!(errors.is_empty());
    assert!(matches!(
        root_kind(&ast),
        ExprKind::Import {
            kind: ImportKind::Qualified(_),
            ..
        }
    ));
}

// ── Variants ──────────────────────────────────────────────────

#[test]
fn variant_none() {
    let (ast, errors) = parse_expr(".None");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::VariantLit(_, args) if args.is_empty()));
}

#[test]
fn variant_with_args() {
    let (ast, errors) = parse_expr(".Some(x)");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::VariantLit(_, args) if args.len() == 1));
}

// ── Type defs ─────────────────────────────────────────────────

#[test]
fn data_product() {
    use music_ast::expr::DataBody;
    let (ast, errors) = parse_expr("data { x : Int; y : Int }");
    assert!(errors.is_empty());
    assert!(
        matches!(root_kind(&ast), ExprKind::DataDef(b) if matches!(b.as_ref(), DataBody::Product(fields) if fields.len() == 2))
    );
}

#[test]
fn data_sum() {
    use music_ast::expr::DataBody;
    let (ast, errors) = parse_expr("data { Some : T | None }");
    assert!(errors.is_empty());
    assert!(
        matches!(root_kind(&ast), ExprKind::DataDef(b) if matches!(b.as_ref(), DataBody::Sum(variants) if variants.len() == 2))
    );
}

// ── Error recovery ────────────────────────────────────────────

#[test]
fn unexpected_token_produces_error() {
    let (_ast, errors) = parse_from(");");
    assert!(!errors.is_empty());
}

// ── Assign ────────────────────────────────────────────────────

#[test]
fn assign() {
    let (ast, errors) = parse_expr("x <- 42");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::Assign(_, _)));
}

// ── Record literal ────────────────────────────────────────────

#[test]
fn record_literal() {
    let (ast, errors) = parse_expr(".{ x := 1, y := 2 }");
    assert!(errors.is_empty());
    assert!(matches!(root_kind(&ast), ExprKind::RecordLit(fields) if fields.len() == 2));
}

// ── Comprehension filter clauses ──────────────────────────────

#[test]
fn comprehension_with_filter() {
    let (ast, errors) = parse_expr("[x | x in xs, x > 0]");
    assert!(errors.is_empty(), "errors: {errors:?}");
    match root_kind(&ast) {
        ExprKind::Comprehension(data) => {
            assert_eq!(data.clauses.len(), 2);
            assert!(matches!(data.clauses[0], CompClause::Generator { .. }));
            assert!(matches!(data.clauses[1], CompClause::Filter(_)));
        }
        other => panic!("expected Comprehension, got {other:?}"),
    }
}

#[test]
fn comprehension_generator_only() {
    let (ast, errors) = parse_expr("[x * 2 | x in xs]");
    assert!(errors.is_empty(), "errors: {errors:?}");
    match root_kind(&ast) {
        ExprKind::Comprehension(data) => {
            assert_eq!(data.clauses.len(), 1);
            assert!(matches!(data.clauses[0], CompClause::Generator { .. }));
        }
        other => panic!("expected Comprehension, got {other:?}"),
    }
}

// ── Array spread ──────────────────────────────────────────────

#[test]
fn array_with_spread() {
    let (ast, errors) = parse_expr("[...a, b, ...c]");
    assert!(errors.is_empty(), "errors: {errors:?}");
    match root_kind(&ast) {
        ExprKind::ArrayLit(items) => {
            assert_eq!(items.len(), 3);
            assert!(matches!(
                ast.exprs.get(items[0]).kind,
                ExprKind::UnaryOp(UnaryOp::Spread, _)
            ));
            assert!(matches!(ast.exprs.get(items[1]).kind, ExprKind::Var(_)));
            assert!(matches!(
                ast.exprs.get(items[2]).kind,
                ExprKind::UnaryOp(UnaryOp::Spread, _)
            ));
        }
        other => panic!("expected ArrayLit, got {other:?}"),
    }
}

// ── Non-associative operators ─────────────────────────────────

#[test]
fn non_associative_comparison_rejected() {
    let (_, errors) = parse_expr("a = b = c");
    assert!(!errors.is_empty());
    assert!(matches!(
        errors[0].kind,
        ParseErrorKind::NonAssociativeChain
    ));
}

#[test]
fn non_associative_range_rejected() {
    let (_, errors) = parse_expr("1..2..3");
    assert!(!errors.is_empty());
    assert!(matches!(
        errors[0].kind,
        ParseErrorKind::NonAssociativeChain
    ));
}

#[test]
fn single_comparison_ok() {
    let (_, errors) = parse_expr("a = b");
    assert!(errors.is_empty(), "errors: {errors:?}");
}

#[test]
fn single_range_ok() {
    let (_, errors) = parse_expr("1..10");
    assert!(errors.is_empty(), "errors: {errors:?}");
}

// ── Matrix ────────────────────────────────────────────────────

#[test]
fn matrix_two_by_two() {
    let (ast, errors) = parse_expr("[1, 2; 3, 4]");
    assert!(errors.is_empty(), "errors: {errors:?}");
    match root_kind(&ast) {
        ExprKind::MatrixLit(rows) => {
            assert_eq!(rows.len(), 2);
            assert_eq!(rows[0].len(), 2);
            assert_eq!(rows[1].len(), 2);
        }
        other => panic!("expected MatrixLit, got {other:?}"),
    }
}

#[test]
fn matrix_three_by_one() {
    let (ast, errors) = parse_expr("[1; 2; 3]");
    assert!(errors.is_empty(), "errors: {errors:?}");
    match root_kind(&ast) {
        ExprKind::MatrixLit(rows) => {
            assert_eq!(rows.len(), 3);
            assert_eq!(rows[0].len(), 1);
        }
        other => panic!("expected MatrixLit, got {other:?}"),
    }
}

// ── Type test/cast named only ─────────────────────────────────

#[test]
fn type_test_named() {
    let (ast, errors) = parse_expr("x :? Int");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(matches!(root_kind(&ast), ExprKind::TypeOp { .. }));
}

#[test]
fn type_cast_named() {
    let (ast, errors) = parse_expr("x :?> String");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(matches!(root_kind(&ast), ExprKind::TypeOp { .. }));
}

// ── Trailing separators ───────────────────────────────────────

#[test]
fn data_sum_trailing_pipe() {
    use music_ast::expr::DataBody;
    let (ast, errors) = parse_expr("data { A | B | }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        matches!(root_kind(&ast), ExprKind::DataDef(b) if matches!(b.as_ref(), DataBody::Sum(variants) if variants.len() == 2))
    );
}

#[test]
fn data_product_leading_semi() {
    use music_ast::expr::DataBody;
    let (ast, errors) = parse_expr("data { ; x : Int ; y : Int }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        matches!(root_kind(&ast), ExprKind::DataDef(b) if matches!(b.as_ref(), DataBody::Product(fields) if fields.len() == 2))
    );
}

#[test]
fn data_product_leading_trailing_semi() {
    use music_ast::expr::DataBody;
    let (ast, errors) = parse_expr("data { ; x : Int ; }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        matches!(root_kind(&ast), ExprKind::DataDef(b) if matches!(b.as_ref(), DataBody::Product(fields) if fields.len() == 1))
    );
}

#[test]
fn data_variant_default() {
    use music_ast::expr::DataBody;
    let (ast, errors) = parse_expr("data { A : Int := 0 | B }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        matches!(root_kind(&ast), ExprKind::DataDef(b) if matches!(b.as_ref(), DataBody::Sum(variants) if variants.len() == 2 && variants[0].default.is_some()))
    );
}

#[test]
fn data_variant_default_no_payload() {
    use music_ast::expr::DataBody;
    let (ast, errors) = parse_expr("data { A := 1 | B }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        matches!(root_kind(&ast), ExprKind::DataDef(b) if matches!(b.as_ref(), DataBody::Sum(variants) if variants.len() == 2 && variants[0].default.is_some() && variants[0].payload.is_none()))
    );
}

#[test]
fn data_empty_braces_is_error() {
    let (_ast, errors) = parse_expr("data {}");
    assert!(!errors.is_empty(), "data {{}} should be a parse error");
}

#[test]
fn bracket_params_trailing_comma() {
    let (ast, errors) = parse_expr("let f [T, U,] := 42");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(matches!(root_kind(&ast), ExprKind::Let(_)));
}

#[test]
fn generic_function_type_params_before_params() {
    let (ast, errors) = parse_expr("let f [T] (x : T) : T := x");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(matches!(root_kind(&ast), ExprKind::Let(_)));
}

#[test]
fn where_trailing_comma() {
    let (_, errors) = parse_expr("let f [T] where T : Eq, := 42");
    assert!(errors.is_empty(), "errors: {errors:?}");
}

#[test]
fn import_selective_trailing_comma() {
    let (ast, errors) = parse_expr("import \"path\" as m.{a, b,}");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(matches!(
        root_kind(&ast),
        ExprKind::Import {
            kind: ImportKind::Selective(_, members),
            ..
        } if members.len() == 2
    ));
}

#[test]
fn comprehension_trailing_comma() {
    let (ast, errors) = parse_expr("[x | x in xs, x > 0,]");
    assert!(errors.is_empty(), "errors: {errors:?}");
    match root_kind(&ast) {
        ExprKind::Comprehension(data) => {
            assert_eq!(data.clauses.len(), 2);
        }
        other => panic!("expected Comprehension, got {other:?}"),
    }
}

// ── Leading pipe ──────────────────────────────────────────────

#[test]
fn case_leading_pipe() {
    let (ast, errors) = parse_expr("case x of (| .A => 1 | .B => 2)");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(matches!(root_kind(&ast), ExprKind::Case(data) if data.arms.len() == 2));
}

#[test]
fn case_trailing_pipe() {
    let (ast, errors) = parse_expr("case x of (.A => 1 | .B => 2 |)");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(matches!(root_kind(&ast), ExprKind::Case(data) if data.arms.len() == 2));
}

#[test]
fn case_leading_and_trailing_pipe() {
    let (ast, errors) = parse_expr("case x of (| .A => 1 | .B => 2 |)");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(matches!(root_kind(&ast), ExprKind::Case(data) if data.arms.len() == 2));
}

#[test]
fn piecewise_leading_pipe() {
    let (ast, errors) = parse_expr("(| x if .True | y if _)");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(matches!(root_kind(&ast), ExprKind::Piecewise(arms) if arms.len() == 2));
}

#[test]
fn piecewise_trailing_pipe() {
    let (ast, errors) = parse_expr("(x if .True | y if _ |)");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(matches!(root_kind(&ast), ExprKind::Piecewise(arms) if arms.len() == 2));
}

#[test]
fn piecewise_leading_and_trailing_pipe() {
    let (ast, errors) = parse_expr("(| x if .True | y if _ |)");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(matches!(root_kind(&ast), ExprKind::Piecewise(arms) if arms.len() == 2));
}
