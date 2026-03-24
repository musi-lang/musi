use music_ast::data::AstData;
use music_ast::expr::{BinOp, ExprKind};
use music_found::Interner;
use music_lex::Lexer;
use music_parse::parse;

use super::lower;

fn parse_and_lower(source: &str) -> AstData {
    let mut interner = Interner::new();
    let (tokens, _) = Lexer::new(source).lex();
    let (mut ast, errors) = parse(&tokens, source, &mut interner);
    assert!(errors.is_empty(), "parse errors: {errors:?}");
    lower(&mut ast);
    ast
}

fn root_kind(ast: &AstData) -> &ExprKind {
    assert!(!ast.root.is_empty(), "AST has no root expressions");
    &ast.exprs.get(ast.root[0]).kind
}

#[test]
fn piecewise_two_arms() {
    let ast = parse_and_lower("(1 if .True | 2 if _);");
    let kind = root_kind(&ast);
    assert!(
        matches!(kind, ExprKind::Branch { .. }),
        "expected Branch, got {kind:?}"
    );
    if let ExprKind::Branch {
        cond,
        then_br,
        else_br,
    } = kind
    {
        assert!(matches!(
            ast.exprs.get(*cond).kind,
            ExprKind::VariantLit(_, _)
        ));
        assert!(matches!(
            ast.exprs.get(*then_br).kind,
            ExprKind::Lit(music_found::Literal::Int(1))
        ));
        assert!(matches!(
            ast.exprs.get(*else_br).kind,
            ExprKind::Lit(music_found::Literal::Int(2))
        ));
    }
}

#[test]
fn piecewise_three_arms() {
    let ast = parse_and_lower("(1 if a | 2 if b | 3 if _);");
    let kind = root_kind(&ast);
    assert!(
        matches!(kind, ExprKind::Branch { .. }),
        "expected outer Branch, got {kind:?}"
    );
    if let ExprKind::Branch { else_br, .. } = kind {
        let inner = &ast.exprs.get(*else_br).kind;
        assert!(
            matches!(inner, ExprKind::Branch { .. }),
            "expected nested Branch, got {inner:?}"
        );
        if let ExprKind::Branch {
            else_br: inner_else,
            ..
        } = inner
        {
            assert!(matches!(
                ast.exprs.get(*inner_else).kind,
                ExprKind::Lit(music_found::Literal::Int(3))
            ));
        }
    }
}

#[test]
fn piecewise_wildcard_last() {
    let ast = parse_and_lower("(42 if .True | 99 if _);");
    let kind = root_kind(&ast);
    if let ExprKind::Branch { else_br, .. } = kind {
        assert!(matches!(
            ast.exprs.get(*else_br).kind,
            ExprKind::Lit(music_found::Literal::Int(99))
        ));
    } else {
        panic!("expected Branch, got {kind:?}");
    }
}

#[test]
fn pipe_right_to_app() {
    let ast = parse_and_lower("x |> f;");
    let kind = root_kind(&ast);
    assert!(
        matches!(kind, ExprKind::App(_, args) if args.len() == 1),
        "expected App with 1 arg, got {kind:?}"
    );
    if let ExprKind::App(callee, args) = kind {
        assert!(matches!(ast.exprs.get(*callee).kind, ExprKind::Var(_)));
        assert!(matches!(ast.exprs.get(args[0]).kind, ExprKind::Var(_)));
    }
}

#[test]
fn chained_pipe() {
    let ast = parse_and_lower("x |> f |> g;");
    let kind = root_kind(&ast);
    assert!(
        matches!(kind, ExprKind::App(_, _)),
        "expected outer App, got {kind:?}"
    );
    if let ExprKind::App(callee, args) = kind {
        assert!(matches!(ast.exprs.get(*callee).kind, ExprKind::Var(_)));
        let inner = &ast.exprs.get(args[0]).kind;
        assert!(
            matches!(inner, ExprKind::App(_, _)),
            "expected inner App, got {inner:?}"
        );
    }
}

#[test]
fn no_op_unchanged() {
    let mut interner = Interner::new();
    let source = "1 + 2;";
    let (tokens, _) = Lexer::new(source).lex();
    let (mut ast, _) = parse(&tokens, source, &mut interner);
    let root_before = ast.root.clone();
    let arena_len_before = ast.exprs.len();
    lower(&mut ast);
    assert_eq!(ast.root, root_before);
    assert_eq!(ast.exprs.len(), arena_len_before);
}

#[test]
fn nested_piecewise() {
    let source = "((1 if .True | 2 if _) if x | 3 if _);";
    let ast = parse_and_lower(source);
    let kind = root_kind(&ast);
    assert!(
        matches!(kind, ExprKind::Branch { .. }),
        "expected outer Branch, got {kind:?}"
    );
    if let ExprKind::Branch { then_br, .. } = kind {
        let inner = &ast.exprs.get(*then_br).kind;
        assert!(
            matches!(inner, ExprKind::Branch { .. }),
            "expected inner Branch from nested piecewise, got {inner:?}"
        );
    }
}

#[test]
fn piecewise_non_exhaustive_last_guard() {
    let ast = parse_and_lower("(1 if a | 2 if b);");
    let kind = root_kind(&ast);
    assert!(
        matches!(kind, ExprKind::Branch { .. }),
        "expected Branch, got {kind:?}"
    );
    if let ExprKind::Branch { else_br, .. } = kind {
        let inner = &ast.exprs.get(*else_br).kind;
        if let ExprKind::Branch {
            then_br, else_br, ..
        } = inner
        {
            assert!(matches!(
                ast.exprs.get(*then_br).kind,
                ExprKind::Lit(music_found::Literal::Int(2))
            ));
            assert!(
                matches!(&ast.exprs.get(*else_br).kind, ExprKind::TupleLit(elems) if elems.is_empty()),
                "non-exhaustive piecewise should have unit fallback"
            );
        } else {
            panic!("expected nested Branch for non-exhaustive piecewise, got {inner:?}");
        }
    }
}

#[test]
fn pipe_inside_piecewise() {
    let ast = parse_and_lower("(x |> f if .True | y if _);");
    let kind = root_kind(&ast);
    assert!(
        matches!(kind, ExprKind::Branch { .. }),
        "expected Branch, got {kind:?}"
    );
    if let ExprKind::Branch { then_br, .. } = kind {
        let inner = &ast.exprs.get(*then_br).kind;
        assert!(
            matches!(inner, ExprKind::App(_, _)),
            "pipe inside piecewise should lower to App, got {inner:?}"
        );
    }
}

#[test]
fn binop_preserved() {
    let ast = parse_and_lower("1 + 2;");
    let kind = root_kind(&ast);
    assert!(matches!(kind, ExprKind::BinOp(BinOp::Add, _, _)));
}
