use music_basic::{SourceMap, Span};
use music_lex::{TokenKind, Trivias};
use music_storage::Arena;
use smallvec::SmallVec;
use std::iter;

use crate::{SyntaxElementId, SyntaxNodeData, SyntaxNodeKind, SyntaxTokenData, SyntaxTree};

use super::*;

#[test]
fn test_source_file_filters_expression_children() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "instance X; let _ := instance X");

    let mut nodes = Arena::new();
    let tokens = Arena::new();

    let first_expr = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::InstanceExpr,
        span: Span::new(0, 10),
        children: SmallVec::default(),
    });
    let second_expr = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::LetExpr,
        span: Span::new(12, 31),
        children: SmallVec::default(),
    });
    let root_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::SourceFile,
        span: Span::new(0, 31),
        children: [
            SyntaxElementId::Node(first_expr),
            SyntaxElementId::Node(second_expr),
        ]
        .into_iter()
        .collect(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, root_id);
    let root = SourceFile::cast(tree.root()).expect("source file");
    let kinds: Vec<_> = root.expressions().map(Expr::kind).collect();

    assert_eq!(kinds, vec![ExprKindView::Instance, ExprKindView::Let]);
}

#[test]
fn test_expr_kind_view_maps_instance_expression() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "instance X");

    let mut nodes = Arena::new();
    let tokens = Arena::new();
    let expr_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::InstanceExpr,
        span: Span::new(0, 10),
        children: SmallVec::default(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, expr_id);
    let expr = Expr::cast(tree.root()).expect("expression");

    assert_eq!(expr.kind(), ExprKindView::Instance);
}

#[test]
fn test_pattern_and_type_wrappers_cast_from_syntax_node() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "_");

    let mut nodes = Arena::new();
    let tokens = Arena::new();
    let pat_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::WildcardPat,
        span: Span::new(0, 1),
        children: SmallVec::default(),
    });
    let ty_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::FunctionTy,
        span: Span::new(0, 1),
        children: SmallVec::default(),
    });
    let root_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::SourceFile,
        span: Span::new(0, 1),
        children: [SyntaxElementId::Node(pat_id), SyntaxElementId::Node(ty_id)]
            .into_iter()
            .collect(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, root_id);
    let mut children = tree.root().child_nodes();
    let pat = Pat::cast(children.next().expect("pattern node")).expect("pattern");
    let ty = Ty::cast(children.next().expect("type node")).expect("type");

    assert_eq!(pat.kind(), PatKindView::Wildcard);
    assert_eq!(ty.kind(), TyKindView::Function);
}

#[test]
fn test_support_wrappers_cast_exact_kinds() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "@a");

    let mut nodes = Arena::new();
    let mut tokens = Arena::new();
    let token_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::At,
        span: Span::new(0, 1),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });

    let ids = [
        nodes.alloc(SyntaxNodeData {
            kind: SyntaxNodeKind::Attr,
            span: Span::new(0, 1),
            children: iter::once(SyntaxElementId::Token(token_id)).collect(),
        }),
        nodes.alloc(SyntaxNodeData {
            kind: SyntaxNodeKind::Param,
            span: Span::new(0, 1),
            children: SmallVec::default(),
        }),
        nodes.alloc(SyntaxNodeData {
            kind: SyntaxNodeKind::Field,
            span: Span::new(0, 1),
            children: SmallVec::default(),
        }),
        nodes.alloc(SyntaxNodeData {
            kind: SyntaxNodeKind::Variant,
            span: Span::new(0, 1),
            children: SmallVec::default(),
        }),
        nodes.alloc(SyntaxNodeData {
            kind: SyntaxNodeKind::TypeParam,
            span: Span::new(0, 1),
            children: SmallVec::default(),
        }),
        nodes.alloc(SyntaxNodeData {
            kind: SyntaxNodeKind::Constraint,
            span: Span::new(0, 1),
            children: SmallVec::default(),
        }),
        nodes.alloc(SyntaxNodeData {
            kind: SyntaxNodeKind::HandlerClause,
            span: Span::new(0, 1),
            children: SmallVec::default(),
        }),
        nodes.alloc(SyntaxNodeData {
            kind: SyntaxNodeKind::Member,
            span: Span::new(0, 1),
            children: SmallVec::default(),
        }),
    ];

    let root_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::SourceFile,
        span: Span::new(0, 1),
        children: ids.iter().copied().map(SyntaxElementId::Node).collect(),
    });
    let tree = SyntaxTree::new(source_id, nodes, tokens, root_id);
    let mut nodes = tree.root().child_nodes();

    assert!(Attr::cast(nodes.next().expect("attr")).is_some());
    assert!(Param::cast(nodes.next().expect("param")).is_some());
    assert!(Field::cast(nodes.next().expect("field")).is_some());
    assert!(Variant::cast(nodes.next().expect("variant")).is_some());
    assert!(TypeParam::cast(nodes.next().expect("type param")).is_some());
    assert!(Constraint::cast(nodes.next().expect("constraint")).is_some());
    assert!(HandlerClause::cast(nodes.next().expect("handler clause")).is_some());
    assert!(Member::cast(nodes.next().expect("member")).is_some());
}

#[test]
fn test_binary_expression_uses_operator_enum() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "x |> f");

    let mut nodes = Arena::new();
    let mut tokens = Arena::new();
    let pipe_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::PipeGt,
        span: Span::new(2, 4),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let expr_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::BinaryExpr,
        span: Span::new(0, 6),
        children: iter::once(SyntaxElementId::Token(pipe_id)).collect(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, expr_id);
    let expr = Expr::cast(tree.root()).expect("binary expression");

    assert_eq!(expr.kind(), ExprKindView::Binary);
    assert_eq!(expr.binary_op(), Some(BinaryExprOp::Pipe));
}

#[test]
fn test_let_expression_uses_modifier_accessors() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "export let mut x := y");

    let mut nodes = Arena::new();
    let mut tokens = Arena::new();
    let export_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::KwExport,
        span: Span::new(0, 6),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let mut_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::KwMut,
        span: Span::new(11, 14),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let expr_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::LetExpr,
        span: Span::new(0, 20),
        children: [
            SyntaxElementId::Token(export_id),
            SyntaxElementId::Token(mut_id),
        ]
        .into_iter()
        .collect(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, expr_id);
    let expr = Expr::cast(tree.root()).expect("let expression");

    assert!(expr.is_exported());
    assert!(expr.is_mutable());
}

#[test]
fn test_named_type_uses_mutability_and_function_flavor_helpers() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "mut Foo -> Bar");

    let mut nodes = Arena::new();
    let mut tokens = Arena::new();
    let mut_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::KwMut,
        span: Span::new(0, 3),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let arrow_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::MinusGt,
        span: Span::new(8, 10),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let named_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::NamedTy,
        span: Span::new(0, 7),
        children: iter::once(SyntaxElementId::Token(mut_id)).collect(),
    });
    let function_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::FunctionTy,
        span: Span::new(0, 14),
        children: iter::once(SyntaxElementId::Token(arrow_id)).collect(),
    });
    let root_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::SourceFile,
        span: Span::new(0, 14),
        children: [
            SyntaxElementId::Node(named_id),
            SyntaxElementId::Node(function_id),
        ]
        .into_iter()
        .collect(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, root_id);
    let mut children = tree.root().child_nodes();
    let named = Ty::cast(children.next().expect("named type")).expect("named type");
    let function = Ty::cast(children.next().expect("function type")).expect("function type");

    assert_eq!(named.kind(), TyKindView::Named);
    assert!(named.is_mutable());
    assert_eq!(function.kind(), TyKindView::Function);
    assert_eq!(function.function_flavor(), Some(FunctionTyFlavor::Pure));
}

#[test]
fn test_member_kind_is_token_driven() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "law x := x");

    let mut nodes = Arena::new();
    let mut tokens = Arena::new();
    let law_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::KwLaw,
        span: Span::new(0, 3),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let member_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::Member,
        span: Span::new(0, 10),
        children: iter::once(SyntaxElementId::Token(law_id)).collect(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, member_id);
    let member = Member::cast(tree.root()).expect("member");

    assert_eq!(member.kind(), Some(MemberKind::Law));
}
