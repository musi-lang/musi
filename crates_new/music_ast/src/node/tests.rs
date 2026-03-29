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

    let alloc_node = |nodes: &mut Arena<SyntaxNodeData>, kind: SyntaxNodeKind| {
        nodes.alloc(SyntaxNodeData {
            kind,
            span: Span::new(0, 1),
            children: SmallVec::default(),
        })
    };

    let mut ids = vec![];
    ids.push(nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::Attr,
        span: Span::new(0, 1),
        children: iter::once(SyntaxElementId::Token(token_id)).collect(),
    }));
    for kind in [
        SyntaxNodeKind::Param,
        SyntaxNodeKind::Field,
        SyntaxNodeKind::Variant,
        SyntaxNodeKind::TypeParam,
        SyntaxNodeKind::Constraint,
        SyntaxNodeKind::ArrayItem,
        SyntaxNodeKind::RecordItem,
        SyntaxNodeKind::EffectSet,
        SyntaxNodeKind::EffectItem,
        SyntaxNodeKind::Arg,
        SyntaxNodeKind::HandlerClause,
        SyntaxNodeKind::Member,
    ] {
        ids.push(alloc_node(&mut nodes, kind));
    }

    let root_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::SourceFile,
        span: Span::new(0, 1),
        children: ids.into_iter().map(SyntaxElementId::Node).collect(),
    });
    let tree = SyntaxTree::new(source_id, nodes, tokens, root_id);
    let mut nodes = tree.root().child_nodes();

    assert!(Attr::cast(nodes.next().expect("attr")).is_some());
    assert!(Param::cast(nodes.next().expect("param")).is_some());
    assert!(Field::cast(nodes.next().expect("field")).is_some());
    assert!(Variant::cast(nodes.next().expect("variant")).is_some());
    assert!(TypeParam::cast(nodes.next().expect("type param")).is_some());
    assert!(Constraint::cast(nodes.next().expect("constraint")).is_some());
    assert!(ArrayItem::cast(nodes.next().expect("array item")).is_some());
    assert!(RecordItem::cast(nodes.next().expect("record item")).is_some());
    assert!(EffectSet::cast(nodes.next().expect("effect set")).is_some());
    assert!(EffectItem::cast(nodes.next().expect("effect item")).is_some());
    assert!(Arg::cast(nodes.next().expect("arg")).is_some());
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
fn test_field_expression_uses_chain_flags() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "x?.y");

    let mut nodes = Arena::new();
    let mut tokens = Arena::new();
    let access_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::QuestionDot,
        span: Span::new(1, 3),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let expr_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::FieldExpr,
        span: Span::new(0, 4),
        children: iter::once(SyntaxElementId::Token(access_id)).collect(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, expr_id);
    let expr = Expr::cast(tree.root()).expect("field expression");

    assert_eq!(expr.kind(), ExprKindView::Field);
    assert!(expr.is_optional_chain());
    assert!(!expr.is_forced_chain());
}

#[test]
fn test_decl_surface_reports_modifiers_and_external_abi() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "export foreign \"c\" let mut x := y");

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
        span: Span::new(23, 26),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let foreign_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::KwForeign,
        span: Span::new(7, 14),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let abi_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::StringLit,
        span: Span::new(15, 18),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let expr_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::LetExpr,
        span: Span::new(0, 32),
        children: [
            SyntaxElementId::Token(export_id),
            SyntaxElementId::Token(foreign_id),
            SyntaxElementId::Token(abi_id),
            SyntaxElementId::Token(mut_id),
        ]
        .into_iter()
        .collect(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, expr_id);
    let expr = Expr::cast(tree.root()).expect("let expression");
    let decl = expr.decl_surface().expect("declaration surface");

    assert!(decl.is_exported());
    assert!(decl.is_external());
    assert!(decl.is_mutable());
    assert!(!decl.is_opaque());
    assert!(matches!(
        decl.external_abi_token().map(crate::SyntaxToken::kind),
        Some(TokenKind::StringLit)
    ));
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
fn test_binary_type_uses_operator_enum() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "A + B");

    let mut nodes = Arena::new();
    let mut tokens = Arena::new();
    let plus_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::Plus,
        span: Span::new(2, 3),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let ty_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::BinaryTy,
        span: Span::new(0, 5),
        children: iter::once(SyntaxElementId::Token(plus_id)).collect(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, ty_id);
    let ty = Ty::cast(tree.root()).expect("binary type");

    assert_eq!(ty.kind(), TyKindView::Binary);
    assert_eq!(ty.binary_op(), Some(BinaryTyOp::Sum));
}

#[test]
fn test_quote_expression_reports_block_flag() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "quote { x; }");

    let mut nodes = Arena::new();
    let mut tokens = Arena::new();
    let lbrace_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::LBrace,
        span: Span::new(6, 7),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let expr_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::QuoteExpr,
        span: Span::new(0, 12),
        children: iter::once(SyntaxElementId::Token(lbrace_id)).collect(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, expr_id);
    let expr = Expr::cast(tree.root()).expect("quote expression");

    assert_eq!(expr.kind(), ExprKindView::Quote);
    assert!(expr.is_block_quote());
}

#[test]
fn test_array_and_record_items_report_flags() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "...x, field := value");

    let mut nodes = Arena::new();
    let mut tokens = Arena::new();
    let spread_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::DotDotDot,
        span: Span::new(0, 3),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let assign_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::ColonEq,
        span: Span::new(12, 14),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let array_item_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::ArrayItem,
        span: Span::new(0, 5),
        children: iter::once(SyntaxElementId::Token(spread_id)).collect(),
    });
    let record_item_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::RecordItem,
        span: Span::new(6, 20),
        children: iter::once(SyntaxElementId::Token(assign_id)).collect(),
    });
    let root_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::SourceFile,
        span: Span::new(0, 20),
        children: [
            SyntaxElementId::Node(array_item_id),
            SyntaxElementId::Node(record_item_id),
        ]
        .into_iter()
        .collect(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, root_id);
    let mut children = tree.root().child_nodes();
    let array_item = ArrayItem::cast(children.next().expect("array item")).expect("array item");
    let record_item = RecordItem::cast(children.next().expect("record item")).expect("record item");

    assert!(array_item.is_spread());
    assert!(!record_item.is_spread());
    assert!(record_item.has_value());
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
