use music_base::Source;
use music_syntax::{
    Lexer, SyntaxElement, SyntaxNode, SyntaxNodeKind, SyntaxToken, TokenKind, parse,
};

use super::model::{SemanticTokenSink, ToolSemanticTokenKind, ToolSemanticTokenList};
use super::ranges::push_span_tokens;

#[must_use]
pub fn semantic_syntax_tokens_for_source(source: &Source) -> ToolSemanticTokenList {
    let lexed = Lexer::new(source.text()).lex();
    let parsed = parse(lexed);
    let mut tokens = Vec::new();
    collect_syntax_type_tokens(source, parsed.tree().root(), false, &mut tokens);
    collect_apply_type_arg_tokens(source, parsed.tree().root(), &mut tokens);
    collect_attribute_name_tokens(source, parsed.tree().root(), &mut tokens);
    collect_variant_and_law_tokens(source, parsed.tree().root(), &mut tokens);
    tokens
}

pub(super) fn syntax_type_tokens(source: &Source) -> ToolSemanticTokenList {
    semantic_syntax_tokens_for_source(source)
}

fn collect_syntax_type_tokens(
    source: &Source,
    node: SyntaxNode<'_, '_>,
    is_type_context: bool,
    out: SemanticTokenSink<'_>,
) {
    let node_kind = node.kind();
    if node_kind == SyntaxNodeKind::Param {
        collect_param_tokens(source, node, out);
        return;
    }
    if node_kind == SyntaxNodeKind::TypeParam {
        collect_type_param_tokens(source, node, out);
        return;
    }
    if is_typed_context_node(node_kind) {
        collect_typed_context_tokens(source, node, out);
        return;
    }
    let type_context = is_type_context || node_kind.is_ty();
    for child in node.children() {
        match child {
            SyntaxElement::Node(child_node) => {
                collect_syntax_type_tokens(source, child_node, type_context, out);
            }
            SyntaxElement::Token(token) => {
                let Some(kind) = syntax_type_token_kind(token, type_context) else {
                    continue;
                };
                push_span_tokens(source, out, token.span(), kind, Vec::new());
            }
        }
    }
}

fn collect_typed_context_tokens(
    source: &Source,
    node: SyntaxNode<'_, '_>,
    out: SemanticTokenSink<'_>,
) {
    let mut after_colon = false;
    for child in node.children() {
        match child {
            SyntaxElement::Token(token) => match token.kind() {
                TokenKind::Colon | TokenKind::LtColon | TokenKind::TildeEq => after_colon = true,
                TokenKind::ColonEq
                | TokenKind::EqGt
                | TokenKind::KwWhere
                | TokenKind::KwRequire => {
                    after_colon = false;
                }
                _ => {}
            },
            SyntaxElement::Node(child_node) if after_colon => {
                collect_type_like_tokens(source, child_node, out);
            }
            SyntaxElement::Node(child_node) => {
                collect_syntax_type_tokens(source, child_node, false, out);
            }
        }
    }
}

const fn is_typed_context_node(kind: SyntaxNodeKind) -> bool {
    matches!(
        kind,
        SyntaxNodeKind::LetExpr
            | SyntaxNodeKind::Member
            | SyntaxNodeKind::LambdaExpr
            | SyntaxNodeKind::Constraint
            | SyntaxNodeKind::EffectItem
    )
}

fn collect_param_tokens(source: &Source, node: SyntaxNode<'_, '_>, out: SemanticTokenSink<'_>) {
    let mut after_colon = false;
    let mut after_bound = false;
    for child in node.children() {
        match child {
            SyntaxElement::Token(token) => match token.kind() {
                TokenKind::Ident if !after_colon && !after_bound => {
                    push_span_tokens(
                        source,
                        out,
                        token.span(),
                        ToolSemanticTokenKind::Parameter,
                        Vec::new(),
                    );
                }
                TokenKind::Colon => after_colon = true,
                TokenKind::ColonEq => after_bound = true,
                _ => {}
            },
            SyntaxElement::Node(child_node) if after_colon && !after_bound => {
                collect_type_like_tokens(source, child_node, out);
            }
            SyntaxElement::Node(child_node) if !after_bound => {
                collect_syntax_type_tokens(source, child_node, false, out);
            }
            SyntaxElement::Node(_) => {}
        }
    }
}

fn collect_type_param_tokens(
    source: &Source,
    node: SyntaxNode<'_, '_>,
    out: SemanticTokenSink<'_>,
) {
    let mut after_colon = false;
    let mut saw_name = false;
    for child in node.children() {
        match child {
            SyntaxElement::Token(token) => match token.kind() {
                TokenKind::Ident if !saw_name => {
                    saw_name = true;
                    push_span_tokens(
                        source,
                        out,
                        token.span(),
                        ToolSemanticTokenKind::TypeParameter,
                        Vec::new(),
                    );
                }
                TokenKind::Colon => after_colon = true,
                _ => {}
            },
            SyntaxElement::Node(child_node) if after_colon => {
                collect_type_like_tokens(source, child_node, out);
            }
            SyntaxElement::Node(child_node) => {
                collect_syntax_type_tokens(source, child_node, false, out);
            }
        }
    }
}

fn collect_apply_type_arg_tokens(
    source: &Source,
    node: SyntaxNode<'_, '_>,
    out: SemanticTokenSink<'_>,
) {
    if node.kind() == SyntaxNodeKind::ApplyExpr {
        let mut in_type_args = false;
        for child in node.children() {
            match child {
                SyntaxElement::Token(token) => match token.kind() {
                    TokenKind::LBracket => in_type_args = true,
                    TokenKind::RBracket => in_type_args = false,
                    _ => {}
                },
                SyntaxElement::Node(child_node) if in_type_args => {
                    collect_type_like_tokens(source, child_node, out);
                }
                SyntaxElement::Node(child_node) => {
                    collect_apply_type_arg_tokens(source, child_node, out);
                }
            }
        }
        return;
    }
    for child in node.children() {
        if let SyntaxElement::Node(child_node) = child {
            collect_apply_type_arg_tokens(source, child_node, out);
        }
    }
}

fn collect_type_like_tokens(source: &Source, node: SyntaxNode<'_, '_>, out: SemanticTokenSink<'_>) {
    for child in node.children() {
        match child {
            SyntaxElement::Node(child_node) => collect_type_like_tokens(source, child_node, out),
            SyntaxElement::Token(token)
                if matches!(
                    token.kind(),
                    TokenKind::Ident | TokenKind::KwAny | TokenKind::KwSome
                ) =>
            {
                push_span_tokens(
                    source,
                    out,
                    token.span(),
                    ToolSemanticTokenKind::Type,
                    Vec::new(),
                );
            }
            SyntaxElement::Token(_) => {}
        }
    }
}

fn collect_attribute_name_tokens(
    source: &Source,
    node: SyntaxNode<'_, '_>,
    out: SemanticTokenSink<'_>,
) {
    if node.kind() == SyntaxNodeKind::Attr {
        let mut in_name = false;
        for child in node.children() {
            match child {
                SyntaxElement::Token(token) => match token.kind() {
                    TokenKind::At => in_name = true,
                    TokenKind::Ident if in_name => push_span_tokens(
                        source,
                        out,
                        token.span(),
                        ToolSemanticTokenKind::Decorator,
                        Vec::new(),
                    ),
                    TokenKind::Dot if in_name => {}
                    _ => in_name = false,
                },
                SyntaxElement::Node(_) => in_name = false,
            }
        }
        return;
    }
    for child in node.children() {
        if let SyntaxElement::Node(child_node) = child {
            collect_attribute_name_tokens(source, child_node, out);
        }
    }
}

fn collect_variant_and_law_tokens(
    source: &Source,
    node: SyntaxNode<'_, '_>,
    out: SemanticTokenSink<'_>,
) {
    match node.kind() {
        SyntaxNodeKind::Variant | SyntaxNodeKind::VariantExpr | SyntaxNodeKind::VariantPat => {
            push_first_direct_ident(source, node, out, ToolSemanticTokenKind::EnumMember);
            return;
        }
        SyntaxNodeKind::Member => {
            push_law_name(source, node, out);
        }
        _ => {}
    }
    for child in node.children() {
        if let SyntaxElement::Node(child_node) = child {
            collect_variant_and_law_tokens(source, child_node, out);
        }
    }
}

fn push_first_direct_ident(
    source: &Source,
    node: SyntaxNode<'_, '_>,
    out: SemanticTokenSink<'_>,
    kind: ToolSemanticTokenKind,
) {
    for child in node.children() {
        if let SyntaxElement::Token(token) = child
            && token.kind() == TokenKind::Ident
        {
            push_span_tokens(source, out, token.span(), kind, Vec::new());
            return;
        }
    }
}

fn push_law_name(source: &Source, node: SyntaxNode<'_, '_>, out: SemanticTokenSink<'_>) {
    let mut after_law = false;
    for child in node.children() {
        if let SyntaxElement::Token(token) = child {
            match token.kind() {
                TokenKind::KwLaw => after_law = true,
                TokenKind::Ident if after_law => {
                    push_span_tokens(
                        source,
                        out,
                        token.span(),
                        ToolSemanticTokenKind::Function,
                        Vec::new(),
                    );
                    return;
                }
                _ => {}
            }
        }
    }
}

fn syntax_type_token_kind(
    token: SyntaxToken<'_, '_>,
    is_type_context: bool,
) -> Option<ToolSemanticTokenKind> {
    let kind = token.kind();
    if is_type_context
        && matches!(
            kind,
            TokenKind::Ident | TokenKind::KwAny | TokenKind::KwSome
        )
    {
        return Some(ToolSemanticTokenKind::Type);
    }
    if kind == TokenKind::Ident
        && token
            .text()
            .is_some_and(|text| builtin_type_name_token_kind(text).is_some())
    {
        return Some(ToolSemanticTokenKind::Type);
    }
    None
}

pub(super) fn builtin_type_name_token_kind(text: &str) -> Option<ToolSemanticTokenKind> {
    if is_builtin_type_name(text) {
        Some(ToolSemanticTokenKind::Type)
    } else {
        None
    }
}

fn is_builtin_type_name(text: &str) -> bool {
    matches!(
        text,
        "Type"
            | "Array"
            | "Any"
            | "Unknown"
            | "Syntax"
            | "Empty"
            | "Unit"
            | "Bool"
            | "Nat"
            | "Int"
            | "Int8"
            | "Int16"
            | "Int32"
            | "Int64"
            | "Nat8"
            | "Nat16"
            | "Nat32"
            | "Nat64"
            | "Float"
            | "Float32"
            | "Float64"
            | "String"
            | "Rune"
            | "Range"
            | "Pin"
            | "CString"
            | "CPtr"
            | "Rangeable"
            | "RangeBounds"
    )
}
