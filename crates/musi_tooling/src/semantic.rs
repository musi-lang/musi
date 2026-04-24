use std::cmp::Reverse;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::Path;

use music_base::{Source, SourceMap, Span};
use music_hir::{HirExprKind, HirTyKind};
use music_names::{NameBindingId, NameBindingKind};
use music_sema::{ExprMemberFact, SemaModule};
use music_syntax::{
    Lexer, SyntaxElement, SyntaxNode, SyntaxNodeKind, SyntaxToken, TokenKind, parse,
};

use crate::{
    analysis::{ToolMemberShape, ToolRange, member_class, tool_range},
    analysis_support::analysis_session,
};

pub type ToolSemanticModifierList = Vec<ToolSemanticModifier>;
pub type ToolSemanticTokenList = Vec<ToolSemanticToken>;
type SemanticTokenSink<'a> = &'a mut ToolSemanticTokenList;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ToolSemanticTokenKind {
    Namespace = 0,
    Type = 1,
    TypeParameter = 2,
    Parameter = 3,
    Variable = 4,
    Property = 5,
    EnumMember = 6,
    Function = 7,
    Procedure = 8,
    Macro = 9,
    Keyword = 10,
    Modifier = 11,
    Comment = 12,
    String = 13,
    Number = 14,
    Operator = 15,
    Decorator = 16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ToolSemanticModifier {
    Declaration = 0,
    Definition = 1,
    Readonly = 2,
    Static = 3,
    Deprecated = 4,
    Documentation = 5,
    DefaultLibrary = 6,
    Modification = 7,
    Module = 8,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolSemanticToken {
    pub range: ToolRange,
    pub kind: ToolSemanticTokenKind,
    pub modifiers: ToolSemanticModifierList,
}

impl ToolSemanticToken {
    #[must_use]
    pub const fn new(
        range: ToolRange,
        kind: ToolSemanticTokenKind,
        modifiers: ToolSemanticModifierList,
    ) -> Self {
        Self {
            range,
            kind,
            modifiers,
        }
    }
}

#[must_use]
pub fn semantic_tokens_for_project_file(path: &Path) -> ToolSemanticTokenList {
    semantic_tokens_for_project_file_with_overlay(path, None)
}

#[must_use]
pub fn semantic_tokens_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
) -> ToolSemanticTokenList {
    let Some((session, module_key)) = analysis_session(path, overlay_text) else {
        return lexical_and_syntax_tokens(path, overlay_text);
    };
    let Some(parsed) = session.parsed_module_cached(&module_key).ok().flatten() else {
        return lexical_and_syntax_tokens(path, overlay_text);
    };
    let Some(source) = session.source(parsed.source_id) else {
        return lexical_and_syntax_tokens(path, overlay_text);
    };
    let mut tokens = Vec::new();
    let mut syntax_tokens = syntax_type_tokens(source);
    if let Some(resolved) = session.resolved_module_cached(&module_key).ok().flatten() {
        let sema = session.sema_module_cached(&module_key).ok().flatten();
        let callable_binding_ranges = resolved
            .names
            .bindings
            .iter()
            .filter(|(binding_id, binding)| {
                binding.site.source_id == parsed.source_id
                    && binding_token_kind(*binding_id, binding.kind, sema).is_callable()
            })
            .map(|(_, binding)| range_key(source, binding.site.span))
            .collect::<BTreeSet<_>>();
        syntax_tokens
            .retain(|token| !callable_binding_ranges.contains(&tool_range_key(token.range)));
        let syntax_priorities = token_range_priorities(&syntax_tokens);
        tokens.extend(syntax_tokens);
        for (binding_id, binding) in &resolved.names.bindings {
            if binding.site.source_id != parsed.source_id {
                continue;
            }
            let token_kind = binding_token_kind(binding_id, binding.kind, sema);
            if syntax_priorities
                .get(&range_key(source, binding.site.span))
                .is_some_and(|priority| *priority >= token_priority(token_kind))
            {
                continue;
            }
            tokens.push(binding_token(
                source,
                binding.site.span,
                binding_id,
                binding.kind,
                sema,
                true,
            ));
        }
        for (site, binding_id) in &resolved.names.refs {
            if site.source_id != parsed.source_id {
                continue;
            }
            let binding = resolved.names.bindings.get(*binding_id);
            let token_kind = binding_token_kind(*binding_id, binding.kind, sema);
            if syntax_priorities
                .get(&range_key(source, site.span))
                .is_some_and(|priority| *priority >= token_priority(token_kind))
            {
                continue;
            }
            tokens.push(binding_token(
                source,
                site.span,
                *binding_id,
                binding.kind,
                sema,
                false,
            ));
        }
        if let Some(sema) = sema {
            tokens.extend(member_tokens(source, sema));
        }
    } else {
        tokens.extend(syntax_tokens);
    }
    normalize_tokens(tokens)
}

fn member_tokens(source: &Source, sema: &SemaModule) -> ToolSemanticTokenList {
    sema.module()
        .store
        .exprs
        .iter()
        .filter_map(|(expr_id, expr)| {
            let HirExprKind::Field { name, .. } = expr.kind else {
                return None;
            };
            let fact = sema.expr_member_fact(expr_id)?;
            Some(ToolSemanticToken::new(
                tool_range(source, name.span),
                member_token_kind(sema, fact),
                Vec::new(),
            ))
        })
        .collect()
}

fn lexical_and_syntax_tokens(path: &Path, overlay_text: Option<&str>) -> ToolSemanticTokenList {
    let source_text = overlay_text
        .map(str::to_owned)
        .or_else(|| fs::read_to_string(path).ok())
        .unwrap_or_default();
    let mut sources = SourceMap::new();
    let Ok(source_id) = sources.add(path.to_path_buf(), source_text) else {
        return Vec::new();
    };
    let Some(source) = sources.get(source_id) else {
        return Vec::new();
    };
    let mut tokens = Vec::new();
    tokens.extend(syntax_type_tokens(source));
    normalize_tokens(tokens)
}

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

fn syntax_type_tokens(source: &Source) -> ToolSemanticTokenList {
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

pub fn builtin_type_name_token_kind(text: &str) -> Option<ToolSemanticTokenKind> {
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

type SemanticRangeKey = (usize, usize, usize, usize);

fn token_range_priorities(tokens: &[ToolSemanticToken]) -> BTreeMap<SemanticRangeKey, u8> {
    let mut priorities: BTreeMap<SemanticRangeKey, u8> = BTreeMap::new();
    for token in tokens {
        let key = tool_range_key(token.range);
        let priority = token_priority(token.kind);
        let _ = priorities
            .entry(key)
            .and_modify(|stored| *stored = (*stored).max(priority))
            .or_insert(priority);
    }
    priorities
}

fn range_key(source: &Source, span: Span) -> SemanticRangeKey {
    tool_range_key(tool_range(source, span))
}

const fn tool_range_key(range: ToolRange) -> SemanticRangeKey {
    (
        range.start_line,
        range.start_col,
        range.end_line,
        range.end_col,
    )
}

fn push_span_tokens(
    source: &Source,
    out: SemanticTokenSink<'_>,
    span: Span,
    kind: ToolSemanticTokenKind,
    modifiers: ToolSemanticModifierList,
) {
    let range = tool_range(source, span);
    if range.start_line == range.end_line {
        if range.start_col < range.end_col {
            out.push(ToolSemanticToken::new(range, kind, modifiers));
        }
        return;
    }
    for line in range.start_line..=range.end_line {
        let Some(line_text) = source.line_text(line) else {
            continue;
        };
        let start_col = if line == range.start_line {
            range.start_col
        } else {
            1
        };
        let end_col = if line == range.end_line {
            range.end_col
        } else {
            line_text.chars().count().saturating_add(1)
        };
        if start_col < end_col {
            out.push(ToolSemanticToken::new(
                ToolRange::new(line, start_col, line, end_col),
                kind,
                modifiers.clone(),
            ));
        }
    }
}

fn binding_token(
    source: &Source,
    span: Span,
    binding_id: NameBindingId,
    kind: NameBindingKind,
    sema: Option<&SemaModule>,
    is_definition: bool,
) -> ToolSemanticToken {
    let mut modifiers = Vec::new();
    if is_definition {
        modifiers.push(ToolSemanticModifier::Declaration);
        modifiers.push(ToolSemanticModifier::Definition);
    }
    if kind == NameBindingKind::Prelude {
        modifiers.push(ToolSemanticModifier::DefaultLibrary);
    }
    ToolSemanticToken::new(
        tool_range(source, span),
        binding_token_kind(binding_id, kind, sema),
        modifiers,
    )
}

fn binding_token_kind(
    binding_id: NameBindingId,
    kind: NameBindingKind,
    sema: Option<&SemaModule>,
) -> ToolSemanticTokenKind {
    match kind {
        NameBindingKind::Param
        | NameBindingKind::PiBinder
        | NameBindingKind::HandleClauseParam
        | NameBindingKind::HandleClauseResult => ToolSemanticTokenKind::Parameter,
        NameBindingKind::TypeParam => ToolSemanticTokenKind::TypeParameter,
        NameBindingKind::Prelude
        | NameBindingKind::Import
        | NameBindingKind::Let
        | NameBindingKind::Pin
        | NameBindingKind::AttachedMethod
        | NameBindingKind::PatternBind => sema
            .and_then(|module| {
                module
                    .binding_type(binding_id)
                    .map(|ty| &module.ty(ty).kind)
            })
            .map_or(ToolSemanticTokenKind::Variable, |ty| match ty {
                HirTyKind::Arrow { .. } | HirTyKind::Pi { .. } => ToolSemanticTokenKind::Function,
                HirTyKind::Type => ToolSemanticTokenKind::Type,
                _ => ToolSemanticTokenKind::Variable,
            }),
    }
}

fn member_token_kind(sema: &SemaModule, fact: &ExprMemberFact) -> ToolSemanticTokenKind {
    match member_class(sema, fact) {
        ToolMemberShape::Function => ToolSemanticTokenKind::Function,
        ToolMemberShape::Procedure => ToolSemanticTokenKind::Procedure,
        ToolMemberShape::Property => ToolSemanticTokenKind::Property,
        ToolMemberShape::Type => ToolSemanticTokenKind::Type,
    }
}

impl ToolSemanticTokenKind {
    const fn is_callable(self) -> bool {
        matches!(self, Self::Function | Self::Procedure)
    }
}

const fn token_priority(kind: ToolSemanticTokenKind) -> u8 {
    match kind {
        ToolSemanticTokenKind::Function | ToolSemanticTokenKind::Procedure => 80,
        ToolSemanticTokenKind::Parameter => 70,
        ToolSemanticTokenKind::EnumMember | ToolSemanticTokenKind::Decorator => 60,
        ToolSemanticTokenKind::TypeParameter => 50,
        ToolSemanticTokenKind::Type => 40,
        ToolSemanticTokenKind::Namespace | ToolSemanticTokenKind::Property => 30,
        ToolSemanticTokenKind::Variable => 10,
        _ => 0,
    }
}

fn normalize_tokens(mut tokens: ToolSemanticTokenList) -> ToolSemanticTokenList {
    tokens.sort_by_key(|token| {
        (
            token.range.start_line,
            token.range.start_col,
            token.range.end_line,
            token.range.end_col,
            Reverse(token_priority(token.kind)),
        )
    });
    let mut out = Vec::new();
    let mut last_line = 0usize;
    let mut last_col = 0usize;
    for token in tokens {
        if token.range.start_line < last_line
            || (token.range.start_line == last_line && token.range.start_col < last_col)
        {
            continue;
        }
        last_line = token.range.end_line;
        last_col = token.range.end_col;
        out.push(token);
    }
    out
}
