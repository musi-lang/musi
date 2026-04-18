use std::collections::BTreeSet;
use std::fs;
use std::path::Path;

use music_base::{Source, SourceMap, Span};
use music_hir::{HirExprKind, HirTyKind};
use music_names::{NameBindingId, NameBindingKind};
use music_sema::{ExprMemberFact, SemaModule};
use music_syntax::{
    LexedSource, Lexer, SyntaxElement, SyntaxNode, SyntaxNodeKind, TokenKind, TriviaKind, parse,
};

use crate::{
    analysis::{ToolMemberClass, ToolRange, member_class, tool_range},
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
    let mut tokens = lexical_tokens(source);
    let syntax_tokens = syntax_type_tokens(source);
    let syntax_type_ranges = token_ranges(&syntax_tokens);
    tokens.extend(syntax_tokens);
    if let Some(resolved) = session.resolved_module_cached(&module_key).ok().flatten() {
        let sema = session.sema_module_cached(&module_key).ok().flatten();
        for (binding_id, binding) in &resolved.names.bindings {
            if binding.site.source_id != parsed.source_id
                || syntax_type_ranges.contains(&range_key(source, binding.site.span))
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
            if site.source_id != parsed.source_id
                || syntax_type_ranges.contains(&range_key(source, site.span))
            {
                continue;
            }
            let binding = resolved.names.bindings.get(*binding_id);
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
    let mut tokens = lexical_tokens(source);
    tokens.extend(syntax_type_tokens(source));
    normalize_tokens(tokens)
}

fn lexical_tokens(source: &Source) -> ToolSemanticTokenList {
    let lexed = Lexer::new(source.text()).lex();
    let mut tokens = Vec::new();
    for token_index in 0..lexed.tokens().len() {
        for trivia in lexed.token_trivia(token_index) {
            let Some(kind) = trivia_token_kind(trivia.kind) else {
                continue;
            };
            push_span_tokens(
                source,
                &mut tokens,
                trivia.span,
                kind,
                trivia_modifiers(trivia.kind),
            );
        }
        let token = lexed.tokens()[token_index];
        let Some(kind) = lexical_token_kind(token.kind) else {
            continue;
        };
        push_span_tokens(source, &mut tokens, token.span, kind, Vec::new());
    }
    tokens
}

fn syntax_type_tokens(source: &Source) -> ToolSemanticTokenList {
    let lexed = Lexer::new(source.text()).lex();
    let parsed = parse(lexed.clone());
    let mut tokens = Vec::new();
    collect_syntax_type_tokens(source, parsed.tree().root(), false, &mut tokens);
    collect_annotation_type_tokens(source, &lexed, &mut tokens);
    tokens
}

fn collect_annotation_type_tokens(
    source: &Source,
    lexed: &LexedSource,
    out: SemanticTokenSink<'_>,
) {
    let mut in_annotation_type = false;
    for token in lexed.tokens() {
        match token.kind {
            TokenKind::Colon => {
                in_annotation_type = true;
            }
            TokenKind::Ident | TokenKind::KwAny | TokenKind::KwSome if in_annotation_type => {
                push_span_tokens(
                    source,
                    out,
                    token.span,
                    ToolSemanticTokenKind::Type,
                    Vec::new(),
                );
            }
            TokenKind::Comma | TokenKind::RParen | TokenKind::RBracket | TokenKind::ColonEq => {
                in_annotation_type = false;
            }
            _ => {}
        }
    }
}

fn collect_syntax_type_tokens(
    source: &Source,
    node: SyntaxNode<'_, '_>,
    is_type_context: bool,
    out: SemanticTokenSink<'_>,
) {
    let node_kind = node.kind();
    let type_context = is_type_context || node_kind.is_ty();
    let type_param_context = node_kind == SyntaxNodeKind::TypeParam;
    for child in node.children() {
        match child {
            SyntaxElement::Node(child_node) => {
                collect_syntax_type_tokens(source, child_node, type_context, out);
            }
            SyntaxElement::Token(token) => {
                let Some(kind) =
                    syntax_type_token_kind(token.kind(), type_context, type_param_context)
                else {
                    continue;
                };
                push_span_tokens(source, out, token.span(), kind, Vec::new());
            }
        }
    }
}

const fn syntax_type_token_kind(
    kind: TokenKind,
    is_type_context: bool,
    is_type_param_context: bool,
) -> Option<ToolSemanticTokenKind> {
    if is_type_param_context && matches!(kind, TokenKind::Ident) {
        return Some(ToolSemanticTokenKind::TypeParameter);
    }
    if is_type_context
        && matches!(
            kind,
            TokenKind::Ident | TokenKind::KwAny | TokenKind::KwSome
        )
    {
        return Some(ToolSemanticTokenKind::Type);
    }
    None
}

type SemanticRangeKey = (usize, usize, usize, usize);

fn token_ranges(tokens: &[ToolSemanticToken]) -> BTreeSet<SemanticRangeKey> {
    tokens
        .iter()
        .map(|token| tool_range_key(token.range))
        .collect()
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

fn trivia_token_kind(kind: TriviaKind) -> Option<ToolSemanticTokenKind> {
    kind.is_comment().then_some(ToolSemanticTokenKind::Comment)
}

fn trivia_modifiers(kind: TriviaKind) -> ToolSemanticModifierList {
    let mut modifiers = Vec::new();
    if kind.is_doc_comment() {
        modifiers.push(ToolSemanticModifier::Documentation);
    }
    if kind.is_module_doc_comment() {
        modifiers.push(ToolSemanticModifier::Module);
    }
    modifiers
}

const fn lexical_token_kind(kind: TokenKind) -> Option<ToolSemanticTokenKind> {
    match kind {
        TokenKind::Int | TokenKind::Float => Some(ToolSemanticTokenKind::Number),
        TokenKind::String
        | TokenKind::Rune
        | TokenKind::TemplateNoSubst
        | TokenKind::TemplateHead
        | TokenKind::TemplateMiddle
        | TokenKind::TemplateTail => Some(ToolSemanticTokenKind::String),
        TokenKind::At => Some(ToolSemanticTokenKind::Decorator),
        TokenKind::OpIdent
        | TokenKind::SymbolicOp
        | TokenKind::Plus
        | TokenKind::Minus
        | TokenKind::Star
        | TokenKind::Slash
        | TokenKind::Percent
        | TokenKind::Eq
        | TokenKind::Lt
        | TokenKind::Gt
        | TokenKind::ColonEq
        | TokenKind::MinusGt
        | TokenKind::TildeGt
        | TokenKind::TildeEq
        | TokenKind::EqGt
        | TokenKind::SlashEq
        | TokenKind::LtEq
        | TokenKind::GtEq
        | TokenKind::LtColon
        | TokenKind::DotDot
        | TokenKind::DotDotLt
        | TokenKind::DotDotDot
        | TokenKind::ColonQuestion
        | TokenKind::ColonQuestionGt
        | TokenKind::PipeGt => Some(ToolSemanticTokenKind::Operator),
        TokenKind::KwComptime
        | TokenKind::KwExport
        | TokenKind::KwForeign
        | TokenKind::KwMut
        | TokenKind::KwOpaque
        | TokenKind::KwPartial
        | TokenKind::KwRec
        | TokenKind::KwUnsafe => Some(ToolSemanticTokenKind::Modifier),
        TokenKind::KwAnd
        | TokenKind::KwAny
        | TokenKind::KwAs
        | TokenKind::KwMatch
        | TokenKind::KwClass
        | TokenKind::KwData
        | TokenKind::KwEffect
        | TokenKind::KwForall
        | TokenKind::KwHandle
        | TokenKind::KwIf
        | TokenKind::KwImport
        | TokenKind::KwIn
        | TokenKind::KwInfix
        | TokenKind::KwInfixl
        | TokenKind::KwInfixr
        | TokenKind::KwInstance
        | TokenKind::KwLaw
        | TokenKind::KwLet
        | TokenKind::KwRequest
        | TokenKind::KwNot
        | TokenKind::KwOr
        | TokenKind::KwQuote
        | TokenKind::KwResume
        | TokenKind::KwShl
        | TokenKind::KwShr
        | TokenKind::KwSome
        | TokenKind::KwUsing
        | TokenKind::KwWhere
        | TokenKind::KwXor => Some(ToolSemanticTokenKind::Keyword),
        TokenKind::Eof
        | TokenKind::Error
        | TokenKind::Ident
        | TokenKind::Hash
        | TokenKind::Backslash
        | TokenKind::LParen
        | TokenKind::RParen
        | TokenKind::LBracket
        | TokenKind::RBracket
        | TokenKind::LBrace
        | TokenKind::RBrace
        | TokenKind::Comma
        | TokenKind::Semicolon
        | TokenKind::Dot
        | TokenKind::Colon
        | TokenKind::Pipe
        | TokenKind::Underscore
        | TokenKind::DotLBracket => None,
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
        | NameBindingKind::Let
        | NameBindingKind::AttachedMethod
        | NameBindingKind::PatternBind => sema
            .and_then(|module| {
                module
                    .binding_type(binding_id)
                    .map(|ty| &module.ty(ty).kind)
            })
            .map_or(ToolSemanticTokenKind::Variable, |ty| match ty {
                HirTyKind::Arrow { .. } | HirTyKind::Pi { .. } => ToolSemanticTokenKind::Function,
                HirTyKind::Module => ToolSemanticTokenKind::Namespace,
                HirTyKind::Type => ToolSemanticTokenKind::Type,
                _ => ToolSemanticTokenKind::Variable,
            }),
    }
}

fn member_token_kind(sema: &SemaModule, fact: &ExprMemberFact) -> ToolSemanticTokenKind {
    match member_class(sema, fact) {
        ToolMemberClass::Function => ToolSemanticTokenKind::Function,
        ToolMemberClass::Procedure => ToolSemanticTokenKind::Procedure,
        ToolMemberClass::Property => ToolSemanticTokenKind::Property,
        ToolMemberClass::Type => ToolSemanticTokenKind::Type,
        ToolMemberClass::Namespace => ToolSemanticTokenKind::Namespace,
    }
}

fn normalize_tokens(mut tokens: ToolSemanticTokenList) -> ToolSemanticTokenList {
    tokens.sort_by_key(|token| {
        (
            token.range.start_line,
            token.range.start_col,
            token.range.end_line,
            token.range.end_col,
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
