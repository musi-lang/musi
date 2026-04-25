use music_base::{Source, Span};
use music_hir::{HirExprKind, HirTyKind};
use music_names::{NameBindingId, NameBindingKind};
use music_sema::{ExprMemberFact, SemaModule};

use super::model::{
    ToolSemanticModifier, ToolSemanticToken, ToolSemanticTokenKind, ToolSemanticTokenList,
};
use crate::analysis::{ToolMemberShape, member_class, tool_range};

pub(super) fn member_tokens(source: &Source, sema: &SemaModule) -> ToolSemanticTokenList {
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
pub(super) fn binding_token(
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

pub(super) fn binding_token_kind(
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

pub(super) fn member_token_kind(sema: &SemaModule, fact: &ExprMemberFact) -> ToolSemanticTokenKind {
    match member_class(sema, fact) {
        ToolMemberShape::Function => ToolSemanticTokenKind::Function,
        ToolMemberShape::Procedure => ToolSemanticTokenKind::Procedure,
        ToolMemberShape::Property => ToolSemanticTokenKind::Property,
        ToolMemberShape::Type => ToolSemanticTokenKind::Type,
    }
}
