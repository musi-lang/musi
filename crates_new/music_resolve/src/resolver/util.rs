use super::*;

use music_arena::SliceRange;
use music_hir::{HirLit, HirLitKind};

use crate::string_lit::{
    decode_rune_lit, decode_string_lit, decode_template_head, decode_template_middle,
    decode_template_no_subst, decode_template_tail,
};

pub(super) fn stmt_inner_expr<'tree, 'src>(
    node: SyntaxNode<'tree, 'src>,
) -> Option<SyntaxNode<'tree, 'src>> {
    if node.kind() != music_syntax::SyntaxNodeKind::SequenceExpr {
        return None;
    }
    let mut children = node.children();
    match (children.next(), children.next(), children.next()) {
        (
            Some(music_syntax::SyntaxElement::Node(expr)),
            Some(music_syntax::SyntaxElement::Token(tok)),
            None,
        ) if tok.kind() == music_syntax::TokenKind::Semicolon => Some(expr),
        _ => None,
    }
}

pub(super) fn parse_u32_lit(raw: &str) -> Option<u32> {
    let raw = raw.replace('_', "");
    let (radix, digits) = raw
        .strip_prefix("0x")
        .or_else(|| raw.strip_prefix("0X"))
        .map_or_else(
            || {
                raw.strip_prefix("0o")
                    .or_else(|| raw.strip_prefix("0O"))
                    .map_or_else(
                        || {
                            raw.strip_prefix("0b")
                                .or_else(|| raw.strip_prefix("0B"))
                                .map_or((10, raw.as_str()), |rest| (2, rest))
                        },
                        |rest| (8, rest),
                    )
            },
            |rest| (16, rest),
        );
    u32::from_str_radix(digits, radix).ok()
}

pub(super) fn child_of_kind<'tree, 'src>(
    node: SyntaxNode<'tree, 'src>,
    kind: music_syntax::SyntaxNodeKind,
) -> Option<SyntaxNode<'tree, 'src>> {
    node.child_nodes().find(|child| child.kind() == kind)
}

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src> {
    pub(super) fn lower_type_params_clause(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<Ident> {
        child_of_kind(node, music_syntax::SyntaxNodeKind::TypeParamList)
            .map_or(SliceRange::EMPTY, |child| self.lower_type_param_list(child))
    }

    pub(super) fn lower_params_clause(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<music_hir::HirParam> {
        child_of_kind(node, music_syntax::SyntaxNodeKind::ParamList)
            .map_or(SliceRange::EMPTY, |child| self.lower_param_list(child))
    }

    pub(super) fn lower_constraints_clause(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<music_hir::HirConstraint> {
        child_of_kind(node, music_syntax::SyntaxNodeKind::ConstraintList)
            .map_or(SliceRange::EMPTY, |child| self.lower_constraint_list(child))
    }

    pub(super) fn alloc_lit_from_token(
        &mut self,
        tok: SyntaxToken<'tree, 'src>,
    ) -> Option<music_hir::HirLitId> {
        let raw = tok.text()?;
        let kind = match tok.kind() {
            TokenKind::Int => HirLitKind::Int { raw: raw.into() },
            TokenKind::Float => HirLitKind::Float { raw: raw.into() },
            TokenKind::String => decode_string_lit(raw).map_or_else(
                |_| HirLitKind::String { value: "".into() },
                |text| HirLitKind::String { value: text.into() },
            ),
            TokenKind::Rune => decode_rune_lit(raw).map_or_else(
                |_| HirLitKind::Rune { value: 0 },
                |value| HirLitKind::Rune { value },
            ),
            TokenKind::TemplateNoSubst => decode_template_no_subst(raw).map_or_else(
                |_| HirLitKind::String { value: "".into() },
                |text| HirLitKind::String { value: text.into() },
            ),
            TokenKind::TemplateHead => decode_template_head(raw).map_or_else(
                |_| HirLitKind::String { value: "".into() },
                |text| HirLitKind::String { value: text.into() },
            ),
            TokenKind::TemplateMiddle => decode_template_middle(raw).map_or_else(
                |_| HirLitKind::String { value: "".into() },
                |text| HirLitKind::String { value: text.into() },
            ),
            TokenKind::TemplateTail => decode_template_tail(raw).map_or_else(
                |_| HirLitKind::String { value: "".into() },
                |text| HirLitKind::String { value: text.into() },
            ),
            _ => return None,
        };
        Some(self.store.alloc_lit(HirLit {
            origin: self.origin_token(tok),
            kind,
        }))
    }
}
