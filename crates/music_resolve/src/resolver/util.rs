use super::*;

use music_arena::SliceRange;
use music_base::{NumericSuffixClass, parse_u32_literal, split_numeric_suffix};
use music_hir::{HirBinder, HirConstraint, HirLit, HirLitId, HirLitKind, HirParam};
use music_syntax::{SyntaxElement, SyntaxNodeKind};

use crate::string_lit::{
    decode_rune_lit, decode_string_lit, decode_template_head, decode_template_middle,
    decode_template_no_subst, decode_template_tail,
};

pub(super) fn stmt_inner_expr<'tree, 'src>(
    node: SyntaxNode<'tree, 'src>,
) -> Option<SyntaxNode<'tree, 'src>> {
    if node.kind() != SyntaxNodeKind::SequenceExpr {
        return None;
    }
    let mut children = node.children();
    match (children.next(), children.next(), children.next()) {
        (Some(SyntaxElement::Node(expr)), Some(SyntaxElement::Token(tok)), None)
            if tok.kind() == TokenKind::Semicolon =>
        {
            Some(expr)
        }
        _ => None,
    }
}

pub(super) fn parse_u32_lit(raw: &str) -> Option<u32> {
    parse_u32_literal(raw)
}

pub(super) fn child_of_kind<'tree, 'src>(
    node: SyntaxNode<'tree, 'src>,
    kind: SyntaxNodeKind,
) -> Option<SyntaxNode<'tree, 'src>> {
    node.child_nodes().find(|child| child.kind() == kind)
}

pub(super) fn is_expr_or_ty(kind: SyntaxNodeKind) -> bool {
    kind.is_expr() || kind.is_ty()
}

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_type_params_clause(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<HirBinder> {
        child_of_kind(node, SyntaxNodeKind::TypeParamList)
            .map_or(SliceRange::EMPTY, |child| self.lower_type_param_list(child))
    }

    pub(super) fn lower_params_clause(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<HirParam> {
        child_of_kind(node, SyntaxNodeKind::ParamList)
            .map_or(SliceRange::EMPTY, |child| self.lower_param_list(child))
    }

    pub(super) fn lower_constraints_clause(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<HirConstraint> {
        child_of_kind(node, SyntaxNodeKind::ConstraintList)
            .map_or(SliceRange::EMPTY, |child| self.lower_constraint_list(child))
    }

    pub(super) fn alloc_lit_from_token(
        &mut self,
        tok: SyntaxToken<'tree, 'src>,
    ) -> Option<HirLitId> {
        let raw = tok.text()?;
        let kind = match tok.kind() {
            TokenKind::Int => {
                let (_, suffix) = split_numeric_suffix(raw);
                if suffix.is_some_and(|suffix| suffix.class == NumericSuffixClass::F) {
                    HirLitKind::Float { raw: raw.into() }
                } else {
                    HirLitKind::Int { raw: raw.into() }
                }
            }
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
        Some(
            self.store
                .alloc_lit(HirLit::new(self.origin_token(tok), kind)),
        )
    }
}
