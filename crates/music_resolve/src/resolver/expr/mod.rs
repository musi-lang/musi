use super::*;

use music_arena::SliceRange;
use music_hir::{
    HirAccessKind, HirArg, HirArrayItem, HirAttr, HirAttrArg, HirBinaryOp, HirConstraint,
    HirConstraintKind, HirDim, HirEffectItem, HirEffectSet, HirExportMod, HirExprId, HirFieldDef,
    HirForeignMod, HirHandleClause, HirLetMods, HirMatchArm, HirMemberDef, HirMemberKind, HirMods,
    HirParam, HirPartialRangeKind, HirPat, HirPatKind, HirPrefixOp, HirQuoteKind, HirRecordItem,
    HirSpliceKind, HirVariantDef,
};
use music_syntax::{SyntaxElement, SyntaxNodeKind};

use crate::string_lit::decode_string_lit;

use super::util::{child_of_kind, parse_u32_lit, stmt_inner_expr};

mod atoms;
mod attrs;
mod callable;
mod composite;
mod control;
mod decls;
mod effects_meta;
mod operators;
mod signature;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        self.try_lower_sequence_like_expr(node)
            .or_else(|| self.try_lower_decl_like_expr(node))
            .or_else(|| self.try_lower_value_like_expr(node))
            .or_else(|| self.try_lower_operator_like_expr(node))
            .or_else(|| self.try_lower_control_like_expr(node))
            .unwrap_or_else(|| self.error_expr(origin))
    }

    fn lower_sequence_or_stmt_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        if let Some(inner) = stmt_inner_expr(node) {
            self.lower_expr(inner)
        } else {
            self.lower_sequence_expr(node)
        }
    }

    fn try_lower_sequence_like_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> Option<HirExprId> {
        (node.kind() == SyntaxNodeKind::SequenceExpr)
            .then(|| self.lower_sequence_or_stmt_expr(node))
    }

    fn try_lower_decl_like_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> Option<HirExprId> {
        let kind = node.kind();
        if matches!(
            kind,
            SyntaxNodeKind::LetExpr
                | SyntaxNodeKind::ImportExpr
                | SyntaxNodeKind::ForeignBlockExpr
                | SyntaxNodeKind::DataExpr
                | SyntaxNodeKind::EffectExpr
                | SyntaxNodeKind::ClassExpr
                | SyntaxNodeKind::InstanceExpr
        ) {
            Some(match kind {
                SyntaxNodeKind::LetExpr => self.lower_let_expr(node),
                SyntaxNodeKind::ImportExpr => self.lower_import_expr(node),
                SyntaxNodeKind::ForeignBlockExpr => self.lower_foreign_block_expr(node),
                SyntaxNodeKind::DataExpr => self.lower_data_expr(node),
                SyntaxNodeKind::EffectExpr => self.lower_effect_expr(node),
                SyntaxNodeKind::ClassExpr => self.lower_class_expr(node),
                SyntaxNodeKind::InstanceExpr => self.lower_instance_expr(node),
                _ => return None,
            })
        } else {
            None
        }
    }

    fn try_lower_value_like_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> Option<HirExprId> {
        Some(match node.kind() {
            SyntaxNodeKind::NameExpr => self.lower_name_expr(node),
            SyntaxNodeKind::LiteralExpr => self.lower_literal_expr(node),
            SyntaxNodeKind::TemplateExpr => self.lower_template_expr(node),
            SyntaxNodeKind::QuoteExpr => self.lower_quote_expr(node),
            SyntaxNodeKind::SpliceExpr => self.lower_splice_expr(node),
            SyntaxNodeKind::AttributedExpr => self.lower_attributed_expr(node),
            SyntaxNodeKind::UnsafeExpr => self.lower_unsafe_expr(node),
            SyntaxNodeKind::TupleExpr => self.lower_tuple_expr(node),
            SyntaxNodeKind::ArrayExpr => self.lower_array_expr_or_ty(node),
            SyntaxNodeKind::ArrayTy => self.lower_array_ty_expr(node),
            SyntaxNodeKind::HandlerTy => self.lower_handler_ty_expr(node),
            SyntaxNodeKind::RecordExpr => self.lower_record_expr(node),
            SyntaxNodeKind::VariantExpr => self.lower_variant_expr(node),
            SyntaxNodeKind::PiExpr => self.lower_pi_expr(node),
            SyntaxNodeKind::LambdaExpr => self.lower_lambda_expr(node),
            SyntaxNodeKind::CallExpr => self.lower_call_expr(node),
            SyntaxNodeKind::ApplyExpr => self.lower_apply_expr(node),
            SyntaxNodeKind::IndexExpr => self.lower_index_expr(node),
            SyntaxNodeKind::RecordUpdateExpr => self.lower_record_update_expr(node),
            SyntaxNodeKind::FieldExpr => self.lower_field_expr(node),
            SyntaxNodeKind::TypeTestExpr => self.lower_type_test_expr(node),
            SyntaxNodeKind::TypeCastExpr => self.lower_type_cast_expr(node),
            _ => return None,
        })
    }

    fn try_lower_operator_like_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> Option<HirExprId> {
        Some(match node.kind() {
            SyntaxNodeKind::PrefixExpr => self.lower_prefix_expr(node),
            SyntaxNodeKind::PostfixExpr => self.lower_postfix_expr(node),
            SyntaxNodeKind::BinaryExpr => self.lower_binary_expr(node),
            _ => return None,
        })
    }

    fn try_lower_control_like_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> Option<HirExprId> {
        let kind = node.kind();
        Some(match kind {
            SyntaxNodeKind::MatchExpr => self.lower_match_expr(node),
            SyntaxNodeKind::RequestExpr => self.lower_request_expr(node),
            SyntaxNodeKind::HandlerExpr => self.lower_handler_expr(node),
            SyntaxNodeKind::HandleExpr => self.lower_handle_expr(node),
            SyntaxNodeKind::ResumeExpr => self.lower_resume_expr(node),
            _ => return None,
        })
    }

    fn lower_optional_expr_clause<I>(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        token_kind: TokenKind,
        exprs: &mut I,
    ) -> Option<HirExprId>
    where
        I: Iterator<Item = SyntaxNode<'tree, 'src>>,
    {
        if node.child_tokens().any(|token| token.kind() == token_kind) {
            exprs.next().map(|expr| self.lower_expr(expr))
        } else {
            None
        }
    }
}
