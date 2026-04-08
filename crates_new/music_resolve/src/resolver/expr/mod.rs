use super::*;

use music_arena::SliceRange;
use music_hir::{
    HirAccessKind, HirArg, HirArrayItem, HirAttr, HirAttrArg, HirBinaryOp, HirCaseArm,
    HirConstraint, HirConstraintKind, HirDim, HirEffectItem, HirEffectSet, HirExprId, HirFieldDef,
    HirForeignDecl, HirHandleClause, HirLetMods, HirMemberDef, HirMemberKind, HirParam, HirPat,
    HirPatKind, HirPrefixOp, HirQuoteKind, HirRecordItem, HirSpliceKind, HirTemplatePart,
    HirVariantDef,
};
use music_syntax::{SyntaxElement, SyntaxNodeKind};

use crate::string_lit::{
    decode_string_lit, decode_template_head, decode_template_middle, decode_template_no_subst,
    decode_template_tail,
};

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
        match node.kind() {
            SyntaxNodeKind::SequenceExpr => {
                if let Some(inner) = stmt_inner_expr(node) {
                    self.lower_expr(inner)
                } else {
                    self.lower_sequence_expr(node)
                }
            }

            SyntaxNodeKind::LetExpr => self.lower_let_expr(node),
            SyntaxNodeKind::ImportExpr => self.lower_import_expr(node),
            SyntaxNodeKind::ExportExpr => self.lower_export_expr(node),
            SyntaxNodeKind::ForeignBlockExpr => self.lower_foreign_block_expr(node),
            SyntaxNodeKind::DataExpr => self.lower_data_expr(node),
            SyntaxNodeKind::EffectExpr => self.lower_effect_expr(node),
            SyntaxNodeKind::ClassExpr => self.lower_class_expr(node),
            SyntaxNodeKind::InstanceExpr => self.lower_instance_expr(node),

            SyntaxNodeKind::NameExpr => self.lower_name_expr(node),
            SyntaxNodeKind::LiteralExpr => self.lower_literal_expr(node),
            SyntaxNodeKind::TemplateExpr => self.lower_template_expr(node),

            SyntaxNodeKind::TupleExpr => self.lower_tuple_expr(node),
            SyntaxNodeKind::ArrayExpr => self.lower_array_expr_or_ty(node),
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

            SyntaxNodeKind::PrefixExpr => self.lower_prefix_expr(node),
            SyntaxNodeKind::BinaryExpr => self.lower_binary_expr(node),

            SyntaxNodeKind::CaseExpr => self.lower_case_expr(node),
            SyntaxNodeKind::PerformExpr => self.lower_perform_expr(node),
            SyntaxNodeKind::HandleExpr => self.lower_handle_expr(node),
            SyntaxNodeKind::ResumeExpr => self.lower_resume_expr(node),
            SyntaxNodeKind::QuoteExpr => self.lower_quote_expr(node),
            SyntaxNodeKind::SpliceExpr => self.lower_splice_expr(node),
            SyntaxNodeKind::AttributedExpr => self.lower_attributed_expr(node),

            _ => self.error_expr(origin),
        }
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
