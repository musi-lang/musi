use std::collections::{HashMap, HashSet};

use music_arena::SliceRange;
use music_base::{Span, diag::DiagContext};
use music_hir::{HirArg, HirExprId, HirTyId, HirTyKind};
use music_names::{Ident, Symbol};

use crate::api::ExprFacts;

use super::super::state::{DataDef, DataVariantDef};
use super::super::{CheckPass, DiagKind};
use super::{check_expr, peel_mut_ty};

type ExprIdList = Vec<HirExprId>;
type TyIdList = Vec<HirTyId>;
type VariantFieldNames = [Option<Box<str>>];

impl CheckPass<'_, '_, '_> {
    pub(super) fn check_variant_expr(&mut self, tag: Ident, args: SliceRange<HirArg>) -> ExprFacts {
        let builtins = self.builtins();
        if let Some(facts) = self.check_sum_constructor_variant(tag, args.clone()) {
            return facts;
        }

        let expected_ty = self
            .expected_ty()
            .and_then(|ty| self.variant_context_ty(ty));
        let expected_ty = expected_ty.or_else(|| self.infer_variant_context_ty(tag));
        let Some(expected_ty) = expected_ty else {
            self.check_variant_arg_effects(args);
            return ExprFacts::new(builtins.unknown);
        };

        let data_def = self.expected_data_def(expected_ty);
        let Some(data_def) = data_def else {
            self.check_variant_arg_effects(args);
            self.diag(tag.span, DiagKind::VariantMissingDataContext, "");
            return ExprFacts::new(builtins.unknown);
        };

        let tag_name = self.resolve_symbol(tag.name).to_owned();
        let Some(variant) = data_def.variant(&tag_name) else {
            self.check_variant_arg_effects(args);
            self.diag_with(
                tag.span,
                DiagKind::UnknownDataVariant,
                DiagContext::new().with("variant", tag_name),
            );
            return ExprFacts::new(expected_ty);
        };
        let data_def = data_def.clone();
        let variant = variant.clone();

        let expected_args = self.variant_expected_arg_tys(expected_ty, &data_def, &variant);
        let field_names = variant.field_names().to_vec();
        let arg_nodes = self.args(args);
        self.typecheck_variant_args(tag.span, &expected_args, &field_names, &arg_nodes);
        let result_ty = self.infer_variant_result_ty(
            expected_ty,
            &data_def,
            &variant,
            &field_names,
            &arg_nodes,
        );

        ExprFacts::new(result_ty)
    }

    fn check_sum_constructor_variant(
        &mut self,
        tag: Ident,
        args: SliceRange<HirArg>,
    ) -> Option<ExprFacts> {
        let builtins = self.builtins();
        let expected_sum_ty = self.expected_ty().and_then(|ty| {
            let inner = peel_mut_ty(self, ty);
            matches!(self.ty(inner).kind, HirTyKind::Sum { .. }).then_some(inner)
        })?;
        let HirTyKind::Sum { left, right } = self.ty(expected_sum_ty).kind else {
            return Some(ExprFacts::new(builtins.unknown));
        };
        let tag_name = self.resolve_symbol(tag.name);
        let chosen = match tag_name {
            "Left" => Some(left),
            "Right" => Some(right),
            _ => None,
        }?;

        let _ = self.ensure_sum_data_def(left, right);
        let arg_exprs = self.args(args);
        if arg_exprs.iter().any(|arg| arg.name.is_some()) {
            self.diag(tag.span, DiagKind::VariantNamedFieldsUnexpected, "");
        }
        let expected_args: TyIdList = match &self.ty(chosen).kind {
            HirTyKind::Tuple { items } => self.ty_ids(*items),
            _ => vec![chosen],
        };
        self.typecheck_positional_args(
            tag.span,
            &expected_args,
            arg_exprs.into_iter().map(|arg| arg.expr).collect(),
            DiagKind::SumConstructorArityMismatch,
        );
        Some(ExprFacts::new(expected_sum_ty))
    }

    fn typecheck_variant_args(
        &mut self,
        diag_span: Span,
        expected_args: &[HirTyId],
        field_names: &VariantFieldNames,
        arg_nodes: &[HirArg],
    ) {
        let named_variant = field_names.iter().any(Option::is_some);
        let named_args = arg_nodes.iter().any(|arg| arg.name.is_some());
        if named_variant {
            self.typecheck_named_variant_args(
                diag_span,
                expected_args,
                field_names,
                arg_nodes,
                named_args,
            );
        } else {
            self.typecheck_ordinary_variant_args(diag_span, expected_args, arg_nodes, named_args);
        }
    }

    fn typecheck_named_variant_args(
        &mut self,
        diag_span: Span,
        expected_args: &[HirTyId],
        field_names: &VariantFieldNames,
        arg_nodes: &[HirArg],
        named_args: bool,
    ) {
        if !named_args {
            self.diag(diag_span, DiagKind::VariantNamedFieldsRequired, "");
            self.typecheck_positional_variant_args(diag_span, expected_args, arg_nodes);
            return;
        }
        let mut seen = HashSet::<Symbol>::new();
        for arg in arg_nodes {
            self.typecheck_named_variant_arg(diag_span, expected_args, field_names, arg, &mut seen);
        }
        self.report_missing_variant_fields(diag_span, field_names, &seen);
    }

    fn typecheck_named_variant_arg(
        &mut self,
        diag_span: Span,
        expected_args: &[HirTyId],
        field_names: &VariantFieldNames,
        arg: &HirArg,
        seen: &mut HashSet<Symbol>,
    ) {
        let Some(name) = arg.name else {
            self.diag(diag_span, DiagKind::VariantNamedFieldsRequired, "");
            return;
        };
        self.record_variant_field_name(name, seen);
        let expected = self.expected_variant_field_ty(name, field_names, expected_args);
        self.push_expected_ty(expected);
        let facts = check_expr(self, arg.expr);
        let _ = self.pop_expected_ty();
        let origin = self.expr(arg.expr).origin;
        self.type_mismatch(origin, expected, facts.ty);
    }

    fn record_variant_field_name(&mut self, name: Ident, seen: &mut HashSet<Symbol>) {
        if !seen.insert(name.name) {
            let field_name = self.resolve_symbol(name.name).to_owned();
            self.diag_with(
                name.span,
                DiagKind::DuplicateVariantField,
                DiagContext::new().with("field", field_name),
            );
        }
    }

    fn expected_variant_field_ty(
        &mut self,
        name: Ident,
        field_names: &VariantFieldNames,
        expected_args: &[HirTyId],
    ) -> HirTyId {
        let field_index = field_names
            .iter()
            .position(|field| field.as_deref() == Some(self.resolve_symbol(name.name)));
        field_index
            .and_then(|index| expected_args.get(index).copied())
            .unwrap_or_else(|| self.unknown_variant_field_ty(name))
    }

    fn unknown_variant_field_ty(&mut self, name: Ident) -> HirTyId {
        let field_name = self.resolve_symbol(name.name).to_owned();
        self.diag_with(
            name.span,
            DiagKind::UnknownVariantField,
            DiagContext::new().with("field", field_name),
        );
        self.builtins().unknown
    }

    fn report_missing_variant_fields(
        &mut self,
        diag_span: Span,
        field_names: &VariantFieldNames,
        seen: &HashSet<Symbol>,
    ) {
        for field_name in field_names.iter().flatten() {
            let expected_symbol = self.intern(field_name);
            if !seen.contains(&expected_symbol) {
                self.diag_with(
                    diag_span,
                    DiagKind::MissingVariantField,
                    DiagContext::new().with("field", field_name),
                );
            }
        }
    }

    fn typecheck_ordinary_variant_args(
        &mut self,
        diag_span: Span,
        expected_args: &[HirTyId],
        arg_nodes: &[HirArg],
        named_args: bool,
    ) {
        if named_args {
            self.diag(diag_span, DiagKind::VariantNamedFieldsUnexpected, "");
        }
        self.typecheck_positional_variant_args(diag_span, expected_args, arg_nodes);
    }

    fn typecheck_positional_variant_args(
        &mut self,
        diag_span: Span,
        expected_args: &[HirTyId],
        arg_nodes: &[HirArg],
    ) {
        self.typecheck_positional_args(
            diag_span,
            expected_args,
            arg_nodes.iter().map(|arg| arg.expr).collect(),
            DiagKind::VariantConstructorArityMismatch,
        );
    }

    fn check_variant_arg_effects(&mut self, args: SliceRange<HirArg>) {
        for arg in self.args(args) {
            let _ = check_expr(self, arg.expr);
        }
    }

    fn typecheck_positional_args(
        &mut self,
        diag_span: Span,
        expected_args: &[HirTyId],
        arg_exprs: ExprIdList,
        arity_diag: DiagKind,
    ) {
        let builtins = self.builtins();
        if expected_args.len() != arg_exprs.len() {
            self.diag(diag_span, arity_diag, "");
        }
        for (index, arg) in arg_exprs.into_iter().enumerate() {
            let expected = expected_args
                .get(index)
                .copied()
                .unwrap_or(builtins.unknown);
            self.push_expected_ty(expected);
            let facts = super::check_expr(self, arg);
            let _ = self.pop_expected_ty();
            let origin = self.expr(arg).origin;
            self.type_mismatch(origin, expected, facts.ty);
        }
    }

    fn variant_context_ty(&self, ty: HirTyId) -> Option<HirTyId> {
        self.expected_data_def(ty).map(|_| ty)
    }

    fn expected_data_def(&self, ty: HirTyId) -> Option<&DataDef> {
        match self.ty(ty).kind {
            HirTyKind::Bool => self.data_def("Bit"),
            HirTyKind::Named { name, .. } => self.data_def(self.resolve_symbol(name)),
            _ => None,
        }
    }

    fn variant_expected_arg_tys(
        &mut self,
        expected_ty: HirTyId,
        data_def: &DataDef,
        variant: &DataVariantDef,
    ) -> TyIdList {
        let mut subst = self.variant_type_subst_from_expected_ty(expected_ty, data_def);
        for param in data_def.type_params().iter().copied() {
            let default_ty = self.default_variant_type_arg(param);
            let _ = subst.entry(param).or_insert(default_ty);
        }
        variant
            .field_tys()
            .iter()
            .copied()
            .map(|field_ty| self.substitute_ty(field_ty, &subst))
            .collect()
    }

    fn infer_variant_result_ty(
        &mut self,
        expected_ty: HirTyId,
        data_def: &DataDef,
        variant: &DataVariantDef,
        field_names: &VariantFieldNames,
        arg_nodes: &[HirArg],
    ) -> HirTyId {
        let type_params = data_def.type_params();
        if type_params.is_empty() {
            return expected_ty;
        }
        let HirTyKind::Named { name, .. } = self.ty(expected_ty).kind else {
            return expected_ty;
        };
        let mut subst = self.variant_type_subst_from_expected_ty(expected_ty, data_def);
        self.augment_variant_subst_from_args(data_def, variant, field_names, arg_nodes, &mut subst);
        let inferred_args = type_params
            .iter()
            .copied()
            .map(|param| {
                subst
                    .get(&param)
                    .copied()
                    .unwrap_or_else(|| self.default_variant_type_arg(param))
            })
            .collect::<Vec<_>>();
        let args = self.alloc_ty_list(inferred_args);
        self.alloc_ty(HirTyKind::Named { name, args })
    }

    fn augment_variant_subst_from_args(
        &mut self,
        data_def: &DataDef,
        variant: &DataVariantDef,
        field_names: &VariantFieldNames,
        arg_nodes: &[HirArg],
        subst: &mut HashMap<Symbol, HirTyId>,
    ) {
        let type_params = data_def.type_params();
        for (field_index, arg_ty) in self.variant_arg_matches(field_names, arg_nodes) {
            let Some(pattern) = variant.field_tys().get(field_index).copied() else {
                continue;
            };
            let _ = self.unify_ty_for_type_params(type_params, pattern, arg_ty, subst);
        }
    }

    fn variant_arg_matches(
        &self,
        field_names: &VariantFieldNames,
        arg_nodes: &[HirArg],
    ) -> Vec<(usize, HirTyId)> {
        let named_variant = field_names.iter().any(Option::is_some);
        if named_variant {
            arg_nodes
                .iter()
                .filter_map(|arg| {
                    let name = arg.name?;
                    let index = self.variant_arg_index(field_names, name)?;
                    Some((index, self.expr_facts(arg.expr).ty))
                })
                .collect()
        } else {
            arg_nodes
                .iter()
                .enumerate()
                .filter_map(|(index, arg)| {
                    (index < field_names.len()).then_some((index, self.expr_facts(arg.expr).ty))
                })
                .collect()
        }
    }

    fn variant_arg_index(&self, field_names: &VariantFieldNames, name: Ident) -> Option<usize> {
        let field_name = self.resolve_symbol(name.name);
        field_names
            .iter()
            .position(|field| field.as_deref() == Some(field_name))
    }

    fn variant_type_subst_from_expected_ty(
        &self,
        expected_ty: HirTyId,
        data_def: &DataDef,
    ) -> HashMap<Symbol, HirTyId> {
        let HirTyKind::Named { args, .. } = self.ty(expected_ty).kind else {
            return HashMap::new();
        };
        data_def
            .type_params()
            .iter()
            .copied()
            .zip(self.ty_ids(args))
            .collect()
    }

    fn default_variant_type_arg(&mut self, name: Symbol) -> HirTyId {
        if self.type_param_kind(name).is_some() {
            return self.alloc_ty(HirTyKind::Named {
                name,
                args: SliceRange::EMPTY,
            });
        }
        self.builtins().unknown
    }

    fn infer_variant_context_ty(&mut self, tag: Ident) -> Option<HirTyId> {
        let tag_name = self.resolve_symbol(tag.name).to_owned();
        let mut matches = self
            .data_defs()
            .iter()
            .filter_map(|(name, data)| data.variant(&tag_name).is_some().then_some(name.clone()))
            .collect::<Vec<Box<str>>>();

        match matches.len() {
            0 => {
                self.diag_with(
                    tag.span,
                    DiagKind::UnknownDataVariant,
                    DiagContext::new().with("variant", tag_name),
                );
                None
            }
            1 => {
                let data_name = matches.pop()?;
                let name = self.intern(data_name.as_ref());
                let args = self.alloc_ty_list([]);
                Some(self.alloc_ty(HirTyKind::Named { name, args }))
            }
            _ => {
                self.diag(tag.span, DiagKind::AmbiguousVariantTag, "");
                None
            }
        }
    }
}
