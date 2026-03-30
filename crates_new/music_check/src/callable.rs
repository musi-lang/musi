use std::collections::HashMap;

use music_hir::{HirArrowFlavor, HirExprId, HirOrigin, HirParam, HirTyId};
use music_names::Symbol;

use super::checker::Checker;
use super::env::ValueScheme;
use super::{EffectRow, SemTy, SemTyId};

impl<'a> Checker<'a> {
    pub(crate) fn typecheck_callable(
        &mut self,
        origin: HirOrigin,
        params: &[HirParam],
        ty_params: &HashMap<Symbol, u32>,
        ret_annot: Option<HirTyId>,
        body: Option<HirExprId>,
        declared_effects: Option<&EffectRow>,
    ) -> (SemTyId, EffectRow) {
        let mut param_tys = Vec::with_capacity(params.len());
        for p in params {
            let ty = p
                .annot
                .map(|t| self.lower_hir_ty(t, ty_params))
                .unwrap_or_else(|| self.state.semtys.fresh_infer_var(p.origin.span));
            param_tys.push(ty);

            if let Some(binding) = self.binding_for_def(p.name.span) {
                self.state.env.insert_value(
                    binding,
                    ValueScheme {
                        generic_count: 0,
                        ty,
                        declared_effects: None,
                    },
                );
                if p.mutable {
                    self.mark_binding_mut(binding, true);
                }
            }
        }

        let mut effs = EffectRow::empty();

        // Defaults are evaluated at call-time, so they contribute to callable latent row.
        for (p, p_ty) in params.iter().zip(param_tys.iter().copied()) {
            if let Some(default) = p.default {
                let (t, e) = self.synth_expr(default);
                effs.union_with(&e);
                let _ = self.unify_or_report(p.origin.span, t, p_ty);
            }
        }

        let input = match param_tys.len() {
            0 => self.state.builtins.unit,
            1 => param_tys[0],
            _ => self.state.semtys.alloc(SemTy::Tuple {
                items: param_tys.into_boxed_slice(),
            }),
        };

        let output = ret_annot
            .map(|t| self.lower_hir_ty(t, ty_params))
            .unwrap_or_else(|| self.state.semtys.fresh_infer_var(origin.span));

        if let Some(body) = body {
            let (body_ty, body_effs) = self.check_expr(body, output);
            effs.union_with(&body_effs);
            let _ = body_ty;
        }

        let flavor = if declared_effects.is_some() || !effs.is_pure() {
            HirArrowFlavor::Effectful
        } else {
            HirArrowFlavor::Pure
        };

        let fn_ty = self.state.semtys.alloc(SemTy::Arrow {
            flavor,
            input,
            output,
        });
        (fn_ty, effs)
    }

    pub(crate) fn synth_lambda(
        &mut self,
        origin: HirOrigin,
        params: Box<[HirParam]>,
        ret: Option<HirTyId>,
        body: HirExprId,
    ) -> (SemTyId, EffectRow) {
        let ty_params = HashMap::new();
        let (fn_ty, effs) =
            self.typecheck_callable(origin, &params, &ty_params, ret, Some(body), None);
        (fn_ty, effs)
    }
}
