use music_hir::{HirExprId, HirOrigin};
use music_names::Ident;

use crate::checker::Checker;
use crate::env::substitute_generics;
use crate::unify;
use crate::{EffectRow, SemTy, SemTyId};

impl<'a> Checker<'a> {
    pub(super) fn check_variant_expr(
        &mut self,
        origin: HirOrigin,
        name: Ident,
        payload: Option<HirExprId>,
        expected: SemTyId,
    ) -> (SemTyId, EffectRow) {
        let expected = unify::resolve(&self.state.semtys, expected);
        let SemTy::Named {
            name: ty_name,
            args,
        } = self.state.semtys.get(expected).clone()
        else {
            return self.synth_and_unify_variant(origin, payload, expected);
        };

        if self.error_if_opaque_repr_access(origin.span, ty_name) {
            return self.synth_and_unify_variant(origin, payload, expected);
        }

        let Some(def) = self.state.env.get_data_def(ty_name).cloned() else {
            return self.synth_and_unify_variant(origin, payload, expected);
        };
        let Some(variants) = def.variants.as_ref() else {
            return self.synth_and_unify_variant(origin, payload, expected);
        };

        let Some(payload_ty) = variants.get(&name.name).copied() else {
            return self.synth_and_unify_variant(origin, payload, expected);
        };

        let mut subst = Vec::with_capacity(def.generic_count as usize);
        for i in 0..def.generic_count {
            subst.push(
                args.get(i as usize)
                    .copied()
                    .unwrap_or(self.state.builtins.unknown),
            );
        }

        let mut effs = EffectRow::empty();
        match (payload_ty, payload) {
            (None, None) => {}
            (None, Some(payload)) => {
                let (_t, e) = self.synth_expr(payload);
                effs.union_with(&e);
            }
            (Some(payload_ty), Some(payload)) => {
                let payload_expected =
                    substitute_generics(&mut self.state.semtys, payload_ty, &subst);
                let (_t, e) = self.check_expr(payload, payload_expected);
                effs.union_with(&e);
            }
            (Some(_), None) => {}
        };

        (expected, effs)
    }

    fn synth_and_unify_variant(
        &mut self,
        origin: HirOrigin,
        payload: Option<HirExprId>,
        expected: SemTyId,
    ) -> (SemTyId, EffectRow) {
        let mut effs = EffectRow::empty();
        if let Some(payload) = payload {
            let (_t, e) = self.synth_expr(payload);
            effs.union_with(&e);
        }
        let ty = self.unify_or_report(origin.span, expected, self.state.builtins.unknown);
        (ty, effs)
    }
}
