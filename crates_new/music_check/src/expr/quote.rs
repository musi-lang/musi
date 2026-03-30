use music_hir::{HirOrigin, HirSpliceId, HirSpliceKind};

use crate::checker::Checker;
use crate::{EffectRow, SemTyId};

impl Checker<'_> {
    pub(super) fn synth_quote(
        &mut self,
        origin: HirOrigin,
        splices: &[HirSpliceId],
    ) -> (SemTyId, EffectRow) {
        let mut effs = EffectRow::empty();

        for &splice_id in splices {
            let splice = self.ctx.store.splices.get(splice_id).clone();
            match splice.kind {
                HirSpliceKind::Name(ident) => {
                    let ty = self
                        .binding_for_use(ident.span)
                        .and_then(|binding| self.state.env.get_value(binding))
                        .map_or(self.state.builtins.unknown, |scheme| {
                            scheme.instantiate(&mut self.state.semtys, ident.span)
                        });
                    let _ = self.unify_or_report(ident.span, self.state.builtins.syntax, ty);
                }
                HirSpliceKind::Expr(expr) => {
                    let (ty, e) = self.synth_expr(expr);
                    effs.union_with(&e);
                    let _ =
                        self.unify_or_report(splice.origin.span, self.state.builtins.syntax, ty);
                }
                HirSpliceKind::ExprArray(exprs) => {
                    for expr in exprs.iter().copied() {
                        let (ty, e) = self.synth_expr(expr);
                        effs.union_with(&e);
                        let _ = self.unify_or_report(
                            splice.origin.span,
                            self.state.builtins.syntax,
                            ty,
                        );
                    }
                }
            }
        }

        let _ = origin;
        (self.state.builtins.syntax, effs)
    }
}
