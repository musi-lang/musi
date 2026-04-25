use music_arena::SliceRange;
use music_hir::{HirParam, HirTyId};

use crate::checker::CheckPass;
use crate::checker::exprs::check_expr;

impl CheckPass<'_, '_, '_> {
    pub fn lower_params(&mut self, range: SliceRange<HirParam>) -> Box<[HirTyId]> {
        let builtins = self.builtins();
        self.params(range)
            .into_iter()
            .map(|param| {
                let ty = param.ty.map_or(builtins.unknown, |expr| {
                    let origin = self.expr(expr).origin;
                    self.lower_type_expr(expr, origin)
                });
                if let Some(binding) = self.binding_id_for_decl(param.name) {
                    self.insert_binding_type(binding, ty);
                }
                if let Some(default) = param.default {
                    let facts = check_expr(self, default);
                    let origin = self.expr(default).origin;
                    self.type_mismatch(origin, ty, facts.ty);
                }
                ty
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }
}
