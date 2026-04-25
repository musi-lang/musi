use music_hir::HirEffectSet;

use crate::checker::PassBase;
use crate::effects::{EffectKey, EffectRow};

impl PassBase<'_, '_, '_> {
    pub fn lower_effect_row(&mut self, row: &HirEffectSet) -> EffectRow {
        let mut out = EffectRow::empty();
        for item in self.effect_items(row) {
            let arg = item.arg.map(|expr| {
                let origin = self.expr(expr).origin;
                self.lower_type_expr(expr, origin)
            });
            out.add(EffectKey {
                name: self.resolve_symbol(item.name.name).into(),
                arg,
            });
        }
        out.open = row
            .open
            .map(|ident| Box::<str>::from(self.resolve_symbol(ident.name)));
        out
    }
}
