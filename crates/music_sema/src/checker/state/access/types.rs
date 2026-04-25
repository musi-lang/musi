use music_arena::SliceRange;
use music_hir::{HirBinder, HirTemplatePart, HirTyId};
use music_names::Symbol;

use crate::checker::state::aliases::{TemplatePartList, TypeParamKindList};

use crate::checker::state::PassBase;

impl PassBase<'_, '_, '_> {
    pub fn lower_type_param_kinds(&mut self, range: SliceRange<HirBinder>) -> TypeParamKindList {
        let builtins = self.builtins();
        self.binders(range)
            .into_iter()
            .map(|binder| {
                let kind = binder.ty.map_or(builtins.type_, |expr| {
                    let origin = self.expr(expr).origin;
                    self.lower_type_expr(expr, origin)
                });
                (binder.name.name, kind)
            })
            .collect()
    }

    pub fn push_type_param_kinds(&mut self, kinds: &[(Symbol, HirTyId)]) {
        self.typing
            .type_param_kind_scopes
            .push(kinds.iter().copied().collect());
    }

    pub fn pop_type_param_kinds(&mut self) {
        let _ = self.typing.type_param_kind_scopes.pop();
    }

    pub fn type_constructor_scheme_arity(&self, symbol: Symbol) -> Option<usize> {
        self.module
            .resolved
            .names
            .bindings
            .iter()
            .find(|(id, binding)| {
                binding.name == symbol && self.typing.binding_schemes.contains_key(id)
            })
            .and_then(|(id, _)| self.typing.binding_schemes.get(&id))
            .map(|scheme| scheme.type_params.len())
    }

    pub fn type_param_kind(&self, symbol: Symbol) -> Option<HirTyId> {
        self.typing
            .type_param_kind_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(&symbol).copied())
    }

    pub fn template_parts(&self, range: SliceRange<HirTemplatePart>) -> TemplatePartList {
        self.module
            .resolved
            .module
            .store
            .template_parts
            .get(range)
            .to_vec()
    }
}
