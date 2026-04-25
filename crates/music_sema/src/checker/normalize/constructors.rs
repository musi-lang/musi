use music_base::diag::DiagContext;
use music_hir::{HirOrigin, HirTyId, HirTyKind};
use music_names::Symbol;

use crate::checker::{DiagKind, PassBase};

impl PassBase<'_, '_, '_> {
    pub(super) fn type_param_is_nat(&self, name: Symbol) -> bool {
        self.type_param_kind(name)
            .is_some_and(|kind| self.ty_matches(kind, self.builtins().nat))
    }

    pub(super) fn type_constructor_param_kinds(&self, name: Symbol) -> Option<Vec<HirTyId>> {
        let known = self.known();
        if name == known.bits {
            return Some(vec![self.builtins().nat]);
        }
        if [
            known.array,
            known.range,
            known.closed_range,
            known.partial_range_from,
            known.partial_range_up_to,
            known.partial_range_thru,
        ]
        .contains(&name)
        {
            return Some(vec![self.builtins().type_]);
        }
        let text = self.resolve_symbol(name);
        if let Some(data) = self.data_def(text) {
            return Some(data.type_param_kinds().to_vec());
        }
        if let Some(facts) = self.shape_facts_by_name(name) {
            return Some(facts.type_param_kinds.to_vec());
        }
        self.type_constructor_scheme_arity(name)
            .map(|arity| vec![self.builtins().type_; arity])
    }

    pub(super) fn type_constructor_kind_from_params(&mut self, param_kinds: &[HirTyId]) -> HirTyId {
        let mut kind = self.builtins().type_;
        for param_kind in param_kinds.iter().rev().copied() {
            let params = self.alloc_ty_list([param_kind]);
            kind = self.alloc_ty(HirTyKind::Arrow {
                params,
                ret: kind,
                is_effectful: false,
            });
        }
        kind
    }

    pub(super) fn remaining_constructor_kind(
        &mut self,
        origin: HirOrigin,
        callee: HirTyId,
        arg_count: usize,
    ) -> Option<HirTyId> {
        let mut kind = match self.ty(callee).kind {
            HirTyKind::Named { name, args } if self.ty_ids(args).is_empty() => {
                self.type_param_kind(name).or_else(|| {
                    self.type_constructor_param_kinds(name)
                        .map(|param_kinds| self.type_constructor_kind_from_params(&param_kinds))
                })?
            }
            HirTyKind::Named { name, args } => {
                let param_kinds = self.type_constructor_param_kinds(name)?;
                let used = self.ty_ids(args).len();
                if used > param_kinds.len() {
                    self.diag(origin.span, DiagKind::TypeApplicationArityMismatch, "");
                    return None;
                }
                self.type_constructor_kind_from_params(&param_kinds[used..])
            }
            HirTyKind::Arrow { .. } => callee,
            HirTyKind::Error | HirTyKind::Unknown => return Some(self.builtins().unknown),
            _ => return None,
        };
        for _ in 0..arg_count {
            let HirTyKind::Arrow {
                params,
                ret,
                is_effectful: false,
            } = self.ty(kind).kind
            else {
                self.diag(origin.span, DiagKind::TypeApplicationArityMismatch, "");
                return None;
            };
            let params = self.ty_ids(params);
            if params.len() != 1 {
                let target = self.render_ty(kind);
                self.diag_with(
                    origin.span,
                    DiagKind::InvalidTypeApplication,
                    DiagContext::new().with("target", target),
                );
                return None;
            }
            kind = ret;
        }
        Some(kind)
    }
}
