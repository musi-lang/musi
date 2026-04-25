use std::fmt::Write as _;

use music_arena::SliceRange;
use music_base::diag::DiagContext;
use music_hir::{HirDim, HirOrigin, HirTyId, HirTyKind, simple_hir_ty_display_name};
use music_names::Symbol;

use crate::checker::{DiagKind, PassBase};

use super::HirTyFieldRange;

impl PassBase<'_, '_, '_> {
    pub fn render_ty(&self, ty: HirTyId) -> String {
        let kind = &self.ty(ty).kind;
        Self::render_ty_builtin(kind)
            .or_else(|| self.render_ty_named_or_callable(kind))
            .or_else(|| self.render_ty_collection(kind))
            .or_else(|| self.render_ty_range(kind))
            .or_else(|| self.render_ty_special(kind))
            .unwrap_or_else(|| "<error>".into())
    }

    pub(super) fn render_ty_builtin(kind: &HirTyKind) -> Option<String> {
        if let HirTyKind::NatLit(value) = kind {
            return Some(value.to_string());
        }
        simple_hir_ty_display_name(kind).map(str::to_owned)
    }

    pub(super) fn render_ty_named_or_callable(&self, kind: &HirTyKind) -> Option<String> {
        Some(match kind {
            HirTyKind::Named { name, args } => self.render_named_ty(*name, *args),
            HirTyKind::Pi {
                binder,
                binder_ty,
                body,
                is_effectful,
            } => {
                let binder = self.resolve_symbol(*binder);
                let arrow = if *is_effectful { " ~> " } else { " -> " };
                format!(
                    "forall ({binder} : {}){arrow}{}",
                    self.render_ty(*binder_ty),
                    self.render_ty(*body)
                )
            }
            HirTyKind::Arrow {
                params,
                ret,
                is_effectful,
            } => self.render_arrow_ty(*params, *ret, *is_effectful),
            HirTyKind::Sum { left, right } => {
                format!("{} + {}", self.render_ty(*left), self.render_ty(*right))
            }
            _ => return None,
        })
    }

    pub(super) fn render_ty_collection(&self, kind: &HirTyKind) -> Option<String> {
        Some(match kind {
            HirTyKind::Tuple { items } => self.render_tuple_ty(*items),
            HirTyKind::Seq { item } => format!("[]{}", self.render_ty(*item)),
            HirTyKind::Array { dims, item } => self.render_array_ty(dims.clone(), *item),
            HirTyKind::Bits { width } => format!("Bits[{width}]"),
            _ => return None,
        })
    }

    pub(super) fn render_ty_range(&self, kind: &HirTyKind) -> Option<String> {
        Some(match kind {
            HirTyKind::Range { bound } => format!("Range[{}]", self.render_ty(*bound)),
            _ => return None,
        })
    }

    pub(super) fn render_ty_special(&self, kind: &HirTyKind) -> Option<String> {
        Some(match kind {
            HirTyKind::Handler {
                effect,
                input,
                output,
            } => format!(
                "answer {} ({} -> {})",
                self.render_ty(*effect),
                self.render_ty(*input),
                self.render_ty(*output)
            ),
            HirTyKind::Mut { inner } => format!("mut {}", self.render_ty(*inner)),
            HirTyKind::AnyShape { capability } => {
                format!("any {}", self.render_ty(*capability))
            }
            HirTyKind::SomeShape { capability } => {
                format!("some {}", self.render_ty(*capability))
            }
            HirTyKind::Record { fields } => self.render_record_ty(fields.clone()),
            _ => return None,
        })
    }
    pub(super) fn render_named_ty(&self, name: Symbol, args: SliceRange<HirTyId>) -> String {
        let args = self.ty_ids(args);
        let name_text = self.resolve_symbol(name);
        let mut out = String::from(name_text);
        if args.is_empty() {
            return out;
        }
        out.push('[');
        for (idx, arg) in args.into_iter().enumerate() {
            if idx != 0 {
                out.push_str(", ");
            }
            out.push_str(&self.render_ty(arg));
        }
        out.push(']');
        out
    }

    pub(super) fn render_arrow_ty(
        &self,
        params: SliceRange<HirTyId>,
        ret: HirTyId,
        is_effectful: bool,
    ) -> String {
        let params = self.ty_ids(params);
        let left = if params.len() == 1 {
            self.render_ty(params[0])
        } else {
            format!(
                "({})",
                params
                    .into_iter()
                    .map(|param| self.render_ty(param))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        let arrow = if is_effectful { " ~> " } else { " -> " };
        format!("{left}{arrow}{}", self.render_ty(ret))
    }

    pub(super) fn render_tuple_ty(&self, items: SliceRange<HirTyId>) -> String {
        format!(
            "({})",
            self.ty_ids(items)
                .into_iter()
                .map(|item| self.render_ty(item))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    pub(super) fn render_array_ty(&self, dims: SliceRange<HirDim>, item: HirTyId) -> String {
        let dims = self.dims(dims);
        let mut out = String::new();
        for dim in dims {
            out.push('[');
            match dim {
                HirDim::Unknown => out.push('_'),
                HirDim::Name(name) => out.push_str(self.resolve_symbol(name.name)),
                HirDim::Int(value) => {
                    write!(&mut out, "{value}").expect("writing to String should not fail");
                }
            }
            out.push(']');
        }
        out.push_str(&self.render_ty(item));
        out
    }

    pub(super) fn render_record_ty(&self, fields: HirTyFieldRange) -> String {
        let mut parts = Vec::new();
        for field in self.ty_fields(fields) {
            parts.push(format!(
                "{} = {}",
                self.resolve_symbol(field.name),
                self.render_ty(field.ty)
            ));
        }
        format!("{{{}}}", parts.join(", "))
    }

    pub fn type_mismatch(&mut self, origin: HirOrigin, expected: HirTyId, found: HirTyId) {
        if self.ty_matches(expected, found) {
            return;
        }
        let expected = self.render_ty(expected);
        let found = self.render_ty(found);
        self.diag_with(
            origin.span,
            DiagKind::TypeMismatch,
            DiagContext::new()
                .with("subject", "value")
                .with("expected", expected)
                .with("found", found),
        );
    }

    pub fn type_mismatch_for(
        &mut self,
        subject: &str,
        origin: HirOrigin,
        expected: HirTyId,
        found: HirTyId,
    ) {
        if self.ty_matches(expected, found) {
            return;
        }
        let expected = self.render_ty(expected);
        let found = self.render_ty(found);
        self.diag_with(
            origin.span,
            DiagKind::TypeMismatch,
            DiagContext::new()
                .with("subject", subject)
                .with("expected", expected)
                .with("found", found),
        );
    }
}
