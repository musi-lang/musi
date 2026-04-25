use music_arena::SliceRange;
use music_base::diag::DiagContext;
use music_hir::{
    HirBinaryOp, HirDim, HirExprId, HirExprKind, HirLitKind, HirOrigin, HirPrefixOp, HirRecordItem,
    HirTyField, HirTyId, HirTyKind,
};

use crate::checker::{DiagKind, PassBase};

impl PassBase<'_, '_, '_> {
    pub fn lower_type_expr(&mut self, expr: HirExprId, origin: HirOrigin) -> HirTyId {
        let builtins = self.builtins();
        self.lower_type_atomic_expr(expr)
            .or_else(|| self.lower_type_aggregate_expr(expr))
            .or_else(|| self.lower_type_callable_expr(expr))
            .or_else(|| self.lower_type_operator_expr(expr, origin))
            .unwrap_or_else(|| {
                let target = self.expr_subject(expr);
                self.diag_with(
                    origin.span,
                    DiagKind::InvalidTypeExpression,
                    DiagContext::new().with("target", target),
                );
                builtins.error
            })
    }

    pub(super) fn lower_type_atomic_expr(&mut self, expr: HirExprId) -> Option<HirTyId> {
        Some(match self.expr(expr).kind {
            HirExprKind::Error => self.builtins().error,
            HirExprKind::Name { name } => self.named_type_for_symbol(name.name),
            HirExprKind::Lit { lit } => match self.lit_kind(lit) {
                HirLitKind::Int { raw } => match raw.parse::<u64>() {
                    Ok(value) => self.alloc_ty(HirTyKind::NatLit(value)),
                    Err(_) => self.builtins().error,
                },
                _ => return None,
            },
            _ => return None,
        })
    }

    pub(super) fn lower_type_aggregate_expr(&mut self, expr: HirExprId) -> Option<HirTyId> {
        Some(match self.expr(expr).kind {
            HirExprKind::Tuple { items } => self.lower_tuple_type_expr(items),
            HirExprKind::ArrayTy { dims, item } => {
                let item_origin = self.expr(item).origin;
                let item_ty = self.lower_type_expr(item, item_origin);
                if self.dims(dims.clone()).is_empty() {
                    self.alloc_ty(HirTyKind::Seq { item: item_ty })
                } else {
                    self.alloc_ty(HirTyKind::Array {
                        dims,
                        item: item_ty,
                    })
                }
            }
            HirExprKind::Record { items } => self.lower_record_type_expr(items),
            _ => return None,
        })
    }

    pub(super) fn lower_type_callable_expr(&mut self, expr: HirExprId) -> Option<HirTyId> {
        Some(match self.expr(expr).kind {
            HirExprKind::AnswerTy {
                effect,
                input,
                output,
            } => {
                let effect_origin = self.expr(effect).origin;
                let effect = self.lower_type_expr(effect, effect_origin);
                let input_origin = self.expr(input).origin;
                let input = self.lower_type_expr(input, input_origin);
                let output_origin = self.expr(output).origin;
                let output = self.lower_type_expr(output, output_origin);
                self.alloc_ty(HirTyKind::Handler {
                    effect,
                    input,
                    output,
                })
            }
            HirExprKind::Pi {
                binder: _,
                binder_ty,
                ret,
                is_effectful,
            } => {
                let binder_origin = self.expr(binder_ty).origin;
                let binder_ty = self.lower_type_expr(binder_ty, binder_origin);
                let params = self.alloc_ty_list([binder_ty]);
                let ret_origin = self.expr(ret).origin;
                let ret = self.lower_type_expr(ret, ret_origin);
                self.alloc_ty(HirTyKind::Arrow {
                    params,
                    ret,
                    is_effectful,
                })
            }
            _ => return None,
        })
    }

    pub(super) fn lower_type_operator_expr(
        &mut self,
        expr: HirExprId,
        origin: HirOrigin,
    ) -> Option<HirTyId> {
        Some(match self.expr(expr).kind {
            HirExprKind::Apply { callee, args } => self.lower_apply_type_expr(origin, callee, args),
            HirExprKind::Index { base, args } => self.lower_apply_type_expr(origin, base, args),
            HirExprKind::Binary { op, left, right } => {
                self.lower_binary_type_expr(origin, &op, left, right)
            }
            HirExprKind::Prefix {
                op: HirPrefixOp::Mut,
                expr,
            } => {
                let origin = self.expr(expr).origin;
                let inner = self.lower_type_expr(expr, origin);
                self.alloc_ty(HirTyKind::Mut { inner })
            }
            HirExprKind::Prefix {
                op: HirPrefixOp::Any,
                expr,
            } => {
                let origin = self.expr(expr).origin;
                let shape = self.lower_type_expr(expr, origin);
                self.alloc_ty(HirTyKind::AnyShape { capability: shape })
            }
            HirExprKind::Prefix {
                op: HirPrefixOp::Some,
                expr,
            } => {
                let origin = self.expr(expr).origin;
                let shape = self.lower_type_expr(expr, origin);
                self.alloc_ty(HirTyKind::SomeShape { capability: shape })
            }
            _ => return None,
        })
    }

    pub(super) fn lower_tuple_type_expr(&mut self, items: SliceRange<HirExprId>) -> HirTyId {
        let items = self
            .expr_ids(items)
            .into_iter()
            .map(|item| {
                let origin = self.expr(item).origin;
                self.lower_type_expr(item, origin)
            })
            .collect::<Vec<_>>();
        let items = self.alloc_ty_list(items);
        self.alloc_ty(HirTyKind::Tuple { items })
    }

    pub(super) fn lower_apply_type_expr(
        &mut self,
        origin: HirOrigin,
        callee: HirExprId,
        args: SliceRange<HirExprId>,
    ) -> HirTyId {
        let callee_ty = self.lower_type_expr(callee, self.expr(callee).origin);
        let args = self
            .expr_ids(args)
            .into_iter()
            .map(|arg| {
                let origin = self.expr(arg).origin;
                self.lower_type_expr(arg, origin)
            })
            .collect::<Vec<_>>();
        if args.is_empty() {
            return callee_ty;
        }
        if self
            .remaining_constructor_kind(origin, callee_ty, args.len())
            .is_none()
        {
            let target = self.render_ty(callee_ty);
            self.diag_with(
                origin.span,
                DiagKind::InvalidTypeApplication,
                DiagContext::new().with("target", target),
            );
            return self.builtins().error;
        }
        let HirTyKind::Named {
            name,
            args: existing_args,
        } = self.ty(callee_ty).kind
        else {
            let target = self.render_ty(callee_ty);
            self.diag_with(
                origin.span,
                DiagKind::InvalidTypeApplication,
                DiagContext::new().with("target", target),
            );
            return self.builtins().error;
        };
        let mut all_args = self.ty_ids(existing_args);
        all_args.extend(args);
        match self.resolve_symbol(name) {
            "Array" if all_args.len() == 1 => {
                let dims = self.alloc_dims([HirDim::Unknown]);
                self.alloc_ty(HirTyKind::Array {
                    dims,
                    item: all_args[0],
                })
            }
            "Range" if all_args.len() == 1 => {
                self.alloc_ty(HirTyKind::Range { bound: all_args[0] })
            }
            "Bits" if all_args.len() == 1 => self.lower_bits_ty(origin, all_args[0]),
            _ => {
                let args = self.alloc_ty_list(all_args);
                self.alloc_ty(HirTyKind::Named { name, args })
            }
        }
    }

    pub(super) fn lower_bits_ty(&mut self, origin: HirOrigin, width_ty: HirTyId) -> HirTyId {
        match self.ty(width_ty).kind {
            HirTyKind::NatLit(width) if width > 0 && u32::try_from(width).is_ok() => {
                self.alloc_ty(HirTyKind::Bits {
                    width: u32::try_from(width).unwrap_or(u32::MAX),
                })
            }
            HirTyKind::Named { name, args }
                if self.ty_ids(args).is_empty() && self.type_param_is_nat(name) =>
            {
                let args = self.alloc_ty_list([width_ty]);
                self.alloc_ty(HirTyKind::Named {
                    name: self.known().bits,
                    args,
                })
            }
            _ => {
                let width = self.render_ty(width_ty);
                self.diag_with(
                    origin.span,
                    DiagKind::InvalidBitsWidth,
                    DiagContext::new().with("width", width),
                );
                self.builtins().error
            }
        }
    }

    pub(super) fn lower_binary_type_expr(
        &mut self,
        origin: HirOrigin,
        op: &HirBinaryOp,
        left: HirExprId,
        right: HirExprId,
    ) -> HirTyId {
        match op {
            &HirBinaryOp::Arrow | &HirBinaryOp::EffectArrow => {
                let left_origin = self.expr(left).origin;
                let left = self.lower_type_expr(left, left_origin);
                let params = self.alloc_ty_list([left]);
                let right_origin = self.expr(right).origin;
                let ret = self.lower_type_expr(right, right_origin);
                self.alloc_ty(HirTyKind::Arrow {
                    params,
                    ret,
                    is_effectful: matches!(op, &HirBinaryOp::EffectArrow),
                })
            }
            &HirBinaryOp::Add => {
                let left_origin = self.expr(left).origin;
                let right_origin = self.expr(right).origin;
                let left = self.lower_type_expr(left, left_origin);
                let right = self.lower_type_expr(right, right_origin);
                self.alloc_ty(HirTyKind::Sum { left, right })
            }
            _ => {
                let target = self.binary_op_subject(op);
                self.diag_with(
                    origin.span,
                    DiagKind::InvalidTypeExpression,
                    DiagContext::new().with("target", target),
                );
                self.builtins().error
            }
        }
    }

    pub(super) fn lower_record_type_expr(&mut self, items: SliceRange<HirRecordItem>) -> HirTyId {
        let fields = self
            .record_items(items)
            .into_iter()
            .filter_map(|item| {
                item.name.map(|name| {
                    let origin = self.expr(item.value).origin;
                    HirTyField::new(name.name, self.lower_type_expr(item.value, origin))
                })
            })
            .collect::<Vec<_>>();
        let fields = self.alloc_ty_fields(fields);
        self.alloc_ty(HirTyKind::Record { fields })
    }
}
