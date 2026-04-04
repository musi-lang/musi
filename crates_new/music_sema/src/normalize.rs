use std::collections::BTreeMap;

use music_arena::SliceRange;
use music_hir::{
    HirBinaryOp, HirConstraint, HirConstraintKind, HirDim, HirEffectSet, HirExprId, HirExprKind,
    HirOrigin, HirParam, HirPrefixOp, HirRecordItem, HirTyField, HirTyId, HirTyKind,
};
use music_names::Symbol;

use crate::api::{ConstraintFacts, ConstraintKind};
use crate::context::{CheckPass, PassBase};
use crate::effects::{EffectKey, EffectRow};
use crate::exprs::check_expr;

pub fn render_ty(ctx: &PassBase<'_, '_, '_>, ty: HirTyId) -> String {
    match ctx.ty(ty).kind {
        HirTyKind::Error => "<error>".into(),
        HirTyKind::Unknown => "Unknown".into(),
        HirTyKind::Type => "Type".into(),
        HirTyKind::Syntax => "Syntax".into(),
        HirTyKind::Any => "Any".into(),
        HirTyKind::Empty => "Empty".into(),
        HirTyKind::Unit => "Unit".into(),
        HirTyKind::Bool => "Bool".into(),
        HirTyKind::Int => "Int".into(),
        HirTyKind::Float => "Float".into(),
        HirTyKind::String => "String".into(),
        HirTyKind::CString => "CString".into(),
        HirTyKind::CPtr => "CPtr".into(),
        HirTyKind::Module => "Module".into(),
        HirTyKind::Named { name, args } => {
            let mut out = String::from(ctx.resolve_symbol(name));
            let args = ctx.ty_ids(args);
            if !args.is_empty() {
                out.push('[');
                for (idx, arg) in args.into_iter().enumerate() {
                    if idx != 0 {
                        out.push_str(", ");
                    }
                    out.push_str(&render_ty(ctx, arg));
                }
                out.push(']');
            }
            out
        }
        HirTyKind::Arrow {
            params,
            ret,
            is_effectful,
        } => {
            let params = ctx.ty_ids(params);
            let left = if params.len() == 1 {
                render_ty(ctx, params[0])
            } else {
                format!(
                    "({})",
                    params
                        .into_iter()
                        .map(|param| render_ty(ctx, param))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            };
            let arrow = if is_effectful { " ~> " } else { " -> " };
            format!("{left}{arrow}{}", render_ty(ctx, ret))
        }
        HirTyKind::Sum { left, right } => {
            format!("{} + {}", render_ty(ctx, left), render_ty(ctx, right))
        }
        HirTyKind::Tuple { items } => format!(
            "({})",
            ctx.ty_ids(items)
                .into_iter()
                .map(|item| render_ty(ctx, item))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        HirTyKind::Array { dims, item } => {
            let dims = ctx.dims(dims);
            let dim_text = dims
                .into_iter()
                .map(|dim| match dim {
                    HirDim::Unknown => "_".into(),
                    HirDim::Name(name) => ctx.resolve_symbol(name.name).into(),
                    HirDim::Int(value) => value.to_string(),
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{dim_text}]{}", render_ty(ctx, item))
        }
        HirTyKind::Mut { inner } => format!("mut {}", render_ty(ctx, inner)),
        HirTyKind::Record { fields } => {
            let mut parts = Vec::new();
            for field in ctx.ty_fields(fields) {
                parts.push(format!(
                    "{} := {}",
                    ctx.resolve_symbol(field.name),
                    render_ty(ctx, field.ty)
                ));
            }
            format!("{{{}}}", parts.join(", "))
        }
    }
}

pub fn type_mismatch(
    ctx: &mut PassBase<'_, '_, '_>,
    origin: HirOrigin,
    expected: HirTyId,
    found: HirTyId,
) {
    if ty_matches(ctx, expected, found) {
        return;
    }
    let label = format!(
        "expected {}, found {}",
        render_ty(ctx, expected),
        render_ty(ctx, found)
    );
    ctx.diag(origin.span, "type mismatch", &label);
}

pub fn ty_matches(ctx: &PassBase<'_, '_, '_>, expected: HirTyId, found: HirTyId) -> bool {
    if expected == found {
        return true;
    }
    let left = ctx.ty(expected).kind;
    let right = ctx.ty(found).kind;
    if matches!(left, HirTyKind::Error | HirTyKind::Unknown)
        || matches!(right, HirTyKind::Error | HirTyKind::Unknown)
    {
        return true;
    }
    match (left, right) {
        (HirTyKind::Type, HirTyKind::Type)
        | (HirTyKind::Syntax, HirTyKind::Syntax)
        | (HirTyKind::Any, HirTyKind::Any)
        | (HirTyKind::Empty, HirTyKind::Empty)
        | (HirTyKind::Unit, HirTyKind::Unit)
        | (HirTyKind::Bool, HirTyKind::Bool)
        | (HirTyKind::Int, HirTyKind::Int)
        | (HirTyKind::Float, HirTyKind::Float)
        | (HirTyKind::String, HirTyKind::String)
        | (HirTyKind::CString, HirTyKind::CString)
        | (HirTyKind::CPtr, HirTyKind::CPtr)
        | (HirTyKind::Module, HirTyKind::Module) => true,
        (
            HirTyKind::Named {
                name: left_name,
                args: left_args,
            },
            HirTyKind::Named {
                name: right_name,
                args: right_args,
            },
        ) => named_tys_match(ctx, left_name, left_args, right_name, right_args),
        (
            HirTyKind::Arrow {
                params: left_params,
                ret: left_ret,
                is_effectful: left_effectful,
            },
            HirTyKind::Arrow {
                params: right_params,
                ret: right_ret,
                is_effectful: right_effectful,
            },
        ) => arrow_tys_match(
            ctx,
            left_params,
            left_ret,
            left_effectful,
            right_params,
            right_ret,
            right_effectful,
        ),
        (
            HirTyKind::Sum { left, right },
            HirTyKind::Sum {
                left: other_left,
                right: other_right,
            },
        ) => ty_matches(ctx, left, other_left) && ty_matches(ctx, right, other_right),
        (HirTyKind::Tuple { items: left }, HirTyKind::Tuple { items: right }) => {
            list_tys_match(ctx, left, right)
        }
        (
            HirTyKind::Array {
                dims: left_dims,
                item: left_item,
            },
            HirTyKind::Array {
                dims: right_dims,
                item: right_item,
            },
        ) => ctx.dims(left_dims) == ctx.dims(right_dims) && ty_matches(ctx, left_item, right_item),
        (HirTyKind::Mut { inner: left }, HirTyKind::Mut { inner: right }) => {
            ty_matches(ctx, left, right)
        }
        (HirTyKind::Record { fields: left }, HirTyKind::Record { fields: right }) => {
            record_tys_match(ctx, left, right)
        }
        _ => false,
    }
}

fn list_tys_match(
    ctx: &PassBase<'_, '_, '_>,
    left: SliceRange<HirTyId>,
    right: SliceRange<HirTyId>,
) -> bool {
    let left = ctx.ty_ids(left);
    let right = ctx.ty_ids(right);
    left.len() == right.len()
        && left
            .into_iter()
            .zip(right)
            .all(|(left, right)| ty_matches(ctx, left, right))
}

fn named_tys_match(
    ctx: &PassBase<'_, '_, '_>,
    left_name: Symbol,
    left_args: SliceRange<HirTyId>,
    right_name: Symbol,
    right_args: SliceRange<HirTyId>,
) -> bool {
    left_name == right_name && list_tys_match(ctx, left_args, right_args)
}

fn arrow_tys_match(
    ctx: &PassBase<'_, '_, '_>,
    left_params: SliceRange<HirTyId>,
    left_ret: HirTyId,
    left_effectful: bool,
    right_params: SliceRange<HirTyId>,
    right_ret: HirTyId,
    right_effectful: bool,
) -> bool {
    left_effectful == right_effectful
        && list_tys_match(ctx, left_params, right_params)
        && ty_matches(ctx, left_ret, right_ret)
}

fn record_tys_match(
    ctx: &PassBase<'_, '_, '_>,
    left: SliceRange<HirTyField>,
    right: SliceRange<HirTyField>,
) -> bool {
    let left_fields = ctx.ty_fields(left);
    let right_fields = ctx.ty_fields(right);
    let right_map = right_fields
        .into_iter()
        .map(|field| (field.name, field.ty))
        .collect::<BTreeMap<_, _>>();
    left_fields.len() == right_map.len()
        && left_fields.into_iter().all(|field| {
            right_map
                .get(&field.name)
                .is_some_and(|right_ty| ty_matches(ctx, field.ty, *right_ty))
        })
}

pub fn named_type_for_symbol(ctx: &mut PassBase<'_, '_, '_>, symbol: Symbol) -> HirTyId {
    let known = ctx.known();
    let builtins = ctx.builtins();
    if symbol == known.type_ {
        builtins.type_
    } else if symbol == known.any {
        builtins.any
    } else if symbol == known.unknown {
        builtins.unknown
    } else if symbol == known.syntax {
        builtins.syntax
    } else if symbol == known.empty {
        builtins.empty
    } else if symbol == known.unit {
        builtins.unit
    } else if symbol == known.bool_ {
        builtins.bool_
    } else if symbol == known.int_ {
        builtins.int_
    } else if symbol == known.float_ {
        builtins.float_
    } else if symbol == known.string_ {
        builtins.string_
    } else if symbol == known.cstring {
        builtins.cstring
    } else if symbol == known.cptr {
        builtins.cptr
    } else {
        let args = ctx.alloc_ty_list(Vec::<HirTyId>::new());
        ctx.alloc_ty(HirTyKind::Named { name: symbol, args })
    }
}

pub fn symbol_value_type(ctx: &PassBase<'_, '_, '_>, symbol: Symbol) -> HirTyId {
    let known = ctx.known();
    let builtins = ctx.builtins();
    if [
        known.type_,
        known.any,
        known.unknown,
        known.syntax,
        known.empty,
        known.unit,
        known.bool_,
        known.int_,
        known.float_,
        known.string_,
        known.cstring,
        known.cptr,
    ]
    .contains(&symbol)
    {
        builtins.type_
    } else {
        builtins.unknown
    }
}

pub fn lower_params(
    ctx: &mut CheckPass<'_, '_, '_>,
    range: SliceRange<HirParam>,
) -> Box<[HirTyId]> {
    let builtins = ctx.builtins();
    ctx.params(range)
        .into_iter()
        .map(|param| {
            let ty = param.ty.map_or(builtins.unknown, |expr| {
                let origin = ctx.expr(expr).origin;
                lower_type_expr(ctx, expr, origin)
            });
            if let Some(binding) = ctx.binding_id_for_decl(param.name) {
                ctx.insert_binding_type(binding, ty);
            }
            if let Some(default) = param.default {
                let facts = check_expr(ctx, default);
                let origin = ctx.expr(default).origin;
                type_mismatch(ctx, origin, ty, facts.ty);
            }
            ty
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub fn lower_type_expr(
    ctx: &mut PassBase<'_, '_, '_>,
    expr: HirExprId,
    origin: HirOrigin,
) -> HirTyId {
    let builtins = ctx.builtins();
    match ctx.expr(expr).kind {
        HirExprKind::Error => builtins.error,
        HirExprKind::Name { name } => named_type_for_symbol(ctx, name.name),
        HirExprKind::Tuple { items } => lower_tuple_type_expr(ctx, items),
        HirExprKind::ArrayTy { dims, item } => {
            let item_origin = ctx.expr(item).origin;
            let item = lower_type_expr(ctx, item, item_origin);
            ctx.alloc_ty(HirTyKind::Array { dims, item })
        }
        HirExprKind::Pi {
            binder: _,
            binder_ty,
            ret,
            is_effectful,
        } => {
            let binder_origin = ctx.expr(binder_ty).origin;
            let binder_ty = lower_type_expr(ctx, binder_ty, binder_origin);
            let params = ctx.alloc_ty_list([binder_ty]);
            let ret_origin = ctx.expr(ret).origin;
            let ret = lower_type_expr(ctx, ret, ret_origin);
            ctx.alloc_ty(HirTyKind::Arrow {
                params,
                ret,
                is_effectful,
            })
        }
        HirExprKind::Apply { callee, args } => lower_apply_type_expr(ctx, origin, callee, args),
        HirExprKind::Binary { op, left, right } => {
            lower_binary_type_expr(ctx, origin, &op, left, right)
        }
        HirExprKind::Prefix {
            op: HirPrefixOp::Mut,
            expr,
        } => {
            let origin = ctx.expr(expr).origin;
            let inner = lower_type_expr(ctx, expr, origin);
            ctx.alloc_ty(HirTyKind::Mut { inner })
        }
        HirExprKind::Import { .. } => builtins.module,
        HirExprKind::Record { items } => lower_record_type_expr(ctx, items),
        _ => {
            ctx.diag(origin.span, "invalid type expression", "");
            builtins.error
        }
    }
}

fn lower_tuple_type_expr(ctx: &mut PassBase<'_, '_, '_>, items: SliceRange<HirExprId>) -> HirTyId {
    let items = ctx
        .expr_ids(items)
        .into_iter()
        .map(|item| {
            let origin = ctx.expr(item).origin;
            lower_type_expr(ctx, item, origin)
        })
        .collect::<Vec<_>>();
    let items = ctx.alloc_ty_list(items);
    ctx.alloc_ty(HirTyKind::Tuple { items })
}

fn lower_apply_type_expr(
    ctx: &mut PassBase<'_, '_, '_>,
    origin: HirOrigin,
    callee: HirExprId,
    args: SliceRange<HirExprId>,
) -> HirTyId {
    let HirExprKind::Name { name } = ctx.expr(callee).kind else {
        ctx.diag(origin.span, "invalid type application", "");
        return ctx.builtins().error;
    };
    let args = ctx
        .expr_ids(args)
        .into_iter()
        .map(|arg| {
            let origin = ctx.expr(arg).origin;
            lower_type_expr(ctx, arg, origin)
        })
        .collect::<Vec<_>>();
    let args = ctx.alloc_ty_list(args);
    ctx.alloc_ty(HirTyKind::Named {
        name: name.name,
        args,
    })
}

fn lower_binary_type_expr(
    ctx: &mut PassBase<'_, '_, '_>,
    origin: HirOrigin,
    op: &HirBinaryOp,
    left: HirExprId,
    right: HirExprId,
) -> HirTyId {
    match op {
        &HirBinaryOp::Arrow | &HirBinaryOp::EffectArrow => {
            let left_origin = ctx.expr(left).origin;
            let left = lower_type_expr(ctx, left, left_origin);
            let params = ctx.alloc_ty_list([left]);
            let right_origin = ctx.expr(right).origin;
            let ret = lower_type_expr(ctx, right, right_origin);
            ctx.alloc_ty(HirTyKind::Arrow {
                params,
                ret,
                is_effectful: matches!(op, &HirBinaryOp::EffectArrow),
            })
        }
        &HirBinaryOp::Add => {
            let left_origin = ctx.expr(left).origin;
            let right_origin = ctx.expr(right).origin;
            let left = lower_type_expr(ctx, left, left_origin);
            let right = lower_type_expr(ctx, right, right_origin);
            ctx.alloc_ty(HirTyKind::Sum { left, right })
        }
        _ => {
            ctx.diag(origin.span, "invalid type expression", "");
            ctx.builtins().error
        }
    }
}

fn lower_record_type_expr(
    ctx: &mut PassBase<'_, '_, '_>,
    items: SliceRange<HirRecordItem>,
) -> HirTyId {
    let fields = ctx
        .record_items(items)
        .into_iter()
        .filter_map(|item| {
            item.name.map(|name| HirTyField {
                name: name.name,
                ty: {
                    let origin = ctx.expr(item.value).origin;
                    lower_type_expr(ctx, item.value, origin)
                },
            })
        })
        .collect::<Vec<_>>();
    let fields = ctx.alloc_ty_fields(fields);
    ctx.alloc_ty(HirTyKind::Record { fields })
}

pub fn lower_effect_row(ctx: &mut PassBase<'_, '_, '_>, row: &HirEffectSet) -> EffectRow {
    let mut out = EffectRow::empty();
    for item in ctx.effect_items(row) {
        let arg = item.arg.map(|expr| {
            let origin = ctx.expr(expr).origin;
            lower_type_expr(ctx, expr, origin)
        });
        out.add(EffectKey {
            name: ctx.resolve_symbol(item.name.name).into(),
            arg,
        });
    }
    out.open = row
        .open
        .map(|ident| Box::<str>::from(ctx.resolve_symbol(ident.name)));
    out
}

pub fn lower_constraints(
    ctx: &mut PassBase<'_, '_, '_>,
    constraints: SliceRange<HirConstraint>,
) -> Box<[ConstraintFacts]> {
    ctx.constraints(constraints)
        .into_iter()
        .map(|constraint| ConstraintFacts {
            name: constraint.name.name,
            kind: match constraint.kind {
                HirConstraintKind::Subtype => ConstraintKind::Subtype,
                HirConstraintKind::Implements => ConstraintKind::Implements,
            },
            value: {
                let origin = ctx.expr(constraint.value).origin;
                lower_type_expr(ctx, constraint.value, origin)
            },
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}
