use super::*;
use music_hir::{SIMPLE_HIR_TYS, simple_hir_ty_name};

pub(super) fn render_type_value_expr_name(
    sema: &SemaModule,
    expr_id: HirExprId,
    interner: &Interner,
) -> Box<str> {
    let kind = &sema.module().store.exprs.get(expr_id).kind;
    if let Some(name) = render_atomic_type_value_expr_name(kind, interner) {
        return name;
    }
    if let Some(name) = render_composite_type_value_expr_name(sema, kind, interner) {
        return name;
    }
    match kind {
        HirExprKind::Pi {
            binder,
            binder_ty,
            ret,
            is_effectful,
        } => {
            let binder = interner.resolve(binder.name);
            let binder_ty = render_type_value_expr_name(sema, *binder_ty, interner);
            let ret = render_type_value_expr_name(sema, *ret, interner);
            let arrow = if *is_effectful { "~>" } else { "->" };
            format!("forall ({binder} : {binder_ty}) {arrow} {ret}").into()
        }
        HirExprKind::Apply { callee, args } => {
            render_apply_type_value_expr_name(sema, *callee, *args, interner)
        }
        HirExprKind::Binary { op, left, right } => {
            let left = render_type_value_expr_name(sema, *left, interner);
            let right = render_type_value_expr_name(sema, *right, interner);
            match op {
                HirBinaryOp::Arrow => format!("{left} -> {right}").into(),
                HirBinaryOp::EffectArrow => format!("{left} ~> {right}").into(),
                HirBinaryOp::Add => format!("{left} + {right}").into(),
                _ => invalid_lowering_path("invalid type-value binary op"),
            }
        }
        HirExprKind::Prefix {
            op: HirPrefixOp::Mut,
            expr,
        } => format!("mut {}", render_type_value_expr_name(sema, *expr, interner)).into(),
        other => invalid_lowering_path(format!("invalid type value expr {other:?}")),
    }
}

fn render_atomic_type_value_expr_name(kind: &HirExprKind, interner: &Interner) -> Option<Box<str>> {
    Some(match kind {
        HirExprKind::Error => "Error".into(),
        HirExprKind::Name { name } | HirExprKind::Field { name, .. } => {
            interner.resolve(name.name).into()
        }
        _ => return None,
    })
}

fn render_composite_type_value_expr_name(
    sema: &SemaModule,
    kind: &HirExprKind,
    interner: &Interner,
) -> Option<Box<str>> {
    Some(match kind {
        HirExprKind::Tuple { items } => {
            let items = sema
                .module()
                .store
                .expr_ids
                .get(*items)
                .iter()
                .copied()
                .map(|item| render_type_value_expr_name(sema, item, interner).into_string())
                .collect::<Vec<_>>();
            format!("({})", items.join(", ")).into()
        }
        HirExprKind::ArrayTy { dims, item } => {
            render_array_type_value_expr_name(sema, dims.clone(), *item, interner)
        }
        HirExprKind::Record { items } => {
            let fields = sema
                .module()
                .store
                .record_items
                .get(items.clone())
                .iter()
                .filter_map(|item| {
                    item.name.map(|name| {
                        let name = interner.resolve(name.name);
                        let value = render_type_value_expr_name(sema, item.value, interner);
                        format!("{name} : {value}")
                    })
                })
                .collect::<Vec<_>>();
            format!("{{ {} }}", fields.join(", ")).into()
        }
        _ => return None,
    })
}

fn render_apply_type_value_expr_name(
    sema: &SemaModule,
    callee: HirExprId,
    args: SliceRange<HirExprId>,
    interner: &Interner,
) -> Box<str> {
    let HirExprKind::Name { name } = sema.module().store.exprs.get(callee).kind else {
        invalid_lowering_path("invalid type-value callee");
    };
    let callee_name = interner.resolve(name.name);
    let args = sema
        .module()
        .store
        .expr_ids
        .get(args)
        .iter()
        .copied()
        .map(|arg| render_type_value_apply_arg_name(sema, arg, interner).into_string())
        .collect::<Vec<_>>();
    format!("{callee_name}[{}]", args.join(", ")).into()
}

fn render_type_value_apply_arg_name(
    sema: &SemaModule,
    expr_id: HirExprId,
    interner: &Interner,
) -> Box<str> {
    match &sema.module().store.exprs.get(expr_id).kind {
        HirExprKind::Lit { lit } => match &sema.module().store.lits.get(*lit).kind {
            HirLitKind::Int { raw } => raw.clone(),
            _ => invalid_lowering_path("invalid type-value literal arg"),
        },
        _ => render_type_value_expr_name(sema, expr_id, interner),
    }
}

pub(super) fn is_builtin_type_name_symbol(text: &str) -> bool {
    SIMPLE_HIR_TYS.iter().any(|ty| ty.parse_name == text)
}

fn render_array_type_value_expr_name(
    sema: &SemaModule,
    dims: SliceRange<HirDim>,
    item: HirExprId,
    interner: &Interner,
) -> Box<str> {
    let item = render_type_value_expr_name(sema, item, interner);
    let dims = render_dim_prefix(sema.module().store.dims.get(dims), interner);
    format!("{dims}{item}").into()
}

pub(super) fn render_ty_name(sema: &SemaModule, ty: HirTyId, interner: &Interner) -> Box<str> {
    let kind = &sema.ty(ty).kind;
    if let Some(name) = render_atomic_ty_name(kind) {
        return name;
    }
    if let Some(name) = render_collection_ty_name(sema, kind, interner) {
        return name;
    }
    if let Some(name) = render_range_ty_name(sema, kind, interner) {
        return name;
    }
    match kind {
        HirTyKind::Named { name, args } => render_named_ty_name(sema, *name, *args, interner),
        HirTyKind::Pi {
            binder,
            binder_ty,
            body,
            is_effectful,
        } => {
            let binder = interner.resolve(*binder);
            let binder_ty = render_ty_name(sema, *binder_ty, interner);
            let body = render_ty_name(sema, *body, interner);
            let arrow = if *is_effectful { "~>" } else { "->" };
            format!("forall ({binder} : {binder_ty}) {arrow} {body}").into()
        }
        HirTyKind::Arrow {
            params,
            ret,
            is_effectful,
        } => render_arrow_ty_name(sema, *params, *ret, *is_effectful, interner),
        HirTyKind::Sum { left, right } => format!(
            "{} + {}",
            render_ty_name(sema, *left, interner),
            render_ty_name(sema, *right, interner)
        )
        .into(),
        HirTyKind::Handler {
            effect,
            input,
            output,
        } => format!(
            "using {} ({} -> {})",
            render_ty_name(sema, *effect, interner),
            render_ty_name(sema, *input, interner),
            render_ty_name(sema, *output, interner)
        )
        .into(),
        HirTyKind::Mut { inner } => {
            format!("mut {}", render_ty_name(sema, *inner, interner)).into()
        }
        HirTyKind::Record { fields } => render_record_ty_name(sema, fields, interner),
        _ => invalid_lowering_path("invalid type name kind"),
    }
}

fn render_atomic_ty_name(kind: &HirTyKind) -> Option<Box<str>> {
    if let HirTyKind::NatLit(value) = kind {
        return Some(value.to_string().into());
    }
    simple_hir_ty_name(kind).map(Into::into)
}

fn render_collection_ty_name(
    sema: &SemaModule,
    kind: &HirTyKind,
    interner: &Interner,
) -> Option<Box<str>> {
    Some(match kind {
        HirTyKind::Tuple { items } => render_tuple_ty_name(sema, *items, interner),
        HirTyKind::Seq { item } => format!("[]{}", render_ty_name(sema, *item, interner)).into(),
        HirTyKind::Array { dims, item } => render_array_ty_name(sema, dims, *item, interner),
        _ => return None,
    })
}

fn render_range_ty_name(
    sema: &SemaModule,
    kind: &HirTyKind,
    interner: &Interner,
) -> Option<Box<str>> {
    Some(match kind {
        HirTyKind::Range { bound } => {
            format!("Range[{}]", render_ty_name(sema, *bound, interner)).into()
        }
        HirTyKind::ClosedRange { bound } => {
            format!("ClosedRange[{}]", render_ty_name(sema, *bound, interner)).into()
        }
        HirTyKind::PartialRangeFrom { bound } => format!(
            "PartialRangeFrom[{}]",
            render_ty_name(sema, *bound, interner)
        )
        .into(),
        HirTyKind::PartialRangeUpTo { bound } => format!(
            "PartialRangeUpTo[{}]",
            render_ty_name(sema, *bound, interner)
        )
        .into(),
        HirTyKind::PartialRangeThru { bound } => format!(
            "PartialRangeThru[{}]",
            render_ty_name(sema, *bound, interner)
        )
        .into(),
        _ => return None,
    })
}

pub(super) fn render_named_ty_name(
    sema: &SemaModule,
    name: Symbol,
    args: SliceRange<HirTyId>,
    interner: &Interner,
) -> Box<str> {
    let args = render_ty_name_list(sema, args, interner);
    if args.is_empty() {
        interner.resolve(name).into()
    } else {
        format!("{}[{}]", interner.resolve(name), args.join(", ")).into()
    }
}

fn render_arrow_ty_name(
    sema: &SemaModule,
    params: SliceRange<HirTyId>,
    ret: HirTyId,
    is_effectful: bool,
    interner: &Interner,
) -> Box<str> {
    let params = render_ty_name_list(sema, params, interner);
    let ret = render_ty_name(sema, ret, interner);
    let arrow = if is_effectful { "~>" } else { "->" };
    format!("({}) {arrow} {}", params.join(", "), ret).into()
}

fn render_tuple_ty_name(
    sema: &SemaModule,
    items: SliceRange<HirTyId>,
    interner: &Interner,
) -> Box<str> {
    let items = render_ty_name_list(sema, items, interner);
    format!("({})", items.join(", ")).into()
}

fn render_array_ty_name(
    sema: &SemaModule,
    dims: &SliceRange<HirDim>,
    item: HirTyId,
    interner: &Interner,
) -> Box<str> {
    let dims = render_dim_prefix(sema.module().store.dims.get(dims.clone()), interner);
    let item = render_ty_name(sema, item, interner);
    format!("{dims}{item}").into()
}

fn render_dim_prefix(dims: &[HirDim], interner: &Interner) -> String {
    let mut rendered = String::new();
    for dim in dims {
        rendered.push('[');
        rendered.push_str(&render_dim(dim, interner));
        rendered.push(']');
    }
    rendered
}

fn render_record_ty_name(
    sema: &SemaModule,
    fields: &SliceRange<HirTyField>,
    interner: &Interner,
) -> Box<str> {
    let fields = sema
        .module()
        .store
        .ty_fields
        .get(fields.clone())
        .iter()
        .map(|field| {
            format!(
                "{}: {}",
                interner.resolve(field.name),
                render_ty_name(sema, field.ty, interner)
            )
        })
        .collect::<Vec<_>>();
    format!("{{ {} }}", fields.join("; ")).into()
}

fn render_ty_name_list(
    sema: &SemaModule,
    tys: SliceRange<HirTyId>,
    interner: &Interner,
) -> Vec<String> {
    sema.module()
        .store
        .ty_ids
        .get(tys)
        .iter()
        .copied()
        .map(|ty| render_ty_name(sema, ty, interner).into_string())
        .collect()
}

fn render_dim(dim: &HirDim, interner: &Interner) -> String {
    match dim {
        HirDim::Unknown => "_".into(),
        HirDim::Name(ident) => interner.resolve(ident.name).into(),
        HirDim::Int(value) => value.to_string(),
    }
}
