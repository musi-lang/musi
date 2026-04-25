use super::*;

#[must_use]
pub(super) fn render_hir_ty(sema: &SemaModule, session: &Session, ty: HirTyId) -> String {
    let kind = &sema.ty(ty).kind;
    if let Some(atomic) = render_atomic_hir_ty(kind) {
        return atomic;
    }
    match kind {
        HirTyKind::Named { name, args } => render_named_hir_ty(
            session.resolve_symbol(*name),
            sema.module().store.ty_ids.get(*args),
            |ty| render_hir_ty(sema, session, ty),
        ),
        HirTyKind::Pi {
            binder,
            binder_ty,
            body,
            is_effectful,
        } => {
            let arrow = if *is_effectful { " ~> " } else { " -> " };
            format!(
                "({} : {}){arrow}{}",
                session.resolve_symbol(*binder),
                render_hir_ty(sema, session, *binder_ty),
                render_hir_ty(sema, session, *body)
            )
        }
        HirTyKind::Arrow {
            params,
            ret,
            is_effectful,
        } => render_arrow_hir_ty(
            sema.module().store.ty_ids.get(*params),
            *ret,
            *is_effectful,
            |ty| render_hir_ty(sema, session, ty),
        ),
        HirTyKind::Sum { left, right } => render_sum_hir_ty(sema, session, *left, *right),
        HirTyKind::Tuple { items } => {
            let values = sema
                .module()
                .store
                .ty_ids
                .get(*items)
                .iter()
                .map(|item| render_hir_ty(sema, session, *item))
                .collect::<Vec<_>>()
                .join(", ");
            format!("({values})")
        }
        HirTyKind::Array { dims, item } => render_array_hir_ty(sema, session, dims, *item),
        HirTyKind::Seq { item } => format!("[]{}", render_hir_ty(sema, session, *item)),
        HirTyKind::Range { bound } => render_applied_hir_ty("Range", sema, session, *bound),
        HirTyKind::Handler {
            effect,
            input,
            output,
        } => render_handler_hir_ty(sema, session, *effect, *input, *output),
        HirTyKind::Mut { inner } => render_prefixed_hir_ty("mut", sema, session, *inner),
        HirTyKind::AnyShape { capability } => {
            render_prefixed_hir_ty("any", sema, session, *capability)
        }
        HirTyKind::SomeShape { capability } => {
            render_prefixed_hir_ty("some", sema, session, *capability)
        }
        HirTyKind::Record { fields } => render_record_hir_ty(sema, session, fields),
        _ => render_atomic_hir_ty(kind).unwrap_or_default(),
    }
}

fn render_prefixed_hir_ty(
    prefix: &str,
    sema: &SemaModule,
    session: &Session,
    inner: HirTyId,
) -> String {
    format!("{prefix} {}", render_hir_ty(sema, session, inner))
}

fn render_atomic_hir_ty(kind: &HirTyKind) -> Option<String> {
    if let HirTyKind::NatLit(value) = kind {
        return Some(value.to_string());
    }
    simple_hir_ty_display_name(kind).map(str::to_owned)
}

fn render_sum_hir_ty(
    sema: &SemaModule,
    session: &Session,
    left: HirTyId,
    right: HirTyId,
) -> String {
    format!(
        "{} + {}",
        render_hir_ty(sema, session, left),
        render_hir_ty(sema, session, right)
    )
}

fn render_array_hir_ty(
    sema: &SemaModule,
    session: &Session,
    dims: &SliceRange<HirDim>,
    item: HirTyId,
) -> String {
    let mut parts = vec![render_hir_ty(sema, session, item)];
    for dim in sema.module().store.dims.get(dims.clone()) {
        parts.push(render_dim(session, dim));
    }
    format!("[{}]", parts.join("; "))
}

fn render_applied_hir_ty(
    name: &str,
    sema: &SemaModule,
    session: &Session,
    bound: HirTyId,
) -> String {
    format!("{name}[{}]", render_hir_ty(sema, session, bound))
}

fn render_handler_hir_ty(
    sema: &SemaModule,
    session: &Session,
    effect: HirTyId,
    input: HirTyId,
    output: HirTyId,
) -> String {
    format!(
        "answer {} ({} -> {})",
        render_hir_ty(sema, session, effect),
        render_hir_ty(sema, session, input),
        render_hir_ty(sema, session, output)
    )
}

fn render_record_hir_ty(
    sema: &SemaModule,
    session: &Session,
    fields: &SliceRange<HirTyField>,
) -> String {
    let rendered = sema
        .module()
        .store
        .ty_fields
        .get(fields.clone())
        .iter()
        .map(|field| render_ty_field(sema, session, field))
        .collect::<Vec<_>>()
        .join(", ");
    format!("{{ {rendered} }}")
}

fn render_named_hir_ty(
    name: &str,
    args: &[HirTyId],
    mut render: impl FnMut(HirTyId) -> String,
) -> String {
    let mut rendered = name.to_owned();
    if !args.is_empty() {
        let contents = args
            .iter()
            .map(|item| render(*item))
            .collect::<Vec<_>>()
            .join(", ");
        rendered.push('[');
        rendered.push_str(&contents);
        rendered.push(']');
    }
    rendered
}

fn render_arrow_hir_ty(
    params: &[HirTyId],
    ret: HirTyId,
    is_effectful: bool,
    mut render: impl FnMut(HirTyId) -> String,
) -> String {
    let left_items = params.iter().map(|item| render(*item)).collect::<Vec<_>>();
    let left = if left_items.len() == 1 {
        left_items[0].clone()
    } else {
        format!("({})", left_items.join(", "))
    };
    let arrow = if is_effectful { " ~> " } else { " -> " };
    format!("{left}{arrow}{}", render(ret))
}

#[must_use]
fn render_dim(session: &Session, dim: &HirDim) -> String {
    match dim {
        HirDim::Unknown => "_".into(),
        HirDim::Name(ident) => session.resolve_symbol(ident.name).to_owned(),
        HirDim::Int(value) => value.to_string(),
    }
}

#[must_use]
fn render_ty_field(sema: &SemaModule, session: &Session, field: &HirTyField) -> String {
    format!(
        "{} : {}",
        session.resolve_symbol(field.name),
        render_hir_ty(sema, session, field.ty)
    )
}
