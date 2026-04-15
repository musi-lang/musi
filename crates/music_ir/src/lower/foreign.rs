use super::*;

pub(super) fn lower_foreign_let(
    sema: &SemaModule,
    interner: &Interner,
    expr_id: HirExprId,
    name: Ident,
    params: HirParamRange,
    exported: bool,
) -> IrForeignDef {
    let expr = sema.module().store.exprs.get(expr_id);
    if expr.mods.foreign.is_none() {
        invalid_lowering_path("foreign let without foreign modifier");
    }

    let name_text: Box<str> = interner.resolve(name.name).into();
    let binding = decl_binding_id(sema, name);
    let mut symbol = name_text.clone();
    let mut link = None::<Box<str>>;
    if let Some(binding) = binding {
        if let Some(attrs) = sema.foreign_link(binding) {
            link.clone_from(&attrs.name);
            if let Some(symbol_override) = attrs.symbol.as_ref() {
                symbol = symbol_override.clone();
            }
        }
    }
    let abi = expr
        .mods
        .foreign
        .as_ref()
        .and_then(|foreign| foreign.abi)
        .map_or("c", |sym| interner.resolve(sym));
    let (param_tys, result_ty) = foreign_signature_tys(sema, interner, binding, expr_id, params);
    IrForeignDef::new(name_text, abi, symbol, param_tys, result_ty)
        .with_binding_opt(binding)
        .with_link_opt(link)
        .with_exported(exported)
        .with_hot(super::toplevel::attrs_have_name(
            sema,
            interner,
            expr.mods.attrs.clone(),
            "hot",
        ))
        .with_cold(super::toplevel::attrs_have_name(
            sema,
            interner,
            expr.mods.attrs.clone(),
            "cold",
        ))
}

fn foreign_signature_tys(
    sema: &SemaModule,
    interner: &Interner,
    binding: Option<NameBindingId>,
    expr_id: HirExprId,
    _params: HirParamRange,
) -> (Box<[Box<str>]>, Box<str>) {
    if let Some(binding) = binding
        && let Some(ty) = sema
            .binding_scheme(binding)
            .map(|scheme| scheme.ty)
            .or_else(|| sema.binding_type(binding))
        && let HirTyKind::Arrow { params, ret, .. } = &sema.ty(ty).kind
    {
        let param_tys = sema
            .module()
            .store
            .ty_ids
            .get(*params)
            .iter()
            .copied()
            .map(|ty| render_ty_name(sema, ty, interner))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let result_ty = render_ty_name(sema, *ret, interner);
        return (param_tys, result_ty);
    }

    let expr_ty = sema
        .try_expr_ty(expr_id)
        .unwrap_or_else(|| invalid_lowering_path("foreign expr type missing"));
    match &sema.ty(expr_ty).kind {
        HirTyKind::Arrow { params, ret, .. } => (
            sema.module()
                .store
                .ty_ids
                .get(*params)
                .iter()
                .copied()
                .map(|ty| render_ty_name(sema, ty, interner))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            render_ty_name(sema, *ret, interner),
        ),
        other => invalid_lowering_path(format!("foreign let without arrow type: {other:?}")),
    }
}
