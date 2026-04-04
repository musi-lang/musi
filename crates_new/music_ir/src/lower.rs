use std::collections::HashSet;

use music_arena::SliceRange;
use music_base::diag::Diag;
use music_hir::{
    HirArg, HirArrayItem, HirBinaryOp, HirCaseArm, HirDim, HirExprId, HirExprKind, HirForeignDecl,
    HirLetMods, HirLitId, HirLitKind, HirParam, HirPatId, HirPatKind, HirTyField, HirTyId,
    HirTyKind,
};
use music_module::ModuleKey;
use music_names::{Ident, Interner, NameBindingId, NameSite, Symbol};
use music_sema::{SemaModule, SurfaceEffectRow, SurfaceTy, SurfaceTyId, SurfaceTyKind};

use crate::api::{
    IrArg, IrAssignTarget, IrBinaryOp, IrCallable, IrCaseArm as IrLoweredCaseArm, IrCasePattern,
    IrClassDef, IrDataDef, IrDiagList, IrEffectDef, IrExpr, IrExprKind, IrForeignDef, IrGlobal,
    IrInstanceDef, IrLit, IrModule, IrNameRef, IrOrigin, IrParam,
};

#[derive(Default)]
struct TopLevelItems {
    callables: Vec<IrCallable>,
    globals: Vec<IrGlobal>,
    data_defs: Vec<IrDataDef>,
    foreigns: Vec<IrForeignDef>,
}

struct LetItemInput {
    pat: HirPatId,
    params: SliceRange<HirParam>,
    value: HirExprId,
    is_callable: bool,
    exported: bool,
}

struct LowerCtx<'a> {
    sema: &'a SemaModule,
    interner: &'a Interner,
    module_key: ModuleKey,
    module_level_bindings: HashSet<NameBindingId>,
    next_lambda_id: u32,
    extra_callables: Vec<IrCallable>,
}

/// Lowers sema-owned module facts into the codegen-facing IR surface.
///
/// # Errors
///
/// Returns semantic diagnostics when exported surface types or effect rows reference invalid
/// sema-owned ids.
pub fn lower_module(sema: &SemaModule, interner: &Interner) -> Result<IrModule, IrDiagList> {
    let mut diags = Vec::<Diag>::new();
    validate_surface(sema, &mut diags);
    if !diags.is_empty() {
        return Err(diags);
    }

    let module_level_bindings = collect_module_level_bindings(sema);
    let mut ctx = LowerCtx {
        sema,
        interner,
        module_key: sema.resolved().module_key.clone(),
        module_level_bindings,
        next_lambda_id: 0,
        extra_callables: Vec::new(),
    };

    let mut items = TopLevelItems::default();
    collect_top_level_items(&mut ctx, sema.module().root, false, &mut items);
    items.callables.extend(ctx.extra_callables);

    Ok(IrModule {
        module_key: sema.resolved().module_key.clone(),
        static_imports: sema.surface().static_imports.to_vec().into_boxed_slice(),
        types: sema.surface().tys.clone(),
        exports: sema.surface().exported_values.clone(),
        callables: items.callables.into_boxed_slice(),
        globals: items.globals.into_boxed_slice(),
        data_defs: items.data_defs.into_boxed_slice(),
        foreigns: items.foreigns.into_boxed_slice(),
        effects: sema
            .surface()
            .exported_effects
            .iter()
            .map(IrEffectDef::from)
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        classes: sema
            .surface()
            .exported_classes
            .iter()
            .map(IrClassDef::from)
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        instances: sema
            .surface()
            .exported_instances
            .iter()
            .map(IrInstanceDef::from)
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    })
}

fn collect_module_level_bindings(sema: &SemaModule) -> HashSet<NameBindingId> {
    let mut bindings = HashSet::new();
    collect_module_level_bindings_from_expr(sema, sema.module().root, &mut bindings);
    bindings
}

fn collect_module_level_bindings_from_expr(
    sema: &SemaModule,
    expr_id: HirExprId,
    out: &mut HashSet<NameBindingId>,
) {
    match &sema.module().store.exprs.get(expr_id).kind {
        HirExprKind::Sequence { exprs } | HirExprKind::Tuple { items: exprs } => {
            for expr in sema.module().store.expr_ids.get(*exprs).iter().copied() {
                collect_module_level_bindings_from_expr(sema, expr, out);
            }
        }
        HirExprKind::Export { expr, .. } => collect_module_level_bindings_from_expr(sema, *expr, out),
        HirExprKind::Let { pat, .. } => {
            if let HirPatKind::Bind { name } = sema.module().store.pats.get(*pat).kind {
                if let Some(binding) = decl_binding_id(sema, name) {
                    let _ = out.insert(binding);
                }
            }
        }
        HirExprKind::Foreign { decls, .. } => {
            for decl in sema.module().store.foreign_decls.get(decls.clone()) {
                if let Some(binding) = decl_binding_id(sema, decl.name) {
                    let _ = out.insert(binding);
                }
            }
        }
        _ => {}
    }
}

fn validate_surface(sema: &SemaModule, diags: &mut IrDiagList) {
    let types = &sema.surface().tys;
    for export in &sema.surface().exported_values {
        validate_surface_ty_id(types, export.ty, diags);
        validate_effect_row(types, &export.effects, diags);
        for constraint in &export.constraints {
            validate_surface_ty_id(types, constraint.value, diags);
        }
    }
    for effect in &sema.surface().exported_effects {
        for op in &effect.ops {
            for param in &op.params {
                validate_surface_ty_id(types, *param, diags);
            }
            validate_surface_ty_id(types, op.result, diags);
        }
    }
    for class in &sema.surface().exported_classes {
        for constraint in &class.constraints {
            validate_surface_ty_id(types, constraint.value, diags);
        }
        for member in &class.members {
            for param in &member.params {
                validate_surface_ty_id(types, *param, diags);
            }
            validate_surface_ty_id(types, member.result, diags);
        }
    }
    for instance in &sema.surface().exported_instances {
        for arg in &instance.class_args {
            validate_surface_ty_id(types, *arg, diags);
        }
        for constraint in &instance.constraints {
            validate_surface_ty_id(types, constraint.value, diags);
        }
    }
}

fn validate_effect_row(types: &[SurfaceTy], row: &SurfaceEffectRow, diags: &mut IrDiagList) {
    for item in &row.items {
        if let Some(arg) = item.arg {
            validate_surface_ty_id(types, arg, diags);
        }
    }
}

fn validate_surface_ty_id(types: &[SurfaceTy], id: SurfaceTyId, diags: &mut IrDiagList) {
    let index = usize::try_from(id.raw()).unwrap_or(usize::MAX);
    let Some(ty) = types.get(index) else {
        diags.push(Diag::error("invalid surface type id"));
        return;
    };
    validate_surface_ty(types, ty, diags);
}

fn validate_surface_ty(types: &[SurfaceTy], ty: &SurfaceTy, diags: &mut IrDiagList) {
    match &ty.kind {
        SurfaceTyKind::Named { args, .. } => {
            for arg in args.iter().copied() {
                validate_surface_ty_id(types, arg, diags);
            }
        }
        SurfaceTyKind::Arrow { params, ret, .. } => {
            for param in params.iter().copied() {
                validate_surface_ty_id(types, param, diags);
            }
            validate_surface_ty_id(types, *ret, diags);
        }
        SurfaceTyKind::Sum { left, right } => {
            validate_surface_ty_id(types, *left, diags);
            validate_surface_ty_id(types, *right, diags);
        }
        SurfaceTyKind::Tuple { items } => {
            for item in items.iter().copied() {
                validate_surface_ty_id(types, item, diags);
            }
        }
        SurfaceTyKind::Array { item, .. } => validate_surface_ty_id(types, *item, diags),
        SurfaceTyKind::Mut { inner } => validate_surface_ty_id(types, *inner, diags),
        SurfaceTyKind::Record { fields } => {
            for field in fields {
                validate_surface_ty_id(types, field.ty, diags);
            }
        }
        SurfaceTyKind::Error
        | SurfaceTyKind::Unknown
        | SurfaceTyKind::Type
        | SurfaceTyKind::Syntax
        | SurfaceTyKind::Any
        | SurfaceTyKind::Empty
        | SurfaceTyKind::Unit
        | SurfaceTyKind::Bool
        | SurfaceTyKind::Int
        | SurfaceTyKind::Float
        | SurfaceTyKind::String
        | SurfaceTyKind::CString
        | SurfaceTyKind::CPtr
        | SurfaceTyKind::Module => {}
    }
}

fn collect_top_level_items(ctx: &mut LowerCtx<'_>, expr_id: HirExprId, exported: bool, items: &mut TopLevelItems) {
    let sema = ctx.sema;
    match &sema.module().store.exprs.get(expr_id).kind {
        HirExprKind::Sequence { exprs } | HirExprKind::Tuple { items: exprs } => {
            for expr in sema.module().store.expr_ids.get(*exprs).iter().copied() {
                collect_top_level_items(ctx, expr, false, items);
            }
        }
        HirExprKind::Export { expr, .. } => {
            collect_top_level_items(ctx, *expr, true, items);
        }
        HirExprKind::Let {
            pat,
            has_param_clause,
            params,
            value,
            ..
        } => {
            let is_callable =
                *has_param_clause || !sema.module().store.params.get(params.clone()).is_empty();
            collect_let_item(
                ctx,
                LetItemInput {
                    pat: *pat,
                    params: params.clone(),
                    value: *value,
                    is_callable,
                    exported,
                },
                items,
            );
        }
        HirExprKind::Foreign { abi, decls } => {
            for decl in sema.module().store.foreign_decls.get(decls.clone()) {
                items.foreigns.push(lower_foreign_decl(
                    sema,
                    ctx.interner,
                    abi.as_deref(),
                    decl,
                ));
            }
        }
        _ => {}
    }
}

fn collect_let_item(ctx: &mut LowerCtx<'_>, input: LetItemInput, items: &mut TopLevelItems) {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let LetItemInput {
        pat,
        params,
        value,
        is_callable,
        exported,
    } = input;
    let HirPatKind::Bind { name } = sema.module().store.pats.get(pat).kind else {
        return;
    };
    let binding = decl_binding_id(sema, name);
    let module_target = sema.expr_module_target(value).cloned();
    let effects = sema.expr_effects(value).clone();
    if matches!(sema.ty(sema.expr_ty(value)).kind, HirTyKind::Module)
        || matches!(
            sema.module().store.exprs.get(value).kind,
            HirExprKind::Import { .. }
        )
    {
        return;
    }

    if let HirExprKind::Data { variants, fields } = &sema.module().store.exprs.get(value).kind {
        items.data_defs.push(IrDataDef {
            name: interner.resolve(name.name).into(),
            variant_count: u32::try_from(sema.module().store.variants.get(variants.clone()).len())
                .expect("variant count overflow"),
            field_count: u32::try_from(sema.module().store.fields.get(fields.clone()).len())
                .expect("field count overflow"),
        });
    } else if is_callable {
        items.callables.push(IrCallable {
            binding,
            name: interner.resolve(name.name).into(),
            params: lower_params(ctx, params),
            body: lower_expr(ctx, value),
            exported,
            effects,
            module_target,
        });
    } else {
        items.globals.push(IrGlobal {
            binding,
            name: interner.resolve(name.name).into(),
            body: lower_expr(ctx, value),
            exported,
            effects,
            module_target,
        });
    }
}

fn lower_params(ctx: &LowerCtx<'_>, params: SliceRange<HirParam>) -> Box<[IrParam]> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    sema
        .module()
        .store
        .params
        .get(params)
        .iter()
        .map(|param| IrParam {
            binding: decl_binding_id(sema, param.name).expect("param binding missing"),
            name: interner.resolve(param.name.name).into(),
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn lower_expr(ctx: &mut LowerCtx<'_>, expr_id: HirExprId) -> IrExpr {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let expr = sema.module().store.exprs.get(expr_id);
    let origin = IrOrigin {
        source_id: expr.origin.source_id,
        span: expr.origin.span,
    };
    let kind = match &expr.kind {
        HirExprKind::Name { name } => lower_name_expr(sema, expr_id, *name, interner),
        HirExprKind::Lit { lit } => lower_lit_expr(sema, *lit),
        HirExprKind::Sequence { exprs } => lower_sequence_expr(ctx, *exprs),
        HirExprKind::Tuple { items } => lower_tuple_expr(ctx, expr_id, *items),
        HirExprKind::Array { items } => lower_array_expr(ctx, expr_id, items.clone()),
        HirExprKind::Let {
            mods,
            pat,
            has_param_clause,
            params,
            value,
            ..
        } => lower_let_expr(ctx, *mods, *pat, *has_param_clause, params, *value),
        HirExprKind::Binary { op, left, right } => {
            lower_binary_expr(ctx, op, *left, *right)
        }
        HirExprKind::Call { callee, args } => lower_call_expr(ctx, *callee, args),
        HirExprKind::Apply { callee, .. } => {
            let mut lowered = lower_expr(ctx, *callee);
            lowered.origin = origin;
            return lowered;
        }
        HirExprKind::Index { base, args } => lower_index_expr(ctx, *base, *args),
        HirExprKind::Field { base, name, .. } => {
            lower_field_expr(sema, expr_id, *base, name.name, &expr.kind, interner)
        }
        HirExprKind::Case { scrutinee, arms } => lower_case_expr(ctx, *scrutinee, arms),
        HirExprKind::Lambda { params, body, .. } => {
            lower_lambda_expr(ctx, origin, params, *body)
        }
        HirExprKind::Export { expr, .. } | HirExprKind::Attributed { expr, .. } => {
            return lower_expr(ctx, *expr);
        }
        HirExprKind::Import { .. }
            if matches!(sema.ty(sema.expr_ty(expr_id)).kind, HirTyKind::Module) =>
        {
            IrExprKind::Unit
        }
        other => IrExprKind::Unsupported {
            description: format!("{other:?}").into(),
        },
    };
    IrExpr { origin, kind }
}

fn lower_name_expr(
    sema: &SemaModule,
    expr_id: HirExprId,
    ident: Ident,
    interner: &Interner,
) -> IrExprKind {
    IrExprKind::Name {
        binding: use_binding_id(sema, ident),
        name: interner.resolve(ident.name).into(),
        module_target: sema.expr_module_target(expr_id).cloned(),
    }
}

fn lower_lit_expr(sema: &SemaModule, lit_id: HirLitId) -> IrExprKind {
    let lit = &sema.module().store.lits.get(lit_id).kind;
    IrExprKind::Lit(match lit {
        HirLitKind::Int { raw } => IrLit::Int { raw: raw.clone() },
        HirLitKind::Float { raw } => IrLit::Float { raw: raw.clone() },
        HirLitKind::String { value } => IrLit::String {
            value: value.clone(),
        },
        HirLitKind::Rune { value } => IrLit::Rune { value: *value },
    })
}

fn lower_sequence_expr(ctx: &mut LowerCtx<'_>, exprs: SliceRange<HirExprId>) -> IrExprKind {
    IrExprKind::Sequence {
        exprs: lower_expr_list(ctx, exprs),
    }
}

fn lower_tuple_expr(ctx: &mut LowerCtx<'_>, expr_id: HirExprId, items: SliceRange<HirExprId>) -> IrExprKind {
    let sema = ctx.sema;
    IrExprKind::Tuple {
        ty_name: render_ty_name(sema, sema.expr_ty(expr_id), ctx.interner),
        items: lower_expr_list(ctx, items),
    }
}

fn lower_array_expr(ctx: &mut LowerCtx<'_>, expr_id: HirExprId, items: SliceRange<HirArrayItem>) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;
    lower_array_items(ctx, items).map_or_else(
        || IrExprKind::Unsupported {
            description: "array spread".into(),
        },
        |items| IrExprKind::Array {
            ty_name: render_ty_name(sema, sema.expr_ty(expr_id), interner),
            items,
        },
    )
}

fn lower_let_expr(
    ctx: &mut LowerCtx<'_>,
    mods: HirLetMods,
    pat: HirPatId,
    has_param_clause: bool,
    params: &SliceRange<HirParam>,
    value: HirExprId,
) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let is_callable = has_param_clause || !sema.module().store.params.get(params.clone()).is_empty();

    if matches!(sema.ty(sema.expr_ty(value)).kind, HirTyKind::Module)
        || matches!(
            sema.module().store.exprs.get(value).kind,
            HirExprKind::Import { .. }
        )
    {
        return IrExprKind::Unit;
    }

    if is_callable {
        if mods.is_rec {
            return IrExprKind::Unsupported {
                description: "rec local callable let".into(),
            };
        }
        return lower_local_callable_let(ctx, mods, pat, params, value);
    }

    match &sema.module().store.pats.get(pat).kind {
        HirPatKind::Bind { name } => IrExprKind::Let {
            binding: decl_binding_id(sema, *name),
            name: interner.resolve(name.name).into(),
            is_mut: mods.is_mut,
            value: Box::new(lower_expr(ctx, value)),
        },
        HirPatKind::Wildcard => IrExprKind::Let {
            binding: None,
            name: "_".into(),
            is_mut: false,
            value: Box::new(lower_expr(ctx, value)),
        },
        other => IrExprKind::Unsupported {
            description: format!("local let pattern {other:?}").into(),
        },
    }
}

fn lower_local_callable_let(
    ctx: &mut LowerCtx<'_>,
    mods: HirLetMods,
    pat: HirPatId,
    params: &SliceRange<HirParam>,
    value: HirExprId,
) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;

    let (binding, name) = match &sema.module().store.pats.get(pat).kind {
        HirPatKind::Bind { name } => (
            decl_binding_id(sema, *name),
            interner.resolve(name.name).into(),
        ),
        HirPatKind::Wildcard => (None, Box::<str>::from("_")),
        other => {
            return IrExprKind::Unsupported {
                description: format!("local callable let pattern {other:?}").into(),
            };
        }
    };

    let mut body = lower_expr(ctx, value);
    body.origin = IrOrigin {
        source_id: sema.module().store.exprs.get(value).origin.source_id,
        span: sema.module().store.exprs.get(value).origin.span,
    };

    let (user_params, user_param_bindings) = lower_user_params(ctx, params);

    let captures = compute_capture_bindings(ctx, &body, &user_param_bindings, binding);
    let capture_params = captures
        .iter()
        .copied()
        .map(|cap| IrParam {
            binding: cap,
            name: binding_name(ctx, cap),
        })
        .collect::<Vec<_>>();

    let capture_exprs = captures
        .iter()
        .copied()
        .map(|cap| name_expr(ctx, body.origin, cap))
        .collect::<Vec<_>>()
        .into_boxed_slice();

    let mut callable_params = Vec::new();
    callable_params.extend(capture_params);
    callable_params.extend(user_params);

    let callable_name = if binding.is_some() && name.as_ref() != "_" {
        name.clone()
    } else {
        fresh_lambda_name(ctx, "localfn", body.origin)
    };

    let callable = IrCallable {
        binding,
        name: callable_name.clone(),
        params: callable_params.into_boxed_slice(),
        body,
        exported: false,
        effects: sema.expr_effects(value).clone(),
        module_target: sema.expr_module_target(value).cloned(),
    };
    ctx.extra_callables.push(callable);

    let closure_value = IrExpr {
        origin: IrOrigin {
            source_id: sema.module().store.exprs.get(value).origin.source_id,
            span: sema.module().store.exprs.get(value).origin.span,
        },
        kind: IrExprKind::ClosureNew {
            callee: IrNameRef {
                binding,
                name: callable_name,
                module_target: Some(ctx.module_key.clone()),
            },
            captures: capture_exprs,
        },
    };

    IrExprKind::Let {
        binding,
        name,
        is_mut: mods.is_mut,
        value: Box::new(closure_value),
    }
}

fn lower_lambda_expr(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    params: &SliceRange<HirParam>,
    body: HirExprId,
) -> IrExprKind {
    let sema = ctx.sema;

    let lowered_body = lower_expr(ctx, body);
    let (user_params, user_param_bindings) = lower_user_params(ctx, params);
    let captures = compute_capture_bindings(ctx, &lowered_body, &user_param_bindings, None);

    let capture_params = captures
        .iter()
        .copied()
        .map(|cap| IrParam {
            binding: cap,
            name: binding_name(ctx, cap),
        })
        .collect::<Vec<_>>();

    let mut callable_params = Vec::new();
    callable_params.extend(capture_params);
    callable_params.extend(user_params);

    let callable_name = fresh_lambda_name(ctx, "lambda", origin);
    let callable = IrCallable {
        binding: None,
        name: callable_name.clone(),
        params: callable_params.into_boxed_slice(),
        body: lowered_body,
        exported: false,
        effects: sema.expr_effects(body).clone(),
        module_target: Some(ctx.module_key.clone()),
    };
    ctx.extra_callables.push(callable);

    let capture_exprs = captures
        .iter()
        .copied()
        .map(|cap| name_expr(ctx, origin, cap))
        .collect::<Vec<_>>()
        .into_boxed_slice();

    IrExprKind::ClosureNew {
        callee: IrNameRef {
            binding: None,
            name: callable_name,
            module_target: Some(ctx.module_key.clone()),
        },
        captures: capture_exprs,
    }
}

fn lower_user_params(
    ctx: &LowerCtx<'_>,
    params: &SliceRange<HirParam>,
) -> (Vec<IrParam>, Vec<NameBindingId>) {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let mut lowered = Vec::new();
    let mut bindings = Vec::new();
    for param in sema.module().store.params.get(params.clone()) {
        let binding = decl_binding_id(sema, param.name).expect("param binding missing");
        bindings.push(binding);
        lowered.push(IrParam {
            binding,
            name: interner.resolve(param.name.name).into(),
        });
    }
    (lowered, bindings)
}

fn compute_capture_bindings(
    ctx: &LowerCtx<'_>,
    body: &IrExpr,
    user_params: &[NameBindingId],
    binder: Option<NameBindingId>,
) -> Vec<NameBindingId> {
    let mut used = HashSet::<NameBindingId>::new();
    collect_used_bindings(body, &mut used);

    let mut local = HashSet::<NameBindingId>::new();
    for param in user_params {
        let _ = local.insert(*param);
    }
    if let Some(binder) = binder {
        let _ = local.insert(binder);
    }
    collect_local_decl_bindings(body, &mut local);

    used.retain(|binding| !local.contains(binding) && !ctx.module_level_bindings.contains(binding));
    let mut captures = used.into_iter().collect::<Vec<_>>();
    captures.sort_by_key(|binding| binding.raw());
    captures
}

fn collect_used_bindings(expr: &IrExpr, out: &mut HashSet<NameBindingId>) {
    match &expr.kind {
        IrExprKind::Name { binding: Some(binding), .. } => {
            let _ = out.insert(*binding);
        }
        IrExprKind::Sequence { exprs } => {
            for expr in exprs {
                collect_used_bindings(expr, out);
            }
        }
        IrExprKind::Tuple { items, .. } | IrExprKind::Array { items, .. } => {
            for item in items {
                collect_used_bindings(item, out);
            }
        }
        IrExprKind::Let { value, .. } => collect_used_bindings(value, out),
        IrExprKind::Assign { target, value } => {
            collect_used_bindings(value, out);
            if let IrAssignTarget::Index { base, index } = target.as_ref() {
                collect_used_bindings(base, out);
                collect_used_bindings(index, out);
            }
        }
        IrExprKind::Index { base, index } => {
            collect_used_bindings(base, out);
            collect_used_bindings(index, out);
        }
        IrExprKind::ClosureNew { captures, .. } => {
            for cap in captures {
                collect_used_bindings(cap, out);
            }
        }
        IrExprKind::Binary { left, right, .. } => {
            collect_used_bindings(left, out);
            collect_used_bindings(right, out);
        }
        IrExprKind::Case { scrutinee, arms } => {
            collect_used_bindings(scrutinee, out);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_used_bindings(guard, out);
                }
                collect_used_bindings(&arm.expr, out);
            }
        }
        IrExprKind::Call { callee, args } => {
            collect_used_bindings(callee, out);
            for arg in args {
                collect_used_bindings(&arg.expr, out);
            }
        }
        IrExprKind::Unit
        | IrExprKind::Lit(_)
        | IrExprKind::Unsupported { .. }
        | IrExprKind::Name { binding: None, .. } => {}
    }
}

fn collect_local_decl_bindings(expr: &IrExpr, out: &mut HashSet<NameBindingId>) {
    match &expr.kind {
        IrExprKind::Let {
            binding: Some(binding),
            value,
            ..
        } => {
            let _ = out.insert(*binding);
            collect_local_decl_bindings(value, out);
        }
        IrExprKind::Let { value, .. } | IrExprKind::Assign { value, .. } => {
            collect_local_decl_bindings(value, out);
        }
        IrExprKind::Sequence { exprs } => {
            for expr in exprs {
                collect_local_decl_bindings(expr, out);
            }
        }
        IrExprKind::Tuple { items, .. } | IrExprKind::Array { items, .. } => {
            for item in items {
                collect_local_decl_bindings(item, out);
            }
        }
        IrExprKind::Index { base, index } => {
            collect_local_decl_bindings(base, out);
            collect_local_decl_bindings(index, out);
        }
        IrExprKind::ClosureNew { captures, .. } => {
            for cap in captures {
                collect_local_decl_bindings(cap, out);
            }
        }
        IrExprKind::Binary { left, right, .. } => {
            collect_local_decl_bindings(left, out);
            collect_local_decl_bindings(right, out);
        }
        IrExprKind::Case { scrutinee, arms } => {
            collect_local_decl_bindings(scrutinee, out);
            for arm in arms {
                if let IrCasePattern::Bind { binding, .. } = &arm.pattern {
                    let _ = out.insert(*binding);
                }
                if let Some(guard) = &arm.guard {
                    collect_local_decl_bindings(guard, out);
                }
                collect_local_decl_bindings(&arm.expr, out);
            }
        }
        IrExprKind::Call { callee, args } => {
            collect_local_decl_bindings(callee, out);
            for arg in args {
                collect_local_decl_bindings(&arg.expr, out);
            }
        }
        IrExprKind::Unit | IrExprKind::Name { .. } | IrExprKind::Lit(_) | IrExprKind::Unsupported { .. } => {}
    }
}

fn binding_name(ctx: &LowerCtx<'_>, binding: NameBindingId) -> Box<str> {
    let binding = ctx.sema.resolved().names.bindings.get(binding);
    ctx.interner.resolve(binding.name).into()
}

fn name_expr(ctx: &LowerCtx<'_>, origin: IrOrigin, binding: NameBindingId) -> IrExpr {
    IrExpr {
        origin,
        kind: IrExprKind::Name {
            binding: Some(binding),
            name: binding_name(ctx, binding),
            module_target: None,
        },
    }
}

fn fresh_lambda_name(ctx: &mut LowerCtx<'_>, prefix: &str, origin: IrOrigin) -> Box<str> {
    let id = ctx.next_lambda_id;
    ctx.next_lambda_id = ctx.next_lambda_id.saturating_add(1);
    format!(
        "{prefix}:{}:{}:{}:{id}",
        origin.source_id.raw(),
        origin.span.start,
        origin.span.end
    )
    .into()
}

fn lower_call_expr(ctx: &mut LowerCtx<'_>, callee: HirExprId, args: &SliceRange<HirArg>) -> IrExprKind {
    let sema = ctx.sema;
    IrExprKind::Call {
        callee: Box::new(lower_expr(ctx, callee)),
        args: sema
            .module()
            .store
            .args
            .get(args.clone())
            .iter()
            .map(|arg| IrArg {
                spread: arg.spread,
                expr: lower_expr(ctx, arg.expr),
            })
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    }
}

fn lower_index_expr(ctx: &mut LowerCtx<'_>, base: HirExprId, args: SliceRange<HirExprId>) -> IrExprKind {
    let sema = ctx.sema;
    let Some(index) = sema.module().store.expr_ids.get(args).first().copied() else {
        return IrExprKind::Unsupported {
            description: "index without argument".into(),
        };
    };
    if sema.module().store.expr_ids.get(args).len() != 1 {
        return IrExprKind::Unsupported {
            description: "multi-index access".into(),
        };
    }
    IrExprKind::Index {
        base: Box::new(lower_expr(ctx, base)),
        index: Box::new(lower_expr(ctx, index)),
    }
}

fn lower_field_expr(
    sema: &SemaModule,
    expr_id: HirExprId,
    base: HirExprId,
    symbol: Symbol,
    kind: &HirExprKind,
    interner: &Interner,
) -> IrExprKind {
    sema.expr_module_target(expr_id)
        .cloned()
        .or_else(|| sema.expr_module_target(base).cloned())
        .map_or_else(
            || IrExprKind::Unsupported {
                description: format!("{kind:?}").into(),
            },
            |module_target| IrExprKind::Name {
                binding: None,
                name: interner.resolve(symbol).into(),
                module_target: Some(module_target),
            },
        )
}

fn lower_case_expr(ctx: &mut LowerCtx<'_>, scrutinee: HirExprId, arms: &SliceRange<HirCaseArm>) -> IrExprKind {
    let sema = ctx.sema;
    let lowered = sema
        .module()
        .store
        .case_arms
        .get(arms.clone())
        .iter()
        .filter_map(|arm| lower_case_arm(ctx, arm))
        .collect::<Vec<_>>();
    if lowered.is_empty() {
        return IrExprKind::Unsupported {
            description: "case without emit-compatible arms".into(),
        };
    }
    IrExprKind::Case {
        scrutinee: Box::new(lower_expr(ctx, scrutinee)),
        arms: lowered.into_boxed_slice(),
    }
}

fn lower_case_arm(ctx: &mut LowerCtx<'_>, arm: &HirCaseArm) -> Option<IrLoweredCaseArm> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    Some(IrLoweredCaseArm {
        pattern: lower_case_pattern(sema, arm.pat, interner)?,
        guard: arm.guard.map(|guard| lower_expr(ctx, guard)),
        expr: lower_expr(ctx, arm.expr),
    })
}

fn lower_case_pattern(
    sema: &SemaModule,
    pat: HirPatId,
    interner: &Interner,
) -> Option<IrCasePattern> {
    match &sema.module().store.pats.get(pat).kind {
        HirPatKind::Wildcard => Some(IrCasePattern::Wildcard),
        HirPatKind::Bind { name } => Some(IrCasePattern::Bind {
            binding: decl_binding_id(sema, *name)?,
            name: interner.resolve(name.name).into(),
        }),
        HirPatKind::Lit { expr } => lower_lit_pattern(sema, *expr),
        _ => None,
    }
}

fn lower_lit_pattern(sema: &SemaModule, expr: HirExprId) -> Option<IrCasePattern> {
    let HirExprKind::Lit { lit } = sema.module().store.exprs.get(expr).kind else {
        return None;
    };
    let lit = &sema.module().store.lits.get(lit).kind;
    Some(IrCasePattern::Lit(match lit {
        HirLitKind::Int { raw } => IrLit::Int { raw: raw.clone() },
        HirLitKind::Float { raw } => IrLit::Float { raw: raw.clone() },
        HirLitKind::String { value } => IrLit::String {
            value: value.clone(),
        },
        HirLitKind::Rune { value } => IrLit::Rune { value: *value },
    }))
}

fn lower_assign_expr(ctx: &mut LowerCtx<'_>, left: HirExprId, right: HirExprId) -> IrExprKind {
    let Some(target) = lower_assign_target(ctx, left) else {
        return IrExprKind::Unsupported {
            description: "unsupported assignment target".into(),
        };
    };
    IrExprKind::Assign {
        target: Box::new(target),
        value: Box::new(lower_expr(ctx, right)),
    }
}

fn lower_assign_target(ctx: &mut LowerCtx<'_>, expr: HirExprId) -> Option<IrAssignTarget> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    match &sema.module().store.exprs.get(expr).kind {
        HirExprKind::Name { name } => Some(IrAssignTarget::Binding {
            binding: use_binding_id(sema, *name),
            name: interner.resolve(name.name).into(),
            module_target: sema.expr_module_target(expr).cloned(),
        }),
        HirExprKind::Index { base, args } => {
            let IrExprKind::Index { base, index } = lower_index_expr(ctx, *base, *args)
            else {
                return None;
            };
            Some(IrAssignTarget::Index { base, index })
        }
        _ => None,
    }
}

fn lower_binary_expr(ctx: &mut LowerCtx<'_>, op: &HirBinaryOp, left: HirExprId, right: HirExprId) -> IrExprKind {
    let interner = ctx.interner;
    if matches!(op, HirBinaryOp::Assign) {
        return lower_assign_expr(ctx, left, right);
    }
    IrExprKind::Binary {
        op: lower_binary_op(op, interner),
        left: Box::new(lower_expr(ctx, left)),
        right: Box::new(lower_expr(ctx, right)),
    }
}

fn lower_expr_list(ctx: &mut LowerCtx<'_>, exprs: SliceRange<HirExprId>) -> Box<[IrExpr]> {
    let sema = ctx.sema;
    sema
        .module()
        .store
        .expr_ids
        .get(exprs)
        .iter()
        .copied()
        .map(|expr| lower_expr(ctx, expr))
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn lower_binary_op(op: &HirBinaryOp, interner: &Interner) -> IrBinaryOp {
    match op {
        HirBinaryOp::Add => IrBinaryOp::Add,
        HirBinaryOp::Sub => IrBinaryOp::Sub,
        HirBinaryOp::Mul => IrBinaryOp::Mul,
        HirBinaryOp::Div => IrBinaryOp::Div,
        HirBinaryOp::Rem => IrBinaryOp::Rem,
        HirBinaryOp::Eq => IrBinaryOp::Eq,
        HirBinaryOp::Ne => IrBinaryOp::Ne,
        HirBinaryOp::Lt => IrBinaryOp::Lt,
        HirBinaryOp::Gt => IrBinaryOp::Gt,
        HirBinaryOp::Le => IrBinaryOp::Le,
        HirBinaryOp::Ge => IrBinaryOp::Ge,
        HirBinaryOp::UserOp(ident) => IrBinaryOp::Other(interner.resolve(ident.name).into()),
        other => IrBinaryOp::Other(format!("{other:?}").into()),
    }
}

fn decl_binding_id(sema: &SemaModule, ident: Ident) -> Option<NameBindingId> {
    let site = NameSite::new(sema.module().source_id, ident.span);
    sema.resolved()
        .names
        .bindings
        .iter()
        .find_map(|(id, binding)| (binding.site == site).then_some(id))
}

fn use_binding_id(sema: &SemaModule, ident: Ident) -> Option<NameBindingId> {
    sema.resolved()
        .names
        .refs
        .get(&NameSite::new(sema.module().source_id, ident.span))
        .copied()
}

fn lower_foreign_decl(
    sema: &SemaModule,
    interner: &Interner,
    abi: Option<&str>,
    decl: &HirForeignDecl,
) -> IrForeignDef {
    IrForeignDef {
        binding: decl_binding_id(sema, decl.name),
        name: interner.resolve(decl.name.name).into(),
        abi: abi.unwrap_or("c").into(),
        param_count: u32::try_from(sema.module().store.params.get(decl.params.clone()).len())
            .expect("param count overflow"),
    }
}

fn lower_array_items(ctx: &mut LowerCtx<'_>, items: SliceRange<HirArrayItem>) -> Option<Box<[IrExpr]>> {
    let sema = ctx.sema;
    let mut lowered = Vec::new();
    for item in sema.module().store.array_items.get(items) {
        if item.spread {
            return None;
        }
        lowered.push(lower_expr(ctx, item.expr));
    }
    Some(lowered.into_boxed_slice())
}

fn render_ty_name(sema: &SemaModule, ty: HirTyId, interner: &Interner) -> Box<str> {
    match &sema.ty(ty).kind {
        HirTyKind::Error => "Error".into(),
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
        HirTyKind::Named { name, args } => render_named_ty_name(sema, *name, *args, interner),
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
        HirTyKind::Tuple { items } => render_tuple_ty_name(sema, *items, interner),
        HirTyKind::Array { dims, item } => render_array_ty_name(sema, dims, *item, interner),
        HirTyKind::Mut { inner } => {
            format!("mut {}", render_ty_name(sema, *inner, interner)).into()
        }
        HirTyKind::Record { fields } => render_record_ty_name(sema, fields, interner),
    }
}

fn render_named_ty_name(
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
    let dims = sema
        .module()
        .store
        .dims
        .get(dims.clone())
        .iter()
        .map(|dim| render_dim(dim, interner))
        .collect::<Vec<_>>()
        .join(", ");
    let item = render_ty_name(sema, item, interner);
    if dims.is_empty() {
        format!("[]{item}").into()
    } else {
        format!("[{dims}]{item}").into()
    }
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
