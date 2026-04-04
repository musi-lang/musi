use music_arena::SliceRange;
use music_base::diag::Diag;
use music_hir::{
    HirBinaryOp, HirExprId, HirExprKind, HirForeignDecl, HirLitKind, HirParam, HirPatId, HirPatKind,
};
use music_names::Interner;
use music_sema::{SemaModule, SurfaceEffectRow, SurfaceTy, SurfaceTyId, SurfaceTyKind};

use crate::api::{
    IrArg, IrBinaryOp, IrCallable, IrClassDef, IrDataDef, IrDiagList, IrEffectDef, IrExpr,
    IrExprKind, IrForeignDef, IrGlobal, IrInstanceDef, IrLit, IrModule, IrOrigin, IrParam,
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

    let mut items = TopLevelItems::default();
    collect_top_level_items(sema, sema.module().root, interner, false, &mut items);

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

fn collect_top_level_items(
    sema: &SemaModule,
    expr_id: HirExprId,
    interner: &Interner,
    exported: bool,
    items: &mut TopLevelItems,
) {
    match &sema.module().store.exprs.get(expr_id).kind {
        HirExprKind::Sequence { exprs } | HirExprKind::Tuple { items: exprs } => {
            for expr in sema.module().store.expr_ids.get(*exprs).iter().copied() {
                collect_top_level_items(sema, expr, interner, false, items);
            }
        }
        HirExprKind::Export { expr, .. } => {
            collect_top_level_items(sema, *expr, interner, true, items);
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
                sema,
                LetItemInput {
                    pat: *pat,
                    params: params.clone(),
                    value: *value,
                    is_callable,
                    exported,
                },
                interner,
                items,
            );
        }
        HirExprKind::Foreign { abi, decls } => {
            for decl in sema.module().store.foreign_decls.get(decls.clone()) {
                items
                    .foreigns
                    .push(lower_foreign_decl(sema, abi.as_deref(), decl, interner));
            }
        }
        _ => {}
    }
}

fn collect_let_item(
    sema: &SemaModule,
    input: LetItemInput,
    interner: &Interner,
    items: &mut TopLevelItems,
) {
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
    let module_target = sema.expr_module_target(value).cloned();
    let effects = sema.expr_effects(value).clone();

    if let HirExprKind::Data { variants, fields } = &sema.module().store.exprs.get(value).kind {
        items.data_defs.push(IrDataDef {
            symbol: name.name,
            name: interner.resolve(name.name).into(),
            variant_count: u32::try_from(sema.module().store.variants.get(variants.clone()).len())
                .expect("variant count overflow"),
            field_count: u32::try_from(sema.module().store.fields.get(fields.clone()).len())
                .expect("field count overflow"),
        });
    } else if is_callable {
        items.callables.push(IrCallable {
            symbol: name.name,
            name: interner.resolve(name.name).into(),
            params: lower_params(sema, params, interner),
            body: lower_expr(sema, value, interner),
            exported,
            effects,
            module_target,
        });
    } else {
        items.globals.push(IrGlobal {
            symbol: name.name,
            name: interner.resolve(name.name).into(),
            body: lower_expr(sema, value, interner),
            exported,
            effects,
            module_target,
        });
    }
}

fn lower_params(
    sema: &SemaModule,
    params: SliceRange<HirParam>,
    interner: &Interner,
) -> Box<[IrParam]> {
    sema.module()
        .store
        .params
        .get(params)
        .iter()
        .map(|param| IrParam {
            symbol: param.name.name,
            name: interner.resolve(param.name.name).into(),
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn lower_expr(sema: &SemaModule, expr_id: HirExprId, interner: &Interner) -> IrExpr {
    let expr = sema.module().store.exprs.get(expr_id);
    let origin = IrOrigin {
        source_id: expr.origin.source_id,
        span: expr.origin.span,
    };
    let kind = match &expr.kind {
        HirExprKind::Name { name } => IrExprKind::Name {
            symbol: name.name,
            name: interner.resolve(name.name).into(),
        },
        HirExprKind::Lit { lit } => {
            let lit = &sema.module().store.lits.get(*lit).kind;
            IrExprKind::Lit(match lit {
                HirLitKind::Int { raw } => IrLit::Int { raw: raw.clone() },
                HirLitKind::Float { raw } => IrLit::Float { raw: raw.clone() },
                HirLitKind::String { value } => IrLit::String {
                    value: value.clone(),
                },
                HirLitKind::Rune { value } => IrLit::Rune { value: *value },
            })
        }
        HirExprKind::Sequence { exprs } => IrExprKind::Sequence {
            exprs: sema
                .module()
                .store
                .expr_ids
                .get(*exprs)
                .iter()
                .copied()
                .map(|expr| lower_expr(sema, expr, interner))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        },
        HirExprKind::Binary { op, left, right } => IrExprKind::Binary {
            op: lower_binary_op(op, interner),
            left: Box::new(lower_expr(sema, *left, interner)),
            right: Box::new(lower_expr(sema, *right, interner)),
        },
        HirExprKind::Call { callee, args } => IrExprKind::Call {
            callee: Box::new(lower_expr(sema, *callee, interner)),
            args: sema
                .module()
                .store
                .args
                .get(args.clone())
                .iter()
                .map(|arg| IrArg {
                    spread: arg.spread,
                    expr: lower_expr(sema, arg.expr, interner),
                })
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        },
        HirExprKind::Export { expr, .. } | HirExprKind::Attributed { expr, .. } => {
            return lower_expr(sema, *expr, interner);
        }
        other => IrExprKind::Unsupported {
            description: format!("{other:?}").into(),
        },
    };
    IrExpr { origin, kind }
}

fn lower_binary_op(op: &HirBinaryOp, interner: &Interner) -> IrBinaryOp {
    match op {
        HirBinaryOp::Add => IrBinaryOp::Add,
        HirBinaryOp::Eq => IrBinaryOp::Eq,
        HirBinaryOp::UserOp(ident) => IrBinaryOp::Other(interner.resolve(ident.name).into()),
        other => IrBinaryOp::Other(format!("{other:?}").into()),
    }
}

fn lower_foreign_decl(
    sema: &SemaModule,
    abi: Option<&str>,
    decl: &HirForeignDecl,
    interner: &Interner,
) -> IrForeignDef {
    IrForeignDef {
        symbol: decl.name.name,
        name: interner.resolve(decl.name.name).into(),
        abi: abi.unwrap_or("c").into(),
        param_count: u32::try_from(sema.module().store.params.get(decl.params.clone()).len())
            .expect("param count overflow"),
    }
}
