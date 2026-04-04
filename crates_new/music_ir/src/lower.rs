use music_base::diag::Diag;
use music_hir::{HirExprId, HirExprKind, HirForeignDecl, HirPatId, HirPatKind, HirTyKind};
use music_names::Interner;
use music_sema::{SemaModule, SurfaceEffectRow, SurfaceTy, SurfaceTyId, SurfaceTyKind};

use crate::api::{
    IrCallable, IrClassDef, IrDataDef, IrDiagList, IrEffectDef, IrForeignDef, IrGlobal,
    IrInstanceDef, IrModule,
};

#[derive(Default)]
struct TopLevelItems {
    callables: Vec<IrCallable>,
    globals: Vec<IrGlobal>,
    data_defs: Vec<IrDataDef>,
    foreigns: Vec<IrForeignDef>,
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
    collect_top_level_items(sema, sema.module().root, interner, &mut items);

    Ok(IrModule {
        module_key: sema.resolved().module_key.clone(),
        hir: sema.module().clone(),
        root: sema.module().root,
        root_ty: sema.expr_ty(sema.module().root),
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
    items: &mut TopLevelItems,
) {
    match sema.module().store.exprs.get(expr_id).kind.clone() {
        HirExprKind::Sequence { exprs } | HirExprKind::Tuple { items: exprs } => {
            for expr in sema.module().store.expr_ids.get(exprs).iter().copied() {
                collect_top_level_items(sema, expr, interner, items);
            }
        }
        HirExprKind::Export { expr, .. } => {
            collect_top_level_items(sema, expr, interner, items);
        }
        HirExprKind::Let {
            pat,
            has_param_clause,
            params,
            value,
            ..
        } => {
            let is_callable =
                has_param_clause || !sema.module().store.params.get(params).is_empty();
            collect_let_item(sema, pat, value, is_callable, interner, items);
        }
        HirExprKind::Foreign { abi, decls } => {
            for decl in sema.module().store.foreign_decls.get(decls) {
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
    pat: HirPatId,
    value: HirExprId,
    is_callable: bool,
    interner: &Interner,
    items: &mut TopLevelItems,
) {
    let HirPatKind::Bind { name } = sema.module().store.pats.get(pat).kind else {
        return;
    };
    let module_target = sema.expr_module_target(value).cloned();
    let ty = sema.expr_ty(value);
    let effects = sema.expr_effects(value).clone();

    if let HirExprKind::Data { variants, fields } = &sema.module().store.exprs.get(value).kind {
        items.data_defs.push(IrDataDef {
            symbol: name.name,
            name: interner.resolve(name.name).into(),
            expr: value,
            variant_count: u32::try_from(sema.module().store.variants.get(variants.clone()).len())
                .expect("variant count overflow"),
            field_count: u32::try_from(sema.module().store.fields.get(fields.clone()).len())
                .expect("field count overflow"),
        });
    } else if is_callable || matches!(sema.ty(ty).kind, HirTyKind::Arrow { .. }) {
        items.callables.push(IrCallable {
            symbol: name.name,
            name: interner.resolve(name.name).into(),
            expr: value,
            ty,
            effects,
            module_target,
        });
    } else {
        items.globals.push(IrGlobal {
            symbol: name.name,
            name: interner.resolve(name.name).into(),
            expr: value,
            ty,
            effects: sema.expr_effects(value).clone(),
            module_target: sema.expr_module_target(value).cloned(),
        });
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
        sig: decl.sig,
        param_count: u32::try_from(sema.module().store.params.get(decl.params.clone()).len())
            .expect("param count overflow"),
    }
}
