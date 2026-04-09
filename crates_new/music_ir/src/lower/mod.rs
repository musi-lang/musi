use std::collections::{BTreeMap, HashSet};

use music_arena::SliceRange;
use music_base::diag::Diag;
use music_hir::{
    HirArg, HirArrayItem, HirBinaryOp, HirCaseArm, HirDim, HirExprId, HirExprKind, HirForeignDecl,
    HirHandleClause, HirLetMods, HirLitId, HirLitKind, HirParam, HirPatId, HirPatKind, HirPrefixOp,
    HirRecordItem, HirRecordPatField, HirTyField, HirTyId, HirTyKind,
};
use music_module::ModuleKey;
use music_names::{Ident, Interner, NameBindingId, NameSite, Symbol};
use music_sema::{DefinitionKey, SemaModule};

use crate::api::{
    IrArg, IrAssignTarget, IrBinaryOp, IrCallable, IrCaseArm as IrLoweredCaseArm, IrCasePattern,
    IrClassDef, IrDataDef, IrDiagList, IrEffectDef, IrExpr, IrExprKind, IrForeignDef, IrGlobal,
    IrHandleOp, IrInstanceDef, IrLit, IrModule, IrNameRef, IrOrigin, IrParam,
    IrRecordField, IrRecordLayoutField, IrSeqPart, IrTempId,
};

mod bindings;
mod call;
mod collect;
mod assign;
mod meta;
mod array;
mod record;
mod toplevel;
mod validate;

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

type RecordLayout = (BTreeMap<Symbol, u16>, Box<[IrRecordLayoutField]>, u16);

struct LowerCtx<'a> {
    sema: &'a SemaModule,
    interner: &'a Interner,
    module_key: ModuleKey,
    module_level_bindings: HashSet<NameBindingId>,
    next_lambda_id: u32,
    next_temp_id: u32,
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
    validate::validate_surface(sema, &mut diags);
    if !diags.is_empty() {
        return Err(diags);
    }

    let module_level_bindings = bindings::collect_module_level_bindings(sema);
    let mut ctx = LowerCtx {
        sema,
        interner,
        module_key: sema.resolved().module_key.clone(),
        module_level_bindings,
        next_lambda_id: 0,
        next_temp_id: 0,
        extra_callables: Vec::new(),
    };

    let mut items = TopLevelItems::default();
    toplevel::collect_top_level_items(&mut ctx, sema.module().root, false, &mut items);
    items.callables.extend(ctx.extra_callables);

    let mut seen_data_keys = HashSet::<DefinitionKey>::new();
    for data_def in &items.data_defs {
        let _ = seen_data_keys.insert(data_def.key.clone());
    }
    for data in sema.data_defs().values() {
        if !data.key.name.starts_with("__sum__") {
            continue;
        }
        if seen_data_keys.contains(&data.key) {
            continue;
        }
        let mut max_field_count = 0u32;
        for variant in data.variants.values() {
            let arity = variant.payload.map_or(0u32, |payload_ty| match &sema.ty(payload_ty).kind {
                HirTyKind::Tuple { items } => {
                    u32::try_from(sema.module().store.ty_ids.get(*items).len()).unwrap_or(u32::MAX)
                }
                _ => 1u32,
            });
            max_field_count = max_field_count.max(arity);
        }
        items.data_defs.push(IrDataDef {
            key: data.key.clone(),
            variant_count: u32::try_from(data.variants.len()).unwrap_or(u32::MAX),
            field_count: max_field_count,
            repr_kind: data.repr_kind.clone(),
            layout_align: data.layout_align,
            layout_pack: data.layout_pack,
        });
    }
    let meta = meta::collect_meta(sema);

    Ok(IrModule {
        module_key: sema.resolved().module_key.clone(),
        static_imports: sema.surface().static_imports.to_vec().into_boxed_slice(),
        types: sema.surface().tys.clone(),
        exports: sema.surface().exported_values.clone(),
        callables: items.callables.into_boxed_slice(),
        globals: items.globals.into_boxed_slice(),
        data_defs: items.data_defs.into_boxed_slice(),
        foreigns: items.foreigns.into_boxed_slice(),
        effects: {
            let mut seen = BTreeMap::<DefinitionKey, IrEffectDef>::new();
            for effect in sema.effect_defs().values() {
                let _ = seen
                    .entry(effect.key.clone())
                    .or_insert_with(|| IrEffectDef::from(effect));
            }
            seen.into_values().collect::<Vec<_>>().into_boxed_slice()
        },
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
        meta,
    })
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
        HirExprKind::Record { items } => lower_record_expr(ctx, expr_id, items.clone()),
        HirExprKind::Let {
            mods,
            pat,
            has_param_clause,
            params,
            value,
            ..
        } => lower_let_expr(ctx, origin, *mods, *pat, *has_param_clause, params, *value),
        HirExprKind::Binary { op, left, right } => {
            lower_binary_expr(ctx, op, *left, *right)
        }
        HirExprKind::Call { callee, args } => lower_call_expr(ctx, *callee, args),
        HirExprKind::Apply { callee, .. } => {
            let mut lowered = lower_expr(ctx, *callee);
            lowered.origin = origin;
            return lowered;
        }
        HirExprKind::Prefix {
            op: HirPrefixOp::Mut,
            expr,
        } => {
            let mut lowered = lower_expr(ctx, *expr);
            lowered.origin = origin;
            return lowered;
        }
        HirExprKind::Index { base, args } => lower_index_expr(ctx, *base, *args),
        HirExprKind::Field { base, name, .. } => {
            lower_field_expr(ctx, expr_id, *base, name.name, &expr.kind)
        }
        HirExprKind::RecordUpdate { base, items } => {
            lower_record_update_expr(ctx, expr_id, *base, items.clone())
        }
        HirExprKind::Case { scrutinee, arms } => lower_case_expr(ctx, *scrutinee, arms),
        HirExprKind::Variant { tag, args } => lower_variant_expr(ctx, expr_id, *tag, *args),
        HirExprKind::Lambda { params, body, .. } => {
            lower_lambda_expr(ctx, origin, params, *body)
        }
        HirExprKind::Perform { expr } => lower_perform_expr(ctx, *expr),
        HirExprKind::Handle {
            expr,
            handler,
            clauses,
        } => lower_handle_expr(ctx, expr_id, *expr, *handler, clauses.clone()),
        HirExprKind::Resume { expr } => IrExprKind::Resume {
            expr: expr.map(|expr| Box::new(lower_expr(ctx, expr))),
        },
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
    array::lower_array_expr(ctx, expr_id, items)
}

fn record_layout_for_ty(
    sema: &SemaModule,
    ty: HirTyId,
    interner: &Interner,
) -> Option<RecordLayout> {
    let HirTyKind::Record { fields } = &sema.ty(ty).kind else {
        return None;
    };
    let mut items = sema
        .module()
        .store
        .ty_fields
        .get(fields.clone())
        .iter()
        .map(|field| (Box::<str>::from(interner.resolve(field.name)), field.name))
        .collect::<Vec<_>>();
    items.sort_by(|(left, _), (right, _)| left.cmp(right));

    let field_count =
        u16::try_from(items.len()).expect("record field count exceeds u16 in lowering");
    let mut indices = BTreeMap::<Symbol, u16>::new();
    let layout = items
        .into_iter()
        .enumerate()
        .map(|(idx, (name, symbol))| {
            let idx =
                u16::try_from(idx).expect("record field index exceeds u16 in lowering");
            let _ = indices.insert(symbol, idx);
            IrRecordLayoutField { name, index: idx }
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    Some((indices, layout, field_count))
}

fn lower_record_expr(ctx: &mut LowerCtx<'_>, expr_id: HirExprId, items: SliceRange<HirRecordItem>) -> IrExprKind {
    record::lower_record_expr(ctx, expr_id, items)
}

fn lower_variant_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    tag: Ident,
    args: SliceRange<HirExprId>,
) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let ty = sema.expr_ty(expr_id);
    let data = match &sema.ty(ty).kind {
        HirTyKind::Named { name, .. } => {
            let data_name = interner.resolve(*name);
            sema.data_def(data_name)
        }
        HirTyKind::Sum { left, right } => {
            let synth_name = format!("__sum__{}_{}", left.raw(), right.raw());
            sema.data_def(synth_name.as_str())
        }
        _ => None,
    };
    let Some(data) = data else {
        return IrExprKind::Unsupported {
            description: "variant without data type definition".into(),
        };
    };
    let tag_name = interner.resolve(tag.name);
    let tag_index = data
        .variants
        .keys()
        .position(|name| name.as_ref() == tag_name)
        .and_then(|index| u16::try_from(index).ok())
        .unwrap_or(u16::MAX);
    let variant_count = u16::try_from(data.variants.len()).unwrap_or(u16::MAX);
    if tag_index == u16::MAX {
        return IrExprKind::Unsupported {
            description: "unknown variant tag".into(),
        };
    }

    let arg_exprs = sema.module().store.expr_ids.get(args);
    let lowered_args = arg_exprs
        .iter()
        .copied()
        .map(|expr| lower_expr(ctx, expr))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let field_count = u16::try_from(lowered_args.len()).unwrap_or(u16::MAX);
    let _ = variant_count;
    IrExprKind::VariantNew {
        data_key: data.key.clone(),
        tag_index,
        field_count,
        args: lowered_args,
    }
}

fn lower_perform_expr(ctx: &mut LowerCtx<'_>, expr: HirExprId) -> IrExprKind {
    call::lower_perform_expr(ctx, expr)
}

fn lower_handle_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    expr: HirExprId,
    handler: Ident,
    clauses: SliceRange<HirHandleClause>,
) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let handler_name = interner.resolve(handler.name);
    let Some(effect) = sema.effect_def(handler_name) else {
        return IrExprKind::Unsupported {
            description: "handle with unknown effect".into(),
        };
    };
    let origin = sema.module().store.exprs.get(expr_id).origin;
    let origin = IrOrigin {
        source_id: origin.source_id,
        span: origin.span,
    };

    let clauses_vec = sema.module().store.handle_clauses.get(clauses).to_vec();
    let mut value_clause = None::<HirHandleClause>;
    let mut op_clauses = Vec::<HirHandleClause>::new();
    for clause in clauses_vec {
        let op_name = interner.resolve(clause.op.name);
        if op_name == "value" {
            value_clause = Some(clause);
        } else {
            op_clauses.push(clause);
        }
    }
    let Some(value_clause) = value_clause else {
        return IrExprKind::Unsupported {
            description: "handle without value clause".into(),
        };
    };

    let value_closure = lower_handler_clause_closure(
        ctx,
        origin,
        "hdl.value",
        &[value_clause.op],
        value_clause.body,
    );

    let mut ops_by_index = vec![None::<IrHandleOp>; effect.ops.len()];
    for clause in op_clauses {
        let op_name = interner.resolve(clause.op.name);
        let Some(op_index) = effect.ops.keys().position(|name| name.as_ref() == op_name) else {
            return IrExprKind::Unsupported {
                description: "handle clause with unknown effect op".into(),
            };
        };
        let closure = lower_handler_clause_closure(
            ctx,
            origin,
            &format!("hdl.op.{op_name}"),
            sema.module().store.idents.get(clause.params),
            clause.body,
        );
        ops_by_index[op_index] = Some(IrHandleOp {
            op_index: u16::try_from(op_index).unwrap_or(u16::MAX),
            name: op_name.into(),
            closure,
        });
    }

    if ops_by_index.iter().any(Option::is_none) {
        return IrExprKind::Unsupported {
            description: "handle missing op clause".into(),
        };
    }

    IrExprKind::Handle {
        effect_key: effect.key.clone(),
        value: Box::new(value_closure),
        ops: ops_by_index.into_iter().flatten().collect::<Vec<_>>().into_boxed_slice(),
        body: Box::new(lower_expr(ctx, expr)),
    }
}

fn lower_handler_clause_closure(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    prefix: &str,
    params: &[Ident],
    body: HirExprId,
) -> IrExpr {
    let sema = ctx.sema;
    let interner = ctx.interner;

    let lowered_body = lower_expr(ctx, body);
    let (user_params, user_param_bindings) = {
        let mut lowered = Vec::<IrParam>::new();
        let mut bindings = Vec::<NameBindingId>::new();
        for param in params {
            let binding = decl_binding_id(sema, *param).expect("handler clause param binding missing");
            bindings.push(binding);
            lowered.push(IrParam {
                binding,
                name: interner.resolve(param.name).into(),
            });
        }
        (lowered, bindings)
    };

    let captures = compute_capture_bindings(ctx, &lowered_body, &user_param_bindings, None);
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
        .map(|cap| name_expr(ctx, origin, cap))
        .collect::<Vec<_>>()
        .into_boxed_slice();

    let mut callable_params = Vec::new();
    callable_params.extend(capture_params);
    callable_params.extend(user_params);

    let callable_name = fresh_lambda_name(ctx, prefix, origin);
    ctx.extra_callables.push(IrCallable {
        binding: None,
        name: callable_name.clone(),
        params: callable_params.into_boxed_slice(),
        body: lowered_body,
        exported: false,
        effects: sema.expr_effects(body).clone(),
        module_target: sema.expr_module_target(body).cloned(),
    });

    IrExpr {
        origin,
        kind: IrExprKind::ClosureNew {
            callee: IrNameRef {
                binding: None,
                name: callable_name,
                module_target: Some(ctx.module_key.clone()),
            },
            captures: capture_exprs,
        },
    }
}

fn lower_let_expr(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
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
        _ => lower_destructure_let(ctx, origin, mods.is_mut, pat, value),
    }
}

fn lower_destructure_let(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    is_mut: bool,
    pat: HirPatId,
    value: HirExprId,
) -> IrExprKind {
    let module_target = ctx.sema.expr_module_target(value);

    let value_expr = lower_expr(ctx, value);
    let temp = fresh_temp(ctx);
    let mut exprs = Vec::<IrExpr>::new();
    exprs.push(IrExpr {
        origin,
        kind: IrExprKind::TempLet {
            temp,
            value: Box::new(value_expr),
        },
    });

    let base = IrExpr {
        origin,
        kind: IrExprKind::Temp { temp },
    };
    lower_irrefutable_pat_bindings(
        ctx,
        origin,
        is_mut,
        module_target,
        pat,
        base,
        &mut exprs,
    );
    exprs.push(IrExpr {
        origin,
        kind: IrExprKind::Unit,
    });
    IrExprKind::Sequence {
        exprs: exprs.into_boxed_slice(),
    }
}

const fn fresh_temp(ctx: &mut LowerCtx<'_>) -> IrTempId {
    let raw = ctx.next_temp_id;
    ctx.next_temp_id = ctx.next_temp_id.saturating_add(1);
    IrTempId::from_raw(raw)
}

#[derive(Clone, Copy)]
struct IrrefutablePatInput<'a> {
    origin: IrOrigin,
    is_mut: bool,
    module_target: Option<&'a ModuleKey>,
}

fn lower_irrefutable_pat_bindings(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    is_mut: bool,
    module_target: Option<&ModuleKey>,
    pat: HirPatId,
    base: IrExpr,
    out: &mut Vec<IrExpr>,
) {
    let sema = ctx.sema;
    let input = IrrefutablePatInput {
        origin,
        is_mut,
        module_target,
    };
    match &sema.module().store.pats.get(pat).kind {
        HirPatKind::Error | HirPatKind::Wildcard => {}
        HirPatKind::Bind { name } => lower_irrefutable_bind(ctx, input, *name, base, out),
        HirPatKind::As { pat: inner, name } => {
            lower_irrefutable_as(ctx, input, *inner, *name, base, out);
        }
        HirPatKind::Tuple { items } | HirPatKind::Array { items } => {
            lower_irrefutable_sequence(ctx, input, *items, base, out);
        }
        HirPatKind::Record { fields } => {
            lower_irrefutable_record(ctx, input, fields.clone(), pat, base, out);
        }
        other => push_unsupported_pat(origin, other, out),
    }
}

fn lower_irrefutable_bind(
    ctx: &LowerCtx<'_>,
    input: IrrefutablePatInput<'_>,
    name: Ident,
    base: IrExpr,
    out: &mut Vec<IrExpr>,
) {
    let sema = ctx.sema;
    let interner = ctx.interner;
    out.push(IrExpr {
        origin: input.origin,
        kind: IrExprKind::Let {
            binding: decl_binding_id(sema, name),
            name: interner.resolve(name.name).into(),
            is_mut: input.is_mut,
            value: Box::new(base),
        },
    });
}

fn lower_irrefutable_as(
    ctx: &mut LowerCtx<'_>,
    input: IrrefutablePatInput<'_>,
    inner: HirPatId,
    name: Ident,
    base: IrExpr,
    out: &mut Vec<IrExpr>,
) {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let stored = store_in_temp(ctx, input.origin, base, out);
    out.push(IrExpr {
        origin: input.origin,
        kind: IrExprKind::Let {
            binding: decl_binding_id(sema, name),
            name: interner.resolve(name.name).into(),
            is_mut: input.is_mut,
            value: Box::new(stored.clone()),
        },
    });
    lower_irrefutable_pat_bindings(
        ctx,
        input.origin,
        input.is_mut,
        input.module_target,
        inner,
        stored,
        out,
    );
}

fn lower_irrefutable_sequence(
    ctx: &mut LowerCtx<'_>,
    input: IrrefutablePatInput<'_>,
    items: SliceRange<HirPatId>,
    base: IrExpr,
    out: &mut Vec<IrExpr>,
) {
    let sema = ctx.sema;
    let stored = store_in_temp(ctx, input.origin, base, out);
    for (index, item) in sema.module().store.pat_ids.get(items).iter().enumerate() {
        let Ok(index_u32) = u32::try_from(index) else {
            continue;
        };
        let proj = IrExpr {
            origin: input.origin,
            kind: IrExprKind::Index {
                base: Box::new(stored.clone()),
                index: Box::new(IrExpr {
                    origin: input.origin,
                    kind: IrExprKind::Lit(IrLit::Int {
                        raw: index_u32.to_string().into(),
                    }),
                }),
            },
        };
        lower_irrefutable_pat_bindings(
            ctx,
            input.origin,
            input.is_mut,
            input.module_target,
            *item,
            proj,
            out,
        );
    }
}

fn lower_irrefutable_record(
    ctx: &mut LowerCtx<'_>,
    input: IrrefutablePatInput<'_>,
    fields: SliceRange<HirRecordPatField>,
    pat: HirPatId,
    base: IrExpr,
    out: &mut Vec<IrExpr>,
) {
    let sema = ctx.sema;
    let pat_ty = sema.pat_ty(pat);
    match &sema.ty(pat_ty).kind {
        HirTyKind::Module => lower_irrefutable_module_record(
            ctx,
            input.origin,
            input.is_mut,
            input.module_target,
            fields,
            out,
        ),
        HirTyKind::Record { .. } => {
            lower_irrefutable_value_record(ctx, input, fields, pat_ty, base, out);
        }
        _ => out.push(IrExpr {
            origin: input.origin,
            kind: IrExprKind::Unsupported {
                description: "record destructuring without record base".into(),
            },
        }),
    }
}

fn lower_irrefutable_module_record(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    is_mut: bool,
    module_target: Option<&ModuleKey>,
    fields: SliceRange<HirRecordPatField>,
    out: &mut Vec<IrExpr>,
) {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let Some(module_target) = module_target else {
        out.push(IrExpr {
            origin,
            kind: IrExprKind::Unsupported {
                description: "module destructuring without module target".into(),
            },
        });
        return;
    };
    for field in sema.module().store.record_pat_fields.get(fields) {
        let name_text: Box<str> = interner.resolve(field.name.name).into();
        let proj = IrExpr {
            origin,
            kind: IrExprKind::Name {
                binding: None,
                name: name_text.clone(),
                module_target: Some(module_target.clone()),
            },
        };
        if let Some(value_pat) = field.value {
            lower_irrefutable_pat_bindings(
                ctx,
                origin,
                is_mut || field.is_mut,
                Some(module_target),
                value_pat,
                proj,
                out,
            );
        } else {
            out.push(IrExpr {
                origin,
                kind: IrExprKind::Let {
                    binding: decl_binding_id(sema, field.name),
                    name: name_text,
                    is_mut: is_mut || field.is_mut,
                    value: Box::new(proj),
                },
            });
        }
    }
}

fn lower_irrefutable_value_record(
    ctx: &mut LowerCtx<'_>,
    input: IrrefutablePatInput<'_>,
    fields: SliceRange<HirRecordPatField>,
    pat_ty: HirTyId,
    base: IrExpr,
    out: &mut Vec<IrExpr>,
) {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let stored = store_in_temp(ctx, input.origin, base, out);
    let Some((indices, _layout, _field_count)) = record_layout_for_ty(sema, pat_ty, interner) else {
        out.push(IrExpr {
            origin: input.origin,
            kind: IrExprKind::Unsupported {
                description: "record destructuring without record layout".into(),
            },
        });
        return;
    };
    for field in sema.module().store.record_pat_fields.get(fields) {
        let Some(index) = indices.get(&field.name.name).copied() else {
            out.push(IrExpr {
                origin: input.origin,
                kind: IrExprKind::Unsupported {
                    description: "record destructuring missing field".into(),
                },
            });
            continue;
        };
        let proj = IrExpr {
            origin: input.origin,
            kind: IrExprKind::RecordGet {
                base: Box::new(stored.clone()),
                index,
            },
        };
        if let Some(value_pat) = field.value {
            lower_irrefutable_pat_bindings(
                ctx,
                input.origin,
                input.is_mut || field.is_mut,
                input.module_target,
                value_pat,
                proj,
                out,
            );
        } else {
            lower_irrefutable_bind(
                ctx,
                IrrefutablePatInput {
                    origin: input.origin,
                    is_mut: input.is_mut || field.is_mut,
                    module_target: input.module_target,
                },
                field.name,
                proj,
                out,
            );
        }
    }
}

fn store_in_temp(ctx: &mut LowerCtx<'_>, origin: IrOrigin, base: IrExpr, out: &mut Vec<IrExpr>) -> IrExpr {
    let temp = fresh_temp(ctx);
    out.push(IrExpr {
        origin,
        kind: IrExprKind::TempLet {
            temp,
            value: Box::new(base),
        },
    });
    IrExpr {
        origin,
        kind: IrExprKind::Temp { temp },
    }
}

fn push_unsupported_pat(origin: IrOrigin, other: &HirPatKind, out: &mut Vec<IrExpr>) {
    out.push(IrExpr {
        origin,
        kind: IrExprKind::Unsupported {
            description: format!("local let pattern {other:?}").into(),
        },
    });
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
    collect::collect_used_bindings(expr, out);
}

fn collect_local_decl_bindings(expr: &IrExpr, out: &mut HashSet<NameBindingId>) {
    collect::collect_local_decl_bindings(expr, out);
}

fn collect_pattern_bindings(pattern: &IrCasePattern, out: &mut HashSet<NameBindingId>) {
    match pattern {
        IrCasePattern::Wildcard | IrCasePattern::Lit(_) => {}
        IrCasePattern::Bind { binding, .. } => {
            let _ = out.insert(*binding);
        }
        IrCasePattern::Tuple { items } | IrCasePattern::Array { items } => {
            for item in items {
                collect_pattern_bindings(item, out);
            }
        }
        IrCasePattern::Variant { args, .. } => {
            for arg in args {
                collect_pattern_bindings(arg, out);
            }
        }
        IrCasePattern::As { pat, binding, .. } => {
            let _ = out.insert(*binding);
            collect_pattern_bindings(pat, out);
        }
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
    call::lower_call_expr(ctx, callee, args)
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
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    base: HirExprId,
    symbol: Symbol,
    kind: &HirExprKind,
) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;

    let base_ty = sema.expr_ty(base);
    let record_ty = match sema.ty(base_ty).kind {
        HirTyKind::Mut { inner } => inner,
        _ => base_ty,
    };
    if matches!(sema.ty(record_ty).kind, HirTyKind::Record { .. }) {
        let Some((indices, _layout, _field_count)) = record_layout_for_ty(sema, record_ty, interner)
        else {
            return IrExprKind::Unsupported {
                description: "record field access without record layout".into(),
            };
        };
        let Some(index) = indices.get(&symbol).copied() else {
            return IrExprKind::Unsupported {
                description: "record field access missing field".into(),
            };
        };
        return IrExprKind::RecordGet {
            base: Box::new(lower_expr(ctx, base)),
            index,
        };
    }

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

fn lower_record_update_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    base: HirExprId,
    items: SliceRange<HirRecordItem>,
) -> IrExprKind {
    record::lower_record_update_expr(ctx, expr_id, base, items)
}

fn lower_case_expr(ctx: &mut LowerCtx<'_>, scrutinee: HirExprId, arms: &SliceRange<HirCaseArm>) -> IrExprKind {
    let sema = ctx.sema;
    let mut lowered = Vec::<IrLoweredCaseArm>::new();
    for arm in sema.module().store.case_arms.get(arms.clone()) {
        lowered.extend(lower_case_arm(ctx, arm));
    }
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

fn lower_case_arm(ctx: &mut LowerCtx<'_>, arm: &HirCaseArm) -> Vec<IrLoweredCaseArm> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let patterns = lower_case_patterns(sema, arm.pat, interner);
    patterns
        .into_iter()
        .map(|pattern| IrLoweredCaseArm {
            pattern,
            guard: arm.guard.map(|guard| lower_expr(ctx, guard)),
            expr: lower_expr(ctx, arm.expr),
        })
        .collect()
}

fn lower_case_patterns(
    sema: &SemaModule,
    pat: HirPatId,
    interner: &Interner,
) -> Vec<IrCasePattern> {
    match &sema.module().store.pats.get(pat).kind {
        HirPatKind::Wildcard => vec![IrCasePattern::Wildcard],
        HirPatKind::Bind { name } => decl_binding_id(sema, *name)
            .map(|binding| {
                vec![IrCasePattern::Bind {
                    binding,
                    name: interner.resolve(name.name).into(),
                }]
            })
            .unwrap_or_default(),
        HirPatKind::Lit { expr } => lower_lit_pattern(sema, *expr).into_iter().collect(),
        HirPatKind::Tuple { items } => lower_product_patterns(
            sema,
            sema.module().store.pat_ids.get(*items),
            interner,
            |items| IrCasePattern::Tuple { items },
        ),
        HirPatKind::Array { items } => lower_product_patterns(
            sema,
            sema.module().store.pat_ids.get(*items),
            interner,
            |items| IrCasePattern::Array { items },
        ),
        HirPatKind::Variant { tag, args } => lower_variant_patterns(sema, pat, *tag, *args, interner),
        HirPatKind::Or { left, right } => {
            let mut out = lower_case_patterns(sema, *left, interner);
            out.extend(lower_case_patterns(sema, *right, interner));
            out
        }
        HirPatKind::As { pat: inner, name } => decl_binding_id(sema, *name)
            .map(|binding| {
                lower_case_patterns(sema, *inner, interner)
                    .into_iter()
                    .map(|pat| IrCasePattern::As {
                        pat: Box::new(pat),
                        binding,
                        name: interner.resolve(name.name).into(),
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default(),
        _ => Vec::new(),
    }
}

fn lower_product_patterns<F>(
    sema: &SemaModule,
    items: &[HirPatId],
    interner: &Interner,
    ctor: F,
) -> Vec<IrCasePattern>
where
    F: Fn(Box<[IrCasePattern]>) -> IrCasePattern,
{
    let mut acc = vec![Vec::<IrCasePattern>::new()];
    for item in items.iter().copied() {
        let alts = lower_case_patterns(sema, item, interner);
        if alts.is_empty() {
            return Vec::new();
        }
        let mut next = Vec::<Vec<IrCasePattern>>::new();
        for prefix in &acc {
            for alt in &alts {
                let mut updated = prefix.clone();
                updated.push(alt.clone());
                next.push(updated);
            }
        }
        acc = next;
    }
    acc.into_iter()
        .map(|items| ctor(items.into_boxed_slice()))
        .collect()
}

fn lower_variant_patterns(
    sema: &SemaModule,
    pat: HirPatId,
    tag: Ident,
    args: SliceRange<HirPatId>,
    interner: &Interner,
) -> Vec<IrCasePattern> {
    let ty = sema.pat_ty(pat);
    let data = match &sema.ty(ty).kind {
        HirTyKind::Named { name, .. } => {
            let data_name = interner.resolve(*name);
            sema.data_def(data_name)
        }
        HirTyKind::Sum { left, right } => {
            let synth_name = format!("__sum__{}_{}", left.raw(), right.raw());
            sema.data_def(synth_name.as_str())
        }
        _ => None,
    };
    let Some(data) = data else {
        return Vec::new();
    };
    let tag_name = interner.resolve(tag.name);
    let Some(tag_index) = data
        .variants
        .keys()
        .position(|name| name.as_ref() == tag_name)
        .and_then(|index| u16::try_from(index).ok())
    else {
        return Vec::new();
    };
    let Some(variant_count) = u16::try_from(data.variants.len()).ok() else {
        return Vec::new();
    };

    lower_product_patterns(
        sema,
        sema.module().store.pat_ids.get(args),
        interner,
        |items| IrCasePattern::Variant {
            data_key: data.key.clone(),
            variant_count,
            tag_index,
            args: items,
        },
    )
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
    assign::lower_assign_expr(ctx, left, right)
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
    exported: bool,
) -> IrForeignDef {
    let name: Box<str> = interner.resolve(decl.name.name).into();
    let binding = decl_binding_id(sema, decl.name);
    let mut symbol = name.clone();
    let mut link = None::<Box<str>>;
    if let Some(binding) = binding {
        if let Some(attrs) = sema.foreign_link(binding) {
            link.clone_from(&attrs.name);
            if let Some(symbol_override) = attrs.symbol.as_ref() {
                symbol = symbol_override.clone();
            }
        }
    }
    IrForeignDef {
        binding,
        symbol,
        name,
        abi: abi.unwrap_or("c").into(),
        param_count: u32::try_from(sema.module().store.params.get(decl.params.clone()).len())
            .expect("param count overflow"),
        link,
        exported,
    }
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
        HirTyKind::Nat => "Nat".into(),
        HirTyKind::Int => "Int".into(),
        HirTyKind::Float => "Float".into(),
        HirTyKind::String => "String".into(),
        HirTyKind::CString => "CString".into(),
        HirTyKind::CPtr => "CPtr".into(),
        HirTyKind::Module => "Module".into(),
        HirTyKind::NatLit(value) => value.to_string().into(),
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
