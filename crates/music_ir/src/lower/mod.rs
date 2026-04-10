use std::collections::{BTreeMap, HashSet};
use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};

use music_arena::SliceRange;
use music_base::diag::Diag;
use music_hir::{
    HirArg, HirArrayItem, HirBinaryOp, HirCaseArm, HirDim, HirExprId, HirExprKind, HirHandleClause,
    HirLetMods, HirLitId, HirLitKind, HirParam, HirPatId, HirPatKind, HirPrefixOp, HirQuoteKind,
    HirRecordItem, HirRecordPatField, HirSpliceKind, HirTemplatePart, HirTyField, HirTyId,
    HirTyKind,
};
use music_module::ModuleKey;
use music_names::{Ident, Interner, NameBindingId, NameSite, Symbol};
use music_sema::{DefinitionKey, SemaModule};

use crate::IrDiagKind;
use crate::api::{
    IrArg, IrAssignTarget, IrBinaryOp, IrCallable, IrCaseArm as IrLoweredCaseArm, IrCasePattern,
    IrCaseRecordField, IrClassDef, IrDataDef, IrDiagList, IrEffectDef, IrEffectOpDef, IrExpr,
    IrExprKind, IrForeignDef, IrGlobal, IrHandleOp, IrInstanceDef, IrLit, IrModule, IrNameRef,
    IrOrigin, IrParam, IrRecordField, IrRecordLayoutField, IrSeqPart, IrTempId,
};

mod array;
mod assign;
mod bindings;
mod call;
mod collect;
mod destructure;
mod meta;
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
    expr_id: HirExprId,
    pat: HirPatId,
    params: SliceRange<HirParam>,
    value: HirExprId,
    is_callable: bool,
    exported: bool,
}

type RecordLayout = (BTreeMap<Symbol, u16>, Box<[IrRecordLayoutField]>, u16);

struct LoweredParams {
    params: Vec<IrParam>,
    bindings: Vec<NameBindingId>,
}

struct ClosureCallableInput<'a> {
    origin: IrOrigin,
    prefix: &'a str,
    body_id: HirExprId,
    body: IrExpr,
    params: LoweredParams,
    binding: Option<NameBindingId>,
    name: Option<Box<str>>,
    callable_module_target: Option<ModuleKey>,
    rewrite_recursive_self: bool,
}

struct LowerCtx<'a> {
    sema: &'a SemaModule,
    interner: &'a Interner,
    module_key: ModuleKey,
    module_level_bindings: HashSet<NameBindingId>,
    next_lambda_id: u32,
    next_temp_id: u32,
    extra_callables: Vec<IrCallable>,
}

struct RecursiveBindingInput<'a, 'b> {
    ctx: &'a LowerCtx<'b>,
    origin: IrOrigin,
    binding: NameBindingId,
    callable_name: &'a str,
    captures: &'a [NameBindingId],
}

struct RecordUpdateRewriteInput {
    ty_name: Box<str>,
    field_count: u16,
    base: IrExpr,
    base_fields: Box<[IrRecordLayoutField]>,
    result_fields: Box<[IrRecordLayoutField]>,
    updates: Box<[IrRecordField]>,
}

#[derive(Debug)]
struct LoweringInvariant {
    description: Box<str>,
}

/// Lowers sema-owned module facts into the codegen-facing IR surface.
///
/// # Errors
///
/// Returns semantic diagnostics when exported surface types or effect rows reference invalid
/// sema-owned ids.
pub fn lower_module(sema: &SemaModule, interner: &Interner) -> Result<IrModule, IrDiagList> {
    match catch_unwind(AssertUnwindSafe(|| lower_module_impl(sema, interner))) {
        Ok(result) => result,
        Err(payload) => {
            let Some(invariant) = payload.downcast_ref::<LoweringInvariant>() else {
                resume_unwind(payload);
            };
            Err(vec![
                Diag::error(IrDiagKind::LoweringInvariantViolated.message())
                    .with_code(IrDiagKind::LoweringInvariantViolated.code())
                    .with_note(format!("detail `{}`", invariant.description)),
            ])
        }
    }
}

fn lower_module_impl(sema: &SemaModule, interner: &Interner) -> Result<IrModule, IrDiagList> {
    let mut diags = Vec::<Diag>::new();
    if !sema.diags().is_empty() {
        diags.push(
            Diag::error(IrDiagKind::LoweringRequiresSemaCleanModule.message())
                .with_code(IrDiagKind::LoweringRequiresSemaCleanModule.code())
                .with_note(format!("sema diagnostic count `{}`", sema.diags().len())),
        );
        return Err(diags);
    }
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
    append_synthesized_sum_data_defs(sema, &mut items);
    let meta = meta::collect_meta(sema);
    let surface = sema.surface();

    Ok(IrModule::new(
        sema.resolved().module_key.clone(),
        surface.static_imports().to_vec().into_boxed_slice(),
        surface.types().to_vec().into_boxed_slice(),
        (
            surface.exported_values().to_vec().into_boxed_slice(),
            items.callables.into_boxed_slice(),
            items.globals.into_boxed_slice(),
            items.data_defs.into_boxed_slice(),
            items.foreigns.into_boxed_slice(),
            build_effect_defs(sema, ctx.interner),
            surface
                .exported_classes()
                .iter()
                .map(IrClassDef::from)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            surface
                .exported_instances()
                .iter()
                .map(IrInstanceDef::from)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            meta,
        ),
    ))
}

fn append_synthesized_sum_data_defs(sema: &SemaModule, items: &mut TopLevelItems) {
    let mut seen_data_keys = HashSet::<DefinitionKey>::new();
    for data_def in &items.data_defs {
        let _ = seen_data_keys.insert(data_def.key.clone());
    }
    for data in sema.data_defs() {
        if !data.key().name.starts_with("__sum__") || seen_data_keys.contains(data.key()) {
            continue;
        }
        let mut max_field_count = 0u32;
        for (_, variant) in data.variants() {
            let arity =
                variant
                    .payload()
                    .map_or(0u32, |payload_ty| match &sema.ty(payload_ty).kind {
                        HirTyKind::Tuple { items } => {
                            u32::try_from(sema.module().store.ty_ids.get(*items).len())
                                .unwrap_or(u32::MAX)
                        }
                        _ => 1u32,
                    });
            max_field_count = max_field_count.max(arity);
        }
        items.data_defs.push(IrDataDef {
            key: data.key().clone(),
            variant_count: u32::try_from(data.variant_count()).unwrap_or(u32::MAX),
            field_count: max_field_count,
            repr_kind: data.repr_kind().map(Into::into),
            layout_align: data.layout_align(),
            layout_pack: data.layout_pack(),
        });
    }
}

fn build_effect_defs(sema: &SemaModule, interner: &Interner) -> Box<[IrEffectDef]> {
    let mut seen = BTreeMap::<DefinitionKey, IrEffectDef>::new();
    for effect in sema.effect_defs() {
        let _ = seen
            .entry(effect.key().clone())
            .or_insert_with(|| IrEffectDef {
                key: effect.key().clone(),
                ops: effect
                    .ops()
                    .map(|(name, def)| IrEffectOpDef {
                        name: name.into(),
                        param_tys: def
                            .params()
                            .iter()
                            .copied()
                            .map(|ty| render_ty_name(sema, ty, interner))
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                        result_ty: render_ty_name(sema, def.result(), interner),
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            });
    }
    seen.into_values().collect::<Vec<_>>().into_boxed_slice()
}

fn lower_expr(ctx: &mut LowerCtx<'_>, expr_id: HirExprId) -> IrExpr {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let expr = sema.module().store.exprs.get(expr_id);
    let origin = IrOrigin {
        source_id: expr.origin.source_id,
        span: expr.origin.span,
    };
    if is_type_value_expr(sema, expr_id) {
        return IrExpr {
            origin,
            kind: IrExprKind::TypeValue {
                ty_name: render_ty_name(
                    sema,
                    sema.try_expr_ty(expr_id)
                        .expect("expr type missing for type value"),
                    interner,
                ),
            },
        };
    }
    let kind = match &expr.kind {
        HirExprKind::Name { name } => lower_name_expr(sema, expr_id, *name, interner),
        HirExprKind::Lit { lit } => lower_lit_expr(sema, *lit),
        HirExprKind::Sequence { exprs } => lower_sequence_expr(ctx, *exprs),
        HirExprKind::Tuple { items } => lower_tuple_expr(ctx, expr_id, *items),
        HirExprKind::Array { items } => lower_array_expr(ctx, expr_id, items.clone()),
        HirExprKind::Template { parts } => lower_template_expr(ctx, origin, parts.clone()),
        HirExprKind::Record { items } => lower_record_expr(ctx, expr_id, items.clone()),
        HirExprKind::Let {
            mods,
            pat,
            has_param_clause,
            params,
            value,
            ..
        } => lower_let_expr(ctx, origin, *mods, *pat, *has_param_clause, params, *value),
        HirExprKind::Binary { op, left, right } => lower_binary_expr(ctx, op, *left, *right),
        HirExprKind::Call { callee, args } => lower_call_expr(ctx, *callee, args),
        HirExprKind::Apply { callee, .. } => return lower_expr_with_origin(ctx, *callee, origin),
        HirExprKind::Prefix { op, expr } => lower_prefix_expr(ctx, expr_id, op, *expr, origin),
        HirExprKind::Index { base, args } => lower_index_expr(ctx, *base, *args),
        HirExprKind::Field { base, name, .. } => {
            lower_field_expr(ctx, expr_id, *base, name.name, &expr.kind)
        }
        HirExprKind::RecordUpdate { base, items } => {
            lower_record_update_expr(ctx, expr_id, *base, items.clone())
        }
        HirExprKind::Case { scrutinee, arms } => lower_case_expr(ctx, *scrutinee, arms),
        HirExprKind::Variant { tag, args } => lower_variant_expr(ctx, expr_id, *tag, *args),
        HirExprKind::Lambda { params, body, .. } => lower_lambda_expr(ctx, origin, params, *body),
        HirExprKind::Perform { expr } => lower_perform_expr(ctx, *expr),
        HirExprKind::Handle {
            expr,
            handler,
            clauses,
        } => lower_handle_expr(ctx, expr_id, *expr, *handler, clauses.clone()),
        HirExprKind::TypeTest { base, .. } => {
            let ty_name = sema.type_test_target(expr_id).map_or_else(
                || Box::<str>::from("Unknown"),
                |target| render_ty_name(sema, target, interner),
            );
            IrExprKind::TyTest {
                base: lower_boxed_expr(ctx, *base),
                ty_name,
            }
        }
        HirExprKind::TypeCast { base, .. } => IrExprKind::TyCast {
            base: lower_boxed_expr(ctx, *base),
            ty_name: render_ty_name(
                sema,
                sema.try_expr_ty(expr_id)
                    .expect("expr type missing for syntax type value"),
                interner,
            ),
        },
        HirExprKind::Resume { expr } => IrExprKind::Resume {
            expr: expr.map(|expr| lower_boxed_expr(ctx, expr)),
        },
        HirExprKind::Import { arg } => {
            if sema.expr_module_target(expr_id).is_some() {
                IrExprKind::Unit
            } else {
                IrExprKind::DynamicImport {
                    spec: lower_boxed_expr(ctx, *arg),
                }
            }
        }
        HirExprKind::Quote { kind } => lower_quote_expr(kind),
        HirExprKind::Splice { kind } => lower_splice_expr(kind),
        other => invalid_lowering_path(format!("missing IR lowering for {other:?}")),
    };
    IrExpr { origin, kind }
}

fn is_type_value_expr(sema: &SemaModule, expr_id: HirExprId) -> bool {
    match &sema.module().store.exprs.get(expr_id).kind {
        HirExprKind::Error => true,
        HirExprKind::Name { name } => {
            use_binding_id(sema, *name).is_none() && sema.expr_module_target(expr_id).is_none()
        }
        HirExprKind::Tuple { items } => sema
            .module()
            .store
            .expr_ids
            .get(*items)
            .iter()
            .copied()
            .all(|item| is_type_value_expr(sema, item)),
        HirExprKind::ArrayTy { item, .. } => is_type_value_expr(sema, *item),
        HirExprKind::Pi { binder_ty, ret, .. } => {
            is_type_value_expr(sema, *binder_ty) && is_type_value_expr(sema, *ret)
        }
        HirExprKind::Apply { callee, args } => {
            is_type_value_expr(sema, *callee)
                && sema
                    .module()
                    .store
                    .expr_ids
                    .get(*args)
                    .iter()
                    .copied()
                    .all(|arg| type_apply_arg_expr(sema, arg))
        }
        HirExprKind::Binary { op, left, right } => {
            matches!(
                op,
                HirBinaryOp::Add | HirBinaryOp::Arrow | HirBinaryOp::EffectArrow
            ) && is_type_value_expr(sema, *left)
                && is_type_value_expr(sema, *right)
        }
        HirExprKind::Prefix {
            op: HirPrefixOp::Mut,
            expr,
        } => is_type_value_expr(sema, *expr),
        HirExprKind::Record { items } => sema
            .module()
            .store
            .record_items
            .get(items.clone())
            .iter()
            .all(|item| is_type_value_expr(sema, item.value)),
        _ => false,
    }
}

fn type_apply_arg_expr(sema: &SemaModule, expr_id: HirExprId) -> bool {
    match &sema.module().store.exprs.get(expr_id).kind {
        HirExprKind::Lit { lit } => matches!(
            sema.module().store.lits.get(*lit).kind,
            HirLitKind::Int { .. }
        ),
        _ => is_type_value_expr(sema, expr_id),
    }
}

fn lower_prefix_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    op: &HirPrefixOp,
    expr: HirExprId,
    origin: IrOrigin,
) -> IrExprKind {
    match op {
        HirPrefixOp::Mut => lower_expr_with_origin(ctx, expr, origin).kind,
        HirPrefixOp::Not => IrExprKind::Not {
            expr: lower_boxed_expr(ctx, expr),
        },
        HirPrefixOp::Neg => {
            let ty = ctx
                .sema
                .try_expr_ty(expr_id)
                .expect("expr type missing for prefix op");
            let (zero, op) = match &ctx.sema.ty(ty).kind {
                HirTyKind::Float => (
                    IrExpr {
                        origin,
                        kind: IrExprKind::Lit(IrLit::Float { raw: "0.0".into() }),
                    },
                    IrBinaryOp::FSub,
                ),
                HirTyKind::Int | HirTyKind::Nat | HirTyKind::NatLit(_) => (
                    IrExpr {
                        origin,
                        kind: IrExprKind::Lit(IrLit::Int { raw: "0".into() }),
                    },
                    IrBinaryOp::ISub,
                ),
                other => invalid_lowering_path(format!("invalid neg operand type {other:?}")),
            };
            let expr = lower_expr(ctx, expr);
            IrExprKind::Binary {
                op,
                left: Box::new(zero),
                right: Box::new(expr),
            }
        }
    }
}

fn lower_expr_with_origin(ctx: &mut LowerCtx<'_>, expr_id: HirExprId, origin: IrOrigin) -> IrExpr {
    let mut lowered = lower_expr(ctx, expr_id);
    lowered.origin = origin;
    lowered
}

fn lower_boxed_expr(ctx: &mut LowerCtx<'_>, expr_id: HirExprId) -> Box<IrExpr> {
    Box::new(lower_expr(ctx, expr_id))
}

fn lower_quote_expr(kind: &HirQuoteKind) -> IrExprKind {
    let raw = match kind {
        HirQuoteKind::Expr { raw, .. } | HirQuoteKind::Block { raw, .. } => raw.clone(),
    };
    IrExprKind::SyntaxValue { raw }
}

fn lower_splice_expr(kind: &HirSpliceKind) -> IrExprKind {
    let raw = match kind {
        HirSpliceKind::Name { raw, .. }
        | HirSpliceKind::Expr { raw, .. }
        | HirSpliceKind::Exprs { raw, .. } => raw.clone(),
    };
    IrExprKind::SyntaxValue { raw }
}

fn invalid_lowering_path(description: impl AsRef<str>) -> ! {
    let description = description.as_ref().to_owned().into_boxed_str();
    debug_assert!(false, "invalid IR lowering path: {description}");
    resume_unwind(Box::new(LoweringInvariant { description }));
}

fn lower_template_expr(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    parts: SliceRange<HirTemplatePart>,
) -> IrExprKind {
    let mut rendered = Vec::<IrExpr>::new();
    for part in ctx.sema.module().store.template_parts.get(parts) {
        match part {
            HirTemplatePart::Text { value } => rendered.push(IrExpr {
                origin,
                kind: IrExprKind::Lit(IrLit::String {
                    value: value.clone(),
                }),
            }),
            HirTemplatePart::Expr { expr } => rendered.push(lower_expr(ctx, *expr)),
        }
    }
    let mut iter = rendered.into_iter();
    let Some(mut acc) = iter.next() else {
        return IrExprKind::Lit(IrLit::String { value: "".into() });
    };
    for expr in iter {
        acc = IrExpr {
            origin,
            kind: IrExprKind::Binary {
                op: IrBinaryOp::StrCat,
                left: Box::new(acc),
                right: Box::new(expr),
            },
        };
    }
    acc.kind
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

fn lower_tuple_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    items: SliceRange<HirExprId>,
) -> IrExprKind {
    let sema = ctx.sema;
    IrExprKind::Tuple {
        ty_name: render_ty_name(
            sema,
            sema.try_expr_ty(expr_id)
                .expect("expr type missing for tuple literal"),
            ctx.interner,
        ),
        items: lower_expr_list(ctx, items),
    }
}

fn lower_array_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    items: SliceRange<HirArrayItem>,
) -> IrExprKind {
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
            let idx = u16::try_from(idx).expect("record field index exceeds u16 in lowering");
            let _ = indices.insert(symbol, idx);
            IrRecordLayoutField { name, index: idx }
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    Some((indices, layout, field_count))
}

fn lower_record_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    items: SliceRange<HirRecordItem>,
) -> IrExprKind {
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
    let ty = sema
        .try_expr_ty(expr_id)
        .expect("expr type missing for variant");
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
        invalid_lowering_path("variant without data type definition");
    };
    let tag_name = interner.resolve(tag.name);
    let tag_index = data.variant_index(tag_name).unwrap_or(u16::MAX);
    let variant_count = u16::try_from(data.variant_count()).unwrap_or(u16::MAX);
    if tag_index == u16::MAX {
        invalid_lowering_path("unknown variant tag");
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
        data_key: data.key().clone(),
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
        invalid_lowering_path("handle with unknown effect");
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
        invalid_lowering_path("handle without value clause");
    };

    let value_closure = lower_handler_clause_closure(
        ctx,
        origin,
        "hdl.value",
        &[value_clause.op],
        value_clause.body,
    );

    let mut ops_by_index = vec![None::<IrHandleOp>; effect.op_count()];
    for clause in op_clauses {
        let op_name = interner.resolve(clause.op.name);
        let Some(op_index) = effect.op_index(op_name).map(usize::from) else {
            invalid_lowering_path("handle clause with unknown effect op");
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
        invalid_lowering_path("handle missing op clause");
    }

    IrExprKind::Handle {
        effect_key: effect.key().clone(),
        value: Box::new(value_closure),
        ops: ops_by_index
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()
            .into_boxed_slice(),
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
    let lowered_body = lower_expr(ctx, body);
    lower_closure_callable(
        ctx,
        ClosureCallableInput {
            origin,
            prefix,
            body_id: body,
            body: lowered_body,
            params: lower_named_params(ctx, params),
            binding: None,
            name: None,
            callable_module_target: ctx.sema.expr_module_target(body).cloned(),
            rewrite_recursive_self: false,
        },
    )
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
    let is_callable =
        has_param_clause || !sema.module().store.params.get(params.clone()).is_empty();

    if is_callable {
        return lower_local_callable_let(ctx, mods, pat, params, value);
    }

    match &sema.module().store.pats.get(pat).kind {
        HirPatKind::Bind { name } => IrExprKind::Let {
            binding: decl_binding_id(sema, *name),
            name: interner.resolve(name.name).into(),
            value: Box::new(lower_expr(ctx, value)),
        },
        HirPatKind::Wildcard => IrExprKind::Let {
            binding: None,
            name: "_".into(),
            value: Box::new(lower_expr(ctx, value)),
        },
        _ => destructure::lower_destructure_let(ctx, origin, pat, value),
    }
}

const fn fresh_temp(ctx: &mut LowerCtx<'_>) -> IrTempId {
    let raw = ctx.next_temp_id;
    ctx.next_temp_id = ctx.next_temp_id.saturating_add(1);
    IrTempId::from_raw(raw)
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
            invalid_lowering_path(format!("local callable let pattern {other:?}"));
        }
    };

    let mut body = lower_expr(ctx, value);
    body.origin = IrOrigin {
        source_id: sema.module().store.exprs.get(value).origin.source_id,
        span: sema.module().store.exprs.get(value).origin.span,
    };
    let closure_value = lower_closure_callable(
        ctx,
        ClosureCallableInput {
            origin: body.origin,
            prefix: "localfn",
            body_id: value,
            body,
            params: lower_user_params(ctx, params),
            binding,
            name: (binding.is_some() && name.as_ref() != "_").then_some(name.clone()),
            callable_module_target: sema.expr_module_target(value).cloned(),
            rewrite_recursive_self: mods.is_rec,
        },
    );

    IrExprKind::Let {
        binding,
        name,
        value: Box::new(closure_value),
    }
}

fn lower_lambda_expr(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    params: &SliceRange<HirParam>,
    body: HirExprId,
) -> IrExprKind {
    let lowered_body = lower_expr(ctx, body);
    lower_closure_callable(
        ctx,
        ClosureCallableInput {
            origin,
            prefix: "lambda",
            body_id: body,
            body: lowered_body,
            params: lower_user_params(ctx, params),
            binding: None,
            name: None,
            callable_module_target: Some(ctx.module_key.clone()),
            rewrite_recursive_self: false,
        },
    )
    .kind
}

fn lower_user_params(ctx: &LowerCtx<'_>, params: &SliceRange<HirParam>) -> LoweredParams {
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
    LoweredParams {
        params: lowered,
        bindings,
    }
}

fn lower_named_params(ctx: &LowerCtx<'_>, params: &[Ident]) -> LoweredParams {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let mut lowered = Vec::new();
    let mut bindings = Vec::new();
    for param in params {
        let binding =
            decl_binding_id(sema, *param).expect("named param binding missing in lowering");
        bindings.push(binding);
        lowered.push(IrParam {
            binding,
            name: interner.resolve(param.name).into(),
        });
    }
    LoweredParams {
        params: lowered,
        bindings,
    }
}

fn lower_closure_callable(ctx: &mut LowerCtx<'_>, input: ClosureCallableInput<'_>) -> IrExpr {
    let sema = ctx.sema;
    let captures =
        compute_capture_bindings(ctx, &input.body, &input.params.bindings, input.binding);
    let body = if input.rewrite_recursive_self {
        if let Some(binding) = input.binding {
            rewrite_recursive_binding_refs(
                ctx,
                input.origin,
                input.body,
                binding,
                input.name.as_deref().unwrap_or("_"),
                &captures,
            )
        } else {
            input.body
        }
    } else {
        input.body
    };
    let capture_params = lower_capture_params(ctx, &captures);
    let capture_exprs = lower_capture_exprs(ctx, input.origin, &captures);

    let mut callable_params = Vec::new();
    callable_params.extend(capture_params);
    callable_params.extend(input.params.params);

    let callable_name = input
        .name
        .unwrap_or_else(|| fresh_lambda_name(ctx, input.prefix, input.origin));

    ctx.extra_callables.push(IrCallable {
        binding: input.binding,
        name: callable_name.clone(),
        params: callable_params.into_boxed_slice(),
        body,
        exported: false,
        effects: sema
            .try_expr_effects(input.body_id)
            .expect("expr effects missing for closure body")
            .clone(),
        module_target: input.callable_module_target,
    });

    IrExpr {
        origin: input.origin,
        kind: IrExprKind::ClosureNew {
            callee: IrNameRef {
                binding: input.binding,
                name: callable_name,
                module_target: Some(ctx.module_key.clone()),
            },
            captures: capture_exprs,
        },
    }
}

fn rewrite_recursive_binding_refs(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    expr: IrExpr,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExpr {
    let input = RecursiveBindingInput {
        ctx,
        origin,
        binding,
        callable_name,
        captures,
    };
    let origin_expr = expr.origin;
    let kind = rewrite_recursive_binding_kind(&input, expr.kind);
    IrExpr {
        origin: origin_expr,
        kind,
    }
}

fn rewrite_recursive_binding_kind(
    input: &RecursiveBindingInput<'_, '_>,
    kind: IrExprKind,
) -> IrExprKind {
    match kind {
        IrExprKind::Name {
            binding: Some(found),
            module_target,
            ..
        } if found == input.binding && module_target.is_none() => IrExprKind::ClosureNew {
            callee: IrNameRef {
                binding: Some(input.binding),
                name: input.callable_name.into(),
                module_target: Some(input.ctx.module_key.clone()),
            },
            captures: lower_capture_exprs(input.ctx, input.origin, input.captures),
        },
        IrExprKind::Sequence { exprs } => rewrite_sequence_kind(
            input.ctx,
            input.origin,
            exprs,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        IrExprKind::Tuple { ty_name, items } => rewrite_tuple_kind(
            input.ctx,
            input.origin,
            ty_name,
            items,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        IrExprKind::Array { ty_name, items } => rewrite_array_kind(
            input.ctx,
            input.origin,
            ty_name,
            items,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        IrExprKind::ArrayCat { ty_name, parts } => rewrite_array_cat_kind(
            input.ctx,
            input.origin,
            ty_name,
            parts,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        IrExprKind::Record {
            ty_name,
            field_count,
            fields,
        } => rewrite_record_kind(input, ty_name, field_count, fields),
        IrExprKind::RecordGet { base, index } => rewrite_record_get_kind(
            input.ctx,
            input.origin,
            *base,
            index,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        IrExprKind::RecordUpdate {
            ty_name,
            field_count,
            base,
            base_fields,
            result_fields,
            updates,
        } => rewrite_record_update_kind(
            input,
            RecordUpdateRewriteInput {
                ty_name,
                field_count,
                base: *base,
                base_fields,
                result_fields,
                updates,
            },
        ),
        other => rewrite_recursive_binding_storage_kind(input, other),
    }
}

fn rewrite_recursive_binding_storage_kind(
    input: &RecursiveBindingInput<'_, '_>,
    kind: IrExprKind,
) -> IrExprKind {
    match kind {
        IrExprKind::Let {
            binding: local_binding,
            name,
            value,
        } => rewrite_let_kind(input, local_binding, name, *value),
        IrExprKind::TempLet { temp, value } => rewrite_temp_let_kind(
            input.ctx,
            input.origin,
            temp,
            *value,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        IrExprKind::Assign { target, value } => rewrite_assign_kind(
            input.ctx,
            input.origin,
            *target,
            *value,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        IrExprKind::Index { base, indices } => rewrite_index_kind(
            input.ctx,
            input.origin,
            *base,
            indices,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        IrExprKind::DynamicImport { spec } => rewrite_dynamic_import_kind(
            input.ctx,
            input.origin,
            *spec,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        other => rewrite_recursive_binding_compute_kind(input, other),
    }
}

fn rewrite_recursive_binding_compute_kind(
    input: &RecursiveBindingInput<'_, '_>,
    kind: IrExprKind,
) -> IrExprKind {
    match kind {
        IrExprKind::Binary { op, left, right } => rewrite_binary_kind(input, op, *left, *right),
        IrExprKind::Not { expr } => rewrite_not_kind(
            input.ctx,
            input.origin,
            *expr,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        IrExprKind::TyTest { base, ty_name } => rewrite_ty_test_kind(
            input.ctx,
            input.origin,
            *base,
            ty_name,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        IrExprKind::TyCast { base, ty_name } => rewrite_ty_cast_kind(
            input.ctx,
            input.origin,
            *base,
            ty_name,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        IrExprKind::Case { scrutinee, arms } => rewrite_case_kind(
            input.ctx,
            input.origin,
            *scrutinee,
            arms,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        IrExprKind::VariantNew {
            data_key,
            tag_index,
            field_count,
            args,
        } => rewrite_variant_kind(input, data_key, tag_index, field_count, args),
        IrExprKind::Call { callee, args } => rewrite_call_kind(
            input.ctx,
            input.origin,
            *callee,
            args,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        IrExprKind::CallSeq { callee, args } => rewrite_call_seq_kind(
            input.ctx,
            input.origin,
            *callee,
            args,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        other => rewrite_recursive_binding_effect_kind(input, other),
    }
}

fn rewrite_recursive_binding_effect_kind(
    input: &RecursiveBindingInput<'_, '_>,
    kind: IrExprKind,
) -> IrExprKind {
    match kind {
        IrExprKind::Perform {
            effect_key,
            op_index,
            args,
        } => rewrite_perform_kind(input, effect_key, op_index, args),
        IrExprKind::PerformSeq {
            effect_key,
            op_index,
            args,
        } => rewrite_perform_seq_kind(input, effect_key, op_index, args),
        IrExprKind::Handle {
            effect_key,
            value,
            ops,
            body,
        } => rewrite_handle_kind(input, effect_key, *value, ops, *body),
        IrExprKind::Resume { expr } => rewrite_resume_kind(
            input.ctx,
            input.origin,
            expr.map(|expr| *expr),
            input.binding,
            input.callable_name,
            input.captures,
        ),
        other => other,
    }
}

fn rewrite_seq_parts(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    parts: Box<[IrSeqPart]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrSeqPart]> {
    parts
        .into_vec()
        .into_iter()
        .map(|part| match part {
            IrSeqPart::Expr(expr) => IrSeqPart::Expr(rewrite_recursive_binding_refs(
                ctx,
                origin,
                expr,
                binding,
                callable_name,
                captures,
            )),
            IrSeqPart::Spread(expr) => IrSeqPart::Spread(rewrite_recursive_binding_refs(
                ctx,
                origin,
                expr,
                binding,
                callable_name,
                captures,
            )),
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn rewrite_sequence_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    exprs: Box<[IrExpr]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Sequence {
        exprs: exprs
            .into_vec()
            .into_iter()
            .map(|item| {
                rewrite_recursive_binding_refs(ctx, origin, item, binding, callable_name, captures)
            })
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    }
}

fn rewrite_tuple_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    ty_name: Box<str>,
    items: Box<[IrExpr]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Tuple {
        ty_name,
        items: rewrite_expr_slice(ctx, origin, items, binding, callable_name, captures),
    }
}

fn rewrite_array_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    ty_name: Box<str>,
    items: Box<[IrExpr]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Array {
        ty_name,
        items: rewrite_expr_slice(ctx, origin, items, binding, callable_name, captures),
    }
}

fn rewrite_array_cat_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    ty_name: Box<str>,
    parts: Box<[IrSeqPart]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::ArrayCat {
        ty_name,
        parts: rewrite_seq_parts(ctx, origin, parts, binding, callable_name, captures),
    }
}

fn rewrite_record_kind(
    input: &RecursiveBindingInput<'_, '_>,
    ty_name: Box<str>,
    field_count: u16,
    fields: Box<[IrRecordField]>,
) -> IrExprKind {
    IrExprKind::Record {
        ty_name,
        field_count,
        fields: rewrite_record_fields(
            input.ctx,
            input.origin,
            fields,
            input.binding,
            input.callable_name,
            input.captures,
        ),
    }
}

fn rewrite_record_get_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    index: u16,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::RecordGet {
        base: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            base,
            binding,
            callable_name,
            captures,
        )),
        index,
    }
}

fn rewrite_record_update_kind(
    input: &RecursiveBindingInput<'_, '_>,
    update: RecordUpdateRewriteInput,
) -> IrExprKind {
    IrExprKind::RecordUpdate {
        ty_name: update.ty_name,
        field_count: update.field_count,
        base: Box::new(rewrite_recursive_binding_refs(
            input.ctx,
            input.origin,
            update.base,
            input.binding,
            input.callable_name,
            input.captures,
        )),
        base_fields: update.base_fields,
        result_fields: update.result_fields,
        updates: rewrite_record_fields(
            input.ctx,
            input.origin,
            update.updates,
            input.binding,
            input.callable_name,
            input.captures,
        ),
    }
}

fn rewrite_let_kind(
    input: &RecursiveBindingInput<'_, '_>,
    local_binding: Option<NameBindingId>,
    name: Box<str>,
    value: IrExpr,
) -> IrExprKind {
    IrExprKind::Let {
        binding: local_binding,
        name,
        value: Box::new(rewrite_recursive_binding_refs(
            input.ctx,
            input.origin,
            value,
            input.binding,
            input.callable_name,
            input.captures,
        )),
    }
}

fn rewrite_temp_let_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    temp: IrTempId,
    value: IrExpr,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::TempLet {
        temp,
        value: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            value,
            binding,
            callable_name,
            captures,
        )),
    }
}

fn rewrite_assign_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    target: IrAssignTarget,
    value: IrExpr,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Assign {
        target: Box::new(rewrite_assign_target(
            ctx,
            origin,
            target,
            binding,
            callable_name,
            captures,
        )),
        value: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            value,
            binding,
            callable_name,
            captures,
        )),
    }
}

fn rewrite_index_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    indices: Box<[IrExpr]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Index {
        base: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            base,
            binding,
            callable_name,
            captures,
        )),
        indices: rewrite_expr_slice(ctx, origin, indices, binding, callable_name, captures),
    }
}

fn rewrite_dynamic_import_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    spec: IrExpr,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::DynamicImport {
        spec: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            spec,
            binding,
            callable_name,
            captures,
        )),
    }
}

fn rewrite_binary_kind(
    input: &RecursiveBindingInput<'_, '_>,
    op: IrBinaryOp,
    left: IrExpr,
    right: IrExpr,
) -> IrExprKind {
    IrExprKind::Binary {
        op,
        left: Box::new(rewrite_recursive_binding_refs(
            input.ctx,
            input.origin,
            left,
            input.binding,
            input.callable_name,
            input.captures,
        )),
        right: Box::new(rewrite_recursive_binding_refs(
            input.ctx,
            input.origin,
            right,
            input.binding,
            input.callable_name,
            input.captures,
        )),
    }
}

fn rewrite_not_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    expr: IrExpr,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Not {
        expr: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            expr,
            binding,
            callable_name,
            captures,
        )),
    }
}

fn rewrite_ty_test_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    ty_name: Box<str>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::TyTest {
        base: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            base,
            binding,
            callable_name,
            captures,
        )),
        ty_name,
    }
}

fn rewrite_ty_cast_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    ty_name: Box<str>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::TyCast {
        base: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            base,
            binding,
            callable_name,
            captures,
        )),
        ty_name,
    }
}

fn rewrite_case_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    scrutinee: IrExpr,
    arms: Box<[IrLoweredCaseArm]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Case {
        scrutinee: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            scrutinee,
            binding,
            callable_name,
            captures,
        )),
        arms: rewrite_case_arms(ctx, origin, arms, binding, callable_name, captures),
    }
}

fn rewrite_variant_kind(
    input: &RecursiveBindingInput<'_, '_>,
    data_key: DefinitionKey,
    tag_index: u16,
    field_count: u16,
    args: Box<[IrExpr]>,
) -> IrExprKind {
    IrExprKind::VariantNew {
        data_key,
        tag_index,
        field_count,
        args: rewrite_expr_slice(
            input.ctx,
            input.origin,
            args,
            input.binding,
            input.callable_name,
            input.captures,
        ),
    }
}

fn rewrite_call_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    callee: IrExpr,
    args: Box<[IrArg]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Call {
        callee: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            callee,
            binding,
            callable_name,
            captures,
        )),
        args: rewrite_call_args(ctx, origin, args, binding, callable_name, captures),
    }
}

fn rewrite_call_seq_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    callee: IrExpr,
    args: Box<[IrSeqPart]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::CallSeq {
        callee: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            callee,
            binding,
            callable_name,
            captures,
        )),
        args: rewrite_seq_parts(ctx, origin, args, binding, callable_name, captures),
    }
}

fn rewrite_perform_kind(
    input: &RecursiveBindingInput<'_, '_>,
    effect_key: DefinitionKey,
    op_index: u16,
    args: Box<[IrExpr]>,
) -> IrExprKind {
    IrExprKind::Perform {
        effect_key,
        op_index,
        args: rewrite_expr_slice(
            input.ctx,
            input.origin,
            args,
            input.binding,
            input.callable_name,
            input.captures,
        ),
    }
}

fn rewrite_perform_seq_kind(
    input: &RecursiveBindingInput<'_, '_>,
    effect_key: DefinitionKey,
    op_index: u16,
    args: Box<[IrSeqPart]>,
) -> IrExprKind {
    IrExprKind::PerformSeq {
        effect_key,
        op_index,
        args: rewrite_seq_parts(
            input.ctx,
            input.origin,
            args,
            input.binding,
            input.callable_name,
            input.captures,
        ),
    }
}

fn rewrite_handle_kind(
    input: &RecursiveBindingInput<'_, '_>,
    effect_key: DefinitionKey,
    value: IrExpr,
    ops: Box<[IrHandleOp]>,
    body: IrExpr,
) -> IrExprKind {
    IrExprKind::Handle {
        effect_key,
        value: Box::new(rewrite_recursive_binding_refs(
            input.ctx,
            input.origin,
            value,
            input.binding,
            input.callable_name,
            input.captures,
        )),
        ops: rewrite_handle_ops(
            input.ctx,
            input.origin,
            ops,
            input.binding,
            input.callable_name,
            input.captures,
        ),
        body: Box::new(rewrite_recursive_binding_refs(
            input.ctx,
            input.origin,
            body,
            input.binding,
            input.callable_name,
            input.captures,
        )),
    }
}

fn rewrite_resume_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    expr: Option<IrExpr>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Resume {
        expr: expr.map(|expr| {
            Box::new(rewrite_recursive_binding_refs(
                ctx,
                origin,
                expr,
                binding,
                callable_name,
                captures,
            ))
        }),
    }
}

fn rewrite_record_fields(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    fields: Box<[IrRecordField]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrRecordField]> {
    fields
        .into_vec()
        .into_iter()
        .map(|field| IrRecordField {
            expr: rewrite_recursive_binding_refs(
                ctx,
                origin,
                field.expr,
                binding,
                callable_name,
                captures,
            ),
            ..field
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn rewrite_call_args(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    args: Box<[IrArg]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrArg]> {
    args.into_vec()
        .into_iter()
        .map(|arg| IrArg {
            expr: rewrite_recursive_binding_refs(
                ctx,
                origin,
                arg.expr,
                binding,
                callable_name,
                captures,
            ),
            ..arg
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn rewrite_case_arms(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    arms: Box<[IrLoweredCaseArm]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrLoweredCaseArm]> {
    arms.into_vec()
        .into_iter()
        .map(|arm| IrLoweredCaseArm {
            guard: arm.guard.map(|guard| {
                rewrite_recursive_binding_refs(ctx, origin, guard, binding, callable_name, captures)
            }),
            expr: rewrite_recursive_binding_refs(
                ctx,
                origin,
                arm.expr,
                binding,
                callable_name,
                captures,
            ),
            ..arm
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn rewrite_handle_ops(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    ops: Box<[IrHandleOp]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrHandleOp]> {
    ops.into_vec()
        .into_iter()
        .map(|op| IrHandleOp {
            closure: rewrite_recursive_binding_refs(
                ctx,
                origin,
                op.closure,
                binding,
                callable_name,
                captures,
            ),
            ..op
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn rewrite_expr_slice(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    exprs: Box<[IrExpr]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrExpr]> {
    exprs
        .into_vec()
        .into_iter()
        .map(|expr| {
            rewrite_recursive_binding_refs(ctx, origin, expr, binding, callable_name, captures)
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn rewrite_assign_target(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    target: IrAssignTarget,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrAssignTarget {
    match target {
        IrAssignTarget::Binding {
            binding,
            name,
            module_target,
        } => IrAssignTarget::Binding {
            binding,
            name,
            module_target,
        },
        IrAssignTarget::Index { base, indices } => IrAssignTarget::Index {
            base: Box::new(rewrite_recursive_binding_refs(
                ctx,
                origin,
                *base,
                binding,
                callable_name,
                captures,
            )),
            indices: rewrite_expr_slice(ctx, origin, indices, binding, callable_name, captures),
        },
        IrAssignTarget::RecordField { base, index } => IrAssignTarget::RecordField {
            base: Box::new(rewrite_recursive_binding_refs(
                ctx,
                origin,
                *base,
                binding,
                callable_name,
                captures,
            )),
            index,
        },
    }
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
        IrCasePattern::Record { fields } => {
            for field in fields {
                collect_pattern_bindings(&field.pat, out);
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

fn lower_capture_params(ctx: &LowerCtx<'_>, captures: &[NameBindingId]) -> Vec<IrParam> {
    captures
        .iter()
        .copied()
        .map(|binding| IrParam {
            binding,
            name: binding_name(ctx, binding),
        })
        .collect()
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

fn lower_capture_exprs(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    captures: &[NameBindingId],
) -> Box<[IrExpr]> {
    captures
        .iter()
        .copied()
        .map(|binding| name_expr(ctx, origin, binding))
        .collect::<Vec<_>>()
        .into_boxed_slice()
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

fn lower_call_expr(
    ctx: &mut LowerCtx<'_>,
    callee: HirExprId,
    args: &SliceRange<HirArg>,
) -> IrExprKind {
    call::lower_call_expr(ctx, callee, args)
}

fn lower_index_expr(
    ctx: &mut LowerCtx<'_>,
    base: HirExprId,
    args: SliceRange<HirExprId>,
) -> IrExprKind {
    let sema = ctx.sema;
    let indices = sema.module().store.expr_ids.get(args);
    if indices.is_empty() {
        invalid_lowering_path("index without argument");
    }
    IrExprKind::Index {
        base: lower_boxed_expr(ctx, base),
        indices: indices
            .iter()
            .copied()
            .map(|index| lower_expr(ctx, index))
            .collect::<Vec<_>>()
            .into_boxed_slice(),
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

    let base_ty = sema
        .try_expr_ty(base)
        .expect("expr type missing for field base");
    let record_ty = match sema.ty(base_ty).kind {
        HirTyKind::Mut { inner } => inner,
        _ => base_ty,
    };
    if matches!(sema.ty(record_ty).kind, HirTyKind::Record { .. }) {
        let Some((indices, _layout, _field_count)) =
            record_layout_for_ty(sema, record_ty, interner)
        else {
            invalid_lowering_path("record field access without record layout");
        };
        let Some(index) = indices.get(&symbol).copied() else {
            invalid_lowering_path("record field access missing field");
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
            || invalid_lowering_path(format!("{kind:?}")),
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

fn lower_case_expr(
    ctx: &mut LowerCtx<'_>,
    scrutinee: HirExprId,
    arms: &SliceRange<HirCaseArm>,
) -> IrExprKind {
    let sema = ctx.sema;
    let mut lowered = Vec::<IrLoweredCaseArm>::new();
    for arm in sema.module().store.case_arms.get(arms.clone()) {
        lowered.extend(lower_case_arm(ctx, arm));
    }
    if lowered.is_empty() {
        invalid_lowering_path("case without emit-compatible arms");
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
        HirPatKind::Record { fields } => lower_record_case_patterns(sema, pat, fields, interner),
        HirPatKind::Variant { tag, args } => {
            lower_variant_patterns(sema, pat, *tag, *args, interner)
        }
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
        HirPatKind::Error => Vec::new(),
    }
}

fn lower_record_case_patterns(
    sema: &SemaModule,
    pat: HirPatId,
    fields: &SliceRange<HirRecordPatField>,
    interner: &Interner,
) -> Vec<IrCasePattern> {
    let Some((indices, _layout, _field_count)) = record_layout_for_ty(
        sema,
        sema.try_pat_ty(pat)
            .expect("pattern type missing for record case"),
        interner,
    ) else {
        return Vec::new();
    };
    let mut acc = vec![Vec::<IrCaseRecordField>::new()];
    for field in sema.module().store.record_pat_fields.get(fields.clone()) {
        let Some(index) = indices.get(&field.name.name).copied() else {
            return Vec::new();
        };
        let nested = field.value.map_or_else(
            || {
                decl_binding_id(sema, field.name)
                    .map(|binding| {
                        vec![IrCasePattern::Bind {
                            binding,
                            name: interner.resolve(field.name.name).into(),
                        }]
                    })
                    .unwrap_or_default()
            },
            |value| lower_case_patterns(sema, value, interner),
        );
        if nested.is_empty() {
            return Vec::new();
        }
        let mut next = Vec::<Vec<IrCaseRecordField>>::new();
        for prefix in &acc {
            for pat in &nested {
                let mut updated = prefix.clone();
                updated.push(IrCaseRecordField {
                    index,
                    pat: Box::new(pat.clone()),
                });
                next.push(updated);
            }
        }
        acc = next;
    }
    acc.into_iter()
        .map(|fields| IrCasePattern::Record {
            fields: fields.into_boxed_slice(),
        })
        .collect()
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
    let ty = sema
        .try_pat_ty(pat)
        .expect("pattern type missing for variant case");
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
    let Some(tag_index) = data.variant_index(tag_name) else {
        return Vec::new();
    };
    let Some(variant_count) = u16::try_from(data.variant_count()).ok() else {
        return Vec::new();
    };

    lower_product_patterns(
        sema,
        sema.module().store.pat_ids.get(args),
        interner,
        |items| IrCasePattern::Variant {
            data_key: data.key().clone(),
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

fn lower_binary_expr(
    ctx: &mut LowerCtx<'_>,
    op: &HirBinaryOp,
    left: HirExprId,
    right: HirExprId,
) -> IrExprKind {
    let interner = ctx.interner;
    if matches!(op, HirBinaryOp::Assign) {
        return lower_assign_expr(ctx, left, right);
    }
    IrExprKind::Binary {
        op: lower_binary_op(ctx, op, left, right, interner),
        left: lower_boxed_expr(ctx, left),
        right: lower_boxed_expr(ctx, right),
    }
}

fn lower_expr_list(ctx: &mut LowerCtx<'_>, exprs: SliceRange<HirExprId>) -> Box<[IrExpr]> {
    let sema = ctx.sema;
    sema.module()
        .store
        .expr_ids
        .get(exprs)
        .iter()
        .copied()
        .map(|expr| lower_expr(ctx, expr))
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn lower_binary_op(
    ctx: &LowerCtx<'_>,
    op: &HirBinaryOp,
    left: HirExprId,
    right: HirExprId,
    interner: &Interner,
) -> IrBinaryOp {
    let sema = ctx.sema;
    let left_ty = sema.ty(sema
        .try_expr_ty(left)
        .expect("expr type missing for binary left"));
    let right_ty = sema.ty(sema
        .try_expr_ty(right)
        .expect("expr type missing for binary right"));
    let wants_float =
        matches!(left_ty.kind, HirTyKind::Float) || matches!(right_ty.kind, HirTyKind::Float);
    let wants_string =
        matches!(left_ty.kind, HirTyKind::String) || matches!(right_ty.kind, HirTyKind::String);
    match op {
        HirBinaryOp::Add => {
            if wants_string {
                IrBinaryOp::StrCat
            } else if wants_float {
                IrBinaryOp::FAdd
            } else {
                IrBinaryOp::IAdd
            }
        }
        HirBinaryOp::Sub => {
            if wants_float {
                IrBinaryOp::FSub
            } else {
                IrBinaryOp::ISub
            }
        }
        HirBinaryOp::Mul => {
            if wants_float {
                IrBinaryOp::FMul
            } else {
                IrBinaryOp::IMul
            }
        }
        HirBinaryOp::Div => {
            if wants_float {
                IrBinaryOp::FDiv
            } else {
                IrBinaryOp::IDiv
            }
        }
        HirBinaryOp::Rem => {
            if wants_float {
                IrBinaryOp::FRem
            } else {
                IrBinaryOp::IRem
            }
        }
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

fn lower_foreign_let(
    sema: &SemaModule,
    interner: &Interner,
    expr_id: HirExprId,
    name: Ident,
    params: SliceRange<HirParam>,
    exported: bool,
) -> IrForeignDef {
    let expr = sema.module().store.exprs.get(expr_id);
    debug_assert!(expr.mods.foreign.is_some());

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
    IrForeignDef {
        binding,
        symbol,
        name: name_text,
        abi: abi.into(),
        param_tys,
        result_ty,
        link,
        exported,
    }
}

fn foreign_signature_tys(
    sema: &SemaModule,
    interner: &Interner,
    binding: Option<NameBindingId>,
    expr_id: HirExprId,
    _params: SliceRange<HirParam>,
) -> (Box<[Box<str>]>, Box<str>) {
    if let Some(binding) = binding
        && let Some(ty) = sema.binding_type(binding)
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
        return (param_tys, render_ty_name(sema, *ret, interner));
    }

    let _ = expr_id;
    invalid_lowering_path("foreign signature type is missing")
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
