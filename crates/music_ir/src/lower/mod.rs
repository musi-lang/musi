use std::any::Any;
use std::collections::{BTreeMap, HashMap, HashSet};
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
use music_sema::{ConstraintEvidence, ConstraintKey, DefinitionKey, ExportedValue, SemaModule};

use crate::IrDiagKind;
use crate::api::{
    IrArg, IrAssignTarget, IrBinaryOp, IrCallable, IrCaseArm as IrLoweredCaseArm, IrCasePattern,
    IrCaseRecordField, IrClassDef, IrDataDef, IrDataVariantDef, IrDiagList, IrEffectDef,
    IrEffectOpDef, IrExpr, IrExprKind, IrForeignDef, IrGlobal, IrHandleOp, IrInstanceDef, IrLit,
    IrModule, IrNameRef, IrOrigin, IrParam, IrRangeEndBound, IrRecordField, IrRecordLayoutField,
    IrSeqPart, IrTempId,
};

mod array;
mod assign;
mod bindings;
mod call;
mod closures;
mod collect;
mod destructure;
mod effects;
mod foreign;
mod meta;
mod patterns;
mod record;
mod rewrite;
mod toplevel;
mod validate;

use closures::{
    ClosureCallableInput, lower_closure_callable, lower_lambda_expr, lower_local_callable_let,
    lower_named_params,
};
use effects::{lower_handle_expr, lower_handler_literal_expr, lower_perform_expr};
use foreign::lower_foreign_let;
use patterns::{collect_pattern_bindings, lower_case_expr};
use rewrite::rewrite_recursive_binding_refs;

#[derive(Default)]
struct TopLevelItems {
    exports: Vec<ExportedValue>,
    callables: Vec<IrCallable>,
    globals: Vec<IrGlobal>,
    data_defs: Vec<IrDataDef>,
    foreigns: Vec<IrForeignDef>,
}

struct LetItemInput {
    expr_id: HirExprId,
    pat: HirPatId,
    params: HirParamRange,
    value: HirExprId,
    is_callable: bool,
    exported: bool,
}

type RecordLayout = (BTreeMap<Box<str>, u16>, Box<[IrRecordLayoutField]>, u16);
type HirParamRange = SliceRange<HirParam>;
type HirRecordItemRange = SliceRange<HirRecordItem>;
type BoundNameSet = HashSet<NameBindingId>;
type LoweredCaseArmList = Box<[IrLoweredCaseArm]>;
type EvidenceBindingMap = HashMap<ConstraintKey, Box<str>>;
type EvidenceBindingStack = Vec<EvidenceBindingMap>;

struct LowerCtx<'a> {
    sema: &'a SemaModule,
    interner: &'a Interner,
    module_key: ModuleKey,
    module_level_bindings: BoundNameSet,
    next_lambda_id: u32,
    next_temp_id: u32,
    extra_callables: Vec<IrCallable>,
    evidence_bindings: EvidenceBindingStack,
}

type LoweringResult<T = IrExprKind> = Result<T, Box<str>>;

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
            let detail = payload.downcast_ref::<LoweringInvariant>().map_or_else(
                || panic_payload_text(payload.as_ref()),
                |invariant| invariant.description.clone(),
            );
            Err(vec![
                Diag::error(IrDiagKind::LoweringInvariantViolated.message())
                    .with_code(IrDiagKind::LoweringInvariantViolated.code())
                    .with_note(format!("detail `{detail}`")),
            ])
        }
    }
}

fn panic_payload_text(payload: &(dyn Any + Send)) -> Box<str> {
    if let Some(text) = payload.downcast_ref::<String>() {
        return text.clone().into_boxed_str();
    }
    if let Some(text) = payload.downcast_ref::<&'static str>() {
        return (*text).into();
    }
    "<non-string panic payload>".into()
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
        evidence_bindings: Vec::new(),
    };

    let mut items = TopLevelItems::default();
    toplevel::collect_top_level_items(&mut ctx, sema.module().root, false, &mut items);
    toplevel::append_builtin_rangeable_items(&ctx, &mut items);
    items.callables.extend(ctx.extra_callables);
    append_synthesized_sum_data_defs(sema, interner, &mut items);
    let meta = meta::collect_meta(sema);
    let surface = sema.surface();

    Ok(IrModule::new(
        sema.resolved().module_key.clone(),
        surface.static_imports().to_vec().into_boxed_slice(),
        surface.types().to_vec().into_boxed_slice(),
        (
            surface
                .exported_values()
                .iter()
                .cloned()
                .chain(items.exports)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
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

fn append_synthesized_sum_data_defs(
    sema: &SemaModule,
    interner: &Interner,
    items: &mut TopLevelItems,
) {
    let mut seen_data_keys = HashSet::<DefinitionKey>::new();
    for data_def in &items.data_defs {
        let _ = seen_data_keys.insert(data_def.key.clone());
    }
    for data in sema.data_defs() {
        if seen_data_keys.contains(data.key()) {
            continue;
        }
        let variants = data
            .variants()
            .map(|(name, variant)| {
                IrDataVariantDef::new(
                    name,
                    variant
                        .field_tys()
                        .iter()
                        .copied()
                        .map(|ty| render_ty_name(sema, ty, interner))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                )
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let mut data_def = IrDataDef::new(data.key().clone(), variants);
        debug_assert_eq!(
            data_def.variant_count,
            u32::try_from(data.variant_count()).unwrap_or(u32::MAX)
        );
        if let Some(repr_kind) = data.repr_kind() {
            data_def = data_def.with_repr_kind(repr_kind);
        }
        if let Some(layout_align) = data.layout_align() {
            data_def = data_def.with_layout_align(layout_align);
        }
        if let Some(layout_pack) = data.layout_pack() {
            data_def = data_def.with_layout_pack(layout_pack);
        }
        items.data_defs.push(data_def);
    }
}

fn build_effect_defs(sema: &SemaModule, interner: &Interner) -> Box<[IrEffectDef]> {
    let mut seen = BTreeMap::<DefinitionKey, IrEffectDef>::new();
    for effect in sema.effect_defs() {
        let _ = seen.entry(effect.key().clone()).or_insert_with(|| {
            IrEffectDef::new(
                effect.key().clone(),
                effect
                    .ops()
                    .map(|(name, def)| {
                        IrEffectOpDef::new(
                            name,
                            def.params()
                                .iter()
                                .copied()
                                .map(|ty| render_ty_name(sema, ty, interner))
                                .collect::<Vec<_>>()
                                .into_boxed_slice(),
                            render_ty_name(sema, def.result(), interner),
                        )
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            )
        });
    }
    seen.into_values().collect::<Vec<_>>().into_boxed_slice()
}

fn lower_expr(ctx: &mut LowerCtx<'_>, expr_id: HirExprId) -> IrExpr {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let expr = sema.module().store.exprs.get(expr_id);
    let origin = IrOrigin::new(expr.origin.source_id, expr.origin.span);
    if is_type_value_expr(sema, expr_id, interner) {
        return IrExpr::new(
            origin,
            IrExprKind::TypeValue {
                ty_name: render_type_value_expr_name(sema, expr_id, interner),
            },
        );
    }
    let kind = match &expr.kind {
        HirExprKind::Name { name } => lower_name_expr(sema, expr_id, *name, interner),
        HirExprKind::Lit { lit } => lower_lit_expr(sema, *lit),
        HirExprKind::Sequence { exprs } => lower_sequence_expr(ctx, *exprs),
        HirExprKind::Tuple { items } => lower_tuple_expr(ctx, expr_id, *items),
        HirExprKind::Array { items } => lower_array_expr(ctx, expr_id, items.clone())
            .unwrap_or_else(|description| invalid_lowering_path(description)),
        HirExprKind::Template { parts } => lower_template_expr(ctx, origin, parts.clone()),
        HirExprKind::Record { items } => lower_record_expr(ctx, expr_id, items.clone())
            .unwrap_or_else(|description| invalid_lowering_path(description)),
        HirExprKind::Let {
            mods,
            pat,
            has_param_clause,
            params,
            value,
            ..
        } => lower_let_expr(ctx, origin, *mods, *pat, *has_param_clause, params, *value),
        HirExprKind::Binary { op, left, right } => {
            lower_binary_expr(ctx, expr_id, op, *left, *right)
        }
        HirExprKind::Call { callee, args } => lower_call_expr(ctx, *callee, args)
            .unwrap_or_else(|description| invalid_lowering_path(description)),
        HirExprKind::Apply { callee, .. } => {
            let lowered = lower_expr_with_origin(ctx, *callee, origin);
            return bind_expr_evidence(ctx, expr_id, origin, lowered);
        }
        HirExprKind::Prefix { op, expr } => lower_prefix_expr(ctx, expr_id, op, *expr, origin),
        HirExprKind::Index { base, args } => lower_index_expr(ctx, *base, *args),
        HirExprKind::Field { base, name, .. } => {
            lower_field_expr(ctx, expr_id, *base, name.name, &expr.kind)
        }
        HirExprKind::RecordUpdate { base, items } => {
            lower_record_update_expr(ctx, expr_id, *base, items.clone())
                .unwrap_or_else(|description| invalid_lowering_path(description))
        }
        HirExprKind::Case { scrutinee, arms } => lower_case_expr(ctx, *scrutinee, arms),
        HirExprKind::Variant { tag, args } => lower_variant_expr(ctx, expr_id, *tag, *args),
        HirExprKind::Lambda { params, body, .. } => lower_lambda_expr(ctx, origin, params, *body),
        HirExprKind::Perform { expr } => lower_perform_expr(ctx, *expr)
            .unwrap_or_else(|description| invalid_lowering_path(description)),
        HirExprKind::HandlerLit { effect, clauses } => {
            lower_handler_literal_expr(ctx, expr_id, *effect, clauses.clone())
        }
        HirExprKind::Handle { expr, handler } => lower_handle_expr(ctx, expr_id, *expr, *handler),
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
                    .unwrap_or_else(|| invalid_lowering_path("expr type missing for type cast")),
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
    bind_expr_evidence(ctx, expr_id, origin, IrExpr::new(origin, kind))
}

fn is_type_value_expr(sema: &SemaModule, expr_id: HirExprId, interner: &Interner) -> bool {
    match &sema.module().store.exprs.get(expr_id).kind {
        HirExprKind::Error => true,
        HirExprKind::Name { name } => {
            if sema.expr_module_target(expr_id).is_some() {
                return false;
            }
            let symbol_text = interner.resolve(name.name);
            match use_binding_id(sema, *name) {
                None => true,
                Some(_) if is_builtin_type_name_symbol(symbol_text) => true,
                Some(_) => false,
            }
        }
        HirExprKind::Tuple { items } => sema
            .module()
            .store
            .expr_ids
            .get(*items)
            .iter()
            .copied()
            .all(|item| is_type_value_expr(sema, item, interner)),
        HirExprKind::ArrayTy { item, .. } => is_type_value_expr(sema, *item, interner),
        HirExprKind::Pi { binder_ty, ret, .. } => {
            is_type_value_expr(sema, *binder_ty, interner)
                && is_type_value_expr(sema, *ret, interner)
        }
        HirExprKind::Apply { callee, args } => {
            is_type_value_expr(sema, *callee, interner)
                && sema
                    .module()
                    .store
                    .expr_ids
                    .get(*args)
                    .iter()
                    .copied()
                    .all(|arg| type_apply_arg_expr(sema, arg, interner))
        }
        HirExprKind::Binary { op, left, right } => {
            matches!(
                op,
                HirBinaryOp::Add | HirBinaryOp::Arrow | HirBinaryOp::EffectArrow
            ) && is_type_value_expr(sema, *left, interner)
                && is_type_value_expr(sema, *right, interner)
        }
        HirExprKind::Prefix {
            op: HirPrefixOp::Mut,
            expr,
        } => is_type_value_expr(sema, *expr, interner),
        HirExprKind::Record { items } => sema
            .module()
            .store
            .record_items
            .get(items.clone())
            .iter()
            .all(|item| is_type_value_expr(sema, item.value, interner)),
        _ => false,
    }
}

fn type_apply_arg_expr(sema: &SemaModule, expr_id: HirExprId, interner: &Interner) -> bool {
    match &sema.module().store.exprs.get(expr_id).kind {
        HirExprKind::Lit { lit } => matches!(
            sema.module().store.lits.get(*lit).kind,
            HirLitKind::Int { .. }
        ),
        _ => is_type_value_expr(sema, expr_id, interner),
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
                .unwrap_or_else(|| invalid_lowering_path("expr type missing for prefix op"));
            let (zero, op) = match &ctx.sema.ty(ty).kind {
                HirTyKind::Float => (
                    IrExpr::new(origin, IrExprKind::Lit(IrLit::Float { raw: "0.0".into() })),
                    IrBinaryOp::FSub,
                ),
                HirTyKind::Int | HirTyKind::Nat | HirTyKind::NatLit(_) => (
                    IrExpr::new(origin, IrExprKind::Lit(IrLit::Int { raw: "0".into() })),
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
            HirTemplatePart::Text { value } => rendered.push(IrExpr::new(
                origin,
                IrExprKind::Lit(IrLit::String {
                    value: value.clone(),
                }),
            )),
            HirTemplatePart::Expr { expr } => rendered.push(lower_expr(ctx, *expr)),
        }
    }
    let mut iter = rendered.into_iter();
    let Some(mut acc) = iter.next() else {
        return IrExprKind::Lit(IrLit::String { value: "".into() });
    };
    for expr in iter {
        acc = IrExpr::new(
            origin,
            IrExprKind::Binary {
                op: IrBinaryOp::StrCat,
                left: Box::new(acc),
                right: Box::new(expr),
            },
        );
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
                .unwrap_or_else(|| invalid_lowering_path("expr type missing for tuple literal")),
            ctx.interner,
        ),
        items: lower_expr_list(ctx, items),
    }
}

fn lower_array_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    items: SliceRange<HirArrayItem>,
) -> LoweringResult {
    array::lower_array_expr(ctx, expr_id, items)
}

fn record_layout_for_ty(
    sema: &SemaModule,
    ty: HirTyId,
    interner: &Interner,
) -> Option<RecordLayout> {
    let items = match &sema.ty(ty).kind {
        HirTyKind::Record { fields } => {
            let mut items = sema
                .module()
                .store
                .ty_fields
                .get(fields.clone())
                .iter()
                .map(|field| Box::<str>::from(interner.resolve(field.name)))
                .collect::<Vec<_>>();
            items.sort();
            items
        }
        HirTyKind::Range { .. } => vec!["start".into(), "end".into(), "end_bound".into()],
        _ => return None,
    };

    let field_count = u16::try_from(items.len())
        .unwrap_or_else(|_| invalid_lowering_path("record field count exceeds u16 in lowering"));
    let mut indices = BTreeMap::<Box<str>, u16>::new();
    let layout = items
        .into_iter()
        .enumerate()
        .map(|(idx, name)| {
            let idx = u16::try_from(idx).unwrap_or_else(|_| {
                invalid_lowering_path("record field index exceeds u16 in lowering")
            });
            let _ = indices.insert(name.clone(), idx);
            IrRecordLayoutField::new(name, idx)
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    Some((indices, layout, field_count))
}

fn lower_record_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    items: HirRecordItemRange,
) -> LoweringResult {
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
        .unwrap_or_else(|| invalid_lowering_path("expr type missing for variant"));
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

fn lower_let_expr(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    mods: HirLetMods,
    pat: HirPatId,
    has_param_clause: bool,
    params: &HirParamRange,
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
        _ => destructure::lower_destructure_let(ctx, origin, pat, value)
            .unwrap_or_else(|description| invalid_lowering_path(description)),
    }
}

const fn fresh_temp(ctx: &mut LowerCtx<'_>) -> IrTempId {
    let raw = ctx.next_temp_id;
    ctx.next_temp_id = ctx.next_temp_id.saturating_add(1);
    IrTempId::from_raw(raw)
}

fn hidden_evidence_name(owner: &str, index: usize) -> Box<str> {
    format!("__ev::{owner}::{index}").into_boxed_str()
}

fn hidden_evidence_params_for_keys(
    owner: &str,
    keys: &[ConstraintKey],
) -> (Vec<IrParam>, EvidenceBindingMap) {
    let mut params = Vec::new();
    let mut bindings = HashMap::new();
    for (index, key) in keys.iter().cloned().enumerate() {
        let name = hidden_evidence_name(owner, index);
        let _ = bindings.insert(key, name.clone());
        params.push(IrParam::synthetic(name));
    }
    (params, bindings)
}

fn hidden_evidence_params_for_binding(
    sema: &SemaModule,
    owner: &str,
    binding: Option<NameBindingId>,
) -> (Vec<IrParam>, EvidenceBindingMap) {
    let keys = binding
        .and_then(|binding| sema.binding_evidence_keys(binding))
        .unwrap_or(&[]);
    hidden_evidence_params_for_keys(owner, keys)
}

fn push_evidence_bindings(ctx: &mut LowerCtx<'_>, bindings: EvidenceBindingMap) {
    ctx.evidence_bindings.push(bindings);
}

fn pop_evidence_bindings(ctx: &mut LowerCtx<'_>) {
    let _ = ctx.evidence_bindings.pop();
}

fn lower_evidence_expr(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    evidence: &ConstraintEvidence,
) -> IrExpr {
    match evidence {
        ConstraintEvidence::Param { key } => {
            let Some(name) = resolve_evidence_binding_name(ctx, key) else {
                invalid_lowering_path("missing evidence binding for constraint");
            };
            IrExpr::new(
                origin,
                IrExprKind::Name {
                    binding: None,
                    name,
                    module_target: None,
                },
            )
        }
        ConstraintEvidence::Provider { module, name, args } => IrExpr::new(
            origin,
            IrExprKind::Call {
                callee: Box::new(IrExpr::new(
                    origin,
                    IrExprKind::Name {
                        binding: None,
                        name: name.clone(),
                        module_target: Some(module.clone()),
                    },
                )),
                args: args
                    .iter()
                    .map(|arg| IrArg::new(false, lower_evidence_expr(ctx, origin, arg)))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
        ),
    }
}

fn resolve_evidence_binding_name(ctx: &LowerCtx<'_>, key: &ConstraintKey) -> Option<Box<str>> {
    ctx.evidence_bindings.iter().rev().find_map(|bindings| {
        bindings.get(key).cloned().or_else(|| {
            bindings.iter().find_map(|(candidate, name)| {
                evidence_keys_equiv(ctx, key, candidate).then(|| name.clone())
            })
        })
    })
}

fn evidence_keys_equiv(ctx: &LowerCtx<'_>, left: &ConstraintKey, right: &ConstraintKey) -> bool {
    left.kind == right.kind
        && left.class_key == right.class_key
        && render_ty_name(ctx.sema, left.subject, ctx.interner)
            == render_ty_name(ctx.sema, right.subject, ctx.interner)
        && render_ty_name(ctx.sema, left.value, ctx.interner)
            == render_ty_name(ctx.sema, right.value, ctx.interner)
}

fn bind_expr_evidence(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    origin: IrOrigin,
    lowered: IrExpr,
) -> IrExpr {
    let Some(evidence) = ctx.sema.expr_evidence(expr_id) else {
        return lowered;
    };
    if evidence.is_empty() {
        return lowered;
    }
    let IrExprKind::Name {
        binding,
        name,
        module_target,
    } = lowered.kind
    else {
        return lowered;
    };
    let is_callable = ctx
        .sema
        .try_expr_ty(expr_id)
        .is_some_and(|ty| matches!(ctx.sema.ty(ty).kind, HirTyKind::Arrow { .. }));
    if !is_callable {
        return IrExpr::new(
            origin,
            IrExprKind::Name {
                binding,
                name,
                module_target,
            },
        );
    }
    if module_target.is_none()
        && binding.is_some_and(|binding| !ctx.module_level_bindings.contains(&binding))
    {
        return IrExpr::new(
            origin,
            IrExprKind::Name {
                binding,
                name,
                module_target,
            },
        );
    }
    IrExpr::new(
        origin,
        IrExprKind::ClosureNew {
            callee: IrNameRef {
                binding,
                name,
                module_target,
            },
            captures: evidence
                .iter()
                .map(|item| lower_evidence_expr(ctx, origin, item))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        },
    )
}

fn lower_call_expr(
    ctx: &mut LowerCtx<'_>,
    callee: HirExprId,
    args: &SliceRange<HirArg>,
) -> LoweringResult {
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
        .unwrap_or_else(|| invalid_lowering_path("expr type missing for field base"));
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
        let Some(index) = indices.get(interner.resolve(symbol)).copied() else {
            invalid_lowering_path("record field access missing field");
        };
        return IrExprKind::RecordGet {
            base: Box::new(lower_expr(ctx, base)),
            index,
        };
    }

    let module_ty = match sema.ty(base_ty).kind {
        HirTyKind::Mut { inner } => inner,
        _ => base_ty,
    };
    if matches!(sema.ty(module_ty).kind, HirTyKind::Module) {
        if let Some(module_target) = sema
            .expr_module_target(expr_id)
            .cloned()
            .or_else(|| sema.expr_module_target(base).cloned())
        {
            return IrExprKind::Name {
                binding: None,
                name: interner.resolve(symbol).into(),
                module_target: Some(module_target),
            };
        }
        return IrExprKind::ModuleGet {
            base: Box::new(lower_expr(ctx, base)),
            name: interner.resolve(symbol).into(),
        };
    }

    invalid_lowering_path(format!("{kind:?}"))
}

fn render_type_value_expr_name(
    sema: &SemaModule,
    expr_id: HirExprId,
    interner: &Interner,
) -> Box<str> {
    match &sema.module().store.exprs.get(expr_id).kind {
        HirExprKind::Error => "Error".into(),
        HirExprKind::Name { name } => interner.resolve(name.name).into(),
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
        other => invalid_lowering_path(format!("invalid type value expr {other:?}")),
    }
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

fn is_builtin_type_name_symbol(text: &str) -> bool {
    matches!(
        text,
        "Type"
            | "Any"
            | "Unknown"
            | "Syntax"
            | "Empty"
            | "Unit"
            | "Bool"
            | "Nat"
            | "Int"
            | "Float"
            | "String"
            | "CString"
            | "CPtr"
            | "Module"
    )
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

fn lower_record_update_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    base: HirExprId,
    items: HirRecordItemRange,
) -> LoweringResult {
    record::lower_record_update_expr(ctx, expr_id, base, items)
}

fn lower_assign_expr(ctx: &mut LowerCtx<'_>, left: HirExprId, right: HirExprId) -> IrExprKind {
    assign::lower_assign_expr(ctx, left, right)
        .unwrap_or_else(|description| invalid_lowering_path(description))
}

fn lower_binary_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    op: &HirBinaryOp,
    left: HirExprId,
    right: HirExprId,
) -> IrExprKind {
    let interner = ctx.interner;
    if matches!(op, HirBinaryOp::Assign) {
        return lower_assign_expr(ctx, left, right);
    }
    if matches!(
        op,
        HirBinaryOp::RangeInclusive | HirBinaryOp::RangeExcludeEnd
    ) {
        return lower_range_expr(ctx, expr_id, op, left, right);
    }
    if matches!(op, HirBinaryOp::In) {
        return lower_in_expr(ctx, expr_id, left, right);
    }
    IrExprKind::Binary {
        op: lower_binary_op(ctx, op, left, right, interner),
        left: lower_boxed_expr(ctx, left),
        right: lower_boxed_expr(ctx, right),
    }
}

fn lower_range_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    op: &HirBinaryOp,
    left: HirExprId,
    right: HirExprId,
) -> IrExprKind {
    let ty = ctx
        .sema
        .try_expr_ty(expr_id)
        .unwrap_or_else(|| invalid_lowering_path("expr type missing for range"));
    let ty_name = render_ty_name(ctx.sema, ty, ctx.interner);
    let end_bound = match op {
        HirBinaryOp::RangeInclusive => IrRangeEndBound::Inclusive,
        HirBinaryOp::RangeExcludeEnd => IrRangeEndBound::Exclusive,
        _ => invalid_lowering_path("invalid range op"),
    };
    IrExprKind::Range {
        ty_name,
        start: lower_boxed_expr(ctx, left),
        end: lower_boxed_expr(ctx, right),
        end_bound,
    }
}

fn lower_in_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    left: HirExprId,
    right: HirExprId,
) -> IrExprKind {
    let origin = IrOrigin::new(
        ctx.sema.module().store.exprs.get(expr_id).origin.source_id,
        ctx.sema.module().store.exprs.get(expr_id).origin.span,
    );
    let evidence = ctx
        .sema
        .expr_evidence(expr_id)
        .and_then(|items| items.first())
        .map_or_else(
            || invalid_lowering_path("range membership evidence missing"),
            |item| lower_evidence_expr(ctx, origin, item),
        );
    IrExprKind::RangeContains {
        value: lower_boxed_expr(ctx, left),
        range: lower_boxed_expr(ctx, right),
        evidence: Box::new(evidence),
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
        .unwrap_or_else(|| invalid_lowering_path("expr type missing for binary left")));
    let right_ty = sema.ty(sema
        .try_expr_ty(right)
        .unwrap_or_else(|| invalid_lowering_path("expr type missing for binary right")));
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
        HirTyKind::Seq { item } => format!("[]{}", render_ty_name(sema, *item, interner)).into(),
        HirTyKind::Array { dims, item } => render_array_ty_name(sema, dims, *item, interner),
        HirTyKind::Range { item } => {
            format!("Range[{}]", render_ty_name(sema, *item, interner)).into()
        }
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
