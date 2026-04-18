use std::any::Any;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};

use music_arena::SliceRange;
use music_base::diag::Diag;
use music_hir::{
    HirArg, HirArrayItem, HirBinaryOp, HirDim, HirExprId, HirExprKind, HirHandleClause, HirLetMods,
    HirLitId, HirLitKind, HirMatchArm, HirParam, HirPartialRangeKind, HirPatId, HirPatKind,
    HirPrefixOp, HirQuoteKind, HirRecordItem, HirRecordPatField, HirSpliceKind, HirTemplatePart,
    HirTyField, HirTyId, HirTyKind,
};
use music_module::ModuleKey;
use music_names::{Ident, Interner, NameBindingId, NameSite, Symbol};
use music_sema::{
    ComptimeValue, ConstraintEvidence, ConstraintKey, DefinitionKey, ExportedValue, ExprMemberKind,
    SemaDataVariantDef, SemaModule,
};

use crate::IrDiagKind;
use crate::api::{
    IrArg, IrAssignTarget, IrBinaryOp, IrCallable, IrCasePattern, IrCaseRecordField, IrClassDef,
    IrDataDef, IrDataVariantDef, IrDiagList, IrEffectDef, IrEffectOpDef, IrExpr, IrExprKind,
    IrForeignDef, IrGlobal, IrHandleOp, IrInstanceDef, IrIntrinsicKind, IrLit,
    IrMatchArm as IrLoweredMatchArm, IrModule, IrModuleInitPart, IrNameRef, IrOrigin, IrParam,
    IrRangeKind, IrRecordField, IrRecordLayoutField, IrSeqPart, IrTempId,
};

mod array;
mod assign;
mod bindings;
mod call;
mod closures;
mod collect;
mod comptime;
mod destructure;
mod effects;
mod evidence;
mod foreign;
mod meta;
mod pats;
mod range;
mod record;
mod rewrite;
mod toplevel;
mod types;
mod validate;

use closures::{
    ClosureCallableInput, lower_closure_callable, lower_lambda_expr, lower_local_callable_let,
    lower_named_params,
};
use comptime::lower_comptime_value;
use effects::{lower_handle_expr, lower_handler_literal_expr, lower_request_expr};
use evidence::{
    bind_expr_evidence, hidden_evidence_params_for_binding, hidden_evidence_params_for_keys,
    lower_evidence_expr, pop_evidence_bindings, push_evidence_bindings,
};
use foreign::lower_foreign_let;
use pats::{collect_pattern_bindings, lower_match_expr};
use range::{lower_in_expr, lower_partial_range_expr, lower_range_expr};
use rewrite::rewrite_recursive_binding_refs;
use types::{
    is_builtin_type_name_symbol, render_named_ty_name, render_ty_name, render_type_value_expr_name,
};

#[derive(Default)]
struct TopLevelItems {
    exports: Vec<ExportedValue>,
    callables: Vec<IrCallable>,
    globals: Vec<IrGlobal>,
    init_parts: Vec<IrModuleInitPart>,
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
type LoweredMatchArmList = Box<[IrLoweredMatchArm]>;
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
    comptime_bindings: HashMap<NameBindingId, ComptimeValue>,
    specialized_callables: HashSet<Box<str>>,
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
        comptime_bindings: HashMap::new(),
        specialized_callables: HashSet::new(),
    };

    let mut items = TopLevelItems::default();
    toplevel::collect_top_level_items(&mut ctx, sema.module().root, false, &mut items);
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
            items.init_parts.into_boxed_slice(),
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
                    variant.tag(),
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
        if data.frozen() {
            data_def = data_def.with_frozen(true);
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
                        .with_comptime_safe(def.is_comptime_safe())
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
    if let HirExprKind::Apply { callee, args } = &expr.kind {
        let type_args = ctx
            .sema
            .module()
            .store
            .expr_ids
            .get(*args)
            .iter()
            .copied()
            .map(|arg| render_type_value_expr_name(ctx.sema, arg, ctx.interner))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let lowered = IrExpr::new(
            origin,
            IrExprKind::TypeApply {
                callee: Box::new(lower_expr(ctx, *callee)),
                type_args,
            },
        );
        return bind_expr_evidence(ctx, expr_id, origin, lowered);
    }
    if let HirExprKind::Unsafe { body } = &expr.kind {
        let lowered = lower_expr_with_origin(ctx, *body, origin);
        return bind_expr_evidence(ctx, expr_id, origin, lowered);
    }
    let kind = match &expr.kind {
        HirExprKind::Name { .. }
        | HirExprKind::Lit { .. }
        | HirExprKind::Sequence { .. }
        | HirExprKind::Tuple { .. }
        | HirExprKind::Array { .. }
        | HirExprKind::Template { .. }
        | HirExprKind::Record { .. } => lower_value_expr(ctx, expr_id, origin, &expr.kind),
        HirExprKind::Let { .. }
        | HirExprKind::Binary { .. }
        | HirExprKind::PartialRange { .. }
        | HirExprKind::Call { .. }
        | HirExprKind::Prefix { .. }
        | HirExprKind::Index { .. }
        | HirExprKind::Field { .. }
        | HirExprKind::RecordUpdate { .. } => {
            lower_operation_expr(ctx, expr_id, origin, &expr.kind)
        }
        HirExprKind::Match { .. }
        | HirExprKind::Variant { .. }
        | HirExprKind::Lambda { .. }
        | HirExprKind::Request { .. }
        | HirExprKind::HandlerLit { .. }
        | HirExprKind::Handle { .. } => lower_control_expr(ctx, expr_id, origin, &expr.kind),
        HirExprKind::TypeTest { .. }
        | HirExprKind::TypeCast { .. }
        | HirExprKind::Resume { .. }
        | HirExprKind::Import { .. }
        | HirExprKind::Quote { .. }
        | HirExprKind::Splice { .. } => lower_misc_expr(ctx, expr_id, &expr.kind),
        other => invalid_lowering_path(format!("missing IR lowering for {other:?}")),
    };
    bind_expr_evidence(ctx, expr_id, origin, IrExpr::new(origin, kind))
}

fn lower_value_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    origin: IrOrigin,
    kind: &HirExprKind,
) -> IrExprKind {
    match kind {
        HirExprKind::Name { name } => lower_name_expr(ctx, expr_id, *name),
        HirExprKind::Lit { lit } => lower_lit_expr(ctx.sema, *lit),
        HirExprKind::Sequence { exprs } => lower_sequence_expr(ctx, *exprs),
        HirExprKind::Tuple { items } => lower_tuple_expr(ctx, expr_id, *items),
        HirExprKind::Array { items } => lower_array_expr(ctx, expr_id, items.clone())
            .unwrap_or_else(|description| invalid_lowering_path(description)),
        HirExprKind::Template { parts } => lower_template_expr(ctx, origin, parts.clone()),
        HirExprKind::Record { items } => lower_record_expr(ctx, expr_id, items.clone())
            .unwrap_or_else(|description| invalid_lowering_path(description)),
        _ => invalid_lowering_path("value expr dispatcher mismatch"),
    }
}

fn lower_operation_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    origin: IrOrigin,
    kind: &HirExprKind,
) -> IrExprKind {
    match kind {
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
        HirExprKind::PartialRange { kind, expr } => {
            lower_partial_range_expr(ctx, expr_id, *kind, *expr)
        }
        HirExprKind::Call { callee, args } => lower_call_expr(ctx, *callee, args)
            .unwrap_or_else(|description| invalid_lowering_path(description)),
        HirExprKind::Prefix { op, expr } => lower_prefix_expr(ctx, expr_id, op, *expr, origin),
        HirExprKind::Index { base, args } => lower_index_expr(ctx, *base, *args),
        HirExprKind::Field { base, name, .. } => {
            lower_field_expr(ctx, expr_id, *base, name.name, kind)
        }
        HirExprKind::RecordUpdate { base, items } => {
            lower_record_update_expr(ctx, expr_id, *base, items.clone())
                .unwrap_or_else(|description| invalid_lowering_path(description))
        }
        _ => invalid_lowering_path("operation expr dispatcher mismatch"),
    }
}

fn lower_control_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    origin: IrOrigin,
    kind: &HirExprKind,
) -> IrExprKind {
    match kind {
        HirExprKind::Match { scrutinee, arms } => lower_match_expr(ctx, *scrutinee, arms),
        HirExprKind::Variant { tag, args } => lower_variant_expr(ctx, expr_id, *tag, args.clone()),
        HirExprKind::Lambda { params, body, .. } => lower_lambda_expr(ctx, origin, params, *body),
        HirExprKind::Request { expr } => lower_request_expr(ctx, *expr)
            .unwrap_or_else(|description| invalid_lowering_path(description)),
        HirExprKind::HandlerLit { effect, clauses } => {
            lower_handler_literal_expr(ctx, expr_id, *effect, clauses.clone())
        }
        HirExprKind::Handle { expr, handler } => lower_handle_expr(ctx, expr_id, *expr, *handler),
        _ => invalid_lowering_path("control expr dispatcher mismatch"),
    }
}

fn lower_misc_expr(ctx: &mut LowerCtx<'_>, expr_id: HirExprId, kind: &HirExprKind) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;
    match kind {
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
                IrExprKind::ModuleLoad {
                    spec: lower_boxed_expr(ctx, *arg),
                }
            }
        }
        HirExprKind::Quote { kind } => lower_quote_expr(kind),
        HirExprKind::Splice { kind } => lower_splice_expr(kind),
        _ => invalid_lowering_path("misc expr dispatcher mismatch"),
    }
}

fn is_type_value_expr(sema: &SemaModule, expr_id: HirExprId, interner: &Interner) -> bool {
    match &sema.module().store.exprs.get(expr_id).kind {
        HirExprKind::Error => true,
        HirExprKind::Name { name } => {
            if matches!(
                sema.ty(sema
                    .try_expr_ty(expr_id)
                    .unwrap_or_else(|| invalid_lowering_path("expr type missing for name ref")))
                    .kind,
                HirTyKind::Type
            ) {
                return true;
            }
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
            op: HirPrefixOp::Mut | HirPrefixOp::Comptime | HirPrefixOp::Any | HirPrefixOp::Some,
            expr,
        } => is_type_value_expr(sema, *expr, interner),
        HirExprKind::Record { items } => sema
            .module()
            .store
            .record_items
            .get(items.clone())
            .iter()
            .all(|item| is_type_value_expr(sema, item.value, interner)),
        HirExprKind::Field { .. } => matches!(
            sema.ty(sema
                .try_expr_ty(expr_id)
                .unwrap_or_else(|| invalid_lowering_path("expr type missing for field ref")))
                .kind,
            HirTyKind::Type
        ),
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
        HirPrefixOp::Comptime => lower_comptime_prefix_expr(ctx, expr_id, expr, origin),
        HirPrefixOp::Mut | HirPrefixOp::Any | HirPrefixOp::Some => {
            lower_expr_with_origin(ctx, expr, origin).kind
        }
        HirPrefixOp::Not => IrExprKind::Not {
            expr: lower_boxed_expr(ctx, expr),
        },
        HirPrefixOp::Neg => {
            let ty = ctx
                .sema
                .try_expr_ty(expr_id)
                .unwrap_or_else(|| invalid_lowering_path("expr type missing for prefix op"));
            let (zero, op) = match &ctx.sema.ty(ty).kind {
                HirTyKind::Float | HirTyKind::Float32 | HirTyKind::Float64 => (
                    IrExpr::new(origin, IrExprKind::Lit(IrLit::Float { raw: "0.0".into() })),
                    IrBinaryOp::FSub,
                ),
                HirTyKind::Int
                | HirTyKind::Nat
                | HirTyKind::Int8
                | HirTyKind::Int16
                | HirTyKind::Int32
                | HirTyKind::Int64
                | HirTyKind::Nat8
                | HirTyKind::Nat16
                | HirTyKind::Nat32
                | HirTyKind::Nat64
                | HirTyKind::NatLit(_) => (
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

fn lower_comptime_prefix_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    expr: HirExprId,
    origin: IrOrigin,
) -> IrExprKind {
    if let Some(value) = ctx.sema.expr_comptime_value(expr_id) {
        if let ComptimeValue::Syntax(term) = value
            && matches!(term.shape(), music_term::SyntaxShape::Expr)
            && let HirExprKind::Quote {
                kind: HirQuoteKind::Expr { expr, .. },
            } = ctx.sema.module().store.exprs.get(expr).kind
        {
            return lower_expr_with_origin(ctx, expr, origin).kind;
        }
        return lower_comptime_value(ctx, value);
    }
    lower_expr_with_origin(ctx, expr, origin).kind
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

fn lower_name_expr(ctx: &mut LowerCtx<'_>, expr_id: HirExprId, ident: Ident) -> IrExprKind {
    let sema = ctx.sema;
    if let Some(binding) = use_binding_id(sema, ident)
        && let Some(value) = ctx.comptime_bindings.get(&binding).cloned()
    {
        return lower_comptime_value(ctx, &value);
    }
    IrExprKind::Name {
        binding: use_binding_id(sema, ident),
        name: ctx.interner.resolve(ident.name).into(),
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
        HirTyKind::Range { .. } | HirTyKind::ClosedRange { .. } => {
            vec!["lowerBound".into(), "upperBound".into()]
        }
        HirTyKind::PartialRangeFrom { .. } => {
            vec!["__upperBound".into(), "lowerBound".into()]
        }
        HirTyKind::PartialRangeUpTo { .. } | HirTyKind::PartialRangeThru { .. } => {
            vec!["__lowerBound".into(), "upperBound".into()]
        }
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
    args: SliceRange<HirArg>,
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

    let Some(variant) = data.variant(tag_name) else {
        invalid_lowering_path("unknown variant payload metadata");
    };
    let arg_exprs = ordered_variant_arg_exprs(sema, variant, args, interner);
    let lowered_args = arg_exprs
        .into_iter()
        .map(|expr| lower_expr(ctx, expr))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let field_count = u16::try_from(lowered_args.len()).unwrap_or(u16::MAX);
    let _ = variant_count;
    IrExprKind::VariantNew {
        data_key: data.key().clone(),
        tag_index,
        tag_value: variant.tag(),
        field_count,
        args: lowered_args,
    }
}

fn ordered_variant_arg_exprs(
    sema: &SemaModule,
    variant: &SemaDataVariantDef,
    args: SliceRange<HirArg>,
    interner: &Interner,
) -> Vec<HirExprId> {
    let arg_nodes = sema.module().store.args.get(args);
    if !variant.field_names().iter().any(Option::is_some) {
        return arg_nodes.iter().map(|arg| arg.expr).collect();
    }
    let mut ordered = vec![None; variant.field_names().len()];
    for arg in arg_nodes {
        let Some(name) = arg.name else {
            invalid_lowering_path("named variant arg missing field name after sema");
        };
        let name = interner.resolve(name.name);
        let Some(index) = variant
            .field_names()
            .iter()
            .position(|field| field.as_deref() == Some(name))
        else {
            invalid_lowering_path("unknown named variant field after sema");
        };
        ordered[index] = Some(arg.expr);
    }
    ordered
        .into_iter()
        .map(|expr| {
            expr.unwrap_or_else(|| invalid_lowering_path("missing named variant field after sema"))
        })
        .collect()
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

    if let Some(fact) = sema.expr_member_fact(expr_id)
        && fact.kind == ExprMemberKind::ClassMember
        && let Some(evidence) = sema.expr_evidence(expr_id).and_then(|items| items.first())
    {
        return IrExprKind::RecordGet {
            base: Box::new(lower_evidence_expr(
                ctx,
                IrOrigin::new(
                    sema.module().store.exprs.get(expr_id).origin.source_id,
                    sema.module().store.exprs.get(expr_id).origin.span,
                ),
                evidence,
            )),
            index: fact.index.unwrap_or_default(),
        };
    }

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

    if let Some(closure) = lower_attached_method_field(ctx, expr_id, base) {
        return closure;
    }

    if let Some(module_target) = sema.expr_module_target(expr_id).cloned() {
        let expr_ty = sema.try_expr_ty(expr_id).unwrap_or_else(|| {
            invalid_lowering_path("expr type missing for dot callable field ref")
        });
        if matches!(sema.ty(expr_ty).kind, HirTyKind::Arrow { .. }) {
            return IrExprKind::ClosureNew {
                callee: IrNameRef::new(interner.resolve(symbol)).with_module_target(module_target),
                captures: vec![lower_expr(ctx, base)].into_boxed_slice(),
            };
        }
    }

    invalid_lowering_path(format!("{kind:?}"))
}

fn lower_attached_method_field(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    base: HirExprId,
) -> Option<IrExprKind> {
    let fact = ctx.sema.expr_member_fact(expr_id)?;
    let captures = match fact.kind {
        ExprMemberKind::AttachedMethodNamespace => Box::new([]),
        ExprMemberKind::DotCallable | ExprMemberKind::AttachedMethod => {
            vec![lower_expr(ctx, base)].into_boxed_slice()
        }
        _ => return None,
    };
    Some(IrExprKind::ClosureNew {
        callee: IrNameRef {
            binding: fact.binding,
            name: ctx.interner.resolve(fact.name).into(),
            module_target: fact
                .module_target
                .clone()
                .or_else(|| ctx.sema.expr_module_target(expr_id).cloned()),
        },
        captures,
    })
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
    if matches!(op, HirBinaryOp::ClosedRange | HirBinaryOp::OpenRange) {
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
    let wants_float = matches!(
        left_ty.kind,
        HirTyKind::Float | HirTyKind::Float32 | HirTyKind::Float64
    ) || matches!(
        right_ty.kind,
        HirTyKind::Float | HirTyKind::Float32 | HirTyKind::Float64
    );
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
