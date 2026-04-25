use super::{
    ComptimeValue, HirArg, HirExprId, HirExprKind, HirParam, HirParamRange, HirPatKind, Ident,
    IrArg, IrCallable, IrExpr, IrExprKind, IrParam, LowerCtx, NameBindingId, SemaModule,
    lower_expr, lower_origin, lowering_invariant_violation, toplevel,
};

type ComptimeArgValues = [(usize, ComptimeValue)];

pub(super) fn lower_comptime_call_expr(
    ctx: &mut LowerCtx<'_>,
    callee: HirExprId,
    args: &[HirArg],
) -> Result<Option<IrExprKind>, Box<str>> {
    let Some((binding, name)) = direct_callee_binding(ctx.sema, callee) else {
        return Ok(None);
    };
    let Some(scheme) = ctx.sema.binding_scheme(binding) else {
        return Ok(None);
    };
    if !scheme
        .comptime_params
        .iter()
        .any(|is_comptime| *is_comptime)
    {
        return Ok(None);
    }
    let Some(definition) = local_callable_definition(ctx, binding) else {
        return Err(format!(
            "comptime callable `{}` cannot be specialized outside its defining module",
            ctx.interner.resolve(name.name)
        )
        .into());
    };
    let mut runtime_args = Vec::new();
    let mut comptime_values = Vec::new();
    for (index, arg) in args.iter().enumerate() {
        let is_comptime = scheme.comptime_params.get(index).copied().unwrap_or(false);
        if is_comptime {
            let Some(value) = ctx.sema.expr_comptime_value(arg.expr).cloned() else {
                return Err(super::lower_errors::lowering_error(
                    "comptime call argument missing sema value",
                ));
            };
            comptime_values.push((index, value));
        } else {
            runtime_args.push(IrArg::new(false, lower_expr(ctx, arg.expr)));
        }
    }
    let specialized_name = specialized_callable_name(ctx, name, &comptime_values);
    ensure_specialized_callable(ctx, &definition, &specialized_name, &comptime_values);
    Ok(Some(IrExprKind::Call {
        callee: Box::new(IrExpr::new(
            lower_origin(ctx.sema, callee),
            IrExprKind::Name {
                binding: None,
                name: specialized_name,
                import_record_target: Some(ctx.module_key.clone()),
            },
        )),
        args: runtime_args.into_boxed_slice(),
    }))
}

#[derive(Clone)]
struct LocalCallableDefinition {
    binding: NameBindingId,
    name: Ident,
    expr_id: HirExprId,
    params: HirParamRange,
    body: HirExprId,
}

fn direct_callee_binding(sema: &SemaModule, callee: HirExprId) -> Option<(NameBindingId, Ident)> {
    match sema.module().store.exprs.get(callee).kind {
        HirExprKind::Name { name } => {
            super::use_binding_id(sema, name).map(|binding| (binding, name))
        }
        HirExprKind::Apply { callee, .. } => direct_callee_binding(sema, callee),
        _ => None,
    }
}

fn local_callable_definition(
    ctx: &LowerCtx<'_>,
    binding: NameBindingId,
) -> Option<LocalCallableDefinition> {
    find_local_callable_definition(ctx, ctx.sema.module().root, binding)
}

fn find_local_callable_definition(
    ctx: &LowerCtx<'_>,
    expr_id: HirExprId,
    binding: NameBindingId,
) -> Option<LocalCallableDefinition> {
    match &ctx.sema.module().store.exprs.get(expr_id).kind {
        HirExprKind::Sequence { exprs } => ctx
            .sema
            .module()
            .store
            .expr_ids
            .get(*exprs)
            .iter()
            .find_map(|expr| find_local_callable_definition(ctx, *expr, binding)),
        HirExprKind::Unsafe { body } | HirExprKind::Pin { body, .. } => {
            find_local_callable_definition(ctx, *body, binding)
        }
        HirExprKind::Let {
            pat,
            params,
            value,
            has_param_clause,
            ..
        } => {
            let HirPatKind::Bind { name } = ctx.sema.module().store.pats.get(*pat).kind else {
                return None;
            };
            let found_binding = super::decl_binding_id(ctx.sema, name)?;
            if found_binding != binding {
                return None;
            }
            let is_callable = *has_param_clause
                || !ctx
                    .sema
                    .module()
                    .store
                    .params
                    .get(params.clone())
                    .is_empty();
            is_callable.then_some(LocalCallableDefinition {
                binding,
                name,
                expr_id,
                params: params.clone(),
                body: *value,
            })
        }
        _ => None,
    }
}

fn specialized_callable_name(
    ctx: &LowerCtx<'_>,
    name: Ident,
    values: &ComptimeArgValues,
) -> Box<str> {
    let base = ctx.interner.resolve(name.name);
    let suffix = values
        .iter()
        .map(|(index, value)| format!("{index}_{}", encode_comptime_value(value)))
        .collect::<Vec<_>>()
        .join("_");
    format!("{base}$ct${suffix}").into_boxed_str()
}

fn encode_comptime_value(value: &ComptimeValue) -> String {
    match value {
        ComptimeValue::Int(value) => format!("i{value}"),
        ComptimeValue::Nat(value) => format!("n{value}"),
        ComptimeValue::Float(raw) => format!("f{}", encode_text(raw)),
        ComptimeValue::String(value) => format!("s{}", encode_text(value)),
        ComptimeValue::Rune(value) => format!("r{value:x}"),
        ComptimeValue::CPtr(value) => format!("p{value:x}"),
        ComptimeValue::Syntax(term) => format!("x{}", encode_text(term.text())),
        ComptimeValue::Unit => "u".to_owned(),
        ComptimeValue::Seq(value) => format!(
            "q{}_{}",
            encode_text(&value.ty.to_string()),
            encode_comptime_values(&value.items)
        ),
        ComptimeValue::Data(value) => format!(
            "d{}_{}_{}",
            encode_text(&value.ty.to_string()),
            encode_text(&value.variant),
            encode_comptime_values(&value.fields)
        ),
        ComptimeValue::Closure(value) => format!(
            "c{}_{}_{}",
            encode_text(value.module.as_str()),
            encode_text(&value.name),
            encode_comptime_values(&value.captures)
        ),
        ComptimeValue::Continuation(value) => format!("k{}", encode_comptime_values(&value.frames)),
        ComptimeValue::Type(value) => format!("t{}", encode_text(&value.term.to_string())),
        ComptimeValue::ImportRecord(value) => format!("m{}", encode_text(value.key.as_str())),
        ComptimeValue::Foreign(value) => format!(
            "g{}_{}_{}",
            encode_text(value.module.as_str()),
            encode_text(&value.name),
            value
                .type_args
                .iter()
                .map(ToString::to_string)
                .map(|text| encode_text(&text))
                .collect::<Vec<_>>()
                .join("_")
        ),
        ComptimeValue::Effect(value) => {
            format!(
                "e{}_{}",
                encode_text(value.module.as_str()),
                encode_text(&value.name)
            )
        }
        ComptimeValue::Shape(value) => {
            format!(
                "h{}_{}",
                encode_text(value.module.as_str()),
                encode_text(&value.name)
            )
        }
    }
}

fn encode_comptime_values(values: &[ComptimeValue]) -> String {
    values
        .iter()
        .map(encode_comptime_value)
        .collect::<Vec<_>>()
        .join("_")
}

fn encode_text(text: &str) -> String {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut encoded = String::with_capacity(text.len() * 2);
    for byte in text.bytes() {
        encoded.push(char::from(HEX[usize::from(byte >> 4)]));
        encoded.push(char::from(HEX[usize::from(byte & 0x0f)]));
    }
    encoded
}

fn ensure_specialized_callable(
    ctx: &mut LowerCtx<'_>,
    definition: &LocalCallableDefinition,
    name: &str,
    comptime_values: &ComptimeArgValues,
) {
    if !ctx.specialized_callables.insert(name.into()) {
        return;
    }
    let params = ctx
        .sema
        .module()
        .store
        .params
        .get(definition.params.clone())
        .to_vec();
    let previous = install_comptime_params(ctx, &params, comptime_values);
    let (hidden_params, constraint_answer_bindings) =
        super::hidden_constraint_answer_params_for_binding(
            ctx.sema,
            ctx.interner.resolve(definition.name.name),
            Some(definition.binding),
        );
    super::push_constraint_answer_bindings(ctx, constraint_answer_bindings);
    let body = lower_expr(ctx, definition.body);
    super::pop_constraint_answer_bindings(ctx);
    restore_comptime_params(ctx, previous);
    let mut lowered_params = hidden_params;
    lowered_params.extend(lower_runtime_params(ctx, &params));
    let effects = ctx
        .sema
        .binding_scheme(definition.binding)
        .map(|scheme| scheme.effects.clone())
        .unwrap_or_default();
    let attrs = ctx
        .sema
        .module()
        .store
        .exprs
        .get(definition.expr_id)
        .mods
        .attrs
        .clone();
    let profile = toplevel::profile_attrs(ctx.sema, ctx.interner, attrs);
    let callable = IrCallable::new(name, lowered_params.into_boxed_slice(), body)
        .with_hot(profile.hot)
        .with_cold(profile.cold)
        .with_effects(effects)
        .with_import_record_target(ctx.module_key.clone());
    ctx.extra_callables.push(callable);
}

fn install_comptime_params(
    ctx: &mut LowerCtx<'_>,
    params: &[HirParam],
    values: &ComptimeArgValues,
) -> Vec<(NameBindingId, Option<ComptimeValue>)> {
    values
        .iter()
        .filter_map(|(index, value)| {
            let param = params.get(*index)?;
            let binding = super::decl_binding_id(ctx.sema, param.name)?;
            let previous = ctx.comptime_bindings.insert(binding, value.clone());
            Some((binding, previous))
        })
        .collect()
}

fn restore_comptime_params(
    ctx: &mut LowerCtx<'_>,
    previous: Vec<(NameBindingId, Option<ComptimeValue>)>,
) {
    for (binding, value) in previous {
        if let Some(value) = value {
            let _prev = ctx.comptime_bindings.insert(binding, value);
        } else {
            let _prev = ctx.comptime_bindings.remove(&binding);
        }
    }
}

fn lower_runtime_params(ctx: &LowerCtx<'_>, params: &[HirParam]) -> Vec<IrParam> {
    params
        .iter()
        .filter(|param| !param.is_comptime)
        .map(|param| {
            IrParam::new(
                super::decl_binding_id(ctx.sema, param.name)
                    .unwrap_or_else(|| lowering_invariant_violation("param binding missing")),
                ctx.interner.resolve(param.name.name),
            )
        })
        .collect()
}
