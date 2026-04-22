use super::*;

struct DotCallableCallTarget {
    receiver: HirExprId,
    binding: Option<NameBindingId>,
    name: Symbol,
    import_record_target: Option<ModuleKey>,
}

type ComptimeArgValues = [(usize, ComptimeValue)];

pub(super) fn lower_call_expr(
    ctx: &mut LowerCtx<'_>,
    callee: HirExprId,
    args: &SliceRange<HirArg>,
) -> Result<IrExprKind, Box<str>> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let arg_nodes = ordered_call_args(
        sema,
        interner,
        callee,
        sema.module().store.args.get(args.clone()),
    );
    if let Some(intrinsic) = lower_std_cmp_intrinsic(ctx, callee, &arg_nodes) {
        return intrinsic;
    }
    if let Some(intrinsic) = lower_ffi_pointer_intrinsic(ctx, callee, &arg_nodes) {
        return intrinsic;
    }
    if let Some(dot_callable) = resolve_dot_callable_call_target(sema, callee) {
        return lower_dot_callable_call_expr(ctx, callee, &arg_nodes, &dot_callable, interner);
    }

    if !arg_nodes.iter().any(|arg| arg.spread) {
        if let Some(specialized) = lower_comptime_call_expr(ctx, callee, &arg_nodes)? {
            return Ok(specialized);
        }
        return Ok(IrExprKind::Call {
            callee: Box::new(lower_expr(ctx, callee)),
            args: arg_nodes
                .iter()
                .map(|arg| IrArg::new(false, lower_expr(ctx, arg.expr)))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        });
    }

    let origin = lower_origin(sema, callee);
    let mut prelude = Vec::<IrExpr>::new();
    let callee_temp = fresh_temp(ctx);
    prelude.push(IrExpr::new(
        origin,
        IrExprKind::TempLet {
            temp: callee_temp,
            value: Box::new(lower_expr(ctx, callee)),
        },
    ));
    let callee_expr = IrExpr::new(origin, IrExprKind::Temp { temp: callee_temp });

    let (arg_prelude, parts, has_runtime_spread) =
        lower_spread_args(ctx, origin, &arg_nodes, SpreadMode::Call)?;
    prelude.extend(arg_prelude);

    prelude.push(IrExpr::new(
        origin,
        if has_runtime_spread {
            IrExprKind::CallParts {
                callee: Box::new(callee_expr),
                args: parts.into_boxed_slice(),
            }
        } else {
            let args = parts
                .into_iter()
                .map(|part| match part {
                    IrSeqPart::Expr(expr) => Some(IrArg::new(false, expr)),
                    IrSeqPart::Spread(_) => None,
                })
                .collect::<Option<Vec<_>>>()
                .map(Vec::into_boxed_slice);
            let Some(args) = args else {
                return Err("call spread lowering invariant".into());
            };
            IrExprKind::Call {
                callee: Box::new(callee_expr),
                args,
            }
        },
    ));

    Ok(IrExprKind::Sequence {
        exprs: prelude.into_boxed_slice(),
    })
}

fn lower_comptime_call_expr(
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
                return Err("comptime call argument missing sema value".into());
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
    let callable = IrCallable::new(name, lowered_params.into_boxed_slice(), body)
        .with_hot(toplevel::attrs_have_name(
            ctx.sema,
            ctx.interner,
            attrs.clone(),
            "hot",
        ))
        .with_cold(toplevel::attrs_have_name(
            ctx.sema,
            ctx.interner,
            attrs,
            "cold",
        ))
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
                    .unwrap_or_else(|| invalid_lowering_path("param binding missing")),
                ctx.interner.resolve(param.name.name),
            )
        })
        .collect()
}

fn lower_std_cmp_intrinsic(
    ctx: &mut LowerCtx<'_>,
    callee: HirExprId,
    args: &[HirArg],
) -> Option<Result<IrExprKind, Box<str>>> {
    if !is_std_cmp_module(&ctx.module_key) {
        return None;
    }
    let HirExprKind::Name { name } = ctx.sema.module().store.exprs.get(callee).kind else {
        return None;
    };
    if ctx.interner.resolve(name.name) != "compareFloatTotalIntrinsic" {
        return None;
    }
    let lowered_args = args
        .iter()
        .map(|arg| IrArg::new(false, lower_expr(ctx, arg.expr)))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    Some(Ok(IrExprKind::IntrinsicCall {
        kind: IrIntrinsicKind::FloatTotalCompare,
        symbol: "cmp.float.total_compare".into(),
        param_tys: Box::new(["Float".into(), "Float".into()]),
        result_ty: "Int".into(),
        args: lowered_args,
    }))
}

fn lower_ffi_pointer_intrinsic(
    ctx: &mut LowerCtx<'_>,
    callee: HirExprId,
    args: &[HirArg],
) -> Option<Result<IrExprKind, Box<str>>> {
    let target = pointer_intrinsic_target(ctx, callee)?;
    let lowered_args = args
        .iter()
        .map(|arg| IrArg::new(false, lower_expr(ctx, arg.expr)))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let call = IrExprKind::IntrinsicCall {
        kind: target.kind,
        symbol: target.symbol,
        param_tys: target.param_tys,
        result_ty: target.result_ty,
        args: lowered_args,
    };
    Some(Ok(call))
}

struct PointerIntrinsicTarget {
    kind: IrIntrinsicKind,
    symbol: Box<str>,
    param_tys: Box<[Box<str>]>,
    result_ty: Box<str>,
}

fn pointer_intrinsic_target(
    ctx: &LowerCtx<'_>,
    callee: HirExprId,
) -> Option<PointerIntrinsicTarget> {
    let (callee, type_arg) = match ctx.sema.module().store.exprs.get(callee).kind {
        HirExprKind::Apply { callee, args } => {
            let args = ctx.sema.module().store.expr_ids.get(args);
            (callee, args.first().copied())
        }
        _ => (callee, None),
    };
    let (HirExprKind::Name { name } | HirExprKind::Field { name, .. }) =
        ctx.sema.module().store.exprs.get(callee).kind
    else {
        return None;
    };
    let name = ctx.interner.resolve(name.name);
    match name {
        "ptrNullIntrinsic" if is_std_ffi_module(&ctx.module_key) => Some(PointerIntrinsicTarget {
            kind: IrIntrinsicKind::FfiPtrNull,
            symbol: "ffi.ptr.null".into(),
            param_tys: Box::default(),
            result_ty: "CPtr".into(),
        }),
        "ptrIsNullIntrinsic" if is_std_ffi_module(&ctx.module_key) => {
            Some(PointerIntrinsicTarget {
                kind: IrIntrinsicKind::FfiPtrIsNull,
                symbol: "ffi.ptr.is_null".into(),
                param_tys: Box::new(["CPtr".into()]),
                result_ty: "Bool".into(),
            })
        }
        "offset" if is_std_ffi_public_pointer_callee(ctx, callee) => {
            pointer_public_offset_target(ctx, type_arg?)
        }
        "read" if is_std_ffi_public_pointer_callee(ctx, callee) => pointer_storage_target(
            ctx,
            type_arg?,
            IrIntrinsicKind::FfiPtrRead,
            "ffi.ptr.read",
            Box::new([pointer_view_ty(ctx, type_arg?)]),
            pointer_storage_result_ty(ctx, type_arg?)?,
        ),
        "write" if is_std_ffi_public_pointer_callee(ctx, callee) => pointer_storage_target(
            ctx,
            type_arg?,
            IrIntrinsicKind::FfiPtrWrite,
            "ffi.ptr.write",
            pointer_public_write_param_tys(ctx, type_arg?)?,
            "Unit".into(),
        ),
        _ => None,
    }
}

fn pointer_public_offset_target(
    ctx: &LowerCtx<'_>,
    type_arg: HirExprId,
) -> Option<PointerIntrinsicTarget> {
    pointer_storage_target(
        ctx,
        type_arg,
        IrIntrinsicKind::FfiPtrOffset,
        "ffi.ptr.offset",
        Box::new([pointer_view_ty(ctx, type_arg), "Int".into()]),
        pointer_view_ty(ctx, type_arg),
    )
}

fn pointer_storage_target(
    ctx: &LowerCtx<'_>,
    type_arg: HirExprId,
    kind: IrIntrinsicKind,
    symbol_prefix: &str,
    param_tys: Box<[Box<str>]>,
    result_ty: Box<str>,
) -> Option<PointerIntrinsicTarget> {
    let suffix = pointer_storage_suffix(ctx, type_arg)?;
    Some(PointerIntrinsicTarget {
        kind,
        symbol: format!("{symbol_prefix}.{suffix}").into(),
        param_tys,
        result_ty,
    })
}

fn pointer_public_write_param_tys(
    ctx: &LowerCtx<'_>,
    type_arg: HirExprId,
) -> Option<Box<[Box<str>]>> {
    let result_ty = pointer_storage_result_ty(ctx, type_arg)?;
    Some(Box::new([pointer_view_ty(ctx, type_arg), result_ty]))
}

fn pointer_view_ty(ctx: &LowerCtx<'_>, type_arg: HirExprId) -> Box<str> {
    let ty = render_type_value_expr_name(ctx.sema, type_arg, ctx.interner);
    format!("Ptr[{ty}]").into()
}

fn pointer_storage_result_ty(ctx: &LowerCtx<'_>, type_arg: HirExprId) -> Option<Box<str>> {
    match pointer_storage_name(ctx, type_arg)?.as_ref() {
        "Float" | "Float32" | "Float64" | "CFloat" | "CDouble" => Some("Float".into()),
        "CPtr" => Some("CPtr".into()),
        "Nat" | "Nat8" | "Nat16" | "Nat32" | "Nat64" | "CUChar" | "CUShort" | "CUInt"
        | "CULong" | "CULongLong" | "CSize" => Some("Nat".into()),
        _ => Some("Int".into()),
    }
}

fn pointer_storage_suffix(ctx: &LowerCtx<'_>, type_arg: HirExprId) -> Option<&'static str> {
    match pointer_storage_name(ctx, type_arg)?.as_ref() {
        "CChar" | "CSChar" | "Int8" => Some("i8"),
        "CUChar" | "Nat8" => Some("u8"),
        "CShort" | "Int16" => Some("i16"),
        "CUShort" | "Nat16" => Some("u16"),
        "CInt" | "Int32" => Some("i32"),
        "CUInt" | "Nat32" => Some("u32"),
        "Int64" | "CLong" | "CLongLong" | "CSizeDiff" | "Int" => Some("i64"),
        "Nat64" | "CULong" | "CULongLong" | "CSize" | "Nat" => Some("u64"),
        "CFloat" | "Float32" => Some("f32"),
        "CDouble" | "Float64" | "Float" => Some("f64"),
        "CPtr" => Some("ptr"),
        _ => None,
    }
}

fn pointer_storage_name(ctx: &LowerCtx<'_>, type_arg: HirExprId) -> Option<Box<str>> {
    let expr = ctx.sema.module().store.exprs.get(type_arg);
    match expr.kind {
        HirExprKind::Name { name } | HirExprKind::Field { name, .. } => {
            Some(ctx.interner.resolve(name.name).into())
        }
        _ => None,
    }
}

fn is_std_ffi_module(module_key: &ModuleKey) -> bool {
    let key = module_key.as_str();
    key == "@std/ffi" || key.ends_with("ffi/index.ms")
}

fn is_std_cmp_module(module_key: &ModuleKey) -> bool {
    let key = module_key.as_str();
    key == "@std/cmp"
        || key.ends_with("cmp/index.ms")
        || key.ends_with("cmp/_core.ms")
        || key.contains("cmp/index.ms::__laws")
        || key.contains("cmp/_core.ms::__laws")
}

fn is_std_ffi_public_pointer_callee(ctx: &LowerCtx<'_>, callee: HirExprId) -> bool {
    match ctx.sema.module().store.exprs.get(callee).kind {
        HirExprKind::Field { base, .. } => is_std_ffi_public_pointer_base(ctx, base),
        HirExprKind::Name { name } => use_binding_id(ctx.sema, name)
            .and_then(|binding| ctx.sema.binding_import_record_target(binding))
            .is_some_and(is_std_ffi_module),
        _ => false,
    }
}

fn is_std_ffi_public_pointer_base(ctx: &LowerCtx<'_>, base: HirExprId) -> bool {
    match ctx.sema.module().store.exprs.get(base).kind {
        HirExprKind::Name { name } => use_binding_id(ctx.sema, name)
            .and_then(|binding| ctx.sema.binding_import_record_target(binding))
            .is_some_and(is_std_ffi_module),
        HirExprKind::Field {
            base: module_base,
            name,
            ..
        } if ctx.interner.resolve(name.name) == "ptr" => {
            is_std_ffi_public_pointer_base(ctx, module_base)
        }
        _ => false,
    }
}

fn lower_dot_callable_call_expr(
    ctx: &mut LowerCtx<'_>,
    callee: HirExprId,
    arg_nodes: &[HirArg],
    dot_callable: &DotCallableCallTarget,
    interner: &Interner,
) -> Result<IrExprKind, Box<str>> {
    let origin = lower_origin(ctx.sema, callee);
    let callee_expr = IrExpr::new(
        origin,
        IrExprKind::Name {
            binding: dot_callable.binding,
            name: interner.resolve(dot_callable.name).into(),
            import_record_target: dot_callable.import_record_target.clone(),
        },
    );

    if !arg_nodes.iter().any(|arg| arg.spread) {
        let mut lowered_args = Vec::with_capacity(arg_nodes.len().saturating_add(1));
        lowered_args.push(IrArg::new(false, lower_expr(ctx, dot_callable.receiver)));
        lowered_args.extend(
            arg_nodes
                .iter()
                .map(|arg| IrArg::new(false, lower_expr(ctx, arg.expr))),
        );
        return Ok(IrExprKind::Call {
            callee: Box::new(callee_expr),
            args: lowered_args.into_boxed_slice(),
        });
    }

    let mut prelude = Vec::<IrExpr>::new();
    let receiver_temp = fresh_temp(ctx);
    prelude.push(IrExpr::new(
        origin,
        IrExprKind::TempLet {
            temp: receiver_temp,
            value: Box::new(lower_expr(ctx, dot_callable.receiver)),
        },
    ));

    let (arg_prelude, arg_parts, has_runtime_spread) =
        lower_spread_args(ctx, origin, arg_nodes, SpreadMode::Call)?;
    prelude.extend(arg_prelude);

    let mut parts = Vec::with_capacity(arg_parts.len().saturating_add(1));
    parts.push(IrSeqPart::Expr(IrExpr::new(
        origin,
        IrExprKind::Temp {
            temp: receiver_temp,
        },
    )));
    parts.extend(arg_parts);

    prelude.push(IrExpr::new(
        origin,
        if has_runtime_spread {
            IrExprKind::CallParts {
                callee: Box::new(callee_expr),
                args: parts.into_boxed_slice(),
            }
        } else {
            let args = parts
                .into_iter()
                .map(|part| match part {
                    IrSeqPart::Expr(expr) => Some(IrArg::new(false, expr)),
                    IrSeqPart::Spread(_) => None,
                })
                .collect::<Option<Vec<_>>>()
                .map(Vec::into_boxed_slice);
            let Some(args) = args else {
                return Err("dot-callable spread lowering invariant".into());
            };
            IrExprKind::Call {
                callee: Box::new(callee_expr),
                args,
            }
        },
    ));

    Ok(IrExprKind::Sequence {
        exprs: prelude.into_boxed_slice(),
    })
}

fn resolve_dot_callable_call_target(
    sema: &SemaModule,
    callee: HirExprId,
) -> Option<DotCallableCallTarget> {
    let HirExprKind::Field { base, .. } = sema.module().store.exprs.get(callee).kind else {
        return None;
    };
    let fact = sema.expr_member_fact(callee)?;
    if !is_dot_callable_member(fact.kind) {
        return None;
    }

    Some(DotCallableCallTarget {
        receiver: base,
        binding: fact.binding,
        name: fact.name,
        import_record_target: fact
            .import_record_target
            .clone()
            .or_else(|| sema.expr_import_record_target(callee).cloned()),
    })
}

const fn is_dot_callable_member(kind: ExprMemberKind) -> bool {
    matches!(
        kind,
        ExprMemberKind::DotCallable | ExprMemberKind::AttachedMethod
    )
}

fn ordered_call_args(
    sema: &SemaModule,
    interner: &Interner,
    callee: HirExprId,
    args: &[HirArg],
) -> Vec<HirArg> {
    if !args.iter().any(|arg| arg.name.is_some()) {
        return args.to_vec();
    }

    let param_names = call_param_names(sema, interner, callee);
    if param_names.is_empty() {
        return args.to_vec();
    }

    let mut prefix = Vec::new();
    let mut named_start = 0usize;
    let mut in_named_suffix = false;

    for arg in args {
        if arg.name.is_some() {
            in_named_suffix = true;
            break;
        }
        let Some(next_index) = named_prefix_advance(sema, arg, named_start) else {
            return args.to_vec();
        };
        named_start = next_index;
        prefix.push(arg.clone());
    }

    let mut ordered = vec![None; param_names.len()];
    for arg in args.iter().skip(prefix.len()) {
        let Some(name) = arg.name else {
            return args.to_vec();
        };
        if arg.spread {
            return args.to_vec();
        }
        let Some(index) = param_names.iter().position(|param| *param == name.name) else {
            return args.to_vec();
        };
        if !in_named_suffix || index < named_start || ordered[index].is_some() {
            return args.to_vec();
        }
        ordered[index] = Some(arg.clone());
    }

    let mut result = prefix;
    for arg in ordered.into_iter().skip(named_start).flatten() {
        result.push(arg);
    }
    if result.len() == args.len() {
        result
    } else {
        args.to_vec()
    }
}

fn named_prefix_advance(sema: &SemaModule, arg: &HirArg, index: usize) -> Option<usize> {
    if !arg.spread {
        return Some(index.saturating_add(1));
    }

    let spread_ty = sema.try_expr_ty(arg.expr)?;
    match &sema.ty(spread_ty).kind {
        HirTyKind::Tuple { items } => {
            Some(index.saturating_add(sema.module().store.ty_ids.get(*items).len()))
        }
        HirTyKind::Array { dims, .. } => {
            let dims = sema.module().store.dims.get(dims.clone());
            (dims.len() == 1).then_some(())?;
            match dims[0] {
                HirDim::Int(len) => Some(index.saturating_add(usize::try_from(len).ok()?)),
                HirDim::Unknown | HirDim::Name(_) => None,
            }
        }
        _ => None,
    }
}

fn call_param_names(sema: &SemaModule, interner: &Interner, callee: HirExprId) -> Box<[Symbol]> {
    if let Some(target) = resolve_dot_callable_call_target(sema, callee)
        && let Some(binding) = target.binding
        && let Some(scheme) = sema.binding_scheme(binding)
    {
        return scheme.param_names.iter().copied().skip(1).collect();
    }

    match sema.module().store.exprs.get(callee).kind {
        HirExprKind::Name { name } => sema
            .resolved()
            .names
            .refs
            .get(&NameSite::new(sema.module().source_id, name.span))
            .copied()
            .and_then(|binding| sema.binding_scheme(binding))
            .map(|scheme| scheme.param_names.clone())
            .unwrap_or_default(),
        HirExprKind::Field { base, name, .. } => {
            if let HirExprKind::Name { name: effect_name } =
                sema.module().store.exprs.get(base).kind
                && let Some(effect) = sema.effect_def(interner.resolve(effect_name.name))
                && let Some(op) = effect.op(interner.resolve(name.name))
            {
                return op.param_names().to_vec().into_boxed_slice();
            }
            Box::default()
        }
        _ => Box::default(),
    }
}

pub(super) fn lower_request_expr(
    ctx: &mut LowerCtx<'_>,
    expr: HirExprId,
) -> Result<IrExprKind, Box<str>> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let (effect_key, op_index, callee, args) =
        resolve_request_target(sema, interner, expr).map_err(Box::<str>::from)?;
    let args_nodes = ordered_call_args(sema, interner, callee, sema.module().store.args.get(args));
    if !args_nodes.iter().any(|arg| arg.spread) {
        let lowered_args = args_nodes
            .iter()
            .map(|arg| lower_expr(ctx, arg.expr))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        return Ok(IrExprKind::Request {
            effect_key,
            op_index,
            args: lowered_args,
        });
    }

    let origin = lower_origin(sema, expr);
    let (prelude, parts, has_runtime_spread) =
        lower_spread_args(ctx, origin, &args_nodes, SpreadMode::Request)?;
    let mut exprs = prelude;
    exprs.push(IrExpr::new(
        origin,
        if has_runtime_spread {
            IrExprKind::RequestSeq {
                effect_key,
                op_index,
                args: parts.into_boxed_slice(),
            }
        } else {
            let args = parts
                .into_iter()
                .map(|part| match part {
                    IrSeqPart::Expr(expr) => Some(expr),
                    IrSeqPart::Spread(_) => None,
                })
                .collect::<Option<Vec<_>>>()
                .map(Vec::into_boxed_slice);
            let Some(args) = args else {
                return Err("ask spread lowering invariant".into());
            };
            IrExprKind::Request {
                effect_key,
                op_index,
                args,
            }
        },
    ));
    Ok(IrExprKind::Sequence {
        exprs: exprs.into_boxed_slice(),
    })
}

#[derive(Clone, Copy)]
enum SpreadMode {
    Call,
    Request,
}

impl SpreadMode {
    const fn runtime_any_message(self) -> &'static str {
        match self {
            Self::Call => "call runtime spread needs []Any",
            Self::Request => "ask runtime spread needs []Any",
        }
    }

    const fn dims_message(self) -> &'static str {
        match self {
            Self::Call => "call spread needs 1D array or tuple",
            Self::Request => "ask spread needs 1D array or tuple",
        }
    }

    const fn source_message(self) -> &'static str {
        match self {
            Self::Call => "call spread source is not tuple/array",
            Self::Request => "ask spread source is not tuple/array",
        }
    }
}

fn lower_origin(sema: &SemaModule, expr: HirExprId) -> IrOrigin {
    let origin = sema.module().store.exprs.get(expr).origin;
    IrOrigin::new(origin.source_id, origin.span)
}

fn resolve_request_target(
    sema: &SemaModule,
    interner: &Interner,
    expr: HirExprId,
) -> Result<(DefinitionKey, u16, HirExprId, SliceRange<HirArg>), &'static str> {
    let HirExprKind::Call { callee, ref args } = sema.module().store.exprs.get(expr).kind else {
        return Err("ask without call");
    };
    let HirExprKind::Field { base, name, .. } = sema.module().store.exprs.get(callee).kind else {
        return Err("ask without effect op field access");
    };
    let HirExprKind::Name { name: effect_name } = sema.module().store.exprs.get(base).kind else {
        return Err("ask without effect name");
    };
    let effect_name = interner.resolve(effect_name.name);
    let op_name = interner.resolve(name.name);
    let Some(effect) = sema.effect_def(effect_name) else {
        return Err("ask with unknown effect");
    };
    let op_index = effect.op_index(op_name).unwrap_or(u16::MAX);
    if op_index == u16::MAX {
        return Err("ask with unknown effect op");
    }
    Ok((effect.key().clone(), op_index, callee, args.clone()))
}

fn lower_spread_args(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    args_nodes: &[HirArg],
    mode: SpreadMode,
) -> Result<(Vec<IrExpr>, Vec<IrSeqPart>, bool), Box<str>> {
    let mut prelude = Vec::<IrExpr>::new();
    let mut parts = Vec::<IrSeqPart>::new();
    let mut has_runtime_spread = false;
    for arg in args_nodes {
        let temp = fresh_temp(ctx);
        prelude.push(IrExpr::new(
            origin,
            IrExprKind::TempLet {
                temp,
                value: Box::new(lower_expr(ctx, arg.expr)),
            },
        ));
        let temp_expr = IrExpr::new(origin, IrExprKind::Temp { temp });
        if !arg.spread {
            parts.push(IrSeqPart::Expr(temp_expr));
            continue;
        }
        has_runtime_spread |=
            lower_spread_arg(ctx, arg.expr, &temp_expr, origin, &mut parts, mode)?;
    }
    Ok((prelude, parts, has_runtime_spread))
}

fn lower_spread_arg(
    ctx: &mut LowerCtx<'_>,
    spread_expr: HirExprId,
    temp_expr: &IrExpr,
    origin: IrOrigin,
    parts: &mut Vec<IrSeqPart>,
    mode: SpreadMode,
) -> Result<bool, Box<str>> {
    let sema = ctx.sema;
    let spread_ty = sema
        .try_expr_ty(spread_expr)
        .unwrap_or_else(|| invalid_lowering_path("expr type missing for spread arg"));
    match &sema.ty(spread_ty).kind {
        HirTyKind::Tuple { items } => {
            for (index, _) in sema.module().store.ty_ids.get(*items).iter().enumerate() {
                let Ok(index_u32) = u32::try_from(index) else {
                    continue;
                };
                parts.push(IrSeqPart::Expr(index_expr(
                    origin,
                    temp_expr.clone(),
                    index_u32,
                )));
            }
            Ok(false)
        }
        HirTyKind::Array { dims, item } => {
            lower_spread_array_arg(sema, dims, *item, temp_expr, origin, parts, mode)
        }
        HirTyKind::Seq { item } => {
            if matches!(sema.ty(*item).kind, HirTyKind::Any) {
                parts.push(IrSeqPart::Spread(temp_expr.clone()));
                Ok(true)
            } else {
                Err(mode.runtime_any_message().into())
            }
        }
        HirTyKind::Range { bound } => {
            let result_ty_name =
                format!("[]{}", super::render_ty_name(sema, *bound, ctx.interner)).into();
            let constraint_answer = sema
                .expr_constraint_answers(spread_expr)
                .and_then(|items| items.first())
                .map(|item| super::lower_constraint_answer_expr(ctx, origin, item));
            let Some(constraint_answer) = constraint_answer else {
                return Err("range spread evidence missing".into());
            };
            parts.push(IrSeqPart::Spread(IrExpr::new(
                origin,
                IrExprKind::RangeMaterialize {
                    range: Box::new(temp_expr.clone()),
                    evidence: Box::new(constraint_answer),
                    result_ty_name,
                },
            )));
            Ok(true)
        }
        _ => Err(mode.source_message().into()),
    }
}

fn lower_spread_array_arg(
    sema: &SemaModule,
    dims: &SliceRange<HirDim>,
    item: HirTyId,
    temp_expr: &IrExpr,
    origin: IrOrigin,
    parts: &mut Vec<IrSeqPart>,
    mode: SpreadMode,
) -> Result<bool, Box<str>> {
    let dims_vec = sema.module().store.dims.get(dims.clone());
    if dims_vec.is_empty() {
        if matches!(sema.ty(item).kind, HirTyKind::Any) {
            parts.push(IrSeqPart::Spread(temp_expr.clone()));
            return Ok(true);
        }
        return Err(mode.runtime_any_message().into());
    }
    if dims_vec.len() != 1 {
        return Err(mode.dims_message().into());
    }
    match dims_vec[0] {
        HirDim::Int(len) => {
            for index_u32 in 0..len {
                parts.push(IrSeqPart::Expr(index_expr(
                    origin,
                    temp_expr.clone(),
                    index_u32,
                )));
            }
            Ok(false)
        }
        HirDim::Unknown | HirDim::Name(_) if matches!(sema.ty(item).kind, HirTyKind::Any) => {
            parts.push(IrSeqPart::Spread(temp_expr.clone()));
            Ok(true)
        }
        HirDim::Unknown | HirDim::Name(_) => Err(mode.runtime_any_message().into()),
    }
}

fn index_expr(origin: IrOrigin, base: IrExpr, index_u32: u32) -> IrExpr {
    IrExpr::new(
        origin,
        IrExprKind::Index {
            base: Box::new(base),
            indices: vec![IrExpr::new(
                origin,
                IrExprKind::Lit(IrLit::Int {
                    raw: index_u32.to_string().into(),
                }),
            )]
            .into_boxed_slice(),
        },
    )
}

#[cfg(test)]
mod tests;
