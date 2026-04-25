use super::{
    HirArg, HirExprId, HirExprKind, IrArg, IrExprKind, IrIntrinsicKind, LowerCtx, ModuleKey,
    lower_expr, render_type_value_expr_name, use_binding_id,
};

pub(super) fn lower_std_cmp_intrinsic(
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

pub(super) fn lower_ffi_pointer_intrinsic(
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
    key == "@std/ffi" || key.ends_with("ffi.ms")
}

fn is_std_cmp_module(module_key: &ModuleKey) -> bool {
    let key = module_key.as_str();
    key == "@std/cmp"
        || key.ends_with("cmp/std.ms")
        || key.ends_with("cmp.ms")
        || key.contains("cmp/std.ms::__laws")
        || key.contains("cmp.ms::__laws")
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
