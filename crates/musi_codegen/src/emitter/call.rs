use musi_ast::{Expr, PostfixOp};
use musi_shared::Idx;

use crate::Module;
use crate::Opcode;
use crate::error::CodegenError;

use super::expr::{emit_args, emit_expr};
use super::field::emit_field_access;
use super::state::{EmitArenas, EmitState, FnEmitter, VariantInfo};

pub(super) fn emit_variant_construct(
    vinfo: &VariantInfo,
    args: &[Idx<Expr>],
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let type_tag = state
        .type_tag_map
        .get(&vinfo.type_name)
        .copied()
        .unwrap_or(0);
    out.push(&Opcode::LdImmI64(vinfo.discriminant));
    emit_args(arenas, state, args, module, out)?;
    let provided = u16::try_from(args.len()).map_err(|_| CodegenError::UnsupportedExpr)?;
    for _ in provided..vinfo.payload_count {
        out.push(&Opcode::LdImmUnit);
    }
    for _ in vinfo.payload_count.saturating_add(1)..vinfo.total_field_count {
        out.push(&Opcode::LdImmUnit);
    }
    out.push(&Opcode::NewObj {
        type_tag,
        field_count: vinfo.total_field_count,
    });
    Ok(())
}

pub(super) fn emit_static_call(
    fn_idx: u16,
    args: &[Idx<Expr>],
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    emit_args(arenas, state, args, module, out)?;
    out.push(&Opcode::Call(fn_idx));
    Ok(())
}

pub(super) fn emit_local_fn_call(
    slot: u16,
    args: &[Idx<Expr>],
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    emit_args(arenas, state, args, module, out)?;
    out.push(&Opcode::LdLoc(slot));
    out.push(&Opcode::CallDynamic);
    Ok(())
}

pub(super) fn receiver_has_field(
    recv_idx: Idx<Expr>,
    field_name: &str,
    arenas: &EmitArenas<'_>,
    state: &EmitState,
    out: &FnEmitter,
) -> bool {
    (|| -> Option<bool> {
        let Expr::Ident {
            name: recv_name, ..
        } = arenas.exprs.get(recv_idx)
        else {
            return None;
        };
        let recv_str = arenas.interner.resolve(*recv_name);
        let slot = out.lookup_local(recv_str)?;
        let type_name = out.local_types.get(&slot)?;
        let tinfo = state.type_map.get(type_name)?;
        Some(tinfo.field_names.iter().any(|f| f == field_name))
    })()
    .unwrap_or(false)
}

pub(super) fn emit_ufcs_call(
    recv_idx: Idx<Expr>,
    method_sym: musi_shared::Symbol,
    args: &[Idx<Expr>],
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let method_name = arenas.interner.resolve(method_sym);

    if receiver_has_field(recv_idx, method_name, arenas, state, out) {
        emit_args(arenas, state, args, module, out)?;
        emit_field_access(arenas, state, recv_idx, method_sym, module, out)?;
        out.push(&Opcode::CallDynamic);
        return Ok(());
    }

    if let Some(&fn_idx) = state.fn_map.get(method_name) {
        let recv_expr = arenas.exprs.get(recv_idx).clone();
        emit_expr(arenas, state, &recv_expr, module, out)?;
        emit_args(arenas, state, args, module, out)?;
        out.push(&Opcode::Call(fn_idx));
        return Ok(());
    }

    if state.class_method_names.contains(method_name) {
        let arg_count = u16::try_from(1 + args.len()).map_err(|_| CodegenError::UnsupportedExpr)?;
        let name_const_idx = module.add_string_const(method_name)?;
        let recv_expr = arenas.exprs.get(recv_idx).clone();
        emit_expr(arenas, state, &recv_expr, module, out)?;
        emit_args(arenas, state, args, module, out)?;
        out.push(&Opcode::CallMethod {
            method_idx: name_const_idx,
            arg_count,
        });
        return Ok(());
    }

    Err(CodegenError::UnknownFunction(method_name.into()))
}

pub(super) fn emit_expr_call(
    callee: &Expr,
    args: &[Idx<Expr>],
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    emit_args(arenas, state, args, module, out)?;
    emit_expr(arenas, state, callee, module, out)?;
    out.push(&Opcode::CallDynamic);
    Ok(())
}

pub(super) fn emit_call(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    base: Idx<Expr>,
    args: &[Idx<Expr>],
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let base_expr = arenas.exprs.get(base);

    if let Expr::Ident { name, .. } = base_expr {
        let callee_str = arenas.interner.resolve(*name);
        if state.variant_map.contains_key(callee_str) {
            return Err(CodegenError::VariantConstructRequiresDot(callee_str.into()));
        }
        if let Some(&fn_idx) = state.fn_map.get(callee_str) {
            return emit_static_call(fn_idx, args, arenas, state, module, out);
        }
        if let Some(slot) = out.lookup_local(callee_str) {
            return emit_local_fn_call(slot, args, arenas, state, module, out);
        }
        return Err(CodegenError::UnknownFunction(callee_str.into()));
    }

    if let Expr::Postfix {
        base: recv_idx,
        op: PostfixOp::Field {
            name: method_sym, ..
        },
        ..
    } = base_expr.clone()
    {
        return emit_ufcs_call(recv_idx, method_sym, args, arenas, state, module, out);
    }

    let callee = base_expr.clone();
    emit_expr_call(&callee, args, arenas, state, module, out)
}
