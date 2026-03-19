//! Effect operation emission: handle, need, resume.

use msc_ast::ExprIdx;
use msc_ast::expr::{Expr, HandlerOp};
use msc_sema::def::DefKind;

use crate::error::{EmitError, EmitResult};

use super::FnCtx;
use super::expr::{emit_call_args, emit_expr, emit_expr_tail, emit_require};
use super::{Emitter, FnBytecode};

pub(super) fn emit_force_unwrap(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    operand: ExprIdx,
) -> EmitResult<bool> {
    emit_require(em, fc, operand, "force-unwrap operand")?;
    let tmp = fc.alloc_local();
    fc.fe.emit_st_loc(tmp);

    fc.fe.emit_ld_loc(tmp);
    fc.fe.emit_unop(msc_bc::Opcode::OPT_IS);
    let ok_label = fc.fresh_label();
    fc.fe.emit_jmp_t(ok_label);
    fc.fe.emit_unop(msc_bc::Opcode::PANIC);
    fc.fe.emit_label(ok_label);

    fc.fe.emit_ld_loc(tmp);
    fc.fe.emit_unop(msc_bc::Opcode::OPT_GET);
    Ok(true)
}

/// Walks the AST type expression to find the effect name, then looks up `effect_id_map`.
/// Falls back to 0 for well-known effects not yet in the pool.
fn resolve_handle_effect_id(em: &Emitter<'_>, effect_ty: ExprIdx) -> u8 {
    let effect_name = match &em.ast.exprs[effect_ty] {
        Expr::Name { name_ref, .. } => em.ast.name_refs[*name_ref].name,
        Expr::TypeApp { callee, .. } => match &em.ast.exprs[*callee] {
            Expr::Name { name_ref, .. } => em.ast.name_refs[*name_ref].name,
            _ => return 0,
        },
        _ => return 0,
    };
    em.sema
        .defs
        .iter()
        .find(|d| d.kind == DefKind::Effect && d.name == effect_name)
        .and_then(|d| em.effect_id_map.get(&d.id).copied())
        .unwrap_or(0)
}

/// Compile each handler op as a nested function, push the effect handler,
/// emit the body, then pop the handler.
pub(super) fn emit_handle(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    effect_ty: ExprIdx,
    ops: &[HandlerOp],
    body: ExprIdx,
) -> EmitResult<bool> {
    let effect_id = resolve_handle_effect_id(em, effect_ty);

    for op in ops {
        let handler_fn_id = em.alloc_fn_id();
        let param_count = u16::try_from(op.params.len())
            .map_err(|_| EmitError::overflow("handler op param count"))?;
        let mut handler_fc = FnCtx::new(param_count);
        for (i, param) in op.params.iter().enumerate() {
            let slot = u32::try_from(i).map_err(|_| EmitError::overflow("handler param index"))?;
            if let Some(&did) = em.pat_defs().get(&param.span) {
                let prev = handler_fc.local_map.insert(did, slot);
                debug_assert!(prev.is_none(), "duplicate local slot for def");
            }
        }
        let had_value = emit_expr(em, &mut handler_fc, op.body)?;
        if had_value {
            handler_fc.fe.emit_cont_resume();
        } else {
            handler_fc.fe.emit_ret_u();
        }
        handler_fc
            .fe
            .resolve_fixups(&format!("<handler@{handler_fn_id}>"))?;
        let _code_len = handler_fc.fe.validate_code_len()?;
        let type_id = if let Some(&ty_idx) = em.expr_types().get(&op.body) {
            em.tp
                .lower_sema_type(ty_idx, &em.sema.types, &em.sema.unify, &em.sema.well_known)
                .unwrap_or(0)
        } else {
            em.tp
                .lower_well_known_def(em.sema.well_known.unit, &em.sema.well_known)
                .ok_or_else(|| EmitError::unresolvable("Unit type"))?
        };
        let fn_bytecode = FnBytecode {
            fn_id: handler_fn_id,
            name_stridx: 0,
            type_id,
            local_count: handler_fc.fe.local_count,
            param_count: handler_fc.fe.param_count,
            max_stack: handler_fc.fe.max_stack,
            upvalue_count: 0,
            code: handler_fc.fe.code,
            handlers: handler_fc.fe.handlers,
        };
        em.nested_fns.push(fn_bytecode);
        fc.fe.emit_cont_mark(u32::from(effect_id), handler_fn_id)?;
    }

    let produced = emit_expr(em, fc, body)?;

    for _ in ops {
        fc.fe.emit_cont_unmark(u32::from(effect_id));
    }

    Ok(produced)
}

pub(super) fn emit_need(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    body: ExprIdx,
    is_tail: bool,
) -> EmitResult<bool> {
    if let Expr::Call { callee, args, .. } = &em.ast.exprs[body] {
        let callee = *callee;
        let args: Vec<_> = args.clone();
        if let Some(&def_id) = em.expr_defs().get(&callee) {
            let is_effect_op = em
                .sema
                .defs
                .iter()
                .any(|d| d.id == def_id && d.kind == DefKind::EffectOp);
            if is_effect_op {
                let op_index = resolve_effect_op_index(em, def_id);
                let arg_count = emit_call_args(em, fc, &args)?;
                let ac = i32::try_from(arg_count)
                    .map_err(|_| EmitError::overflow("effect op arg count"))?;
                fc.fe.emit_cont_save(op_index, ac);
                return Ok(true);
            }
        }
    }
    emit_expr_tail(em, fc, body, is_tail)
}

pub(super) fn emit_resume(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    value: Option<ExprIdx>,
) -> EmitResult<bool> {
    if let Some(val_idx) = value {
        emit_require(em, fc, val_idx, "resume value")?;
    } else {
        fc.fe.emit_ld_unit();
    }
    fc.fe.emit_cont_resume();
    Ok(false)
}

fn resolve_effect_op_index(em: &Emitter<'_>, op_def_id: msc_sema::DefId) -> u32 {
    let def = em.sema.defs.iter().find(|d| d.id == op_def_id);
    let Some(def) = def else { return 0 };
    let Some(parent_id) = def.parent else {
        return 0;
    };
    let mut siblings: Vec<u32> = em
        .sema
        .defs
        .iter()
        .filter(|d| d.kind == DefKind::EffectOp && d.parent == Some(parent_id))
        .map(|d| d.id.0)
        .collect();
    siblings.sort_unstable();
    u32::try_from(siblings.iter().position(|&x| x == op_def_id.0).unwrap_or(0)).unwrap_or(0)
}
