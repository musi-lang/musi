//! Closure and nested function emission.

use std::collections::HashSet;

use msc_ast::ExprIdx;
use msc_ast::expr::Param;
use msc_sema::def::DefKind;

use crate::error::EmitError;

use super::super::emitter::{Emitter, FnBytecode};
use super::FnCtx;
use super::capture::{CaptureSource, collect_free_vars};
use super::expr::{emit_deferred_cleanup, emit_expr_tail};

pub(super) fn emit_fn(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    params: &[Param],
    body: ExprIdx,
) -> Result<bool, EmitError> {
    let nested_fn_id = em.alloc_fn_id();
    let nested_param_count =
        u16::try_from(params.len()).map_err(|_| EmitError::overflow("nested fn param count"))?;

    let mut nested_fc = FnCtx::new(nested_param_count);

    let mut local_defs = HashSet::new();
    for (i, param) in params.iter().enumerate() {
        let slot = u32::try_from(i).map_err(|_| EmitError::overflow("nested fn param index"))?;
        if let Some(&did) = em.pat_defs().get(&param.span) {
            let prev = nested_fc.local_map.insert(did, slot);
            debug_assert!(prev.is_none(), "duplicate local slot for def");
            let _ = local_defs.insert(did);
        } else {
            for def in &em.sema.defs {
                if def.kind == DefKind::Param && def.name == param.name && def.span == param.span {
                    let prev = nested_fc.local_map.insert(def.id, slot);
                    debug_assert!(prev.is_none(), "duplicate local slot for def");
                    let _ = local_defs.insert(def.id);
                    break;
                }
            }
        }
    }

    let captures = collect_free_vars(em, body, &local_defs, &fc.local_map, &fc.upvalue_map);
    let upvalue_count = u16::try_from(captures.len())
        .map_err(|_| EmitError::overflow("too many captured variables"))?;

    for (upv_idx, &(def_id, _)) in captures.iter().enumerate() {
        let idx = u16::try_from(upv_idx)
            .map_err(|_| EmitError::overflow("upvalue index in populate loop"))?;
        let _ = nested_fc.upvalue_map.insert(def_id, idx);
        if fc.ref_locals.contains(&def_id) || fc.ref_upvalues.contains(&def_id) {
            let _ = nested_fc.ref_upvalues.insert(def_id);
        }
    }

    let had_value = emit_expr_tail(em, &mut nested_fc, body, true)?;
    if had_value {
        if !nested_fc.deferred.is_empty() {
            let tmp = nested_fc.alloc_local();
            nested_fc.fe.emit_st_loc(tmp);
            emit_deferred_cleanup(em, &mut nested_fc)?;
            nested_fc.fe.emit_ld_loc(tmp);
        }
        nested_fc.fe.emit_ret();
    } else {
        emit_deferred_cleanup(em, &mut nested_fc)?;
        nested_fc.fe.emit_ret_u();
    }

    let nested_name = format!("<closure@{nested_fn_id}>");
    nested_fc.fe.resolve_fixups(&nested_name)?;
    let _code_len = nested_fc.fe.validate_code_len()?;

    let type_id = if let Some(&ty_idx) = em.expr_types().get(&body) {
        em.tp
            .lower_sema_type(ty_idx, &em.sema.types, &em.sema.unify, &em.sema.well_known)
            .unwrap_or(0)
    } else {
        em.tp
            .lower_well_known_def(em.sema.well_known.unit, &em.sema.well_known)
            .ok_or_else(|| EmitError::unresolvable("Unit type"))?
    };

    let fn_bytecode = FnBytecode {
        fn_id: nested_fn_id,
        name_stridx: 0,
        type_id,
        local_count: nested_fc.fe.local_count,
        param_count: nested_fc.fe.param_count,
        max_stack: nested_fc.fe.max_stack,
        upvalue_count,
        code: nested_fc.fe.code,
        handlers: nested_fc.fe.handlers,
    };
    em.nested_fns.push(fn_bytecode);

    if upvalue_count == 0 {
        fc.fe.emit_cls_new(nested_fn_id);
    } else {
        fc.fe.emit_cls_new(nested_fn_id);
        for &(_, source) in &captures {
            match source {
                CaptureSource::Local(slot) => fc.fe.emit_cls_upv_local(slot),
                CaptureSource::Upvalue(idx) => {
                    let u8_idx = u8::try_from(idx)
                        .map_err(|_| EmitError::overflow("upvalue index exceeds 255"))?;
                    fc.fe.emit_cls_upv_parent(u8_idx);
                }
            }
        }
    }
    Ok(true)
}
