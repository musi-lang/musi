//! Closure and nested function emission.

use std::collections::HashSet;

use msc_ast::ExprIdx;
use msc_ast::PatIdx;
use msc_ast::expr::Param;
use msc_sema::DefId;
use msc_sema::def::DefKind;

use crate::error::EmitError;
use crate::error::EmitResult;

use super::FnCtx;
use super::capture::{CaptureSource, collect_free_vars};
use super::expr::{emit_deferred_cleanup, emit_expr_tail};
use super::{Emitter, FnBytecode};

pub(super) fn emit_fn(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    params: &[Param],
    body: ExprIdx,
) -> EmitResult<bool> {
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
        exported: false,
        effectful: false,
        dict_param_count: 0,
        local_count: nested_fc.fe.local_count,
        param_count: nested_fc.fe.param_count,
        max_stack: nested_fc.fe.max_stack,
        upvalue_count,
        source_map: nested_fc.fe.source_map,
        local_names: vec![],
        code: nested_fc.fe.code,
        handlers: nested_fc.fe.handlers,
        safepoints: nested_fc.fe.safepoints,
        effect_refs: vec![],
    };
    em.nested_fns.push(fn_bytecode);

    fc.fe.emit_cls_new(nested_fn_id);
    if upvalue_count > 0 {
        // Capturing closure: attach each upvalue cell.
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

/// Emit a recursive local closure via the ref-cell letrec pattern:
/// 1. Pre-allocate a ref cell with a placeholder value
/// 2. Emit the closure (which captures the ref cell as an upvalue)
/// 3. Patch the ref cell with the actual closure value
pub(super) fn emit_letrec_fn(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    def_id: DefId,
    _pat_idx: PatIdx,
    _val_idx: ExprIdx,
    params: &[Param],
    fn_body: ExprIdx,
) -> EmitResult {
    // 1. Pre-allocate ref cell: LD_UNIT → ALC_REF → ST_LOC
    let wk = &em.sema.well_known;
    let type_id = em.tp.lower_well_known_def(wk.any, wk).unwrap_or(0);
    fc.fe.emit_ld_unit();
    fc.fe.emit_alc_ref(type_id);
    let slot = fc.alloc_local();
    fc.fe.emit_st_loc(slot);

    // Register the binding before emitting the closure so collect_free_vars
    // finds it in fc.local_map and captures the ref cell.
    let prev = fc.local_map.insert(def_id, slot);
    debug_assert!(prev.is_none(), "duplicate local slot for letrec def");
    let _ = fc.ref_locals.insert(def_id);

    // 2. Emit the closure (standard path - now finds self in parent_locals).
    let produced = emit_fn(em, fc, params, fn_body)?;
    if !produced {
        return Ok(());
    }

    // 3. Patch: store closure into the ref cell's field 0.
    let tmp = fc.alloc_local();
    fc.fe.emit_st_loc(tmp);
    fc.fe.emit_ld_loc(slot);
    fc.fe.emit_ld_loc(tmp);
    fc.fe.emit_st_fld(0)?;
    Ok(())
}
