use musi_ast::{Expr, LitValue, MatchArm, Pat, PatSuffix, PostfixOp};
use musi_shared::{Idx, Interner};

use crate::error::CodegenError;
use crate::Module;
use crate::Opcode;

use super::state::{EmitArenas, EmitState, FnEmitter};
use super::expr::emit_expr;

pub(super) fn emit_discriminant_test(disc: i64, scrutinee_slot: u16, out: &mut FnEmitter) -> usize {
    out.push(&Opcode::LdLoc(scrutinee_slot));
    out.push(&Opcode::LdTag);
    out.push(&Opcode::LdImmI64(disc));
    out.push(&Opcode::EqI64);
    out.emit_jump_placeholder(FnEmitter::BR_FALSE)
}

/// Returns `(variant_name, sub_patterns)` for patterns that match a choice variant.
pub(super) fn variant_name_and_args<'p>(pat: &'p Pat, interner: &'p Interner) -> Option<(&'p str, &'p [Pat])> {
    match pat {
        Pat::Ident { name, suffix: Some(PatSuffix::Positional { args, .. }), .. } => {
            Some((interner.resolve(*name), args.as_slice()))
        }
        Pat::DotPrefix { name, args, .. } => Some((interner.resolve(*name), args.as_slice())),
        _ => None,
    }
}

pub(super) fn emit_pattern_test(
    arenas: &EmitArenas<'_>,
    state: &EmitState,
    pat: &Pat,
    scrutinee_slot: u16,
    _module: &mut Module,
    out: &mut FnEmitter,
) -> Result<Option<usize>, CodegenError> {
    match pat {
        Pat::Wild { .. } => Ok(None),

        Pat::Ident { name, suffix: None, .. } => {
            let name_str = arenas.interner.resolve(*name);
            state.variant_map.get(name_str).map_or(Ok(None), |vinfo| {
                let fixup = emit_discriminant_test(vinfo.discriminant, scrutinee_slot, out);
                Ok(Some(fixup))
            })
        }

        Pat::Lit { value, .. } => {
            out.push(&Opcode::LdLoc(scrutinee_slot));
            match value {
                LitValue::Int(v) => out.push(&Opcode::LdImmI64(*v)),
                _ => return Err(CodegenError::UnsupportedExpr),
            }
            out.push(&Opcode::EqI64);
            let fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);
            Ok(Some(fixup))
        }

        pat => {
            if let Some((variant_name, _)) = variant_name_and_args(pat, arenas.interner) {
                let vinfo = state.variant_map.get(variant_name)
                    .ok_or_else(|| CodegenError::UnknownVariant(variant_name.into()))?;
                let fixup = emit_discriminant_test(vinfo.discriminant, scrutinee_slot, out);
                Ok(Some(fixup))
            } else {
                Err(CodegenError::UnsupportedExpr)
            }
        }
    }
}

pub(super) fn emit_pattern_bindings(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    pat: &Pat,
    scrutinee_slot: u16,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match pat {
        Pat::Ident { name, suffix: None, .. } => {
            let name_str = arenas.interner.resolve(*name);
            if !state.variant_map.contains_key(name_str) {
                let slot = out.define_local(name_str)?;
                out.push(&Opcode::LdLoc(scrutinee_slot));
                out.push(&Opcode::StLoc(slot));
            }
            Ok(())
        }

        Pat::Wild { .. } | Pat::Lit { .. } => Ok(()),

        pat => {
            if let Some((_, sub_pats)) = variant_name_and_args(pat, arenas.interner) {
                for (field_i, sub_pat) in sub_pats.iter().enumerate() {
                    let field_idx = u16::try_from(field_i + 1).map_err(|_| CodegenError::UnsupportedExpr)?;
                    match sub_pat {
                        Pat::Wild { .. } => {}
                        Pat::Ident { name, suffix: None, .. } => {
                            let name_str = arenas.interner.resolve(*name);
                            let sub_slot = out.define_local(name_str)?;
                            out.push(&Opcode::LdLoc(scrutinee_slot));
                            out.push(&Opcode::LdFld(field_idx));
                            out.push(&Opcode::StLoc(sub_slot));
                        }
                        _ => {
                            let temp_name = format!("$sub_{}_{}_{}", scrutinee_slot, field_i, out.next_slot);
                            let temp_slot = out.define_local(&temp_name)?;
                            out.push(&Opcode::LdLoc(scrutinee_slot));
                            out.push(&Opcode::LdFld(field_idx));
                            out.push(&Opcode::StLoc(temp_slot));
                            emit_pattern_bindings(arenas, state, sub_pat, temp_slot, out)?;
                        }
                    }
                }
                Ok(())
            } else {
                Err(CodegenError::UnsupportedExpr)
            }
        }
    }
}

pub(super) fn track_type_of_binding(
    init_expr: &Expr,
    slot: u16,
    arenas: &EmitArenas<'_>,
    state: &EmitState,
    out: &mut FnEmitter,
) {
    match init_expr {
        Expr::Postfix { base, op: PostfixOp::RecDot { .. }, .. } => {
            if let Expr::Ident { name, .. } = arenas.exprs.get(*base) {
                let type_name = arenas.interner.resolve(*name);
                if state.type_map.contains_key(type_name) {
                    let _prev = out.local_types.insert(slot, type_name.to_owned());
                }
            }
        }
        Expr::Postfix { base, op: PostfixOp::Call { .. }, .. } => {
            if let Expr::Ident { name, .. } = arenas.exprs.get(*base) {
                let callee_name = arenas.interner.resolve(*name);
                if let Some(vinfo) = state.variant_map.get(callee_name) {
                    let _prev = out.local_types.insert(slot, vinfo.type_name.clone());
                } else if let Some(ret_type) = state.fn_return_types.get(callee_name).cloned() {
                    if state.type_map.contains_key(&ret_type) {
                        let _prev = out.local_types.insert(slot, ret_type);
                    }
                }
            } else if let Expr::Postfix { base: recv_idx, op: PostfixOp::Field { .. }, .. } = arenas.exprs.get(*base) {
                if let Expr::Ident { name: recv_name, .. } = arenas.exprs.get(*recv_idx) {
                    let recv_str = arenas.interner.resolve(*recv_name);
                    if let Some(recv_slot) = out.lookup_local(recv_str) {
                        if let Some(type_name) = out.local_types.get(&recv_slot).cloned() {
                            let _prev = out.local_types.insert(slot, type_name);
                        }
                    }
                }
            }
        }
        Expr::Ident { name, .. } => {
            let name_str = arenas.interner.resolve(*name);
            if let Some(vinfo) = state.variant_map.get(name_str) {
                let _prev = out.local_types.insert(slot, vinfo.type_name.clone());
            }
        }
        _ => {}
    }
}

pub(super) fn emit_match(
    arenas: &EmitArenas<'_>,
    state: &mut EmitState,
    scrutinee: Idx<Expr>,
    arms: &[MatchArm],
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let scrutinee_name = format!("$scrutinee_{}", out.next_slot);
    let scrutinee_slot = out.define_local(&scrutinee_name)?;
    let scr = arenas.exprs.get(scrutinee).clone();
    emit_expr(arenas, state, &scr, module, out)?;
    out.push(&Opcode::StLoc(scrutinee_slot));

    let mut end_fixups: Vec<usize> = Vec::new();

    for arm in arms {
        let next_arm_fixup = emit_pattern_test(arenas, state, &arm.pat, scrutinee_slot, module, out)?;

        out.push_scope();
        emit_pattern_bindings(arenas, state, &arm.pat, scrutinee_slot, out)?;
        let guard_fixup: Option<usize> = if let Some(guard_idx) = arm.guard {
            let guard_expr = arenas.exprs.get(guard_idx).clone();
            emit_expr(arenas, state, &guard_expr, module, out)?;
            Some(out.emit_jump_placeholder(FnEmitter::BR_FALSE))
        } else {
            None
        };

        let body = arenas.exprs.get(arm.body).clone();
        emit_expr(arenas, state, &body, module, out)?;
        out.pop_scope();

        end_fixups.push(out.emit_jump_placeholder(FnEmitter::BR));

        if let Some(fixup) = guard_fixup { out.patch_jump_to_here(fixup)?; }
        if let Some(fixup) = next_arm_fixup { out.patch_jump_to_here(fixup)?; }
    }

    out.push(&Opcode::HaltError);
    for fixup in end_fixups { out.patch_jump_to_here(fixup)?; }
    Ok(())
}
