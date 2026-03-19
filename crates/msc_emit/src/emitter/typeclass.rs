//! Typeclass dictionary construction at call sites.

use msc_ast::ExprIdx;
use msc_sema::DefId;
use msc_sema::def::DefKind;
use msc_sema::types::Type;
use msc_sema::unify::types_match;
use msc_shared::Symbol;

use crate::const_pool::ConstValue;
use crate::error::EmitError;
use crate::error::EmitResult;

use super::Emitter;
use super::FnCtx;

/// Returns the field index of a method within a class's member list (declaration order).
pub(super) fn class_method_index(
    em: &Emitter<'_>,
    class_def: DefId,
    method_sym: Symbol,
) -> EmitResult<u32> {
    let members: Vec<(Symbol, DefId)> = em
        .sema
        .defs
        .iter()
        .filter(|d| d.parent == Some(class_def) && d.kind == DefKind::Fn)
        .map(|d| (d.name, d.id))
        .collect();

    for (i, &(sym, _)) in members.iter().enumerate() {
        if sym == method_sym {
            return u32::try_from(i).map_err(|_| EmitError::overflow("class method index"));
        }
    }

    Err(EmitError::UnsupportedFeature {
        desc: "class method not found for dictionary lookup".into(),
    })
}

/// Emits dictionary construction at a call site for a constrained function.
/// For each constraint, builds a product of method fn-id values from the
/// concrete instance that satisfies the constraint at this call site.
pub(super) fn emit_dict_for_call(
    em: &mut Emitter<'_>,
    fc: &mut FnCtx,
    callee_def: DefId,
    callee_expr: ExprIdx,
) -> EmitResult<u32> {
    let constraints = match em.active_fn_constraints().get(&callee_def) {
        Some(c) => c.clone(),
        None => return Ok(0),
    };

    let mut dict_count = 0;
    for constraint in &constraints {
        let concrete_ty = constraint.args.first().and_then(|&arg| {
            let resolved = em.sema.unify.resolve(arg, &em.sema.types);
            match &em.sema.types[resolved] {
                Type::Var(_) | Type::Rigid(_) => None,
                _ => Some(resolved),
            }
        });

        if let Some(concrete_ty) = concrete_ty {
            let instance = em.sema.instances.iter().find(|inst| {
                inst.class == constraint.class
                    && types_match(&em.sema.types, &em.sema.unify, inst.target, concrete_ty)
            });

            if let Some(inst) = instance {
                let class_members: Vec<(Symbol, DefId)> = em
                    .sema
                    .defs
                    .iter()
                    .filter(|d| d.parent == Some(constraint.class) && d.kind == DefKind::Fn)
                    .map(|d| (d.name, d.id))
                    .collect();

                let method_count = class_members.len();
                for (class_sym, _) in &class_members {
                    let inst_method = inst.members.iter().find(|(s, _)| s == class_sym);
                    if let Some((_, method_def)) = inst_method {
                        if let Some(&fn_id) = em.fn_map.get(method_def) {
                            let cst_idx = em.cp.intern(&ConstValue::Int(i64::from(fn_id)))?;
                            fc.fe.emit_ld_cst(cst_idx);
                        } else {
                            return Err(EmitError::UnsupportedFeature {
                                desc: "instance method not compiled for dict construction".into(),
                            });
                        }
                    } else {
                        return Err(EmitError::UnsupportedFeature {
                            desc: "instance missing method required by class".into(),
                        });
                    }
                }
                let mc =
                    u32::try_from(method_count).map_err(|_| EmitError::overflow("method count"))?;
                let sp =
                    i32::try_from(method_count).map_err(|_| EmitError::overflow("method count"))?;
                fc.fe.emit_mk_prd(mc, sp)?;
            } else {
                return Err(EmitError::UnsupportedFeature {
                    desc: "no instance found for call-site constraint".into(),
                });
            }
        } else if let Some(&dict_slot) = fc.dict_slots.get(&constraint.class) {
            fc.fe.emit_ld_loc(dict_slot);
        } else {
            return Err(EmitError::UnsupportedFeature {
                desc: "no dictionary to forward for nested generic call".into(),
            });
        }
        let _ = callee_expr;
        dict_count += 1;
    }

    Ok(dict_count)
}
