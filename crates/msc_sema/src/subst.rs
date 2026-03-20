//! Type substitution for dependent types.

use msc_shared::Arena;

use crate::def::DefId;
use crate::types::{RecordField, SumVariant, Type, TypeIdx};

/// Substitutes all occurrences of `param_def` in `ty` with `replacement`.
///
/// Used for Π-type application: `(Π(x : A). B)(v)` becomes `B[x := v]`.
#[must_use]
#[expect(
    clippy::too_many_lines,
    reason = "exhaustive match over all type variants"
)]
pub fn subst_type(
    ty: TypeIdx,
    param_def: DefId,
    replacement: TypeIdx,
    arena: &mut Arena<Type>,
) -> TypeIdx {
    match arena[ty].clone() {
        Type::Named { def, args } if def == param_def && args.is_empty() => replacement,
        Type::Named { def, args } => {
            let new_args: Vec<_> = args
                .iter()
                .map(|&a| subst_type(a, param_def, replacement, arena))
                .collect();
            if new_args == args {
                ty
            } else {
                arena.alloc(Type::Named {
                    def,
                    args: new_args,
                })
            }
        }
        Type::Fn {
            params,
            ret,
            effects,
        } => {
            let new_params: Vec<_> = params
                .iter()
                .map(|&p| subst_type(p, param_def, replacement, arena))
                .collect();
            let new_ret = subst_type(ret, param_def, replacement, arena);
            if new_params == params && new_ret == ret {
                ty
            } else {
                arena.alloc(Type::Fn {
                    params: new_params,
                    ret: new_ret,
                    effects,
                })
            }
        }
        Type::Tuple { elems } => {
            let new_elems: Vec<_> = elems
                .iter()
                .map(|&e| subst_type(e, param_def, replacement, arena))
                .collect();
            if new_elems == elems {
                ty
            } else {
                arena.alloc(Type::Tuple { elems: new_elems })
            }
        }
        Type::Record { fields, rest } => {
            let new_fields: Vec<_> = fields
                .iter()
                .map(|f| RecordField {
                    name: f.name,
                    ty: subst_type(f.ty, param_def, replacement, arena),
                    ty_params: f.ty_params.clone(),
                    binding: f.binding,
                })
                .collect();
            let new_rest = rest.map(|r| subst_type(r, param_def, replacement, arena));
            let changed = new_fields
                .iter()
                .zip(fields.iter())
                .any(|(a, b)| a.ty != b.ty)
                || new_rest != rest;
            if changed {
                arena.alloc(Type::Record {
                    fields: new_fields,
                    rest: new_rest,
                })
            } else {
                ty
            }
        }
        Type::Sum { variants } => {
            let new_variants: Vec<_> = variants
                .iter()
                .map(|v| SumVariant {
                    name: v.name,
                    fields: v
                        .fields
                        .iter()
                        .map(|&f| subst_type(f, param_def, replacement, arena))
                        .collect(),
                })
                .collect();
            arena.alloc(Type::Sum {
                variants: new_variants,
            })
        }
        Type::AnonSum { variants } => {
            let new_variants: Vec<_> = variants
                .iter()
                .map(|&v| subst_type(v, param_def, replacement, arena))
                .collect();
            if new_variants == variants {
                ty
            } else {
                arena.alloc(Type::AnonSum {
                    variants: new_variants,
                })
            }
        }
        Type::Array { elem, len } => {
            let new_elem = subst_type(elem, param_def, replacement, arena);
            if new_elem == elem {
                ty
            } else {
                arena.alloc(Type::Array {
                    elem: new_elem,
                    len,
                })
            }
        }
        Type::Ref { inner } => {
            let new_inner = subst_type(inner, param_def, replacement, arena);
            if new_inner == inner {
                ty
            } else {
                arena.alloc(Type::Ref { inner: new_inner })
            }
        }
        Type::Pi {
            param_name,
            param_def: pd,
            param_ty,
            body,
        } => {
            let new_param_ty = subst_type(param_ty, param_def, replacement, arena);
            // Capture avoidance: if Pi binds the same def, don't substitute in body.
            let new_body = if pd == param_def {
                body
            } else {
                subst_type(body, param_def, replacement, arena)
            };
            if new_param_ty == param_ty && new_body == body {
                ty
            } else {
                arena.alloc(Type::Pi {
                    param_name,
                    param_def: pd,
                    param_ty: new_param_ty,
                    body: new_body,
                })
            }
        }
        Type::Quantified {
            kind,
            params,
            constraints,
            body,
        } => {
            let new_body = subst_type(body, param_def, replacement, arena);
            if new_body == body {
                ty
            } else {
                arena.alloc(Type::Quantified {
                    kind,
                    params,
                    constraints,
                    body: new_body,
                })
            }
        }
        Type::Var(_) | Type::Rigid(_) | Type::Universe { .. } | Type::Error => ty,
    }
}
