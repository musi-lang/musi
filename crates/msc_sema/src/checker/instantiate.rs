//! Polymorphic type instantiation: substitute and freshen.

use std::collections::{HashMap, HashSet};
use std::hash::{BuildHasher, Hash};

use crate::checker::Checker;
use crate::def::DefId;
use crate::types::{RecordField, SumVariant, TyVarId, Type, TypeIdx};
use msc_shared::Span;

pub(super) fn substitute_ty<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    ty: TypeIdx,
    subst: &HashMap<DefId, TypeIdx>,
) -> TypeIdx {
    let ty = ck.resolve_ty(ty);
    match &ck.store.types[ty] {
        Type::Named { def, args } if args.is_empty() && subst.contains_key(def) => subst[def],
        Type::Named { def, args } => {
            let (def, args) = (*def, args.clone());
            let new_args = substitute_list(ck, &args, subst);
            ck.alloc_ty(Type::Named {
                def,
                args: new_args,
            })
        }
        Type::Fn {
            params,
            ret,
            effects,
        } => {
            let (params, ret, effects) = (params.clone(), *ret, effects.clone());
            let params = substitute_list(ck, &params, subst);
            let ret = substitute_ty(ck, ret, subst);
            ck.alloc_ty(Type::Fn {
                params,
                ret,
                effects,
            })
        }
        Type::Tuple { elems } => {
            let elems = elems.clone();
            let elems = substitute_list(ck, &elems, subst);
            ck.alloc_ty(Type::Tuple { elems })
        }
        Type::AnonSum { variants } => {
            let variants = variants.clone();
            let variants = substitute_list(ck, &variants, subst);
            ck.alloc_ty(Type::AnonSum { variants })
        }
        Type::Record { fields, rest } => {
            let (fields, rest) = (fields.clone(), *rest);
            let fields = substitute_record_fields(ck, &fields, subst);
            ck.alloc_ty(Type::Record { fields, rest })
        }
        Type::Sum { variants } => {
            let variants = variants.clone();
            let variants = substitute_sum_variants(ck, &variants, subst);
            ck.alloc_ty(Type::Sum { variants })
        }
        Type::Array { elem, len } => {
            let (elem, len) = (*elem, *len);
            let elem = substitute_ty(ck, elem, subst);
            ck.alloc_ty(Type::Array { elem, len })
        }
        Type::Ref { inner } => {
            let inner = *inner;
            let inner = substitute_ty(ck, inner, subst);
            ck.alloc_ty(Type::Ref { inner })
        }
        Type::Quantified {
            kind,
            params,
            constraints,
            body,
        } => {
            let (kind, params, constraints, body) =
                (*kind, params.clone(), constraints.clone(), *body);
            let body = substitute_ty(ck, body, subst);
            ck.alloc_ty(Type::Quantified {
                kind,
                params,
                constraints,
                body,
            })
        }
        Type::Pi {
            param_name,
            param_def,
            param_ty,
            body,
        } => {
            let (param_name, param_def, param_ty, body) =
                (*param_name, *param_def, *param_ty, *body);
            // Don't substitute in body if this Pi's own param shadows the substituted def.
            let new_param_ty = substitute_ty(ck, param_ty, subst);
            let new_body = if subst.contains_key(&param_def) {
                body
            } else {
                substitute_ty(ck, body, subst)
            };
            ck.alloc_ty(Type::Pi {
                param_name,
                param_def,
                param_ty: new_param_ty,
                body: new_body,
            })
        }
        Type::Universe { .. } | Type::Var(_) | Type::Rigid(_) | Type::Error => ty,
    }
}

/// Deep-copies a type, replacing both:
///
/// 1. `Named(DefId)` nodes matching type parameter defs (explicit annotations)
/// 2. Unbound unification variables (inferred polymorphic types)
///
/// with fresh unification variables. Co-occurring nodes get the same fresh var.
pub(super) fn freshen_poly<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    ty: TypeIdx,
    ty_param_defs: &[DefId],
    span: Span,
) -> TypeIdx {
    let param_set: HashSet<DefId> = ty_param_defs.iter().copied().collect();
    let mut var_map: HashMap<TyVarId, TypeIdx> = HashMap::new();
    let mut def_map: HashMap<DefId, TypeIdx> = HashMap::new();
    freshen_walk(ck, ty, span, &param_set, &mut var_map, &mut def_map)
}

/// Get or create a fresh type variable for a key, caching in `map`.
fn freshen_key<S: BuildHasher, K: Eq + Hash + Copy>(
    ck: &mut Checker<'_, S>,
    span: Span,
    map: &mut HashMap<K, TypeIdx>,
    key: K,
) -> TypeIdx {
    if let Some(&fresh) = map.get(&key) {
        return fresh;
    }
    let fresh = ck.fresh_var(span);
    let _prev = map.insert(key, fresh);
    fresh
}

/// Freshen each type in a list.
fn freshen_list<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    tys: &[TypeIdx],
    span: Span,
    param_set: &HashSet<DefId>,
    var_map: &mut HashMap<TyVarId, TypeIdx>,
    def_map: &mut HashMap<DefId, TypeIdx>,
) -> Vec<TypeIdx> {
    tys.iter()
        .map(|&t| freshen_walk(ck, t, span, param_set, var_map, def_map))
        .collect()
}

#[allow(clippy::too_many_lines)] // large match over all type variants; extraction would add indirection without clarity
fn freshen_walk<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    ty: TypeIdx,
    span: Span,
    param_set: &HashSet<DefId>,
    var_map: &mut HashMap<TyVarId, TypeIdx>,
    def_map: &mut HashMap<DefId, TypeIdx>,
) -> TypeIdx {
    let resolved = ck.resolve_ty(ty);
    match &ck.store.types[resolved] {
        Type::Var(v) => freshen_key(ck, span, var_map, *v),
        Type::Named { def, args } if args.is_empty() && param_set.contains(def) => {
            freshen_key(ck, span, def_map, *def)
        }
        Type::Named { def, args } => {
            let (def, args) = (*def, args.clone());
            let new_args = freshen_list(ck, &args, span, param_set, var_map, def_map);
            ck.alloc_ty(Type::Named {
                def,
                args: new_args,
            })
        }
        Type::Fn {
            params,
            ret,
            effects,
        } => {
            let (params, ret, effects) = (params.clone(), *ret, effects.clone());
            let new_params = freshen_list(ck, &params, span, param_set, var_map, def_map);
            let new_ret = freshen_walk(ck, ret, span, param_set, var_map, def_map);
            ck.alloc_ty(Type::Fn {
                params: new_params,
                ret: new_ret,
                effects,
            })
        }
        Type::Tuple { elems } => {
            let elems = elems.clone();
            let new_elems = freshen_list(ck, &elems, span, param_set, var_map, def_map);
            ck.alloc_ty(Type::Tuple { elems: new_elems })
        }
        Type::Array { elem, len } => {
            let (elem, len) = (*elem, *len);
            let new_elem = freshen_walk(ck, elem, span, param_set, var_map, def_map);
            ck.alloc_ty(Type::Array {
                elem: new_elem,
                len,
            })
        }
        Type::Record { fields, rest } => {
            let (fields, rest) = (fields.clone(), *rest);
            let new_fields: Vec<_> = fields
                .iter()
                .map(|f| RecordField {
                    name: f.name,
                    ty: freshen_walk(ck, f.ty, span, param_set, var_map, def_map),
                    ty_params: f.ty_params.clone(),
                    binding: f.binding,
                })
                .collect();
            let new_rest = rest.map(|r| freshen_walk(ck, r, span, param_set, var_map, def_map));
            ck.alloc_ty(Type::Record {
                fields: new_fields,
                rest: new_rest,
            })
        }
        Type::Sum { variants } => {
            let variants = variants.clone();
            let new_variants: Vec<_> = variants
                .iter()
                .map(|v| SumVariant {
                    name: v.name,
                    fields: freshen_list(ck, &v.fields, span, param_set, var_map, def_map),
                })
                .collect();
            ck.alloc_ty(Type::Sum {
                variants: new_variants,
            })
        }
        Type::Ref { inner } => {
            let inner = *inner;
            let new_inner = freshen_walk(ck, inner, span, param_set, var_map, def_map);
            ck.alloc_ty(Type::Ref { inner: new_inner })
        }
        Type::AnonSum { variants } => {
            let variants = variants.clone();
            let new_variants = freshen_list(ck, &variants, span, param_set, var_map, def_map);
            ck.alloc_ty(Type::AnonSum {
                variants: new_variants,
            })
        }
        Type::Quantified {
            kind,
            params,
            constraints,
            body,
        } => {
            let (kind, params, constraints, body) =
                (*kind, params.clone(), constraints.clone(), *body);
            let new_body = freshen_walk(ck, body, span, param_set, var_map, def_map);
            ck.alloc_ty(Type::Quantified {
                kind,
                params,
                constraints,
                body: new_body,
            })
        }
        Type::Pi {
            param_name,
            param_def,
            param_ty,
            body,
        } => {
            let (param_name, param_def, param_ty, body) =
                (*param_name, *param_def, *param_ty, *body);
            let new_param_ty = freshen_walk(ck, param_ty, span, param_set, var_map, def_map);
            // Capture avoidance: if this Pi's param is in param_set, don't freshen body.
            let new_body = if param_set.contains(&param_def) {
                body
            } else {
                freshen_walk(ck, body, span, param_set, var_map, def_map)
            };
            ck.alloc_ty(Type::Pi {
                param_name,
                param_def,
                param_ty: new_param_ty,
                body: new_body,
            })
        }
        Type::Universe { .. } | Type::Rigid(_) | Type::Error => resolved,
    }
}

fn substitute_list<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    tys: &[TypeIdx],
    subst: &HashMap<DefId, TypeIdx>,
) -> Vec<TypeIdx> {
    tys.iter().map(|&t| substitute_ty(ck, t, subst)).collect()
}

fn substitute_record_fields<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    fields: &[RecordField],
    subst: &HashMap<DefId, TypeIdx>,
) -> Vec<RecordField> {
    fields
        .iter()
        .map(|f| RecordField {
            name: f.name,
            ty: substitute_ty(ck, f.ty, subst),
            ty_params: f.ty_params.clone(),
            binding: f.binding,
        })
        .collect()
}

fn substitute_sum_variants<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    variants: &[SumVariant],
    subst: &HashMap<DefId, TypeIdx>,
) -> Vec<SumVariant> {
    variants
        .iter()
        .map(|v| SumVariant {
            name: v.name,
            fields: substitute_list(ck, &v.fields, subst),
        })
        .collect()
}
