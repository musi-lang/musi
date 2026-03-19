//! Declaration type checking (class, given, effect, foreign).

use std::collections::HashSet;
use std::hash::BuildHasher;

use msc_ast::ExprIdx;
use msc_ast::decl::{ClassMember, ForeignDecl};
use msc_ast::expr::{Expr, InstanceBody, Param};
use msc_ast::ty_param::TyParam;
use msc_ast::util::collect_ty_var_nodes;
use msc_shared::{Idx, Span, Symbol};

use crate::checker::Checker;
use crate::checker::expr::{check, synth};
use crate::checker::ty::lower_type_expr;
use crate::def::{DefId, DefKind};
use crate::error::SemaError;
use crate::types::{EffectRow, InstanceInfo, LawObligation, RecordField, Type};

/// Checks a class/given member's default body with sig params in scope.
/// Also stores the method's function type so cross-module callers can resolve it.
fn check_member_fn<S: BuildHasher>(ck: &mut Checker<'_, S>, member: &ClassMember) {
    let ClassMember::Fn { sig, default, .. } = member else {
        return;
    };

    let parent = ck.current_scope;
    ck.current_scope = ck.scopes.push_child(parent);

    let mut param_tys = Vec::with_capacity(sig.params.len());
    for param in &sig.params {
        let param_ty = if let Some(ty) = param.ty {
            lower_type_expr(ck, ty)
        } else {
            ck.fresh_var(param.span)
        };
        param_tys.push(param_ty);
        // Only allocate param defs when a body exists. Abstract signatures
        // (default == None) have no body to reference params, so allocating
        // defs would produce false "unused parameter" warnings.
        if default.is_some() {
            let id = if let Some(&existing) = ck.ctx.pat_defs.get(&param.span) {
                existing
            } else {
                ck.defs
                    .alloc(param.name, DefKind::Param, param.span, ck.ctx.file_id)
            };
            let _prev = ck.scopes.define(ck.current_scope, param.name, id);
            ck.defs.get_mut(id).ty_info.ty = Some(param_ty);
        }
    }

    let ret_ty = if let Some(ret) = sig.ret {
        let expected = lower_type_expr(ck, ret);
        if let Some(body) = default {
            check(ck, *body, expected);
        }
        expected
    } else if let Some(body) = default {
        synth(ck, *body)
    } else {
        ck.fresh_var(sig.span)
    };

    // Store the method's function type for cross-module resolution.
    if let Some(&fn_def_id) = ck.ctx.pat_defs.get(&sig.span) {
        let fn_ty = ck.alloc_ty(Type::Fn {
            params: param_tys,
            ret: ret_ty,
            effects: EffectRow::PURE,
        });
        ck.defs.get_mut(fn_def_id).ty_info.ty = Some(fn_ty);
    }

    ck.current_scope = parent;
}

fn check_member_law<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    params: &[Param],
    body: ExprIdx,
    span: Span,
    class_ty_params: &[TyParam],
) {
    let parent = ck.current_scope;
    ck.current_scope = ck.scopes.push_child(parent);

    if params.is_empty() {
        // Implicit: law vars get the first class `over` type param as their type.
        let class_ty = match class_ty_params
            .first()
            .and_then(|p| ck.scopes.lookup(ck.current_scope, p.name))
        {
            Some(def_id) => ck.named_ty(def_id),
            None => ck.fresh_var(span),
        };

        if let Some(law_vars) = ck.ctx.law_inferred_vars.get(&span) {
            for &(sym, def_id) in law_vars {
                let _prev = ck.scopes.define(ck.current_scope, sym, def_id);
                ck.defs.get_mut(def_id).ty_info.ty = Some(class_ty);
            }
        }
    } else {
        // Explicit: lower type annotations for each param.
        for param in params {
            let param_ty = if let Some(ty) = param.ty {
                lower_type_expr(ck, ty)
            } else {
                ck.fresh_var(param.span)
            };
            let id = if let Some(&existing) = ck.ctx.pat_defs.get(&param.span) {
                existing
            } else {
                ck.defs
                    .alloc(param.name, DefKind::LawVar, param.span, ck.ctx.file_id)
            };
            let _prev = ck.scopes.define(ck.current_scope, param.name, id);
            ck.defs.get_mut(id).ty_info.ty = Some(param_ty);
        }
    }

    // Law bodies are propositions - check as Bool.
    let bool_ty = ck.named_ty(ck.ctx.well_known.bool);
    check(ck, body, bool_ty);

    ck.current_scope = parent;
}

fn check_class_members<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    members: &[ClassMember],
    ty_params: &[TyParam],
) {
    for member in members {
        match member {
            ClassMember::Fn { .. } => check_member_fn(ck, member),
            ClassMember::Law {
                params, body, span, ..
            } => check_member_law(ck, params, *body, *span, ty_params),
        }
    }
}

fn check_instance_method_coverage<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    class_name: Symbol,
    instance_members: &[ClassMember],
    span: Span,
) {
    let class_name_str = ck.ctx.interner.resolve(class_name).to_owned();

    let required_methods: Vec<String> = find_class_required_methods(ck, class_name);

    let provided: HashSet<String> = instance_members
        .iter()
        .filter_map(|m| {
            if let ClassMember::Fn { sig, .. } = m {
                Some(ck.ctx.interner.resolve(sig.name).to_owned())
            } else {
                None
            }
        })
        .collect();

    for method in &required_methods {
        if !provided.contains(method.as_str()) {
            let _d = ck.diags.report(
                &SemaError::MissingInstanceMethod {
                    class: Box::from(class_name_str.as_str()),
                    method: Box::from(method.as_str()),
                },
                span,
                ck.ctx.file_id,
            );
        }
    }
}

fn find_class_required_methods<S: BuildHasher>(
    ck: &Checker<'_, S>,
    class_name: Symbol,
) -> Vec<String> {
    let n = ck.ctx.ast.exprs.len();
    for i in 0..n {
        let idx = Idx::from_raw(u32::try_from(i).expect("expr index in range"));
        if let Expr::Class { name, members, .. } = &ck.ctx.ast.exprs[idx]
            && *name == class_name
        {
            return members
                .iter()
                .filter_map(|m| {
                    if let ClassMember::Fn {
                        sig, default: None, ..
                    } = m
                    {
                        Some(ck.ctx.interner.resolve(sig.name).to_owned())
                    } else {
                        None
                    }
                })
                .collect();
        }
    }
    vec![]
}

/// Checks a declaration expression (class, given, effect, foreign).
#[allow(clippy::too_many_lines)] // large match over declaration variants; extraction would add indirection without clarity
pub fn check_decl<S: BuildHasher>(ck: &mut Checker<'_, S>, expr_idx: ExprIdx) {
    match ck.ctx.ast.exprs[expr_idx].clone() {
        Expr::Class {
            params,
            members,
            span,
            ..
        } => {
            let parent = if params.is_empty() {
                None
            } else {
                let (p, _ids) = ck.enter_ty_param_scope(&params);
                Some(p)
            };
            check_class_members(ck, &members, &params);

            if let Some(&class_def_id) = ck.ctx.pat_defs.get(&span) {
                let method_fields: Vec<RecordField> = members
                    .iter()
                    .filter_map(|m| {
                        if let ClassMember::Fn { sig, .. } = m {
                            ck.ctx.pat_defs.get(&sig.span).map(|&fn_def| {
                                let fn_ty = ck
                                    .defs
                                    .get(fn_def)
                                    .ty_info
                                    .ty
                                    .unwrap_or_else(|| ck.error_ty());
                                RecordField {
                                    name: sig.name,
                                    ty: fn_ty,
                                    ty_params: vec![],
                                    binding: None,
                                }
                            })
                        } else {
                            None
                        }
                    })
                    .collect();

                let record_ty = ck.alloc_ty(Type::Record {
                    fields: method_fields,
                    rest: None,
                });

                let class_ty = params.first().map_or(record_ty, |first_param| {
                    ck.scopes.lookup(ck.current_scope, first_param.name).map_or(
                        record_ty,
                        |param_def| {
                            let u0 = ck.alloc_ty(Type::Universe { level: 0 });
                            ck.alloc_ty(Type::Pi {
                                param_name: first_param.name,
                                param_def,
                                param_ty: u0,
                                body: record_ty,
                            })
                        },
                    )
                });

                ck.defs.get_mut(class_def_id).ty_info.ty = Some(class_ty);

                let laws: Vec<LawObligation> = members
                    .iter()
                    .filter_map(|m| {
                        if let ClassMember::Law {
                            name,
                            params: law_params,
                            body,
                            span: law_span,
                        } = m
                        {
                            let param_tys: Vec<_> = law_params
                                .iter()
                                .filter_map(|p| p.ty.map(|ty| lower_type_expr(ck, ty)))
                                .collect();
                            Some(LawObligation {
                                name: *name,
                                params: param_tys,
                                body_expr: *body,
                                span: *law_span,
                            })
                        } else {
                            None
                        }
                    })
                    .collect();
                ck.defs.get_mut(class_def_id).law_obligations = laws;
            }

            if let Some(p) = parent {
                ck.current_scope = p;
            }
        }
        Expr::Instance {
            target,
            params,
            body,
            span,
            ..
        } => match body {
            InstanceBody::Manual { members } => {
                check_instance(ck, target, params, &members, span);
            }
            InstanceBody::Via { delegate, .. } => {
                check_instance_via(ck, target, params, delegate, span);
            }
            InstanceBody::Derives { span, .. } => {
                let _d = ck.diags.report(
                    &SemaError::Unsupported {
                        feature: "derives".into(),
                    },
                    span,
                    ck.ctx.file_id,
                );
            }
        },
        Expr::Effect { ops, .. } => {
            for op in &ops {
                let _op_ty = lower_type_expr(ck, op.ty);
            }
        }
        Expr::Foreign { decls, .. } => {
            for decl in &decls {
                if let ForeignDecl::Fn { ty, span, .. } = decl {
                    let mut ty_params = vec![];
                    collect_ty_var_nodes(*ty, ck.ctx.ast, &mut ty_params);
                    ty_params.retain(|p| ck.scopes.lookup(ck.current_scope, p.name).is_none());
                    let (parent, ty_param_defs) = if ty_params.is_empty() {
                        (None, vec![])
                    } else {
                        let (p, ids) = ck.enter_ty_param_scope(&ty_params);
                        (Some(p), ids)
                    };
                    let fn_ty = lower_type_expr(ck, *ty);
                    if let Some(&def_id) = ck.ctx.pat_defs.get(span) {
                        ck.defs.get_mut(def_id).ty_info.ty = Some(fn_ty);
                        ck.defs.get_mut(def_id).ty_info.ty_params = ty_param_defs;
                    }
                    if let Some(p) = parent {
                        ck.current_scope = p;
                    }
                }
            }
        }
        _ => {}
    }
}

fn extract_class_name_from_expr<S: BuildHasher>(
    ck: &Checker<'_, S>,
    expr: ExprIdx,
) -> Option<Symbol> {
    match &ck.ctx.ast.exprs[expr] {
        Expr::Name { name_ref, .. } => Some(ck.ctx.ast.name_refs[*name_ref].name),
        Expr::TypeApp { callee, .. } => {
            if let Expr::Name { name_ref, .. } = &ck.ctx.ast.exprs[*callee] {
                Some(ck.ctx.ast.name_refs[*name_ref].name)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn check_instance_via<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    target: ExprIdx,
    params: Vec<TyParam>,
    delegate: ExprIdx,
    span: Span,
) {
    // Extract names BEFORE entering the ty-param scope. Both `target` and
    // `delegate` are class-application expressions like `Ord of Int` or `Ord of 'A`.
    // The class name is the callee; the first argument is the concrete type.
    let (target_name_opt, first_arg_opt) = match &ck.ctx.ast.exprs[target] {
        Expr::Name { name_ref, .. } => (Some(ck.ctx.ast.name_refs[*name_ref].name), None),
        Expr::TypeApp { callee, args, .. } => {
            let name = if let Expr::Name { name_ref, .. } = &ck.ctx.ast.exprs[*callee] {
                Some(ck.ctx.ast.name_refs[*name_ref].name)
            } else {
                None
            };
            (name, args.first().copied())
        }
        _ => (None, None),
    };

    let delegate_name_opt = extract_class_name_from_expr(ck, delegate);

    let Some(target_name) = target_name_opt else {
        return;
    };

    let target_name_str = ck.ctx.interner.resolve(target_name).to_owned();
    let delegate_name_str = delegate_name_opt.map(|n| ck.ctx.interner.resolve(n).to_owned());

    let Some(delegate_name) = delegate_name_opt else {
        // Delegate expression has no resolvable class name - resolver will have
        // reported any undefined-name error; skip silently here.
        return;
    };

    // Mirror `check_instance`: enter a ty-param scope with all names from the
    // target expression (including concrete names like `Ord` and `Int`). This
    // ensures `class_def` is a freshly-allocated DefId consistent with how
    // future `find_instance_method` / `resolve_obligations` calls will look
    // things up (they also go through the same scope machinery).
    let mut all_params: Vec<TyParam> = params;
    collect_ty_var_nodes(target, ck.ctx.ast, &mut all_params);
    all_params.retain(|p| ck.scopes.lookup(ck.current_scope, p.name).is_none());

    let parent = if all_params.is_empty() {
        None
    } else {
        let (p, _ids) = ck.enter_ty_param_scope(&all_params);
        Some(p)
    };

    let class_def_opt = ck.scopes.lookup(ck.current_scope, target_name);
    // delegate_class_def_opt is only used to confirm the class is in scope;
    // the actual instance search uses name matching (see comment below).
    let delegate_class_def_opt = ck.scopes.lookup(ck.current_scope, delegate_name);

    if let (Some(class_def), Some(_)) = (class_def_opt, delegate_class_def_opt) {
        let target_ty = if let Some(first_arg) = first_arg_opt {
            lower_type_expr(ck, first_arg)
        } else {
            ck.fresh_var(span)
        };

        // Search by class name rather than DefId: `check_instance` allocates
        // a fresh def for the class name inside its ty-param scope, so the
        // `class` field in each `InstanceInfo` is the name's DefId from THAT
        // scope allocation, which changes across calls. Matching by the
        // interned name symbol is stable.
        let delegate_members_opt = ck
            .store
            .instances
            .iter()
            .find(|inst| ck.defs.get(inst.class).name == delegate_name)
            .map(|inst| inst.members.clone());

        if let Some(delegate_members) = delegate_members_opt {
            ck.store.instances.push(InstanceInfo {
                class: class_def,
                target: target_ty,
                params: vec![],
                constraints: vec![],
                members: delegate_members,
                span,
            });
        } else {
            let _d = ck.diags.report(
                &SemaError::NoDelegateInstance {
                    class: Box::from(target_name_str.as_str()),
                    delegate: Box::from(delegate_name_str.as_deref().unwrap_or("<unknown>")),
                },
                span,
                ck.ctx.file_id,
            );
        }
    }
    // If one or both class names couldn't be resolved, the resolver will have
    // reported undefined-name errors; skip silently here.

    if let Some(p) = parent {
        ck.current_scope = p;
    }
}

fn check_instance<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    target: ExprIdx,
    params: Vec<TyParam>,
    members: &[ClassMember],
    span: Span,
) {
    let mut all_params: Vec<TyParam> = params;
    collect_ty_var_nodes(target, ck.ctx.ast, &mut all_params);
    all_params.retain(|p| ck.scopes.lookup(ck.current_scope, p.name).is_none());
    let parent = if all_params.is_empty() {
        None
    } else {
        let (p, _ids) = ck.enter_ty_param_scope(&all_params);
        Some(p)
    };
    check_class_members(ck, members, &all_params);
    // Extract class name and first type arg from the target expression.
    let (target_name_opt, first_arg_opt) = match &ck.ctx.ast.exprs[target] {
        Expr::Name { name_ref, .. } => (Some(ck.ctx.ast.name_refs[*name_ref].name), None),
        Expr::TypeApp { callee, args, .. } => {
            let name = if let Expr::Name { name_ref, .. } = &ck.ctx.ast.exprs[*callee] {
                Some(ck.ctx.ast.name_refs[*name_ref].name)
            } else {
                None
            };
            (name, args.first().copied())
        }
        _ => (None, None),
    };
    let Some(target_name) = target_name_opt else {
        if let Some(p) = parent {
            ck.current_scope = p;
        }
        return;
    };
    check_instance_method_coverage(ck, target_name, members, span);

    if let Some(class_def) = ck.scopes.lookup(ck.current_scope, target_name) {
        let target_ty = if let Some(first_arg) = first_arg_opt {
            lower_type_expr(ck, first_arg)
        } else {
            ck.fresh_var(span)
        };
        let member_defs: Vec<(Symbol, DefId)> = members
            .iter()
            .filter_map(|m| {
                if let ClassMember::Fn { sig, .. } = m {
                    ck.ctx.pat_defs.get(&sig.span).map(|&id| (sig.name, id))
                } else {
                    None
                }
            })
            .collect();
        ck.store.instances.push(InstanceInfo {
            class: class_def,
            target: target_ty,
            params: vec![],
            constraints: vec![],
            members: member_defs,
            span,
        });
    }

    if let Some(p) = parent {
        ck.current_scope = p;
    }
}
