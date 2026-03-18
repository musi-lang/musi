//! Lowering AST type expressions (`Expr` type variants) to semantic `Type`.

use music_ast::expr::{Arrow, EffectItem, EffectSet, Expr, FieldKey};
use music_ast::ExprIdx;
use music_shared::{Span, Symbol};
use std::hash::BuildHasher;

use crate::checker::Checker;
use crate::def::{DefId, DefKind};
use crate::error::SemaError;
use crate::types::{EffectEntry, EffectRow, Type, TypeIdx};

/// Looks up `name` in scope, reporting `UndefinedName` if missing.
fn lookup_name_or_error<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    name: Symbol,
    span: Span,
) -> Option<DefId> {
    if let Some(def_id) = ck.scopes.lookup(ck.current_scope, name) {
        Some(def_id)
    } else {
        let name_str = ck.ctx.interner.resolve(name);
        let _d = ck.diags.report(
            &SemaError::UndefinedName {
                name: Box::from(name_str),
            },
            span,
            ck.ctx.file_id,
        );
        None
    }
}

/// Lowers an AST type-expression node to a semantic `Type` in the checker's arena.
pub fn lower_type_expr<S: BuildHasher>(ck: &mut Checker<'_, S>, expr_idx: ExprIdx) -> TypeIdx {
    match ck.ctx.ast.exprs[expr_idx].clone() {
        Expr::Name { name_ref, span } => {
            let nr = ck.ctx.ast.name_refs[name_ref];
            if let Some(def_id) = lookup_name_or_error(ck, nr.name, span) {
                ck.alloc_ty(Type::Named {
                    def: def_id,
                    args: vec![],
                })
            } else {
                ck.error_ty()
            }
        }
        Expr::TypeApp { callee, args, span } => lower_type_app(ck, callee, &args, span),
        Expr::OptionType { inner, .. } => {
            let inner_ty = lower_type_expr(ck, inner);
            let def = ck.ctx.well_known.option;
            ck.alloc_ty(Type::Named {
                def,
                args: vec![inner_ty],
            })
        }
        Expr::Field {
            object,
            field: FieldKey::Name { name, .. },
            span,
            ..
        } => lower_type_qualified_expr(ck, object, name, &[], span),
        Expr::FnType {
            params,
            ret,
            arrow,
            effects,
            ..
        } => lower_type_fn(ck, &params, ret, arrow, effects.as_ref()),
        Expr::ProductType { fields, .. } => {
            if fields.is_empty() {
                return ck.named_ty(ck.ctx.well_known.unit);
            }
            let elems: Vec<_> = fields.iter().map(|&f| lower_type_expr(ck, f)).collect();
            ck.alloc_ty(Type::Tuple { elems })
        }
        Expr::SumType { variants, .. } => {
            let variant_tys: Vec<_> = variants.iter().map(|&v| lower_type_expr(ck, v)).collect();
            ck.alloc_ty(Type::AnonSum {
                variants: variant_tys,
            })
        }
        Expr::ArrayType { elem, len, .. } => {
            let elem_ty = lower_type_expr(ck, elem);
            ck.alloc_ty(Type::Array { elem: elem_ty, len })
        }
        Expr::PiType {
            param,
            param_ty,
            body,
            span,
        } => {
            let lowered_param_ty = lower_type_expr(ck, param_ty);
            let parent = ck.current_scope;
            ck.current_scope = ck.scopes.push_child(parent);
            let param_def = ck.defs.alloc(param, DefKind::Param, span, ck.ctx.file_id);
            let _prev = ck.scopes.define(ck.current_scope, param, param_def);
            ck.defs.get_mut(param_def).ty_info.ty = Some(lowered_param_ty);
            let lowered_body = lower_type_expr(ck, body);
            ck.current_scope = parent;
            ck.alloc_ty(Type::Pi {
                param_name: param,
                param_def,
                param_ty: lowered_param_ty,
                body: lowered_body,
            })
        }
        _ => ck.error_ty(),
    }
}

fn lower_type_app<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    callee: ExprIdx,
    args: &[ExprIdx],
    span: Span,
) -> TypeIdx {
    match ck.ctx.ast.exprs[callee].clone() {
        Expr::Name { name_ref, .. } => {
            let nr = ck.ctx.ast.name_refs[name_ref];
            if let Some(def_id) = lookup_name_or_error(ck, nr.name, span) {
                let lowered_args: Vec<_> = args.iter().map(|&a| lower_type_expr(ck, a)).collect();
                ck.alloc_ty(Type::Named {
                    def: def_id,
                    args: lowered_args,
                })
            } else {
                ck.error_ty()
            }
        }
        Expr::Field {
            object,
            field: FieldKey::Name { name, .. },
            span: field_span,
            ..
        } => lower_type_qualified_expr(ck, object, name, args, field_span),
        _ => ck.error_ty(),
    }
}

fn lower_type_qualified_expr<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    object: ExprIdx,
    name: Symbol,
    args: &[ExprIdx],
    span: Span,
) -> TypeIdx {
    let mod_name = match &ck.ctx.ast.exprs[object] {
        Expr::Name { name_ref, .. } => {
            let nr = ck.ctx.ast.name_refs[*name_ref];
            nr.name
        }
        _ => return ck.error_ty(),
    };
    let Some(mod_def) = lookup_name_or_error(ck, mod_name, span) else {
        return ck.error_ty();
    };
    let Some(ty_idx) = ck.defs.get(mod_def).ty_info.ty else {
        let name_str = ck.ctx.interner.resolve(name);
        let _d = ck.diags.report(
            &SemaError::UndefinedName {
                name: Box::from(name_str),
            },
            span,
            ck.ctx.file_id,
        );
        return ck.error_ty();
    };
    let Type::Record { ref fields, .. } = ck.store.types[ty_idx] else {
        let name_str = ck.ctx.interner.resolve(name);
        let _d = ck.diags.report(
            &SemaError::UndefinedName {
                name: Box::from(name_str),
            },
            span,
            ck.ctx.file_id,
        );
        return ck.error_ty();
    };
    let fields = fields.clone();
    if let Some(field) = fields.iter().find(|f| f.name == name) {
        let lowered_args: Vec<_> = args.iter().map(|&a| lower_type_expr(ck, a)).collect();
        if lowered_args.is_empty() {
            field.ty
        } else {
            ck.alloc_ty(Type::Named {
                def: mod_def,
                args: lowered_args,
            })
        }
    } else {
        let name_str = ck.ctx.interner.resolve(name);
        let _d = ck.diags.report(
            &SemaError::UndefinedName {
                name: Box::from(name_str),
            },
            span,
            ck.ctx.file_id,
        );
        ck.error_ty()
    }
}

fn lower_type_fn<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    params: &[ExprIdx],
    ret: ExprIdx,
    arrow: Arrow,
    effects: Option<&EffectSet>,
) -> TypeIdx {
    let param_tys: Vec<_> = params.iter().map(|&p| lower_type_expr(ck, p)).collect();
    let ret_ty = lower_type_expr(ck, ret);
    let effect_row = match (arrow, effects) {
        (Arrow::Pure, _) => EffectRow::PURE,
        (Arrow::Effectful, Some(eff_set)) => lower_effect_set(ck, eff_set),
        (Arrow::Effectful, None) => EffectRow {
            effects: vec![],
            row_var: None,
        },
    };
    ck.alloc_ty(Type::Fn {
        params: param_tys,
        ret: ret_ty,
        effects: effect_row,
    })
}

fn lower_effect_set<S: BuildHasher>(ck: &mut Checker<'_, S>, eff_set: &EffectSet) -> EffectRow {
    let mut effects = vec![];
    let mut row_var = None;
    for item in &eff_set.effects {
        match item {
            EffectItem::Named { name, arg, span } => {
                if let Some(def_id) = lookup_name_or_error(ck, *name, *span) {
                    let args = arg
                        .iter()
                        .map(|&expr_idx| lower_type_expr(ck, expr_idx))
                        .collect();
                    effects.push(EffectEntry { def: def_id, args });
                }
            }
            EffectItem::Var { span, .. } => {
                let var_id = ck.store.unify.fresh_var_id(*span);
                row_var = Some(var_id);
            }
        }
    }
    EffectRow { effects, row_var }
}
