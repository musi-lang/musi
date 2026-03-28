use music_ast::common::MemberDecl;
use music_ast::expr::{ExprKind, InstanceBody, InstanceDef};
use music_ast::ty::TyKind;
use music_ast::{ExprId, TyId};
use music_owned::types::BuiltinType;
use music_shared::{Span, Symbol, SymbolList};

use super::SemaDb;
use crate::dispatch::builtin_has_instance;
use crate::env::InstanceEntry;
use crate::errors::{SemaError, SemaErrorKind};
use crate::types::{NominalKey, SemaTypeId, SemaTypeList, Ty};

impl SemaDb {
    pub(super) fn check_constraints_at_call(
        &mut self,
        callee: ExprId,
        args: &[ExprId],
        span: Span,
    ) {
        let callee_kind = self.db.ast.exprs.get(callee).kind.clone();
        let callee_name = match callee_kind {
            ExprKind::Var(ident) => ident.name,
            _ => return,
        };
        let Some(def_id) = self.find_def_for_name(callee_name) else {
            return;
        };
        let Some(constraints) = self.def_constraints.get(&def_id).cloned() else {
            return;
        };
        if constraints.is_empty() || args.is_empty() {
            return;
        }

        let arg_tys: SemaTypeList = args
            .iter()
            .map(|&a| {
                let ty = self
                    .env
                    .type_map
                    .get(&a)
                    .copied()
                    .unwrap_or_else(|| self.env.intern(Ty::Any));
                self.env.resolve_var(ty)
            })
            .collect();

        for &(_ty_param, class_name) in &constraints {
            if let Some(&arg_ty_id) = arg_tys.first() {
                let arg_ty = self.env.types.get(arg_ty_id).clone();
                let class_str = self.db.interner.resolve(class_name);
                let satisfied = match arg_ty {
                    Ty::Builtin(bt) => builtin_has_instance(bt, class_str),
                    Ty::Any | Ty::Var(_) | Ty::Param(_) => true,
                    _ => self.find_instance_for_type(arg_ty_id, class_name),
                };
                if !satisfied {
                    self.errors.push(SemaError {
                        kind: SemaErrorKind::ConstraintNotSatisfied {
                            constraint: class_name,
                        },
                        span,
                        context: None,
                    });
                }
            }
        }
    }

    pub(super) fn find_instance_for_type(&self, ty_id: SemaTypeId, class: Symbol) -> bool {
        let ty = self.env.types.get(self.env.resolve_var(ty_id));
        if matches!(ty, Ty::Builtin(_)) {
            return true;
        }
        let Some(ty_key) = self.env.type_key(ty_id) else {
            return false;
        };
        self.env.instances.contains_key(&(class, ty_key))
    }

    pub(super) fn synth_instance_def(
        &mut self,
        expr_id: ExprId,
        inst: &InstanceDef,
        span: Span,
    ) -> SemaTypeId {
        let class_name = inst.ty.name.name;

        let (instance_ty, type_name) = if let Some(&first_arg) = inst.ty.args.first() {
            (
                self.lower_ty(first_arg),
                self.type_ctor_name(first_arg).unwrap_or(class_name),
            )
        } else {
            (
                self.env.intern(Ty::Named(NominalKey {
                    module_name: None,
                    name: class_name,
                })),
                class_name,
            )
        };

        let methods: SymbolList = match &inst.body {
            InstanceBody::Methods(members) => members
                .iter()
                .filter_map(|m| match m {
                    MemberDecl::Fn(fn_decl) => Some(super::expr::member_name_symbol(&fn_decl.name)),
                    MemberDecl::Law(_) => None,
                })
                .collect(),
            InstanceBody::Via(_) => Vec::new(),
        };

        let Some(type_key) = self.env.type_key(instance_ty) else {
            self.errors.push(SemaError {
                kind: SemaErrorKind::NoInstance { class: class_name },
                span,
                context: None,
            });
            return self.env.intern(Ty::Builtin(BuiltinType::Type));
        };

        let key = (class_name, type_key.clone());

        if self.env.instances.contains_key(&key) {
            self.errors.push(SemaError {
                kind: SemaErrorKind::DuplicateInstance {
                    class: class_name,
                    ty: type_name,
                },
                span,
                context: None,
            });
        }

        let _prev = self.env.instances.insert(
            key,
            InstanceEntry {
                span,
                methods,
                ty: type_key.clone(),
            },
        );
        let _ = self.env.instance_keys.insert(expr_id, type_key);

        self.env.intern(Ty::Builtin(BuiltinType::Type))
    }

    pub(super) fn type_ctor_name(&self, ty_id: TyId) -> Option<Symbol> {
        match &self.db.ast.types.get(ty_id).kind {
            TyKind::Named { name, .. } => Some(name.name),
            _ => None,
        }
    }
}
