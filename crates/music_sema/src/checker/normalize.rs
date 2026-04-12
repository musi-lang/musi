use std::collections::BTreeMap;

use music_arena::SliceRange;
use music_hir::{
    HirBinaryOp, HirConstraint, HirConstraintKind, HirDim, HirEffectSet, HirExprId, HirExprKind,
    HirLitKind, HirOrigin, HirParam, HirPrefixOp, HirRecordItem, HirTyField, HirTyId, HirTyKind,
};
use music_names::Symbol;

use super::exprs::check_expr;
use super::surface::surface_key;
use super::{CheckPass, DiagKind, PassBase};
use crate::api::{ConstraintFacts, ConstraintKind, DefinitionKey};
use crate::effects::{EffectKey, EffectRow};

type HirTyFieldRange = SliceRange<HirTyField>;

impl PassBase<'_, '_, '_> {
    pub fn render_ty(&self, ty: HirTyId) -> String {
        match self.ty(ty).kind {
            HirTyKind::Error => "<error>".into(),
            HirTyKind::Unknown => "Unknown".into(),
            HirTyKind::Type => "Type".into(),
            HirTyKind::Syntax => "Syntax".into(),
            HirTyKind::Any => "Any".into(),
            HirTyKind::Empty => "Empty".into(),
            HirTyKind::Unit => "Unit".into(),
            HirTyKind::Bool => "Bool".into(),
            HirTyKind::Nat => "Nat".into(),
            HirTyKind::Int => "Int".into(),
            HirTyKind::Float => "Float".into(),
            HirTyKind::String => "String".into(),
            HirTyKind::CString => "CString".into(),
            HirTyKind::CPtr => "CPtr".into(),
            HirTyKind::Module => "Module".into(),
            HirTyKind::NatLit(value) => value.to_string(),
            HirTyKind::Named { name, args } => self.render_named_ty(name, args),
            HirTyKind::Pi {
                binder,
                binder_ty,
                body,
                is_effectful,
            } => {
                let binder = self.resolve_symbol(binder);
                let arrow = if is_effectful { " ~> " } else { " -> " };
                format!(
                    "forall ({binder} : {}){arrow}{}",
                    self.render_ty(binder_ty),
                    self.render_ty(body)
                )
            }
            HirTyKind::Arrow {
                params,
                ret,
                is_effectful,
            } => self.render_arrow_ty(params, ret, is_effectful),
            HirTyKind::Sum { left, right } => {
                format!("{} + {}", self.render_ty(left), self.render_ty(right))
            }
            HirTyKind::Tuple { items } => self.render_tuple_ty(items),
            HirTyKind::Array { dims, item } => self.render_array_ty(dims, item),
            HirTyKind::Mut { inner } => format!("mut {}", self.render_ty(inner)),
            HirTyKind::Record { fields } => self.render_record_ty(fields),
        }
    }
    fn render_named_ty(&self, name: Symbol, args: SliceRange<HirTyId>) -> String {
        let mut out = String::from(self.resolve_symbol(name));
        let args = self.ty_ids(args);
        if args.is_empty() {
            return out;
        }
        out.push('[');
        for (idx, arg) in args.into_iter().enumerate() {
            if idx != 0 {
                out.push_str(", ");
            }
            out.push_str(&self.render_ty(arg));
        }
        out.push(']');
        out
    }

    fn render_arrow_ty(
        &self,
        params: SliceRange<HirTyId>,
        ret: HirTyId,
        is_effectful: bool,
    ) -> String {
        let params = self.ty_ids(params);
        let left = if params.len() == 1 {
            self.render_ty(params[0])
        } else {
            format!(
                "({})",
                params
                    .into_iter()
                    .map(|param| self.render_ty(param))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        let arrow = if is_effectful { " ~> " } else { " -> " };
        format!("{left}{arrow}{}", self.render_ty(ret))
    }

    fn render_tuple_ty(&self, items: SliceRange<HirTyId>) -> String {
        format!(
            "({})",
            self.ty_ids(items)
                .into_iter()
                .map(|item| self.render_ty(item))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    fn render_array_ty(&self, dims: SliceRange<HirDim>, item: HirTyId) -> String {
        let dims = self.dims(dims);
        let mut parts = vec![self.render_ty(item)];
        for dim in dims {
            parts.push(match dim {
                HirDim::Unknown => "_".into(),
                HirDim::Name(name) => self.resolve_symbol(name.name).into(),
                HirDim::Int(value) => value.to_string(),
            });
        }
        format!("Array[{}]", parts.join(", "))
    }

    fn render_record_ty(&self, fields: HirTyFieldRange) -> String {
        let mut parts = Vec::new();
        for field in self.ty_fields(fields) {
            parts.push(format!(
                "{} = {}",
                self.resolve_symbol(field.name),
                self.render_ty(field.ty)
            ));
        }
        format!("{{{}}}", parts.join(", "))
    }

    pub fn type_mismatch(&mut self, origin: HirOrigin, expected: HirTyId, found: HirTyId) {
        if self.ty_matches(expected, found) {
            return;
        }
        let label = format!(
            "expected {}, found {}",
            self.render_ty(expected),
            self.render_ty(found)
        );
        self.diag(origin.span, DiagKind::TypeMismatch, &label);
    }

    pub fn ty_matches(&self, expected: HirTyId, found: HirTyId) -> bool {
        if expected == found {
            return true;
        }
        let left = self.ty(expected).kind;
        let right = self.ty(found).kind;
        if matches!(left, HirTyKind::Any) {
            return true;
        }
        if let HirTyKind::Mut { inner } = right {
            if self.ty_matches(expected, inner) {
                return true;
            }
        }
        if matches!(left, HirTyKind::Error | HirTyKind::Unknown)
            || matches!(right, HirTyKind::Error | HirTyKind::Unknown)
        {
            return true;
        }
        match (left, right) {
            (HirTyKind::Type, HirTyKind::Type)
            | (HirTyKind::Syntax, HirTyKind::Syntax)
            | (HirTyKind::Any, HirTyKind::Any)
            | (HirTyKind::Empty, HirTyKind::Empty)
            | (HirTyKind::Unit, HirTyKind::Unit)
            | (HirTyKind::Bool, HirTyKind::Bool)
            | (HirTyKind::Int, HirTyKind::Int)
            | (HirTyKind::Float, HirTyKind::Float)
            | (HirTyKind::String, HirTyKind::String)
            | (HirTyKind::CString, HirTyKind::CString)
            | (HirTyKind::CPtr, HirTyKind::CPtr)
            | (HirTyKind::Module, HirTyKind::Module) => true,
            (
                HirTyKind::Named {
                    name: left_name,
                    args: left_args,
                },
                HirTyKind::Named {
                    name: right_name,
                    args: right_args,
                },
            ) => self.named_tys_match(left_name, left_args, right_name, right_args),
            (
                HirTyKind::Arrow {
                    params: left_params,
                    ret: left_ret,
                    is_effectful: left_effectful,
                },
                HirTyKind::Arrow {
                    params: right_params,
                    ret: right_ret,
                    is_effectful: right_effectful,
                },
            ) => self.arrow_tys_match(
                left_params,
                left_ret,
                left_effectful,
                right_params,
                right_ret,
                right_effectful,
            ),
            (
                HirTyKind::Sum { left, right },
                HirTyKind::Sum {
                    left: other_left,
                    right: other_right,
                },
            ) => self.ty_matches(left, other_left) && self.ty_matches(right, other_right),
            (HirTyKind::Tuple { items: left }, HirTyKind::Tuple { items: right }) => {
                self.list_tys_match(left, right)
            }
            (
                HirTyKind::Array {
                    dims: left_dims,
                    item: left_item,
                },
                HirTyKind::Array {
                    dims: right_dims,
                    item: right_item,
                },
            ) => {
                self.dims(left_dims) == self.dims(right_dims)
                    && self.ty_matches(left_item, right_item)
            }
            (HirTyKind::Mut { inner: left }, HirTyKind::Mut { inner: right }) => {
                self.ty_matches(left, right)
            }
            (HirTyKind::Record { fields: left }, HirTyKind::Record { fields: right }) => {
                self.record_tys_match(left, right)
            }
            _ => false,
        }
    }

    fn list_tys_match(&self, left: SliceRange<HirTyId>, right: SliceRange<HirTyId>) -> bool {
        let left = self.ty_ids(left);
        let right = self.ty_ids(right);
        left.len() == right.len()
            && left
                .into_iter()
                .zip(right)
                .all(|(left, right)| self.ty_matches(left, right))
    }

    fn named_tys_match(
        &self,
        left_name: Symbol,
        left_args: SliceRange<HirTyId>,
        right_name: Symbol,
        right_args: SliceRange<HirTyId>,
    ) -> bool {
        left_name == right_name && self.list_tys_match(left_args, right_args)
    }

    fn arrow_tys_match(
        &self,
        left_params: SliceRange<HirTyId>,
        left_ret: HirTyId,
        left_effectful: bool,
        right_params: SliceRange<HirTyId>,
        right_ret: HirTyId,
        right_effectful: bool,
    ) -> bool {
        left_effectful == right_effectful
            && self.list_tys_match(left_params, right_params)
            && self.ty_matches(left_ret, right_ret)
    }

    fn record_tys_match(&self, left: HirTyFieldRange, right: HirTyFieldRange) -> bool {
        let left_fields = self.ty_fields(left);
        let right_fields = self.ty_fields(right);
        let right_map = right_fields
            .into_iter()
            .map(|field| (field.name, field.ty))
            .collect::<BTreeMap<_, _>>();
        left_fields.len() == right_map.len()
            && left_fields.into_iter().all(|field| {
                right_map
                    .get(&field.name)
                    .is_some_and(|right_ty| self.ty_matches(field.ty, *right_ty))
            })
    }

    pub fn named_type_for_symbol(&mut self, symbol: Symbol) -> HirTyId {
        let known = self.known();
        let builtins = self.builtins();
        if symbol == known.type_ {
            builtins.type_
        } else if symbol == known.any {
            builtins.any
        } else if symbol == known.unknown {
            builtins.unknown
        } else if symbol == known.syntax {
            builtins.syntax
        } else if symbol == known.empty {
            builtins.empty
        } else if symbol == known.unit {
            builtins.unit
        } else if symbol == known.bool_ {
            builtins.bool_
        } else if symbol == known.nat {
            builtins.nat
        } else if symbol == known.int_ {
            builtins.int_
        } else if symbol == known.float_ {
            builtins.float_
        } else if symbol == known.string_ {
            builtins.string_
        } else if symbol == known.cstring {
            builtins.cstring
        } else if symbol == known.cptr {
            builtins.cptr
        } else {
            let args = self.alloc_ty_list(Vec::<HirTyId>::new());
            self.alloc_ty(HirTyKind::Named { name: symbol, args })
        }
    }

    pub fn symbol_value_type(&self, symbol: Symbol) -> HirTyId {
        let known = self.known();
        let builtins = self.builtins();
        if [
            known.type_,
            known.any,
            known.unknown,
            known.syntax,
            known.empty,
            known.unit,
            known.bool_,
            known.nat,
            known.int_,
            known.float_,
            known.string_,
            known.cstring,
            known.cptr,
        ]
        .contains(&symbol)
        {
            builtins.type_
        } else {
            builtins.unknown
        }
    }

    pub fn lower_type_expr(&mut self, expr: HirExprId, origin: HirOrigin) -> HirTyId {
        let builtins = self.builtins();
        match self.expr(expr).kind {
            HirExprKind::Error => builtins.error,
            HirExprKind::Name { name } => self.named_type_for_symbol(name.name),
            HirExprKind::Tuple { items } => self.lower_tuple_type_expr(items),
            HirExprKind::ArrayTy { dims, item } => {
                let item_origin = self.expr(item).origin;
                let item = self.lower_type_expr(item, item_origin);
                self.alloc_ty(HirTyKind::Array { dims, item })
            }
            HirExprKind::Pi {
                binder: _,
                binder_ty,
                ret,
                is_effectful,
            } => {
                let binder_origin = self.expr(binder_ty).origin;
                let binder_ty = self.lower_type_expr(binder_ty, binder_origin);
                let params = self.alloc_ty_list([binder_ty]);
                let ret_origin = self.expr(ret).origin;
                let ret = self.lower_type_expr(ret, ret_origin);
                self.alloc_ty(HirTyKind::Arrow {
                    params,
                    ret,
                    is_effectful,
                })
            }
            HirExprKind::Apply { callee, args } => self.lower_apply_type_expr(origin, callee, args),
            HirExprKind::Binary { op, left, right } => {
                self.lower_binary_type_expr(origin, &op, left, right)
            }
            HirExprKind::Prefix {
                op: HirPrefixOp::Mut,
                expr,
            } => {
                let origin = self.expr(expr).origin;
                let inner = self.lower_type_expr(expr, origin);
                self.alloc_ty(HirTyKind::Mut { inner })
            }
            HirExprKind::Import { .. } => builtins.module,
            HirExprKind::Record { items } => self.lower_record_type_expr(items),
            _ => {
                self.diag(origin.span, DiagKind::InvalidTypeExpression, "");
                builtins.error
            }
        }
    }

    fn lower_tuple_type_expr(&mut self, items: SliceRange<HirExprId>) -> HirTyId {
        let items = self
            .expr_ids(items)
            .into_iter()
            .map(|item| {
                let origin = self.expr(item).origin;
                self.lower_type_expr(item, origin)
            })
            .collect::<Vec<_>>();
        let items = self.alloc_ty_list(items);
        self.alloc_ty(HirTyKind::Tuple { items })
    }

    fn lower_apply_type_expr(
        &mut self,
        origin: HirOrigin,
        callee: HirExprId,
        args: SliceRange<HirExprId>,
    ) -> HirTyId {
        let HirExprKind::Name { name } = self.expr(callee).kind else {
            self.diag(origin.span, DiagKind::InvalidTypeApplication, "");
            return self.builtins().error;
        };

        if self.resolve_symbol(name.name) == "Array" {
            let args = self.expr_ids(args);
            let Some((item_expr, dims_exprs)) = args.split_first() else {
                self.diag(origin.span, DiagKind::ArrayTypeRequiresItem, "");
                return self.builtins().error;
            };
            let item_origin = self.expr(*item_expr).origin;
            let item = self.lower_type_expr(*item_expr, item_origin);
            let dims = dims_exprs
                .iter()
                .filter_map(|expr| match self.expr(*expr).kind {
                    HirExprKind::Name { name } => Some(HirDim::Name(name)),
                    HirExprKind::Lit { lit } => match self.lit(lit).kind {
                        HirLitKind::Int { raw } => {
                            let raw = raw.replace('_', "");
                            let value = raw.strip_prefix("0x").map_or_else(
                                || {
                                    raw.strip_prefix("0o").map_or_else(
                                        || {
                                            raw.strip_prefix("0b").map_or_else(
                                                || raw.parse::<u32>().ok(),
                                                |bin| u32::from_str_radix(bin, 2).ok(),
                                            )
                                        },
                                        |oct| u32::from_str_radix(oct, 8).ok(),
                                    )
                                },
                                |hex| u32::from_str_radix(hex, 16).ok(),
                            );
                            value.map(HirDim::Int)
                        }
                        _ => None,
                    },
                    _ => None,
                })
                .collect::<Vec<_>>();
            let dims = self.alloc_dims(dims);
            return self.alloc_ty(HirTyKind::Array { dims, item });
        }

        let args = self
            .expr_ids(args)
            .into_iter()
            .map(|arg| {
                let origin = self.expr(arg).origin;
                self.lower_type_expr(arg, origin)
            })
            .collect::<Vec<_>>();
        let args = self.alloc_ty_list(args);
        self.alloc_ty(HirTyKind::Named {
            name: name.name,
            args,
        })
    }

    fn lower_binary_type_expr(
        &mut self,
        origin: HirOrigin,
        op: &HirBinaryOp,
        left: HirExprId,
        right: HirExprId,
    ) -> HirTyId {
        match op {
            &HirBinaryOp::Arrow | &HirBinaryOp::EffectArrow => {
                let left_origin = self.expr(left).origin;
                let left = self.lower_type_expr(left, left_origin);
                let params = self.alloc_ty_list([left]);
                let right_origin = self.expr(right).origin;
                let ret = self.lower_type_expr(right, right_origin);
                self.alloc_ty(HirTyKind::Arrow {
                    params,
                    ret,
                    is_effectful: matches!(op, &HirBinaryOp::EffectArrow),
                })
            }
            &HirBinaryOp::Add => {
                let left_origin = self.expr(left).origin;
                let right_origin = self.expr(right).origin;
                let left = self.lower_type_expr(left, left_origin);
                let right = self.lower_type_expr(right, right_origin);
                self.alloc_ty(HirTyKind::Sum { left, right })
            }
            _ => {
                self.diag(origin.span, DiagKind::InvalidTypeExpression, "");
                self.builtins().error
            }
        }
    }

    fn lower_record_type_expr(&mut self, items: SliceRange<HirRecordItem>) -> HirTyId {
        let fields = self
            .record_items(items)
            .into_iter()
            .filter_map(|item| {
                item.name.map(|name| {
                    let origin = self.expr(item.value).origin;
                    HirTyField::new(name.name, self.lower_type_expr(item.value, origin))
                })
            })
            .collect::<Vec<_>>();
        let fields = self.alloc_ty_fields(fields);
        self.alloc_ty(HirTyKind::Record { fields })
    }

    pub fn lower_effect_row(&mut self, row: &HirEffectSet) -> EffectRow {
        let mut out = EffectRow::empty();
        for item in self.effect_items(row) {
            let arg = item.arg.map(|expr| {
                let origin = self.expr(expr).origin;
                self.lower_type_expr(expr, origin)
            });
            out.add(EffectKey {
                name: self.resolve_symbol(item.name.name).into(),
                arg,
            });
        }
        out.open = row
            .open
            .map(|ident| Box::<str>::from(self.resolve_symbol(ident.name)));
        out
    }

    pub fn lower_constraints(
        &mut self,
        constraints: SliceRange<HirConstraint>,
    ) -> Box<[ConstraintFacts]> {
        self.constraints(constraints)
            .into_iter()
            .map(|constraint| {
                let kind = match constraint.kind {
                    HirConstraintKind::Subtype => ConstraintKind::Subtype,
                    HirConstraintKind::Implements => ConstraintKind::Implements,
                };
                let value = {
                    let origin = self.expr(constraint.value).origin;
                    self.lower_type_expr(constraint.value, origin)
                };
                {
                    let lowered = ConstraintFacts::new(constraint.name.name, kind, value);
                    if let Some(class_key) = self.constraint_class_key(kind, value) {
                        lowered.with_class_key(class_key)
                    } else {
                        lowered
                    }
                }
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    fn constraint_class_key(&self, kind: ConstraintKind, value: HirTyId) -> Option<DefinitionKey> {
        if kind != ConstraintKind::Implements {
            return None;
        }
        let HirTyKind::Named { name, .. } = self.ty(value).kind else {
            return None;
        };
        Some(self.class_facts_by_name(name).map_or_else(
            || surface_key(self.module_key(), self.interner(), name),
            |facts| facts.key.clone(),
        ))
    }
}

impl CheckPass<'_, '_, '_> {
    pub fn lower_params(&mut self, range: SliceRange<HirParam>) -> Box<[HirTyId]> {
        let builtins = self.builtins();
        self.params(range)
            .into_iter()
            .map(|param| {
                let ty = param.ty.map_or(builtins.unknown, |expr| {
                    let origin = self.expr(expr).origin;
                    self.lower_type_expr(expr, origin)
                });
                if let Some(binding) = self.binding_id_for_decl(param.name) {
                    self.insert_binding_type(binding, ty);
                }
                if let Some(default) = param.default {
                    let facts = check_expr(self, default);
                    let origin = self.expr(default).origin;
                    self.type_mismatch(origin, ty, facts.ty);
                }
                ty
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }
}
