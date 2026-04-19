use std::collections::BTreeMap;

use music_arena::SliceRange;
use music_hir::{
    HirBinaryOp, HirConstraint, HirConstraintKind, HirDim, HirEffectSet, HirExprId, HirExprKind,
    HirLitKind, HirOrigin, HirParam, HirPrefixOp, HirRecordItem, HirTyField, HirTyId, HirTyKind,
    simple_hir_ty_display_name,
};
use music_names::{KnownSymbols, Symbol};

use super::exprs::check_expr;
use super::state::Builtins;
use super::surface::surface_key;
use super::{CheckPass, DiagKind, PassBase};
use crate::api::{ConstraintFacts, ConstraintKind, DefinitionKey};
use crate::effects::{EffectKey, EffectRow};

type HirTyFieldRange = SliceRange<HirTyField>;

fn simple_named_type_for_symbol(
    known: KnownSymbols,
    builtins: Builtins,
    symbol: Symbol,
) -> Option<HirTyId> {
    [
        (known.any, builtins.any),
        (known.unknown, builtins.unknown),
        (known.syntax, builtins.syntax),
        (known.empty, builtins.empty),
        (known.unit, builtins.unit),
        (known.bool_, builtins.bool_),
        (known.nat, builtins.nat),
        (known.int_, builtins.int_),
        (known.int8, builtins.int8),
        (known.int16, builtins.int16),
        (known.int32, builtins.int32),
        (known.int64, builtins.int64),
        (known.nat8, builtins.nat8),
        (known.nat16, builtins.nat16),
        (known.nat32, builtins.nat32),
        (known.nat64, builtins.nat64),
        (known.float_, builtins.float_),
        (known.float32, builtins.float32),
        (known.float64, builtins.float64),
        (known.string_, builtins.string_),
        (known.rune, builtins.rune),
        (known.cstring, builtins.cstring),
        (known.cptr, builtins.cptr),
    ]
    .into_iter()
    .find_map(|(known_symbol, ty)| (symbol == known_symbol).then_some(ty))
}

impl PassBase<'_, '_, '_> {
    pub fn render_ty(&self, ty: HirTyId) -> String {
        let kind = &self.ty(ty).kind;
        Self::render_ty_builtin(kind)
            .or_else(|| self.render_ty_named_or_callable(kind))
            .or_else(|| self.render_ty_collection(kind))
            .or_else(|| self.render_ty_range(kind))
            .or_else(|| self.render_ty_special(kind))
            .unwrap_or_else(|| "<error>".into())
    }

    fn render_ty_builtin(kind: &HirTyKind) -> Option<String> {
        if let HirTyKind::NatLit(value) = kind {
            return Some(value.to_string());
        }
        simple_hir_ty_display_name(kind).map(str::to_owned)
    }

    fn render_ty_named_or_callable(&self, kind: &HirTyKind) -> Option<String> {
        Some(match kind {
            HirTyKind::Named { name, args } => self.render_named_ty(*name, *args),
            HirTyKind::Pi {
                binder,
                binder_ty,
                body,
                is_effectful,
            } => {
                let binder = self.resolve_symbol(*binder);
                let arrow = if *is_effectful { " ~> " } else { " -> " };
                format!(
                    "forall ({binder} : {}){arrow}{}",
                    self.render_ty(*binder_ty),
                    self.render_ty(*body)
                )
            }
            HirTyKind::Arrow {
                params,
                ret,
                is_effectful,
            } => self.render_arrow_ty(*params, *ret, *is_effectful),
            HirTyKind::Sum { left, right } => {
                format!("{} + {}", self.render_ty(*left), self.render_ty(*right))
            }
            _ => return None,
        })
    }

    fn render_ty_collection(&self, kind: &HirTyKind) -> Option<String> {
        Some(match kind {
            HirTyKind::Tuple { items } => self.render_tuple_ty(*items),
            HirTyKind::Seq { item } => format!("[]{}", self.render_ty(*item)),
            HirTyKind::Array { dims, item } => self.render_array_ty(dims.clone(), *item),
            _ => return None,
        })
    }

    fn render_ty_range(&self, kind: &HirTyKind) -> Option<String> {
        Some(match kind {
            HirTyKind::Range { bound } => format!("Range[{}]", self.render_ty(*bound)),
            HirTyKind::ClosedRange { bound } => format!("ClosedRange[{}]", self.render_ty(*bound)),
            HirTyKind::PartialRangeFrom { bound } => {
                format!("PartialRangeFrom[{}]", self.render_ty(*bound))
            }
            HirTyKind::PartialRangeUpTo { bound } => {
                format!("PartialRangeUpTo[{}]", self.render_ty(*bound))
            }
            HirTyKind::PartialRangeThru { bound } => {
                format!("PartialRangeThru[{}]", self.render_ty(*bound))
            }
            _ => return None,
        })
    }

    fn render_ty_special(&self, kind: &HirTyKind) -> Option<String> {
        Some(match kind {
            HirTyKind::Handler {
                effect,
                input,
                output,
            } => format!(
                "using {} ({} -> {})",
                self.render_ty(*effect),
                self.render_ty(*input),
                self.render_ty(*output)
            ),
            HirTyKind::Mut { inner } => format!("mut {}", self.render_ty(*inner)),
            HirTyKind::AnyClass { class } => format!("any {}", self.render_ty(*class)),
            HirTyKind::SomeClass { class } => format!("some {}", self.render_ty(*class)),
            HirTyKind::Record { fields } => self.render_record_ty(fields.clone()),
            _ => return None,
        })
    }
    fn render_named_ty(&self, name: Symbol, args: SliceRange<HirTyId>) -> String {
        let args = self.ty_ids(args);
        let name_text = self.resolve_symbol(name);
        let mut out = String::from(name_text);
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
        let mut out = String::new();
        for dim in dims {
            out.push('[');
            out.push_str(&match dim {
                HirDim::Unknown => "_".into(),
                HirDim::Name(name) => self.resolve_symbol(name.name).into(),
                HirDim::Int(value) => value.to_string(),
            });
            out.push(']');
        }
        out.push_str(&self.render_ty(item));
        out
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
        let expected = self.render_ty(expected);
        let found = self.render_ty(found);
        let message = format!("value expected `{expected}`, found `{found}`");
        let label = format!("value has type `{found}` here");
        self.diag_message(origin.span, DiagKind::TypeMismatch, message, label);
    }

    pub fn type_mismatch_for(
        &mut self,
        subject: &str,
        origin: HirOrigin,
        expected: HirTyId,
        found: HirTyId,
    ) {
        if self.ty_matches(expected, found) {
            return;
        }
        let expected = self.render_ty(expected);
        let found = self.render_ty(found);
        self.diag_message(
            origin.span,
            DiagKind::TypeMismatch,
            format!("{subject} expected `{expected}`, found `{found}`"),
            format!("{subject} has type `{found}` here"),
        );
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
        self.ty_matches_kinds(&left, &right)
    }

    fn ty_matches_kinds(&self, left: &HirTyKind, right: &HirTyKind) -> bool {
        Self::ty_matches_primitives(left, right)
            || self.ty_matches_named_or_arrow(left, right)
            || self.ty_matches_sums_and_collections(left, right)
            || self.ty_matches_ranges_and_mut(left, right)
            || self.ty_matches_handler_or_record(left, right)
    }

    const fn ty_matches_primitives(left: &HirTyKind, right: &HirTyKind) -> bool {
        matches!(
            (left, right),
            (HirTyKind::Type, HirTyKind::Type)
                | (HirTyKind::Syntax, HirTyKind::Syntax)
                | (HirTyKind::Any, HirTyKind::Any)
                | (HirTyKind::Empty, HirTyKind::Empty)
                | (HirTyKind::Unit, HirTyKind::Unit)
                | (HirTyKind::Bool, HirTyKind::Bool)
                | (HirTyKind::Nat, HirTyKind::Nat)
                | (HirTyKind::Int, HirTyKind::Int)
                | (HirTyKind::Int8, HirTyKind::Int8)
                | (HirTyKind::Int16, HirTyKind::Int16)
                | (HirTyKind::Int32, HirTyKind::Int32)
                | (HirTyKind::Int64, HirTyKind::Int64)
                | (HirTyKind::Nat8, HirTyKind::Nat8)
                | (HirTyKind::Nat16, HirTyKind::Nat16)
                | (HirTyKind::Nat32, HirTyKind::Nat32)
                | (HirTyKind::Nat64, HirTyKind::Nat64)
                | (HirTyKind::Float, HirTyKind::Float)
                | (HirTyKind::Float32, HirTyKind::Float32)
                | (HirTyKind::Float64, HirTyKind::Float64)
                | (HirTyKind::String, HirTyKind::String)
                | (HirTyKind::CString, HirTyKind::CString)
                | (HirTyKind::CPtr, HirTyKind::CPtr)
                | (HirTyKind::Module, HirTyKind::Module)
        )
    }

    fn ty_matches_named_or_arrow(&self, left: &HirTyKind, right: &HirTyKind) -> bool {
        match (left, right) {
            (
                HirTyKind::Named {
                    name: left_name,
                    args: left_args,
                },
                HirTyKind::Named {
                    name: right_name,
                    args: right_args,
                },
            ) => self.named_tys_match(*left_name, *left_args, *right_name, *right_args),
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
                *left_params,
                *left_ret,
                *left_effectful,
                *right_params,
                *right_ret,
                *right_effectful,
            ),
            _ => false,
        }
    }

    fn ty_matches_sums_and_collections(&self, left: &HirTyKind, right: &HirTyKind) -> bool {
        match (left, right) {
            (
                HirTyKind::Sum { left, right },
                HirTyKind::Sum {
                    left: other_left,
                    right: other_right,
                },
            ) => self.ty_matches(*left, *other_left) && self.ty_matches(*right, *other_right),
            (HirTyKind::Tuple { items: left }, HirTyKind::Tuple { items: right }) => {
                self.list_tys_match(*left, *right)
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
                self.dims(left_dims.clone()) == self.dims(right_dims.clone())
                    && self.ty_matches(*left_item, *right_item)
            }
            (
                HirTyKind::Seq { item: left_item },
                HirTyKind::Array {
                    item: right_item, ..
                },
            ) => self.ty_matches(*left_item, *right_item),
            _ => false,
        }
    }

    fn ty_matches_ranges_and_mut(&self, left: &HirTyKind, right: &HirTyKind) -> bool {
        match (left, right) {
            (HirTyKind::Seq { item: left }, HirTyKind::Seq { item: right })
            | (HirTyKind::Range { bound: left }, HirTyKind::Range { bound: right })
            | (HirTyKind::ClosedRange { bound: left }, HirTyKind::ClosedRange { bound: right })
            | (
                HirTyKind::PartialRangeFrom { bound: left },
                HirTyKind::PartialRangeFrom { bound: right },
            )
            | (
                HirTyKind::PartialRangeUpTo { bound: left },
                HirTyKind::PartialRangeUpTo { bound: right },
            )
            | (
                HirTyKind::PartialRangeThru { bound: left },
                HirTyKind::PartialRangeThru { bound: right },
            )
            | (HirTyKind::Mut { inner: left }, HirTyKind::Mut { inner: right })
            | (HirTyKind::AnyClass { class: left }, HirTyKind::AnyClass { class: right })
            | (HirTyKind::SomeClass { class: left }, HirTyKind::SomeClass { class: right }) => {
                self.ty_matches(*left, *right)
            }
            _ => false,
        }
    }

    fn ty_matches_handler_or_record(&self, left: &HirTyKind, right: &HirTyKind) -> bool {
        match (left, right) {
            (
                HirTyKind::Handler {
                    effect: left_effect,
                    input: left_input,
                    output: left_output,
                },
                HirTyKind::Handler {
                    effect: right_effect,
                    input: right_input,
                    output: right_output,
                },
            ) => {
                self.ty_matches(*left_effect, *right_effect)
                    && self.ty_matches(*left_input, *right_input)
                    && self.ty_matches(*left_output, *right_output)
            }
            (HirTyKind::Record { fields: left }, HirTyKind::Record { fields: right }) => {
                self.record_tys_match(left.clone(), right.clone())
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
}

impl PassBase<'_, '_, '_> {
    pub fn named_type_for_symbol(&mut self, symbol: Symbol) -> HirTyId {
        let known = self.known();
        let builtins = self.builtins();
        if symbol == known.type_ || self.is_universe_symbol(symbol) {
            return builtins.type_;
        }
        if let Some(ty) = simple_named_type_for_symbol(known, builtins, symbol) {
            return ty;
        }
        let args = self.alloc_ty_list(Vec::<HirTyId>::new());
        self.alloc_ty(HirTyKind::Named { name: symbol, args })
    }

    fn is_universe_symbol(&self, symbol: Symbol) -> bool {
        let Some(rest) = self.resolve_symbol(symbol).strip_prefix("Type") else {
            return false;
        };
        !rest.is_empty() && rest.bytes().all(|byte| byte.is_ascii_digit())
    }

    pub fn symbol_value_type(&self, symbol: Symbol) -> HirTyId {
        let known = self.known();
        let builtins = self.builtins();
        if [
            known.type_,
            known.array,
            known.any,
            known.unknown,
            known.syntax,
            known.empty,
            known.unit,
            known.bool_,
            known.range,
            known.closed_range,
            known.partial_range_from,
            known.partial_range_up_to,
            known.partial_range_thru,
            known.nat,
            known.int_,
            known.int8,
            known.int16,
            known.int32,
            known.int64,
            known.nat8,
            known.nat16,
            known.nat32,
            known.nat64,
            known.float_,
            known.float32,
            known.float64,
            known.string_,
            known.rune,
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
        self.lower_type_atomic_expr(expr)
            .or_else(|| self.lower_type_aggregate_expr(expr))
            .or_else(|| self.lower_type_callable_expr(expr))
            .or_else(|| self.lower_type_operator_expr(expr, origin))
            .unwrap_or_else(|| {
                self.diag(origin.span, DiagKind::InvalidTypeExpression, "");
                builtins.error
            })
    }

    fn lower_type_atomic_expr(&mut self, expr: HirExprId) -> Option<HirTyId> {
        Some(match self.expr(expr).kind {
            HirExprKind::Error => self.builtins().error,
            HirExprKind::Name { name } => self.named_type_for_symbol(name.name),
            HirExprKind::Lit { lit } => match self.lit_kind(lit) {
                HirLitKind::Int { raw } => match raw.parse::<u64>() {
                    Ok(value) => self.alloc_ty(HirTyKind::NatLit(value)),
                    Err(_) => self.builtins().error,
                },
                _ => return None,
            },
            HirExprKind::Import { .. } => self.builtins().module,
            _ => return None,
        })
    }

    fn lower_type_aggregate_expr(&mut self, expr: HirExprId) -> Option<HirTyId> {
        Some(match self.expr(expr).kind {
            HirExprKind::Tuple { items } => self.lower_tuple_type_expr(items),
            HirExprKind::ArrayTy { dims, item } => {
                let item_origin = self.expr(item).origin;
                let item = self.lower_type_expr(item, item_origin);
                if self.dims(dims.clone()).is_empty() {
                    self.alloc_ty(HirTyKind::Seq { item })
                } else {
                    self.alloc_ty(HirTyKind::Array { dims, item })
                }
            }
            HirExprKind::Record { items } => self.lower_record_type_expr(items),
            _ => return None,
        })
    }

    fn lower_type_callable_expr(&mut self, expr: HirExprId) -> Option<HirTyId> {
        Some(match self.expr(expr).kind {
            HirExprKind::HandlerTy {
                effect,
                input,
                output,
            } => {
                let effect_origin = self.expr(effect).origin;
                let effect = self.lower_type_expr(effect, effect_origin);
                let input_origin = self.expr(input).origin;
                let input = self.lower_type_expr(input, input_origin);
                let output_origin = self.expr(output).origin;
                let output = self.lower_type_expr(output, output_origin);
                self.alloc_ty(HirTyKind::Handler {
                    effect,
                    input,
                    output,
                })
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
            _ => return None,
        })
    }

    fn lower_type_operator_expr(&mut self, expr: HirExprId, origin: HirOrigin) -> Option<HirTyId> {
        Some(match self.expr(expr).kind {
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
            HirExprKind::Prefix {
                op: HirPrefixOp::Any,
                expr,
            } => {
                let origin = self.expr(expr).origin;
                let class = self.lower_type_expr(expr, origin);
                self.alloc_ty(HirTyKind::AnyClass { class })
            }
            HirExprKind::Prefix {
                op: HirPrefixOp::Some,
                expr,
            } => {
                let origin = self.expr(expr).origin;
                let class = self.lower_type_expr(expr, origin);
                self.alloc_ty(HirTyKind::SomeClass { class })
            }
            _ => return None,
        })
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
        let callee_ty = self.lower_type_expr(callee, self.expr(callee).origin);
        let args = self
            .expr_ids(args)
            .into_iter()
            .map(|arg| {
                let origin = self.expr(arg).origin;
                self.lower_type_expr(arg, origin)
            })
            .collect::<Vec<_>>();
        if self
            .remaining_constructor_kind(origin, callee_ty, args.len())
            .is_none()
        {
            self.diag(origin.span, DiagKind::InvalidTypeApplication, "");
            return self.builtins().error;
        }
        let HirTyKind::Named {
            name,
            args: existing_args,
        } = self.ty(callee_ty).kind
        else {
            self.diag(origin.span, DiagKind::InvalidTypeApplication, "");
            return self.builtins().error;
        };
        let mut all_args = self.ty_ids(existing_args);
        all_args.extend(args);
        match self.resolve_symbol(name) {
            "Array" if all_args.len() == 1 => {
                let dims = self.alloc_dims([HirDim::Unknown]);
                self.alloc_ty(HirTyKind::Array {
                    dims,
                    item: all_args[0],
                })
            }
            "Range" if all_args.len() == 1 => {
                self.alloc_ty(HirTyKind::Range { bound: all_args[0] })
            }
            "ClosedRange" if all_args.len() == 1 => {
                self.alloc_ty(HirTyKind::ClosedRange { bound: all_args[0] })
            }
            "PartialRangeFrom" if all_args.len() == 1 => {
                self.alloc_ty(HirTyKind::PartialRangeFrom { bound: all_args[0] })
            }
            "PartialRangeUpTo" if all_args.len() == 1 => {
                self.alloc_ty(HirTyKind::PartialRangeUpTo { bound: all_args[0] })
            }
            "PartialRangeThru" if all_args.len() == 1 => {
                self.alloc_ty(HirTyKind::PartialRangeThru { bound: all_args[0] })
            }
            _ => {
                let args = self.alloc_ty_list(all_args);
                self.alloc_ty(HirTyKind::Named { name, args })
            }
        }
    }

    fn type_constructor_param_kinds(&self, name: Symbol) -> Option<Vec<HirTyId>> {
        let known = self.known();
        if [
            known.array,
            known.range,
            known.closed_range,
            known.partial_range_from,
            known.partial_range_up_to,
            known.partial_range_thru,
        ]
        .contains(&name)
        {
            return Some(vec![self.builtins().type_]);
        }
        let text = self.resolve_symbol(name);
        if let Some(data) = self.data_def(text) {
            return Some(data.type_param_kinds().to_vec());
        }
        if let Some(facts) = self.class_facts_by_name(name) {
            return Some(facts.type_param_kinds.to_vec());
        }
        self.type_constructor_scheme_arity(name)
            .map(|arity| vec![self.builtins().type_; arity])
    }

    fn type_constructor_kind_from_params(&mut self, param_kinds: &[HirTyId]) -> HirTyId {
        let mut kind = self.builtins().type_;
        for param_kind in param_kinds.iter().rev().copied() {
            let params = self.alloc_ty_list([param_kind]);
            kind = self.alloc_ty(HirTyKind::Arrow {
                params,
                ret: kind,
                is_effectful: false,
            });
        }
        kind
    }

    fn remaining_constructor_kind(
        &mut self,
        origin: HirOrigin,
        callee: HirTyId,
        arg_count: usize,
    ) -> Option<HirTyId> {
        let mut kind = match self.ty(callee).kind {
            HirTyKind::Named { name, args } if self.ty_ids(args).is_empty() => {
                self.type_param_kind(name).or_else(|| {
                    self.type_constructor_param_kinds(name)
                        .map(|param_kinds| self.type_constructor_kind_from_params(&param_kinds))
                })?
            }
            HirTyKind::Named { name, args } => {
                let param_kinds = self.type_constructor_param_kinds(name)?;
                let used = self.ty_ids(args).len();
                if used > param_kinds.len() {
                    self.diag(origin.span, DiagKind::TypeApplicationArityMismatch, "");
                    return None;
                }
                self.type_constructor_kind_from_params(&param_kinds[used..])
            }
            HirTyKind::Arrow { .. } => callee,
            HirTyKind::Error | HirTyKind::Unknown => return Some(self.builtins().unknown),
            _ => return None,
        };
        for _ in 0..arg_count {
            let HirTyKind::Arrow {
                params,
                ret,
                is_effectful: false,
            } = self.ty(kind).kind
            else {
                self.diag(origin.span, DiagKind::TypeApplicationArityMismatch, "");
                return None;
            };
            let params = self.ty_ids(params);
            if params.len() != 1 {
                self.diag(origin.span, DiagKind::InvalidTypeApplication, "");
                return None;
            }
            kind = ret;
        }
        Some(kind)
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
                    HirConstraintKind::TypeEq => ConstraintKind::TypeEq,
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
