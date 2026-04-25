use super::{IrExpr, IrOrigin, LowerCtx, NameBindingId};

mod core;
mod parts;

use core::rewrite_recursive_binding_refs as rewrite_recursive_binding_refs_inner;

pub(crate) fn rewrite_recursive_binding_refs(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    expr: IrExpr,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExpr {
    rewrite_recursive_binding_refs_inner(ctx, origin, expr, binding, callable_name, captures)
}
