//! Declaration lowering: top-level bindings -> `IrFunction` stubs + bodies.

use std::collections::HashMap;

use music_ast::attr::{Attr, AttrValue};
use music_ast::decl::ForeignDecl;
use music_ast::expr::{Expr, LetFields, Param};
use music_ast::lit::Lit;
use music_ast::pat::Pat;
use music_ast::{AstArenas, ParsedModule};
use music_sema::types::Type;
use music_sema::{DefId, SemaResult};
use music_shared::{Arena, Idx, Interner, Symbol};

use crate::IrModule;
use crate::error::IrError;
use crate::func::{IrFnId, IrForeignFn, IrFunction, IrLocal, IrLocalDecl, IrParam, IrParamMode};
use crate::inst::{IrInst, IrOperand};
use crate::types::{IrEffectMask, IrType};

use super::ty::lower_ty;

pub(super) struct LowerCtx<'src> {
    pub sema: &'src SemaResult,
    pub ast: &'src AstArenas,
    pub interner: &'src Interner,
    pub ir: IrModule,
    /// Maps sema `DefId` for a function binding to its allocated slot.
    pub fn_map: HashMap<DefId, Idx<IrFunction>>,
    /// Maps sema `DefId` for a foreign function to its index in `ir.foreign_fns`.
    pub foreign_fn_map: HashMap<DefId, u32>,
    pub next_fn_id: u32,
    fn_entries: Vec<FnEntry>,
    entry_candidate: Option<Idx<IrFunction>>,
}

struct FnEntry {
    fn_idx: Idx<IrFunction>,
    binding_expr: Idx<Expr>,
}

pub(super) fn lower_module(
    parsed: &ParsedModule,
    sema: &SemaResult,
    interner: &Interner,
) -> Result<IrModule, IrError> {
    let mut cx = LowerCtx {
        sema,
        ast: &parsed.arenas,
        interner,
        ir: IrModule::new(),
        fn_map: HashMap::new(),
        foreign_fn_map: HashMap::new(),
        next_fn_id: 0,
        fn_entries: vec![],
        entry_candidate: None,
    };

    for stmt in &parsed.stmts {
        register_fn_stub(&mut cx, stmt.expr)?;
    }

    let entries: Vec<FnEntry> = cx.fn_entries.drain(..).collect();
    for entry in entries {
        lower_fn_body(&mut cx, entry.fn_idx, entry.binding_expr)?;
    }

    set_entry_point(&mut cx);
    Ok(cx.ir)
}

fn register_fn_stub(cx: &mut LowerCtx<'_>, expr_idx: Idx<Expr>) -> Result<(), IrError> {
    let expr = cx.ast.exprs[expr_idx].clone();

    // Handle foreign declarations (possibly wrapped in Annotated).
    if let Some((foreign_expr, attrs)) = extract_foreign(expr.clone(), cx.ast) {
        return register_foreign_fns(cx, &foreign_expr, &attrs);
    }

    let Some((fields, binding_span, attrs)) = extract_fn_fields(expr, cx.ast) else {
        return Ok(());
    };

    let fn_expr = cx.ast.exprs[fields.value].clone();
    let Expr::Fn { params, .. } = &fn_expr else {
        return Ok(());
    };
    let params = params.clone();

    let pat = cx.ast.pats[fields.pat].clone();
    let Pat::Bind { span: pat_span, .. } = pat else {
        return Ok(());
    };
    let Some(&def_id) = cx.sema.resolution.pat_defs.get(&pat_span) else {
        return Ok(());
    };

    let def_idx = usize::try_from(def_id.0).map_err(|_| IrError::UnsupportedExpr)?;
    let fn_name = cx.sema.defs[def_idx].name;

    let Some(&fn_ty_sema) = cx.sema.expr_types.get(&fields.value) else {
        return Err(IrError::UnsupportedExpr);
    };
    let fn_type = cx.sema.types[fn_ty_sema].clone();
    let Type::Fn {
        params: param_sema_tys,
        ret: ret_sema_ty,
        ..
    } = fn_type
    else {
        return Err(IrError::UnsupportedExpr);
    };

    let ir_ret_ty = lower_ty(ret_sema_ty, cx.sema, &mut cx.ir.types)?;
    let (ir_params, ir_locals) =
        build_param_locals(&params, &param_sema_tys, cx.sema, &mut cx.ir.types)?;

    let fn_id = IrFnId(cx.next_fn_id);
    cx.next_fn_id += 1;

    let stub = IrFunction {
        id: fn_id,
        source_def: Some(def_id),
        name: fn_name,
        params: ir_params,
        ret_ty: ir_ret_ty,
        effects: IrEffectMask::PURE,
        body: vec![],
        locals: ir_locals,
        is_closure: false,
        span: binding_span,
    };

    let fn_idx = cx.ir.functions.alloc(stub);
    let _ = cx.fn_map.insert(def_id, fn_idx);
    cx.fn_entries.push(FnEntry {
        fn_idx,
        binding_expr: expr_idx,
    });

    if has_entrypoint_attr(&attrs, cx.interner) {
        if cx.entry_candidate.is_some() {
            return Err(IrError::DuplicateEntryPoint);
        }
        cx.entry_candidate = Some(fn_idx);
    }

    Ok(())
}

fn build_param_locals(
    params: &[Param],
    param_sema_tys: &[Idx<Type>],
    sema: &SemaResult,
    ir_types: &mut Arena<IrType>,
) -> Result<(Vec<IrParam>, Vec<IrLocalDecl>), IrError> {
    let mut ir_params = Vec::with_capacity(params.len());
    let mut ir_locals = Vec::with_capacity(params.len());
    for (i, (param, &sema_ty)) in params.iter().zip(param_sema_tys.iter()).enumerate() {
        let local = IrLocal(u32::try_from(i).map_err(|_| IrError::UnsupportedExpr)?);
        let ir_ty = lower_ty(sema_ty, sema, ir_types)?;
        ir_params.push(IrParam {
            local,
            ty: ir_ty,
            mode: IrParamMode::Value,
            span: param.span,
        });
        ir_locals.push(IrLocalDecl {
            local,
            ty: ir_ty,
            mutable: false,
            span: param.span,
        });
    }
    Ok((ir_params, ir_locals))
}

fn lower_fn_body(
    cx: &mut LowerCtx<'_>,
    fn_idx: Idx<IrFunction>,
    binding_expr: Idx<Expr>,
) -> Result<(), IrError> {
    let binding = cx.ast.exprs[binding_expr].clone();
    let Some((fields, _, _)) = extract_fn_fields(binding, cx.ast) else {
        return Err(IrError::UnsupportedExpr);
    };
    let fn_expr = cx.ast.exprs[fields.value].clone();
    let Expr::Fn {
        params,
        body: body_expr,
        ..
    } = fn_expr
    else {
        return Err(IrError::UnsupportedExpr);
    };

    let fn_span = cx.ir.functions[fn_idx].span;
    let param_locals = cx.ir.functions[fn_idx].locals.clone();
    let n_params = u32::try_from(param_locals.len()).map_err(|_| IrError::UnsupportedExpr)?;

    let local_map = build_param_local_map(&params, cx.sema);

    let mut fn_cx = super::expr::FnLowerCtx {
        cx,
        local_map,
        locals: param_locals,
        body: vec![],
        next_local: n_params,
        next_label: 0,
    };

    let result_local = super::expr::lower_expr(&mut fn_cx, body_expr)?;
    fn_cx.emit(IrInst::Return {
        value: Some(IrOperand::Local(result_local)),
        span: fn_span,
    });

    let new_body = fn_cx.body;
    let new_locals = fn_cx.locals;
    cx.ir.functions[fn_idx].body = new_body;
    cx.ir.functions[fn_idx].locals = new_locals;
    Ok(())
}

fn build_param_local_map(params: &[Param], sema: &SemaResult) -> HashMap<DefId, IrLocal> {
    let mut map = HashMap::new();
    for (i, param) in params.iter().enumerate() {
        let Some(&def_id) = sema.resolution.pat_defs.get(&param.span) else {
            continue;
        };
        let Ok(idx) = u32::try_from(i) else { continue };
        let _ = map.insert(def_id, IrLocal(idx));
    }
    map
}

/// Extracts a `Foreign` expr, possibly unwrapping `Annotated` wrappers.
fn extract_foreign(expr: Expr, ast: &AstArenas) -> Option<(Expr, Vec<Attr>)> {
    match expr {
        Expr::Foreign { .. } => Some((expr, vec![])),
        Expr::Annotated { attrs, inner, .. } => {
            let inner_expr = ast.exprs[inner].clone();
            let (foreign_expr, mut inner_attrs) = extract_foreign(inner_expr, ast)?;
            let mut all_attrs = attrs;
            all_attrs.append(&mut inner_attrs);
            Some((foreign_expr, all_attrs))
        }
        _ => None,
    }
}

fn register_foreign_fns(cx: &mut LowerCtx<'_>, expr: &Expr, attrs: &[Attr]) -> Result<(), IrError> {
    let Expr::Foreign { decls, .. } = expr else {
        return Ok(());
    };

    let library = extract_link_attr(attrs, cx.interner);

    for decl in decls {
        match decl {
            ForeignDecl::Fn {
                name,
                ext_name,
                ty,
                span,
            } => {
                let ext_sym = ext_name.unwrap_or(*name);

                // Look up the DefId for this foreign fn
                let Some(&def_id) = cx.sema.resolution.pat_defs.get(span) else {
                    // Foreign fns are defined at the name span, not pat span.
                    // Try to find by iterating defs.
                    register_foreign_fn_by_name(cx, *name, ext_sym, *ty, library, *span)?;
                    continue;
                };
                register_foreign_fn_with_def(cx, def_id, ext_sym, *ty, library, *span)?;
            }
            ForeignDecl::OpaqueType { .. } => {
                // Opaque types don't generate IR — they're just names
            }
        }
    }
    Ok(())
}

fn register_foreign_fn_by_name(
    cx: &mut LowerCtx<'_>,
    name: music_shared::Symbol,
    ext_name: music_shared::Symbol,
    ty: Idx<music_ast::Ty>,
    library: Option<music_shared::Symbol>,
    span: music_shared::Span,
) -> Result<(), IrError> {
    // Find the DefId by looking through all defs for a matching name and span
    let def_id = cx
        .sema
        .defs
        .iter()
        .find(|d| d.name == name && d.span == span)
        .map(|d| d.id);

    let Some(def_id) = def_id else {
        return Ok(());
    };

    register_foreign_fn_with_def(cx, def_id, ext_name, ty, library, span)
}

fn register_foreign_fn_with_def(
    cx: &mut LowerCtx<'_>,
    def_id: DefId,
    ext_name: music_shared::Symbol,
    _ty: Idx<music_ast::Ty>,
    library: Option<music_shared::Symbol>,
    _span: music_shared::Span,
) -> Result<(), IrError> {
    // Lower the type annotation to get param and return types
    let Some(&_ty_idx) = cx.sema.expr_types.values().next() else {
        // Fall back: just create a simple foreign fn entry
        let idx = u32::try_from(cx.ir.foreign_fns.len()).map_err(|_| IrError::UnsupportedExpr)?;
        let name =
            cx.sema.defs[usize::try_from(def_id.0).map_err(|_| IrError::UnsupportedExpr)?].name;
        let unit_ty = cx.ir.types.alloc(IrType::Unit);
        cx.ir.foreign_fns.push(IrForeignFn {
            name,
            ext_name,
            library,
            param_tys: vec![],
            ret_ty: unit_ty,
            variadic: false,
        });
        let _ = cx.foreign_fn_map.insert(def_id, idx);
        return Ok(());
    };

    // Try to resolve the sema type for the type annotation
    let def_idx = usize::try_from(def_id.0).map_err(|_| IrError::UnsupportedExpr)?;
    let name = cx.sema.defs[def_idx].name;
    let def_ty_info = &cx.sema.defs[def_idx].ty_info;

    let (param_ir_tys, ret_ir_ty, variadic) = if let Some(sema_ty) = def_ty_info.ty {
        let resolved = cx.sema.unify.resolve(sema_ty, &cx.sema.types);
        if let Type::Fn { params, ret, .. } = &cx.sema.types[resolved] {
            let mut ptys = Vec::with_capacity(params.len());
            for &p in params {
                ptys.push(lower_ty(p, cx.sema, &mut cx.ir.types)?);
            }
            let rty = lower_ty(*ret, cx.sema, &mut cx.ir.types)?;
            (ptys, rty, false)
        } else {
            let unit_ty = cx.ir.types.alloc(IrType::Unit);
            (vec![], unit_ty, false)
        }
    } else {
        let unit_ty = cx.ir.types.alloc(IrType::Unit);
        (vec![], unit_ty, false)
    };

    let idx = u32::try_from(cx.ir.foreign_fns.len()).map_err(|_| IrError::UnsupportedExpr)?;
    cx.ir.foreign_fns.push(IrForeignFn {
        name,
        ext_name,
        library,
        param_tys: param_ir_tys,
        ret_ty: ret_ir_ty,
        variadic,
    });
    let _ = cx.foreign_fn_map.insert(def_id, idx);
    Ok(())
}

fn extract_link_attr(attrs: &[Attr], interner: &Interner) -> Option<Symbol> {
    attrs.iter().find_map(|a| {
        if interner.resolve(a.name) == "link"
            && let Some(AttrValue::Lit {
                lit: Lit::Str { value, .. },
                ..
            }) = &a.value
        {
            return Some(*value);
        }
        None
    })
}

/// Returns `(fields, span, attrs)` from `Expr::Binding`, `Expr::Let { body: None }`,
/// or `Expr::Annotated` wrapping either of those.
///
/// `Expr::Annotated` is unwrapped recursively, accumulating attributes.
fn extract_fn_fields(
    expr: Expr,
    ast: &AstArenas,
) -> Option<(LetFields, music_shared::Span, Vec<Attr>)> {
    match expr {
        Expr::Binding { fields, span, .. }
        | Expr::Let {
            fields,
            body: None,
            span,
        } => Some((fields, span, vec![])),
        Expr::Annotated { attrs, inner, .. } => {
            let inner_expr = ast.exprs[inner].clone();
            let (fields, span, mut inner_attrs) = extract_fn_fields(inner_expr, ast)?;
            let mut all_attrs = attrs;
            all_attrs.append(&mut inner_attrs);
            Some((fields, span, all_attrs))
        }
        _ => None,
    }
}

fn has_entrypoint_attr(attrs: &[Attr], interner: &Interner) -> bool {
    attrs
        .iter()
        .any(|a| interner.resolve(a.name) == "entrypoint")
}

const fn set_entry_point(cx: &mut LowerCtx<'_>) {
    cx.ir.entry = cx.entry_candidate;
}
