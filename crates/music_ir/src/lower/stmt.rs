//! Declaration lowering: top-level bindings -> `IrFunction` stubs + bodies.

use std::collections::HashMap;

use music_ast::attr::{Attr, AttrValue};
use music_ast::decl::ForeignDecl;
use music_ast::expr::{Expr, LetFields, Param};
use music_ast::lit::Lit;
use music_ast::pat::Pat;
use music_ast::{AstArenas, ExprIdx, ParsedModule, TyIdx};
use music_sema::types::Type;
use music_sema::{DefId, SemaResult, TypeIdx};
use music_shared::{Arena, Interner, Span, Symbol};

use crate::error::{IrError, SpannedIrError};
use crate::func::{
    IrFnId, IrFnIdx, IrForeignFn, IrFunction, IrLocal, IrLocalDecl, IrParam, IrParamMode,
};
use crate::inst::{IrInst, IrOperand};
use crate::types::IrType;
use crate::{DepModuleIr, IrModule};

use super::expr::expr_span;
use super::ty::lower_ty;

pub(super) struct LowerCtx<'src> {
    pub sema: &'src SemaResult,
    pub ast: &'src AstArenas,
    pub pat_defs: &'src HashMap<Span, DefId>,
    pub expr_defs: &'src HashMap<ExprIdx, DefId>,
    pub module_expr_types: &'src HashMap<ExprIdx, TypeIdx>,
    pub interner: &'src mut Interner,
    pub ir: IrModule,
    /// Maps sema `DefId` for a function binding to its allocated slot.
    pub fn_map: HashMap<DefId, IrFnIdx>,
    /// Maps sema `DefId` for a foreign function to its index in `ir.foreign_fns`.
    pub foreign_fn_map: HashMap<DefId, u32>,
    pub next_fn_id: u32,
    fn_entries: Vec<FnEntry>,
    entry_candidate: Option<IrFnIdx>,
}

struct FnEntry {
    fn_idx: IrFnIdx,
    binding_expr: ExprIdx,
}

pub(super) fn lower_module(
    parsed: &ParsedModule,
    sema: &SemaResult,
    interner: &mut Interner,
) -> Result<IrModule, SpannedIrError> {
    let mut cx = LowerCtx {
        sema,
        ast: &parsed.arenas,
        pat_defs: &sema.resolution.pat_defs,
        expr_defs: &sema.resolution.expr_defs,
        module_expr_types: &sema.expr_types,
        interner,
        ir: IrModule::new(),
        fn_map: HashMap::new(),
        foreign_fn_map: HashMap::new(),
        next_fn_id: 0,
        fn_entries: vec![],
        entry_candidate: None,
    };

    register_well_known_fns(&mut cx)?;

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

pub(super) fn lower_module_multi(
    parsed: &ParsedModule,
    sema: &SemaResult,
    dep_modules: &[DepModuleIr],
    interner: &mut Interner,
) -> Result<IrModule, SpannedIrError> {
    let mut cx = LowerCtx {
        sema,
        ast: &parsed.arenas,
        pat_defs: &sema.resolution.pat_defs,
        expr_defs: &sema.resolution.expr_defs,
        module_expr_types: &sema.expr_types,
        interner,
        ir: IrModule::new(),
        fn_map: HashMap::new(),
        foreign_fn_map: HashMap::new(),
        next_fn_id: 0,
        fn_entries: vec![],
        entry_candidate: None,
    };

    register_well_known_fns(&mut cx)?;

    // Phase 1: register fn stubs from all dependency modules, collecting
    // per-module fn entries so we can lower bodies with the correct context.
    let mut dep_entries: Vec<Vec<FnEntry>> = Vec::with_capacity(dep_modules.len());
    for dep in dep_modules {
        cx.ast = &dep.parsed.arenas;
        cx.pat_defs = &dep.resolution.pat_defs;
        cx.expr_defs = &dep.resolution.expr_defs;
        cx.module_expr_types = &dep.expr_types;
        for stmt in &dep.parsed.stmts {
            register_fn_stub(&mut cx, stmt.expr)?;
        }
        dep_entries.push(cx.fn_entries.drain(..).collect());
    }

    // Phase 2: register fn stubs from the entry module.
    cx.ast = &parsed.arenas;
    cx.pat_defs = &sema.resolution.pat_defs;
    cx.expr_defs = &sema.resolution.expr_defs;
    cx.module_expr_types = &sema.expr_types;
    for stmt in &parsed.stmts {
        register_fn_stub(&mut cx, stmt.expr)?;
    }
    let entry_entries: Vec<FnEntry> = cx.fn_entries.drain(..).collect();

    // Phase 3: lower fn bodies from dependency modules.
    for (dep, entries) in dep_modules.iter().zip(dep_entries) {
        cx.ast = &dep.parsed.arenas;
        cx.pat_defs = &dep.resolution.pat_defs;
        cx.expr_defs = &dep.resolution.expr_defs;
        cx.module_expr_types = &dep.expr_types;
        for entry in entries {
            lower_fn_body(&mut cx, entry.fn_idx, entry.binding_expr).map_err(|mut e| {
                e.file_id = Some(dep.file_id);
                e
            })?;
        }
    }

    // Phase 4: lower fn bodies from the entry module.
    cx.ast = &parsed.arenas;
    cx.pat_defs = &sema.resolution.pat_defs;
    cx.expr_defs = &sema.resolution.expr_defs;
    cx.module_expr_types = &sema.expr_types;
    for entry in entry_entries {
        lower_fn_body(&mut cx, entry.fn_idx, entry.binding_expr)?;
    }

    set_entry_point(&mut cx);
    Ok(cx.ir)
}

/// Registers well-known prelude functions (e.g. `writeln`) as foreign fns so
/// that `lower_call` can find them without an explicit `foreign` declaration.
fn register_well_known_fns(cx: &mut LowerCtx<'_>) -> Result<(), SpannedIrError> {
    let writeln_def = cx.sema.well_known.fns.writeln;
    let puts_sym = cx.interner.intern("puts");
    let any_ty = cx.ir.types.alloc(IrType::Any);
    let unit_ty = cx.ir.types.alloc(IrType::Unit);
    let span = Span::new(0, 0);

    let def_idx = usize::try_from(writeln_def.0).map_err(|_| IrError::IndexOverflow.at(span))?;
    let name = cx.sema.defs[def_idx].name;

    let idx =
        u32::try_from(cx.ir.foreign_fns.len()).map_err(|_| IrError::IndexOverflow.at(span))?;
    cx.ir.foreign_fns.push(IrForeignFn {
        name,
        ext_name: puts_sym,
        library: None,
        param_tys: vec![any_ty],
        ret_ty: unit_ty,
        variadic: false,
    });
    let _ = cx.foreign_fn_map.insert(writeln_def, idx);

    // Register `show` as a foreign fn (musi_show).
    let show_def = cx.sema.well_known.fns.show;
    let show_ext = cx.interner.intern("musi_show");
    let show_def_idx = usize::try_from(show_def.0).map_err(|_| IrError::IndexOverflow.at(span))?;
    let show_name = cx.sema.defs[show_def_idx].name;
    let show_idx =
        u32::try_from(cx.ir.foreign_fns.len()).map_err(|_| IrError::IndexOverflow.at(span))?;
    cx.ir.foreign_fns.push(IrForeignFn {
        name: show_name,
        ext_name: show_ext,
        library: None,
        param_tys: vec![any_ty],
        ret_ty: any_ty,
        variadic: false,
    });
    let _ = cx.foreign_fn_map.insert(show_def, show_idx);

    // Register `str_cat` as a foreign fn (musi_str_cat).
    let str_cat_sym = cx.interner.intern("str_cat");
    let str_cat_ext = cx.interner.intern("musi_str_cat");
    let str_cat_def_id = cx
        .sema
        .defs
        .iter()
        .find(|d| d.name == str_cat_sym)
        .map_or(music_sema::DefId(u32::MAX - 1), |d| d.id);
    let str_cat_idx =
        u32::try_from(cx.ir.foreign_fns.len()).map_err(|_| IrError::IndexOverflow.at(span))?;
    cx.ir.foreign_fns.push(IrForeignFn {
        name: str_cat_sym,
        ext_name: str_cat_ext,
        library: None,
        param_tys: vec![any_ty, any_ty],
        ret_ty: any_ty,
        variadic: false,
    });
    let _ = cx.foreign_fn_map.insert(str_cat_def_id, str_cat_idx);

    Ok(())
}

/// Returns the foreign fn index for `musi_show`.
pub(super) fn show_ffi_idx(cx: &LowerCtx<'_>) -> Option<u32> {
    cx.foreign_fn_map.get(&cx.sema.well_known.fns.show).copied()
}

/// Returns the foreign fn index for `musi_str_cat`.
pub(super) fn str_cat_ffi_idx(cx: &LowerCtx<'_>) -> Option<u32> {
    cx.foreign_fn_map
        .get(&music_sema::DefId(u32::MAX - 1))
        .copied()
}

fn register_fn_stub(cx: &mut LowerCtx<'_>, expr_idx: ExprIdx) -> Result<(), SpannedIrError> {
    let expr = cx.ast.exprs[expr_idx].clone();
    let span = expr_span(&expr);

    // Handle foreign declarations (possibly wrapped in Annotated).
    if let Some((foreign_expr, attrs)) = extract_foreign(expr.clone(), cx.ast) {
        return register_foreign_fns(cx, &foreign_expr, &attrs, span);
    }

    let Some((fields, binding_span, attrs)) = extract_fn_fields(expr, cx.ast) else {
        return Ok(());
    };

    let Some(value_idx) = fields.value else {
        return Ok(());
    };
    let fn_expr = cx.ast.exprs[value_idx].clone();
    let Expr::Fn { params, .. } = &fn_expr else {
        return Ok(());
    };
    let params = params.clone();

    let pat = cx.ast.pats[fields.pat].clone();
    let Pat::Bind { span: pat_span, .. } = pat else {
        return Ok(());
    };
    let Some(&def_id) = cx.pat_defs.get(&pat_span) else {
        return Ok(());
    };

    let def_idx = usize::try_from(def_id.0).map_err(|_| IrError::IndexOverflow.at(binding_span))?;
    let fn_name = cx.sema.defs[def_idx].name;

    let Some(&fn_ty_sema) = cx.module_expr_types.get(&value_idx) else {
        return Err(IrError::MissingExprType.at(binding_span));
    };
    let fn_type = cx.sema.types[fn_ty_sema].clone();
    let Type::Fn {
        params: param_sema_tys,
        ret: ret_sema_ty,
        effects: effect_row,
    } = fn_type
    else {
        return Err(IrError::UnsupportedType.at(binding_span));
    };

    let ir_ret_ty =
        lower_ty(ret_sema_ty, cx.sema, &mut cx.ir.types).map_err(|e| e.at(binding_span))?;
    let (ir_params, ir_locals) =
        build_param_locals(&params, &param_sema_tys, cx.sema, &mut cx.ir.types)
            .map_err(|e| e.at(binding_span))?;
    let ir_effects = super::effect::lower_effect_row(&effect_row, &cx.sema.well_known.effects);

    let fn_id = IrFnId(cx.next_fn_id);
    cx.next_fn_id += 1;

    let stub = IrFunction {
        id: fn_id,
        source_def: Some(def_id),
        name: fn_name,
        params: ir_params,
        ret_ty: ir_ret_ty,
        effects: ir_effects,
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
            return Err(IrError::DuplicateEntryPoint.at(binding_span));
        }
        cx.entry_candidate = Some(fn_idx);
    }

    Ok(())
}

fn build_param_locals(
    params: &[Param],
    param_sema_tys: &[TypeIdx],
    sema: &SemaResult,
    ir_types: &mut Arena<IrType>,
) -> Result<(Vec<IrParam>, Vec<IrLocalDecl>), IrError> {
    let mut ir_params = Vec::with_capacity(params.len());
    let mut ir_locals = Vec::with_capacity(params.len());
    for (i, (param, &sema_ty)) in params.iter().zip(param_sema_tys.iter()).enumerate() {
        let local = IrLocal(u32::try_from(i).map_err(|_| IrError::IndexOverflow)?);
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
    fn_idx: IrFnIdx,
    binding_expr: ExprIdx,
) -> Result<(), SpannedIrError> {
    let binding = cx.ast.exprs[binding_expr].clone();
    let binding_span = expr_span(&binding);
    let Some((fields, _, _)) = extract_fn_fields(binding, cx.ast) else {
        return Err(IrError::UnsupportedExpr {
            kind: "non-function binding",
        }
        .at(binding_span));
    };
    let Some(value_idx) = fields.value else {
        return Err(IrError::UnsupportedExpr {
            kind: "binding without value",
        }
        .at(binding_span));
    };
    let fn_expr = cx.ast.exprs[value_idx].clone();
    let Expr::Fn {
        params,
        body: body_expr,
        ..
    } = fn_expr
    else {
        return Err(IrError::UnsupportedExpr {
            kind: "non-function value in binding",
        }
        .at(binding_span));
    };

    let fn_span = cx.ir.functions[fn_idx].span;
    let param_locals = cx.ir.functions[fn_idx].locals.clone();
    let n_params =
        u32::try_from(param_locals.len()).map_err(|_| IrError::IndexOverflow.at(fn_span))?;

    let local_map = build_param_local_map(&params, cx.pat_defs);

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

pub(super) fn build_param_local_map(
    params: &[Param],
    pat_defs: &HashMap<Span, DefId>,
) -> HashMap<DefId, IrLocal> {
    let mut map = HashMap::new();
    for (i, param) in params.iter().enumerate() {
        let Some(&def_id) = pat_defs.get(&param.span) else {
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

fn register_foreign_fns(
    cx: &mut LowerCtx<'_>,
    expr: &Expr,
    attrs: &[Attr],
    span: Span,
) -> Result<(), SpannedIrError> {
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
                span: decl_span,
            } => {
                let ext_sym = ext_name.unwrap_or(*name);

                // Look up the DefId for this foreign fn
                let Some(&def_id) = cx.pat_defs.get(decl_span) else {
                    // Foreign fns are defined at the name span, not pat span.
                    // Try to find by iterating defs.
                    register_foreign_fn_by_name(cx, *name, ext_sym, *ty, library, *decl_span)?;
                    continue;
                };
                register_foreign_fn_with_def(cx, def_id, ext_sym, *ty, library, *decl_span)?;
            }
            ForeignDecl::OpaqueType { .. } => {
                // Opaque types don't generate IR — they're just names
            }
        }
    }
    let _ = span;
    Ok(())
}

fn register_foreign_fn_by_name(
    cx: &mut LowerCtx<'_>,
    name: Symbol,
    ext_name: Symbol,
    ty: TyIdx,
    library: Option<Symbol>,
    span: Span,
) -> Result<(), SpannedIrError> {
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
    ext_name: Symbol,
    _ty: TyIdx,
    library: Option<Symbol>,
    span: Span,
) -> Result<(), SpannedIrError> {
    // Lower the type annotation to get param and return types
    let Some(&_ty_idx) = cx.module_expr_types.values().next() else {
        // Fall back: just create a simple foreign fn entry
        let idx =
            u32::try_from(cx.ir.foreign_fns.len()).map_err(|_| IrError::IndexOverflow.at(span))?;
        let name = cx.sema.defs
            [usize::try_from(def_id.0).map_err(|_| IrError::IndexOverflow.at(span))?]
        .name;
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
    let def_idx = usize::try_from(def_id.0).map_err(|_| IrError::IndexOverflow.at(span))?;
    let name = cx.sema.defs[def_idx].name;
    let def_ty_info = &cx.sema.defs[def_idx].ty_info;

    let (param_ir_tys, ret_ir_ty, variadic) = if let Some(sema_ty) = def_ty_info.ty {
        let resolved = cx.sema.unify.resolve(sema_ty, &cx.sema.types);
        if let Type::Fn { params, ret, .. } = &cx.sema.types[resolved] {
            let mut ptys = Vec::with_capacity(params.len());
            for &p in params {
                ptys.push(lower_ty(p, cx.sema, &mut cx.ir.types).map_err(|e| e.at(span))?);
            }
            let rty = lower_ty(*ret, cx.sema, &mut cx.ir.types).map_err(|e| e.at(span))?;
            (ptys, rty, false)
        } else {
            let unit_ty = cx.ir.types.alloc(IrType::Unit);
            (vec![], unit_ty, false)
        }
    } else {
        let unit_ty = cx.ir.types.alloc(IrType::Unit);
        (vec![], unit_ty, false)
    };

    let idx =
        u32::try_from(cx.ir.foreign_fns.len()).map_err(|_| IrError::IndexOverflow.at(span))?;
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
fn extract_fn_fields(expr: Expr, ast: &AstArenas) -> Option<(LetFields, Span, Vec<Attr>)> {
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
