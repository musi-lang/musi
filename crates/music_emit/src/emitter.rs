//! Main emitter orchestrator: tree-walks AST+sema to produce bytecode.

#[cfg(test)]
mod tests;

mod control;
mod desugar;
pub(crate) mod expr;
mod fn_emitter;

use std::collections::HashMap;

use music_ast::Pat;
use music_ast::attr::Attr;
use music_ast::expr::{Expr, LetFields, Param};
use music_ast::{AstArenas, ExprIdx, ParsedModule, PatIdx, Stmt};
use music_sema::SemaResult;
use music_sema::def::{DefId, DefKind};
use music_shared::{Interner, Span, Symbol};

use crate::const_pool::ConstPool;
use crate::error::EmitError;
use crate::module::{EffectDef, ForeignFn};
use crate::type_pool::TypePool;

pub(crate) use fn_emitter::{FnEmitter, HandlerEntry};

/// Per-function bytecode output.
pub struct FnBytecode {
    pub fn_id: u32,
    pub type_id: u32,
    pub local_count: u16,
    pub param_count: u16,
    pub max_stack: u16,
    pub effect_mask: u16,
    pub code: Vec<u8>,
    pub handlers: Vec<HandlerEntry>,
}

/// A pending top-level function to emit.
pub(crate) struct FnEntry {
    pub fn_id: u32,
    pub name: String,
    pub params: Vec<Param>,
    pub body: ExprIdx,
    pub effect_mask: u16,
}

/// Orchestrates full module emission directly from AST+sema.
pub struct Emitter<'a> {
    pub(crate) ast: &'a AstArenas,
    pub(crate) sema: &'a SemaResult,
    pub interner: &'a mut Interner,
    stmts: Vec<Stmt>,
    pub cp: ConstPool,
    pub tp: TypePool,
    /// DefId → bytecode fn_id for user-defined functions.
    pub(crate) fn_map: HashMap<DefId, u32>,
    /// DefId → index into foreign_fns for FFI functions.
    pub(crate) foreign_map: HashMap<DefId, u32>,
    pub foreign_fns: Vec<ForeignFn>,
    pub effects: Vec<EffectDef>,
    /// Index into foreign_fns for the str_cat helper (used by f-string desugar).
    pub(crate) str_cat_ffi_idx: Option<u32>,
    next_fn_id: u32,
    pub entry_fn_id: Option<u32>,
    fn_entries: Vec<FnEntry>,
    pub(crate) nested_fns: Vec<FnBytecode>,
}

/// Per-function emission context.
pub(crate) struct FnCtx {
    pub fe: FnEmitter,
    pub local_map: HashMap<DefId, u32>,
    next_label: u32,
}

impl FnCtx {
    fn new(param_count: u16) -> Self {
        Self {
            fe: FnEmitter::new(param_count, param_count),
            local_map: HashMap::new(),
            next_label: 0,
        }
    }

    pub fn fresh_label(&mut self) -> u32 {
        let l = self.next_label;
        self.next_label += 1;
        l
    }

    pub fn alloc_local(&mut self) -> u32 {
        self.fe.alloc_local()
    }
}

impl<'a> Emitter<'a> {
    pub fn new(parsed: &'a ParsedModule, sema: &'a SemaResult, interner: &'a mut Interner) -> Self {
        Self {
            ast: &parsed.arenas,
            sema,
            interner,
            stmts: parsed.stmts.clone(),
            cp: ConstPool::new(),
            tp: TypePool::new(),
            fn_map: HashMap::new(),
            foreign_map: HashMap::new(),
            foreign_fns: vec![],
            effects: vec![],
            str_cat_ffi_idx: None,
            next_fn_id: 0,
            entry_fn_id: None,
            fn_entries: vec![],
            nested_fns: vec![],
        }
    }

    pub fn emit_all(&mut self) -> Result<Vec<FnBytecode>, EmitError> {
        self.register_well_known_fns()?;
        self.scan_top_level()?;
        self.emit_functions()
    }

    pub(crate) fn alloc_fn_id(&mut self) -> u32 {
        let id = self.next_fn_id;
        self.next_fn_id += 1;
        id
    }

    fn register_well_known_fns(&mut self) -> Result<(), EmitError> {
        let wk = &self.sema.well_known;

        let str_type_id = self.tp.lower_well_known_def(wk.string, wk).ok_or_else(|| {
            EmitError::UnresolvableType {
                desc: "String type".into(),
            }
        })?;
        let unit_type_id = self.tp.lower_well_known_def(wk.unit, wk).ok_or_else(|| {
            EmitError::UnresolvableType {
                desc: "Unit type".into(),
            }
        })?;
        let any_type_id = self.tp.lower_well_known_def(wk.any, wk).ok_or_else(|| {
            EmitError::UnresolvableType {
                desc: "Any type".into(),
            }
        })?;

        // writeln: (String) ~> Unit under { IO }
        let writeln_sym = self.interner.intern("musi_writeln");
        let writeln_idx = push_foreign_fn(
            &mut self.foreign_fns,
            writeln_sym,
            &[str_type_id],
            unit_type_id,
        )?;
        let _ = self.foreign_map.insert(wk.fns.writeln, writeln_idx);

        // write: (String) ~> Unit under { IO }
        let write_sym = self.interner.intern("musi_write");
        let write_idx = push_foreign_fn(
            &mut self.foreign_fns,
            write_sym,
            &[str_type_id],
            unit_type_id,
        )?;
        let _ = self.foreign_map.insert(wk.fns.write, write_idx);

        // show: (Any) -> String
        let show_sym = self.interner.intern("musi_show");
        let show_idx =
            push_foreign_fn(&mut self.foreign_fns, show_sym, &[any_type_id], str_type_id)?;
        let _ = self.foreign_map.insert(wk.fns.show, show_idx);

        // str_cat: (String, String) -> String  [internal helper for f-string desugar]
        let str_cat_sym = self.interner.intern("musi_str_cat");
        let str_cat_idx = push_foreign_fn(
            &mut self.foreign_fns,
            str_cat_sym,
            &[str_type_id, str_type_id],
            str_type_id,
        )?;
        self.str_cat_ffi_idx = Some(str_cat_idx);

        Ok(())
    }

    fn scan_top_level(&mut self) -> Result<(), EmitError> {
        let stmts = self.stmts.clone();
        for stmt in &stmts {
            self.scan_stmt(stmt.expr)?;
        }
        Ok(())
    }

    fn scan_stmt(&mut self, expr_idx: ExprIdx) -> Result<(), EmitError> {
        let expr = self.ast.exprs[expr_idx].clone();
        match expr {
            Expr::Annotated { attrs, inner, .. } => {
                self.scan_annotated(&attrs, inner)?;
            }
            Expr::Binding { fields, .. } => {
                self.scan_binding(&fields, false)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn scan_annotated(&mut self, attrs: &[Attr], inner: ExprIdx) -> Result<(), EmitError> {
        let is_entry = has_entrypoint_attr(attrs, self.interner);
        let inner_expr = self.ast.exprs[inner].clone();
        match inner_expr {
            Expr::Binding { fields, .. } => {
                self.scan_binding(&fields, is_entry)?;
            }
            Expr::Annotated {
                attrs: inner_attrs,
                inner: inner2,
                ..
            } => {
                let mut combined = attrs.to_vec();
                combined.extend_from_slice(&inner_attrs);
                self.scan_annotated(&combined, inner2)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn scan_binding(&mut self, fields: &LetFields, is_entry: bool) -> Result<(), EmitError> {
        let Some(value_idx) = fields.value else {
            return Ok(());
        };

        let (fn_params, fn_body) = match extract_fn(value_idx, self.ast) {
            Some(p) => p,
            None => return Ok(()),
        };

        let fn_id = self.alloc_fn_id();

        let binding_span = pat_span(fields.pat, self.ast);
        if let Some(&did) = self.sema.resolution.pat_defs.get(&binding_span) {
            let _ = self.fn_map.insert(did, fn_id);
            if is_entry {
                self.entry_fn_id = Some(fn_id);
            }
        }

        let fn_name = pat_name_str(fields.pat, self.ast, self.interner);
        self.fn_entries.push(FnEntry {
            fn_id,
            name: fn_name,
            params: fn_params,
            body: fn_body,
            effect_mask: 0,
        });
        Ok(())
    }

    fn emit_functions(&mut self) -> Result<Vec<FnBytecode>, EmitError> {
        let entries: Vec<FnEntry> = std::mem::take(&mut self.fn_entries);
        let mut results = Vec::with_capacity(entries.len());
        for entry in entries {
            let bc = self.emit_one_function(&entry)?;
            results.push(bc);
        }
        let mut nested = std::mem::take(&mut self.nested_fns);
        results.append(&mut nested);
        results.sort_by_key(|b| b.fn_id);
        Ok(results)
    }

    fn emit_one_function(&mut self, entry: &FnEntry) -> Result<FnBytecode, EmitError> {
        let param_count =
            u16::try_from(entry.params.len()).map_err(|_| EmitError::UnresolvableType {
                desc: "too many params".into(),
            })?;

        let mut fc = FnCtx::new(param_count);

        for (i, param) in entry.params.iter().enumerate() {
            let slot = u32::try_from(i).map_err(|_| EmitError::UnresolvableType {
                desc: "param index overflow".into(),
            })?;
            if let Some(&did) = self.sema.resolution.pat_defs.get(&param.span) {
                let _ = fc.local_map.insert(did, slot);
            } else {
                for def in &self.sema.defs {
                    if def.kind == DefKind::Param
                        && def.name == param.name
                        && def.span == param.span
                    {
                        let _ = fc.local_map.insert(def.id, slot);
                        break;
                    }
                }
            }
        }

        let had_value = expr::emit_expr(self, &mut fc, entry.body)?;
        if had_value {
            fc.fe.emit_ret();
        } else {
            fc.fe.emit_ret_u();
        }

        fc.fe.resolve_fixups(&entry.name)?;
        let _code_len = fc.fe.validate_code_len()?;

        let type_id = self
            .tp
            .lower_well_known_def(self.sema.well_known.unit, &self.sema.well_known)
            .ok_or_else(|| EmitError::UnresolvableType {
                desc: "Unit type".into(),
            })?;

        Ok(FnBytecode {
            fn_id: entry.fn_id,
            type_id,
            local_count: fc.fe.local_count,
            param_count: fc.fe.param_count,
            max_stack: fc.fe.max_stack,
            effect_mask: entry.effect_mask,
            code: fc.fe.code,
            handlers: fc.fe.handlers,
        })
    }
}

/// Serialize the function pool section into `buf`.
pub fn write_function_pool(buf: &mut Vec<u8>, functions: &[FnBytecode]) -> Result<(), EmitError> {
    let count = u32::try_from(functions.len()).map_err(|_| EmitError::UnresolvableType {
        desc: "too many functions".into(),
    })?;
    buf.extend_from_slice(&count.to_le_bytes());
    for fn_bc in functions {
        buf.extend_from_slice(&fn_bc.fn_id.to_le_bytes());
        buf.extend_from_slice(&fn_bc.type_id.to_le_bytes());
        buf.extend_from_slice(&fn_bc.local_count.to_le_bytes());
        buf.extend_from_slice(&fn_bc.param_count.to_le_bytes());
        buf.extend_from_slice(&fn_bc.max_stack.to_le_bytes());
        buf.extend_from_slice(&fn_bc.effect_mask.to_le_bytes());
        let code_len = u32::try_from(fn_bc.code.len()).map_err(|_| EmitError::FunctionTooLarge)?;
        buf.extend_from_slice(&code_len.to_le_bytes());
        buf.extend_from_slice(&fn_bc.code);
        let handler_count =
            u16::try_from(fn_bc.handlers.len()).map_err(|_| EmitError::OperandOverflow {
                desc: "too many handler entries".into(),
            })?;
        buf.extend_from_slice(&handler_count.to_le_bytes());
        for h in &fn_bc.handlers {
            buf.push(h.effect_id);
            buf.extend_from_slice(&h.handler_fn_id.to_le_bytes());
        }
    }
    Ok(())
}

// ── private helpers ───────────────────────────────────────────────────────────

fn push_foreign_fn(
    fns: &mut Vec<ForeignFn>,
    ext_name: Symbol,
    param_type_ids: &[u32],
    ret_type_id: u32,
) -> Result<u32, EmitError> {
    fns.push(ForeignFn {
        ext_name,
        library: None,
        param_type_ids: param_type_ids.to_vec(),
        ret_type_id,
        variadic: false,
    });
    u32::try_from(fns.len() - 1).map_err(|_| EmitError::UnresolvableType {
        desc: "foreign fn index overflow".into(),
    })
}

/// Recursively unwrap Annotated/Quantified/Paren to find an inner Fn.
fn extract_fn(expr_idx: ExprIdx, ast: &AstArenas) -> Option<(Vec<Param>, ExprIdx)> {
    match &ast.exprs[expr_idx] {
        Expr::Fn { params, body, .. } => Some((params.clone(), *body)),
        Expr::Annotated { inner, .. } => extract_fn(*inner, ast),
        Expr::Quantified { body, .. } => extract_fn(*body, ast),
        Expr::Paren { inner, .. } => extract_fn(*inner, ast),
        _ => None,
    }
}

fn has_entrypoint_attr(attrs: &[Attr], interner: &Interner) -> bool {
    attrs
        .iter()
        .any(|a| interner.resolve(a.name) == "entrypoint")
}

fn pat_span(pat_idx: PatIdx, ast: &AstArenas) -> Span {
    match &ast.pats[pat_idx] {
        Pat::Bind { span, .. }
        | Pat::Wild { span }
        | Pat::Lit { span, .. }
        | Pat::Variant { span, .. }
        | Pat::Record { span, .. }
        | Pat::Tuple { span, .. }
        | Pat::Array { span, .. }
        | Pat::Or { span, .. }
        | Pat::Error { span } => *span,
    }
}

fn pat_name_str(pat_idx: PatIdx, ast: &AstArenas, interner: &Interner) -> String {
    match &ast.pats[pat_idx] {
        Pat::Bind { name, .. } => interner.resolve(*name).to_owned(),
        _ => "<anonymous>".to_owned(),
    }
}
