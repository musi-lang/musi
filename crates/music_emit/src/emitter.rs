//! Main emitter orchestrator: tree-walks AST+sema to produce bytecode.

#[cfg(test)]
mod tests;

mod control;
mod desugar;
pub mod expr;
mod fn_emitter;

use std::collections::{HashMap, HashSet};
use std::mem;

use music_ast::Pat;
use music_ast::attr::Attr;
use music_ast::decl::{ClassMember, EffectOp, ForeignDecl};
use music_ast::expr::{Expr, LetFields, Param};
use music_ast::{AstArenas, ExprIdx, ParsedModule, PatIdx, Stmt};
use music_sema::SemaResult;
use music_sema::Type;
use music_sema::def::{DefId, DefKind};
use music_shared::{FileId, Interner, Span, Symbol};

use crate::const_pool::ConstPool;
use crate::error::EmitError;
use crate::module::{EffectDef, EffectOpDef, ForeignFn};
use crate::type_pool::TypePool;

pub use fn_emitter::{FnEmitter, HandlerEntry};

/// Per-function bytecode output.
pub struct FnBytecode {
    pub fn_id: u32,
    pub type_id: u32,
    pub local_count: u16,
    pub param_count: u16,
    pub max_stack: u16,
    pub effect_mask: u16,
    pub upvalue_count: u16,
    pub code: Vec<u8>,
    pub handlers: Vec<HandlerEntry>,
}

/// A pending top-level function to emit.
pub struct FnEntry {
    pub fn_id: u32,
    pub def_id: Option<DefId>,
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
    /// `DefId` -> bytecode `fn_id` for user-defined functions.
    pub(crate) fn_map: HashMap<DefId, u32>,
    /// `DefId` -> index into `foreign_fns` for FFI functions.
    pub(crate) foreign_map: HashMap<DefId, u32>,
    pub foreign_fns: Vec<ForeignFn>,
    pub effects: Vec<EffectDef>,
    /// `DefId` -> numeric effect ID (index into `effects`) for user-defined effects.
    pub(crate) effect_id_map: HashMap<DefId, u8>,
    /// `DefId` -> `op.id` for user-defined effect operations.
    pub(crate) op_id_map: HashMap<DefId, u32>,
    /// Index into `foreign_fns` for the `str_cat` helper (used by f-string desugar).
    pub(crate) str_cat_ffi_idx: Option<u32>,
    /// Variant tag for `Some` (resolved at init, fallback 0).
    pub(crate) some_tag: u32,
    /// Variant tag for `Ok` (resolved at init, fallback 0).
    pub(crate) ok_tag: u32,
    next_fn_id: u32,
    pub entry_fn_id: Option<u32>,
    fn_entries: Vec<FnEntry>,
    pub(crate) nested_fns: Vec<FnBytecode>,
    pub file_id: FileId,
}

/// Per-function emission context.
pub struct FnCtx {
    pub fe: FnEmitter,
    pub local_map: HashMap<DefId, u32>,
    pub ref_locals: HashSet<DefId>,
    /// Maps captured `DefId`s to upvalue indices within the closure object.
    pub upvalue_map: HashMap<DefId, u16>,
    pub deferred: Vec<ExprIdx>,
    next_label: u32,
    /// Maps constraint class `DefId` -> local slot holding the dictionary.
    pub dict_slots: HashMap<DefId, u32>,
}

impl FnCtx {
    fn new(param_count: u16) -> Self {
        Self {
            fe: FnEmitter::new(param_count, param_count),
            local_map: HashMap::new(),
            ref_locals: HashSet::new(),
            upvalue_map: HashMap::new(),
            deferred: vec![],
            next_label: 0,
            dict_slots: HashMap::new(),
        }
    }

    pub const fn fresh_label(&mut self) -> u32 {
        let l = self.next_label;
        self.next_label += 1;
        l
    }

    pub fn alloc_local(&mut self) -> u32 {
        self.fe.alloc_local()
    }
}

impl<'a> Emitter<'a> {
    pub fn new(
        parsed: &'a ParsedModule,
        sema: &'a SemaResult,
        interner: &'a mut Interner,
        file_id: FileId,
    ) -> Self {
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
            effect_id_map: HashMap::new(),
            op_id_map: HashMap::new(),
            str_cat_ffi_idx: None,
            some_tag: 0,
            ok_tag: 0,
            next_fn_id: 0,
            entry_fn_id: None,
            fn_entries: vec![],
            nested_fns: vec![],
            file_id,
        }
    }

    pub fn emit_all(&mut self) -> Result<Vec<FnBytecode>, EmitError> {
        self.register_well_known_fns()?;
        self.resolve_well_known_tags();
        self.scan_top_level()?;
        let functions = self.emit_functions()?;
        if functions.is_empty() && self.entry_fn_id.is_some() {
            return Err(EmitError::UnsupportedFeature {
                desc: format!("file {} has entry point but no functions", self.file_id.0).into(),
            });
        }
        Ok(functions)
    }

    pub const fn alloc_fn_id(&mut self) -> u32 {
        let id = self.next_fn_id;
        self.next_fn_id += 1;
        id
    }

    #[allow(clippy::too_many_lines)]
    fn register_well_known_fns(&mut self) -> Result<(), EmitError> {
        let wk = &self.sema.well_known;

        let str_type_id = self
            .tp
            .lower_well_known_def(wk.string, wk)
            .ok_or_else(|| EmitError::unresolvable("String type"))?;
        let unit_type_id = self
            .tp
            .lower_well_known_def(wk.unit, wk)
            .ok_or_else(|| EmitError::unresolvable("Unit type"))?;
        let any_type_id = self
            .tp
            .lower_well_known_def(wk.any, wk)
            .ok_or_else(|| EmitError::unresolvable("Any type"))?;

        // writeln: (String) ~> Unit with { IO }
        let writeln_sym = self.interner.intern("musi_writeln");
        let writeln_idx = push_foreign_fn(
            &mut self.foreign_fns,
            writeln_sym,
            &[str_type_id],
            unit_type_id,
        )?;
        let _ = self.foreign_map.insert(wk.fns.writeln, writeln_idx);

        // write: (String) ~> Unit with { IO }
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

        // Core builtins (musi:core)
        let int_type_id = self
            .tp
            .lower_well_known_def(wk.ints.int, wk)
            .ok_or_else(|| EmitError::unresolvable("Int type"))?;
        let bool_type_id = self
            .tp
            .lower_well_known_def(wk.bool, wk)
            .ok_or_else(|| EmitError::unresolvable("Bool type"))?;

        let core = &wk.core;
        let mut register_core =
            |def_id, ext_name: &str, params: &[u32], ret| -> Result<(), EmitError> {
                let sym = self.interner.intern(ext_name);
                let idx = push_foreign_fn(&mut self.foreign_fns, sym, params, ret)?;
                let _ = self.foreign_map.insert(def_id, idx);
                Ok(())
            };

        register_core(core.int_abs, "musi_int_abs", &[int_type_id], int_type_id)?;
        register_core(
            core.int_min,
            "musi_int_min",
            &[int_type_id, int_type_id],
            int_type_id,
        )?;
        register_core(
            core.int_max,
            "musi_int_max",
            &[int_type_id, int_type_id],
            int_type_id,
        )?;
        register_core(
            core.int_clamp,
            "musi_int_clamp",
            &[int_type_id, int_type_id, int_type_id],
            int_type_id,
        )?;
        register_core(
            core.int_pow,
            "musi_int_pow",
            &[int_type_id, int_type_id],
            int_type_id,
        )?;
        register_core(core.str_len, "musi_str_len", &[str_type_id], int_type_id)?;
        register_core(
            core.str_contains,
            "musi_str_contains",
            &[str_type_id, str_type_id],
            bool_type_id,
        )?;
        register_core(
            core.str_starts_with,
            "musi_str_starts_with",
            &[str_type_id, str_type_id],
            bool_type_id,
        )?;
        register_core(
            core.str_ends_with,
            "musi_str_ends_with",
            &[str_type_id, str_type_id],
            bool_type_id,
        )?;
        register_core(core.arr_len, "musi_arr_len", &[any_type_id], int_type_id)?;
        register_core(
            core.arr_push,
            "musi_arr_push",
            &[any_type_id, any_type_id],
            unit_type_id,
        )?;
        register_core(core.arr_pop, "musi_arr_pop", &[any_type_id], any_type_id)?;
        register_core(
            core.arr_reverse,
            "musi_arr_reverse",
            &[any_type_id],
            any_type_id,
        )?;

        Ok(())
    }

    fn resolve_well_known_tags(&mut self) {
        let some_sym = self.interner.intern("Some");
        let ok_sym = self.interner.intern("Ok");
        self.some_tag = expr::resolve_variant_tag_by_name(self, some_sym).unwrap_or(0);
        self.ok_tag = expr::resolve_variant_tag_by_name(self, ok_sym).unwrap_or(0);
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
                self.scan_binding(&fields, false);
            }
            Expr::Effect { name, ops, .. } => {
                self.scan_effect(name, &ops)?;
            }
            Expr::Foreign { abi, decls, .. } => {
                self.scan_foreign(abi, &decls)?;
            }
            Expr::Instance { members, .. } => {
                self.scan_instance_members(&members);
            }
            Expr::Lit { .. }
            | Expr::Name { .. }
            | Expr::Paren { .. }
            | Expr::Tuple { .. }
            | Expr::Block { .. }
            | Expr::Let { .. }
            | Expr::Fn { .. }
            | Expr::Call { .. }
            | Expr::Field { .. }
            | Expr::Index { .. }
            | Expr::Update { .. }
            | Expr::Record { .. }
            | Expr::Array { .. }
            | Expr::Variant { .. }
            | Expr::Choice { .. }
            | Expr::RecordDef { .. }
            | Expr::BinOp { .. }
            | Expr::UnaryOp { .. }
            | Expr::Piecewise { .. }
            | Expr::Match { .. }
            | Expr::Return { .. }
            | Expr::Import { .. }
            | Expr::Export { .. }
            | Expr::Class { .. }
            | Expr::TypeCheck { .. }
            | Expr::Handle { .. }
            | Expr::Error { .. } => {}
        }
        Ok(())
    }

    fn scan_annotated(&mut self, attrs: &[Attr], inner: ExprIdx) -> Result<(), EmitError> {
        let is_entry = has_entrypoint_attr(attrs, self.interner);
        let inner_expr = self.ast.exprs[inner].clone();
        match inner_expr {
            Expr::Binding { fields, .. } => {
                self.scan_binding(&fields, is_entry);
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
            Expr::Effect { name, ops, .. } => {
                self.scan_effect(name, &ops)?;
            }
            Expr::Foreign { abi, decls, .. } => {
                self.scan_foreign(abi, &decls)?;
            }
            Expr::Instance { members, .. } => {
                self.scan_instance_members(&members);
            }
            Expr::Lit { .. }
            | Expr::Name { .. }
            | Expr::Paren { .. }
            | Expr::Tuple { .. }
            | Expr::Block { .. }
            | Expr::Let { .. }
            | Expr::Fn { .. }
            | Expr::Call { .. }
            | Expr::Field { .. }
            | Expr::Index { .. }
            | Expr::Update { .. }
            | Expr::Record { .. }
            | Expr::Array { .. }
            | Expr::Variant { .. }
            | Expr::Choice { .. }
            | Expr::RecordDef { .. }
            | Expr::BinOp { .. }
            | Expr::UnaryOp { .. }
            | Expr::Piecewise { .. }
            | Expr::Match { .. }
            | Expr::Return { .. }
            | Expr::Import { .. }
            | Expr::Export { .. }
            | Expr::Class { .. }
            | Expr::TypeCheck { .. }
            | Expr::Handle { .. }
            | Expr::Error { .. } => {}
        }
        Ok(())
    }

    fn scan_foreign(&mut self, abi: Symbol, decls: &[ForeignDecl]) -> Result<(), EmitError> {
        let abi_str = self.interner.resolve(abi).to_owned();
        let any_type_id = self
            .tp
            .lower_well_known_def(self.sema.well_known.any, &self.sema.well_known)
            .ok_or_else(|| EmitError::unresolvable("Any type"))?;
        let unit_type_id = self
            .tp
            .lower_well_known_def(self.sema.well_known.unit, &self.sema.well_known)
            .ok_or_else(|| EmitError::unresolvable("Unit type"))?;

        for decl in decls {
            let ForeignDecl::Fn {
                name,
                ext_name,
                span,
                ..
            } = decl
            else {
                continue;
            };

            let def_id = self
                .sema
                .defs
                .iter()
                .find(|d| d.kind == DefKind::ForeignFn && d.name == *name && d.span == *span)
                .map(|d| d.id);

            let Some(def_id) = def_id else {
                continue;
            };

            // Skip if already registered (e.g., a well-known fn).
            if self.foreign_map.contains_key(&def_id) {
                continue;
            }

            let (param_type_ids, ret_type_id) =
                self.lower_foreign_fn_type(def_id, any_type_id, unit_type_id);

            let ext_sym = ext_name.unwrap_or(*name);
            let library = if abi_str == "C" { None } else { Some(abi) };

            let idx = u32::try_from(self.foreign_fns.len())
                .map_err(|_| EmitError::overflow("foreign fn index overflow"))?;
            self.foreign_fns.push(ForeignFn {
                ext_name: ext_sym,
                library,
                param_type_ids,
                ret_type_id,
                variadic: false,
            });
            let _ = self.foreign_map.insert(def_id, idx);
        }
        Ok(())
    }

    fn lower_foreign_fn_type(
        &mut self,
        def_id: DefId,
        any_type_id: u32,
        unit_type_id: u32,
    ) -> (Vec<u32>, u32) {
        let ty_idx = self
            .sema
            .defs
            .iter()
            .find(|d| d.id == def_id)
            .and_then(|d| d.ty_info.ty);

        let Some(ty_idx) = ty_idx else {
            return (vec![], unit_type_id);
        };

        let resolved = self.sema.unify.resolve(ty_idx, &self.sema.types);
        let (params, ret) = match &self.sema.types[resolved] {
            Type::Fn { params, ret, .. } => (params.clone(), *ret),
            _ => return (vec![], unit_type_id),
        };

        let param_type_ids: Vec<u32> = params
            .iter()
            .map(|&p| {
                self.tp
                    .lower_sema_type(p, &self.sema.types, &self.sema.unify, &self.sema.well_known)
                    .unwrap_or(any_type_id)
            })
            .collect();

        let ret_type_id = self
            .tp
            .lower_sema_type(
                ret,
                &self.sema.types,
                &self.sema.unify,
                &self.sema.well_known,
            )
            .unwrap_or(unit_type_id);

        (param_type_ids, ret_type_id)
    }

    fn scan_effect(&mut self, effect_name: Symbol, ops: &[EffectOp]) -> Result<(), EmitError> {
        let effect_def_id = self
            .sema
            .defs
            .iter()
            .find(|d| d.kind == DefKind::Effect && d.name == effect_name)
            .map(|d| d.id);

        let Some(effect_def_id) = effect_def_id else {
            return Ok(());
        };

        if self.effect_id_map.contains_key(&effect_def_id) {
            return Ok(());
        }

        let raw_effect_id = self.effects.len();
        let effect_id =
            u8::try_from(raw_effect_id).map_err(|_| EmitError::overflow("too many effects"))?;

        let unit_type_id = self
            .tp
            .lower_well_known_def(self.sema.well_known.unit, &self.sema.well_known)
            .ok_or_else(|| EmitError::unresolvable("Unit type"))?;

        let mut op_defs = Vec::with_capacity(ops.len());
        for (op_idx, op) in ops.iter().enumerate() {
            let op_def_id = self
                .sema
                .defs
                .iter()
                .find(|d| {
                    d.kind == DefKind::EffectOp
                        && d.name == op.name
                        && d.parent == Some(effect_def_id)
                })
                .map(|d| d.id);

            let op_id = u32::try_from(op_idx)
                .map_err(|_| EmitError::overflow("effect op index overflow"))?;

            let (param_type_ids, ret_type_id) = op_def_id.map_or_else(
                || (vec![], unit_type_id),
                |did| self.resolve_effect_op_types(did, unit_type_id),
            );

            if let Some(did) = op_def_id {
                let _ = self.op_id_map.insert(did, op_id);
            }

            op_defs.push(EffectOpDef {
                id: op_id,
                name: op.name,
                param_type_ids,
                ret_type_id,
                fatal: op.fatal,
            });
        }

        let effect_id_u32 = u32::from(effect_id);
        self.effects.push(EffectDef {
            id: effect_id_u32,
            name: effect_name,
            ops: op_defs,
        });
        let _ = self.effect_id_map.insert(effect_def_id, effect_id);
        Ok(())
    }

    fn resolve_effect_op_types(&mut self, op_def_id: DefId, unit_type_id: u32) -> (Vec<u32>, u32) {
        let ty_idx = self
            .sema
            .defs
            .iter()
            .find(|d| d.id == op_def_id)
            .and_then(|d| d.ty_info.ty);

        let Some(ty_idx) = ty_idx else {
            return (vec![], unit_type_id);
        };

        let resolved = self.sema.unify.resolve(ty_idx, &self.sema.types);
        let (params, ret) = match &self.sema.types[resolved] {
            Type::Fn { params, ret, .. } => (params.clone(), *ret),
            _ => return (vec![], unit_type_id),
        };

        let param_type_ids: Vec<u32> = params
            .iter()
            .map(|&p| {
                self.tp
                    .lower_sema_type(p, &self.sema.types, &self.sema.unify, &self.sema.well_known)
                    .unwrap_or(unit_type_id)
            })
            .collect();

        let ret_type_id = self
            .tp
            .lower_sema_type(
                ret,
                &self.sema.types,
                &self.sema.unify,
                &self.sema.well_known,
            )
            .unwrap_or(unit_type_id);

        (param_type_ids, ret_type_id)
    }

    fn scan_binding(&mut self, fields: &LetFields, is_entry: bool) {
        let Some(value_idx) = fields.value else {
            return;
        };

        let Some((fn_params, fn_body)) = extract_fn(value_idx, self.ast) else {
            return;
        };

        let fn_id = self.alloc_fn_id();

        let binding_span = pat_span(fields.pat, self.ast);
        let mut binding_def_id = None;
        if let Some(&did) = self.sema.resolution.pat_defs.get(&binding_span) {
            let _ = self.fn_map.insert(did, fn_id);
            binding_def_id = Some(did);
            if is_entry {
                self.entry_fn_id = Some(fn_id);
            }
        }

        let fn_name = pat_name_str(fields.pat, self.ast, self.interner);
        self.fn_entries.push(FnEntry {
            fn_id,
            def_id: binding_def_id,
            name: fn_name,
            params: fn_params,
            body: fn_body,
            effect_mask: 0,
        });
    }

    /// Register instance method bodies as top-level functions so `emit_functions`
    /// can compile them. Laws are skipped — they are not emitted.
    fn scan_instance_members(&mut self, members: &[ClassMember]) {
        for member in members {
            let ClassMember::Fn {
                sig,
                default: Some(body),
                ..
            } = member
            else {
                continue;
            };
            // Find the sema DefId for this instance method by matching name+span.
            let def_id = self
                .sema
                .defs
                .iter()
                .find(|d| d.kind == DefKind::Fn && d.name == sig.name && d.span == sig.span)
                .map(|d| d.id);

            let fn_id = self.alloc_fn_id();

            if let Some(did) = def_id {
                let _ = self.fn_map.insert(did, fn_id);
            }

            let fn_name = self.interner.resolve(sig.name).to_owned();
            self.fn_entries.push(FnEntry {
                fn_id,
                def_id,
                name: fn_name,
                params: sig.params.clone(),
                body: *body,
                effect_mask: 0,
            });
        }
    }

    fn emit_functions(&mut self) -> Result<Vec<FnBytecode>, EmitError> {
        let entries: Vec<FnEntry> = mem::take(&mut self.fn_entries);
        let mut results = Vec::with_capacity(entries.len());
        for entry in entries {
            let bc = self.emit_one_function(&entry)?;
            results.push(bc);
        }
        let mut nested = mem::take(&mut self.nested_fns);
        results.append(&mut nested);
        results.sort_by_key(|b| b.fn_id);
        Ok(results)
    }

    fn resolve_fn_type_id(&mut self, def_id: Option<DefId>) -> Result<u32, EmitError> {
        if let Some(did) = def_id
            && let Some(def_info) = self.sema.defs.iter().find(|d| d.id == did)
            && let Some(ty_idx) = def_info.ty_info.ty
        {
            return self.tp.lower_sema_type(
                ty_idx,
                &self.sema.types,
                &self.sema.unify,
                &self.sema.well_known,
            );
        }
        self.tp
            .lower_well_known_def(self.sema.well_known.unit, &self.sema.well_known)
            .ok_or_else(|| EmitError::unresolvable("Unit type"))
    }

    fn emit_one_function(&mut self, entry: &FnEntry) -> Result<FnBytecode, EmitError> {
        // Determine implicit dictionary parameter count from constraints
        let constraints = entry
            .def_id
            .and_then(|did| self.sema.fn_constraints.get(&did));
        let dict_count = constraints.map_or(0, Vec::len);
        let total_params = entry.params.len() + dict_count;
        let param_count =
            u16::try_from(total_params).map_err(|_| EmitError::overflow("too many params"))?;

        let mut fc = FnCtx::new(param_count);

        // Slots 0..dict_count are implicit dictionary parameters
        if let Some(constraints) = constraints {
            for (i, ob) in constraints.iter().enumerate() {
                let slot =
                    u32::try_from(i).map_err(|_| EmitError::overflow("dict slot overflow"))?;
                let _ = fc.dict_slots.insert(ob.class, slot);
            }
        }

        // Slots dict_count..total are explicit parameters (shifted by dict_count)
        for (i, param) in entry.params.iter().enumerate() {
            let slot = u32::try_from(i + dict_count)
                .map_err(|_| EmitError::overflow("param index overflow"))?;
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

        let had_value = expr::emit_expr_tail(self, &mut fc, entry.body, true)?;
        if had_value {
            if !fc.deferred.is_empty() {
                let tmp = fc.alloc_local();
                fc.fe.emit_st_loc(tmp);
                expr::emit_deferred_cleanup(self, &mut fc)?;
                fc.fe.emit_ld_loc(tmp);
            }
            fc.fe.emit_ret();
        } else {
            expr::emit_deferred_cleanup(self, &mut fc)?;
            fc.fe.emit_ret_u();
        }

        fc.fe.resolve_fixups(&entry.name)?;
        let _code_len = fc.fe.validate_code_len()?;

        let type_id = self.resolve_fn_type_id(entry.def_id)?;

        Ok(FnBytecode {
            fn_id: entry.fn_id,
            type_id,
            local_count: fc.fe.local_count,
            param_count: fc.fe.param_count,
            max_stack: fc.fe.max_stack,
            effect_mask: entry.effect_mask,
            upvalue_count: 0,
            code: fc.fe.code,
            handlers: fc.fe.handlers,
        })
    }
}

/// Serialize the function pool section into `buf`.
pub fn write_function_pool(buf: &mut Vec<u8>, functions: &[FnBytecode]) -> Result<(), EmitError> {
    let count =
        u32::try_from(functions.len()).map_err(|_| EmitError::overflow("too many functions"))?;
    buf.extend_from_slice(&count.to_le_bytes());
    for fn_bc in functions {
        buf.extend_from_slice(&fn_bc.fn_id.to_le_bytes());
        buf.extend_from_slice(&fn_bc.type_id.to_le_bytes());
        buf.extend_from_slice(&fn_bc.local_count.to_le_bytes());
        buf.extend_from_slice(&fn_bc.param_count.to_le_bytes());
        buf.extend_from_slice(&fn_bc.max_stack.to_le_bytes());
        buf.extend_from_slice(&fn_bc.effect_mask.to_le_bytes());
        buf.extend_from_slice(&fn_bc.upvalue_count.to_le_bytes());
        let code_len = u32::try_from(fn_bc.code.len()).map_err(|_| EmitError::FunctionTooLarge)?;
        buf.extend_from_slice(&code_len.to_le_bytes());
        buf.extend_from_slice(&fn_bc.code);
        let handler_count = u16::try_from(fn_bc.handlers.len())
            .map_err(|_| EmitError::overflow("too many handler entries"))?;
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
    u32::try_from(fns.len() - 1).map_err(|_| EmitError::overflow("foreign fn index overflow"))
}

/// Recursively unwrap Annotated/Paren to find an inner Fn.
fn extract_fn(expr_idx: ExprIdx, ast: &AstArenas) -> Option<(Vec<Param>, ExprIdx)> {
    match &ast.exprs[expr_idx] {
        Expr::Fn { params, body, .. } => Some((params.clone(), *body)),
        Expr::Annotated { inner, .. } | Expr::Paren { inner, .. } => extract_fn(*inner, ast),
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
