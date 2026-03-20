//! Main emitter orchestrator: tree-walks AST+sema to produce bytecode.

#[cfg(test)]
mod tests;

mod capture;
mod closure;
mod control;
mod desugar;
mod effects;
pub mod expr;
mod fn_emitter;
mod record;
mod type_query;
mod typeclass;

use std::collections::{HashMap, HashSet};
use std::iter::repeat_n;
use std::mem;

use msc_ast::Pat;
use msc_ast::attr::{Attr, AttrValue};
use msc_ast::decl::{ClassMember, EffectOp, ForeignDecl};
use msc_ast::expr::{BinOp, BindKind, Expr, InstanceBody, LetFields, Param};
use msc_ast::lit::Lit;
use msc_ast::ty_param::TyParam;
use msc_ast::{AstArenas, ExprIdx, ParsedModule, PatIdx, Stmt};
use msc_sema::Type;
use msc_sema::def::{DefFlags, DefId, DefKind};
use msc_sema::{DictLookup, Obligation, ResolutionMap, SemaResult, TypeIdx};
use msc_shared::{FileId, Interner, Span, Symbol};

use crate::const_pool::ConstPool;
use crate::error::{EmitError, EmitResult};
use crate::global_table::GlobalTable;
use crate::global_table::{self, GlobalEntry};
use crate::module::{
    ClassDef, ClassInstanceDef, ClassMethodDef, EffectDef, EffectOpDef, ForeignFn,
};
use crate::string_table::StringTable;
use crate::type_table::TypeTable;

pub use fn_emitter::{FnEmitter, HandlerEntry, SafepointEntry};

/// Per-function bytecode output.
pub struct FnBytecode {
    pub fn_id: u32,
    pub name_stridx: u16,
    pub type_id: u32,
    pub local_count: u16,
    pub param_count: u16,
    pub max_stack: u16,
    pub upvalue_count: u16,
    /// Number of implicit dictionary parameters prepended before explicit params.
    pub dict_param_count: u8,
    /// Whether this function is exported from the module (bit0 of flags).
    pub exported: bool,
    /// Whether this function has effectful arrow `~>` (bit2 of flags).
    pub effectful: bool,
    pub code: Vec<u8>,
    pub handlers: Vec<HandlerEntry>,
    /// GC safepoint map entries, one per allocation/call site.
    pub safepoints: Vec<SafepointEntry>,
    /// Effect set: indices into the module's effect table used by this function.
    pub effect_refs: Vec<u16>,
    /// Source map entries: (`bytecode_offset`, `span_byte_offset`, `column=0`).
    pub source_map: Vec<(u32, u32, u16)>,
    /// Local variable debug names: per slot index, (`name_stridx`, `scope_start`, `scope_end`).
    /// Slot order matches the function's local slot layout.
    pub local_names: Vec<(u16, u32, u32)>,
}

/// Per-dependency-module context for multi-module emission.
pub struct DepEmitCtx<'a> {
    pub ast: &'a AstArenas,
    pub stmts: Vec<Stmt>,
    pub resolution: &'a ResolutionMap,
    pub expr_types: &'a HashMap<ExprIdx, TypeIdx>,
    pub binop_dispatch: &'a HashMap<ExprIdx, DefId>,
    pub binop_dict_dispatch: &'a HashMap<ExprIdx, DictLookup>,
    pub fn_constraints: &'a HashMap<DefId, Vec<Obligation>>,
}

/// A pending top-level function to emit.
pub struct FnEntry {
    pub fn_id: u32,
    pub def_id: Option<DefId>,
    pub name: String,
    pub params: Vec<Param>,
    pub body: ExprIdx,
    pub dep_idx: Option<usize>,
}

/// Orchestrates full module emission directly from AST+sema.
pub struct Emitter<'a> {
    pub(crate) ast: &'a AstArenas,
    pub(crate) sema: &'a SemaResult,
    pub interner: &'a mut Interner,
    stmts: Vec<Stmt>,
    pub cp: ConstPool,
    pub tp: TypeTable,
    pub string_table: StringTable,
    pub global_table: GlobalTable,
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
    pub classes: Vec<ClassDef>,
    pub class_instances: Vec<ClassInstanceDef>,
    /// Class name `Symbol` -> index into `classes` vec (stable across calls).
    pub(crate) class_id_map: HashMap<Symbol, u16>,
    /// Variant tag for `Some` (resolved at init, fallback 0).
    pub(crate) some_tag: u32,
    /// Variant tag for `Ok` (resolved at init, fallback 0).
    pub(crate) ok_tag: u32,
    next_fn_id: u32,
    pub entry_fn_id: Option<u32>,
    fn_entries: Vec<FnEntry>,
    pub(crate) nested_fns: Vec<FnBytecode>,
    pub file_id: FileId,
    script: bool,
    dep_contexts: Vec<DepEmitCtx<'a>>,
    active_dep: Option<usize>,
    /// `DefId` -> global slot index for module-level non-function bindings.
    pub(crate) global_map: HashMap<DefId, u32>,
    next_global: u32,
    /// Non-function dep bindings: (`value_expr`, `dep_idx`, `def_id`, `global_slot`).
    dep_global_inits: Vec<(ExprIdx, usize, DefId, u32)>,
    /// Top-level side-effect statements from dep modules (e.g., assignments).
    dep_side_effects: Vec<(ExprIdx, usize)>,
    /// `dep_idx` → local slot in the entry function holding that dep's module record.
    ///
    /// Populated during `emit_script_entry` after all dep globals are initialised.
    /// Used by expression emission to lower imported-global loads to
    /// `ld.loc module_slot` + `rec.get field` instead of the flat `LD_GLB` path.
    pub(crate) dep_module_slots: HashMap<usize, u32>,
    /// `dep_idx` → ordered list of `(DefId, field_index_within_module_record)`.
    ///
    /// Built incrementally by `register_global` when `active_dep` is `Some`.
    /// The field order matches the `TUP_NEW` constructed in `emit_script_entry`.
    pub(crate) dep_global_fields: HashMap<usize, Vec<(DefId, u32)>>,
}

/// Per-function emission context.
pub struct FnCtx {
    pub fe: FnEmitter,
    pub local_map: HashMap<DefId, u32>,
    pub ref_locals: HashSet<DefId>,
    /// Maps captured `DefId`s to upvalue indices within the closure object.
    pub upvalue_map: HashMap<DefId, u16>,
    /// Captured upvalues that are ref cells (need auto-deref via `LD_FLD 0`).
    pub ref_upvalues: HashSet<DefId>,
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
            ref_upvalues: HashSet::new(),
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
        script: bool,
        deps: &[crate::DepEmitInput<'a>],
    ) -> Self {
        let dep_contexts = deps
            .iter()
            .map(|d| DepEmitCtx {
                ast: &d.parsed.arenas,
                stmts: d.parsed.stmts.clone(),
                resolution: d.resolution,
                expr_types: d.expr_types,
                binop_dispatch: d.binop_dispatch,
                binop_dict_dispatch: d.binop_dict_dispatch,
                fn_constraints: d.fn_constraints,
            })
            .collect();
        Self {
            ast: &parsed.arenas,
            sema,
            interner,
            stmts: parsed.stmts.clone(),
            cp: ConstPool::new(),
            tp: TypeTable::new(),
            string_table: StringTable::new(),
            global_table: GlobalTable::new(),
            fn_map: HashMap::new(),
            foreign_map: HashMap::new(),
            foreign_fns: vec![],
            effects: vec![],
            effect_id_map: HashMap::new(),
            op_id_map: HashMap::new(),
            classes: vec![],
            class_instances: vec![],
            class_id_map: HashMap::new(),
            some_tag: 0,
            ok_tag: 0,
            next_fn_id: 0,
            entry_fn_id: None,
            fn_entries: vec![],
            nested_fns: vec![],
            file_id,
            script,
            dep_contexts,
            active_dep: None,
            global_map: HashMap::new(),
            next_global: 0,
            dep_global_inits: vec![],
            dep_side_effects: vec![],
            dep_module_slots: HashMap::new(),
            dep_global_fields: HashMap::new(),
        }
    }

    pub(crate) fn expr_defs(&self) -> &HashMap<ExprIdx, DefId> {
        self.active_dep
            .map_or(&self.sema.resolution.expr_defs, |i| {
                &self.dep_contexts[i].resolution.expr_defs
            })
    }

    pub(crate) fn pat_defs(&self) -> &HashMap<Span, DefId> {
        self.active_dep.map_or(&self.sema.resolution.pat_defs, |i| {
            &self.dep_contexts[i].resolution.pat_defs
        })
    }

    pub(crate) fn active_binop_dispatch(&self) -> &HashMap<ExprIdx, DefId> {
        self.active_dep.map_or(&self.sema.binop_dispatch, |i| {
            self.dep_contexts[i].binop_dispatch
        })
    }

    pub(crate) fn active_binop_dict_dispatch(&self) -> &HashMap<ExprIdx, DictLookup> {
        self.active_dep.map_or(&self.sema.binop_dict_dispatch, |i| {
            self.dep_contexts[i].binop_dict_dispatch
        })
    }

    pub(crate) fn active_fn_constraints(&self) -> &HashMap<DefId, Vec<Obligation>> {
        self.active_dep.map_or(&self.sema.fn_constraints, |i| {
            self.dep_contexts[i].fn_constraints
        })
    }

    pub(crate) fn expr_types(&self) -> &HashMap<ExprIdx, TypeIdx> {
        self.active_dep
            .map_or(&self.sema.expr_types, |i| self.dep_contexts[i].expr_types)
    }

    pub fn emit_all(&mut self) -> EmitResult<Vec<FnBytecode>> {
        self.resolve_well_known_tags();
        self.mark_repr_c_types();
        self.scan_top_level()?;
        self.scan_dep_modules()?;
        self.collect_class_instances();
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

    /// Pre-scan sema defs for `#[repr("C")]` types and mark their `TypeIdx`
    /// on the type table so that `lower_sema_type` emits `TAG_CSTRUCT`.
    fn mark_repr_c_types(&mut self) {
        for def in &self.sema.defs {
            if def.kind == DefKind::Type && def.flags.has(DefFlags::REPR_C) {
                if let Some(ty_idx) = def.ty_info.ty {
                    let resolved = self.sema.unify.resolve(ty_idx, &self.sema.types);
                    self.tp.mark_repr_c(resolved);
                }
            }
        }
    }

    fn resolve_well_known_tags(&mut self) {
        let some_sym = self.interner.intern("Some");
        let ok_sym = self.interner.intern("Ok");
        self.some_tag = type_query::resolve_variant_tag(self, some_sym).unwrap_or(0);
        self.ok_tag = type_query::resolve_variant_tag(self, ok_sym).unwrap_or(0);
    }

    fn scan_top_level(&mut self) -> EmitResult {
        let stmts = self.stmts.clone();
        for stmt in &stmts {
            self.scan_stmt(stmt.expr)?;
        }
        // Script mode: synthesize an entry function from top-level statements.
        if self.script && self.entry_fn_id.is_none() {
            let fn_id = self.alloc_fn_id();
            self.entry_fn_id = Some(fn_id);
        }
        Ok(())
    }

    fn scan_stmt(&mut self, expr_idx: ExprIdx) -> EmitResult {
        let expr = self.ast.exprs[expr_idx].clone();
        match expr {
            Expr::Annotated { attrs, inner, .. } => {
                self.scan_annotated(&attrs, inner, None)?;
            }
            Expr::Let { fields, .. } => {
                self.scan_binding(&fields);
            }
            Expr::Effect { name, ops, .. } => {
                self.scan_effect(name, &ops)?;
            }
            Expr::Foreign { abi, decls, .. } => {
                self.scan_foreign_with_attrs(abi, &decls, &[])?;
            }
            Expr::Instance { body, .. } => {
                if let InstanceBody::Manual { members } = body {
                    self.scan_instance_members(&members, None);
                }
            }
            Expr::Class {
                name,
                params,
                members,
                ..
            } => {
                self.scan_class(name, &params, &members)?;
            }
            Expr::Lit { .. }
            | Expr::Name { .. }
            | Expr::Paren { .. }
            | Expr::Tuple { .. }
            | Expr::Block { .. }
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
            | Expr::TypeCheck { .. }
            | Expr::Handle { .. }
            | Expr::Need { .. }
            | Expr::Resume { .. }
            | Expr::Error { .. }
            | Expr::TypeApp { .. }
            | Expr::FnType { .. }
            | Expr::TypeExpr { .. } => {}
        }
        Ok(())
    }

    fn scan_annotated(
        &mut self,
        attrs: &[Attr],
        inner: ExprIdx,
        dep_idx: Option<usize>,
    ) -> EmitResult {
        let inner_expr = self.ast.exprs[inner].clone();
        match inner_expr {
            Expr::Let { fields, .. } => match dep_idx {
                Some(di) => self.scan_dep_binding(&fields, di)?,
                None => self.scan_binding(&fields),
            },
            Expr::Annotated {
                attrs: inner_attrs,
                inner: inner2,
                ..
            } => {
                let mut combined = attrs.to_vec();
                combined.extend_from_slice(&inner_attrs);
                self.scan_annotated(&combined, inner2, dep_idx)?;
            }
            Expr::Effect { name, ops, .. } if dep_idx.is_none() => {
                self.scan_effect(name, &ops)?;
            }
            Expr::Foreign { abi, decls, .. } => {
                self.scan_foreign_with_attrs(abi, &decls, attrs)?;
            }
            Expr::Instance {
                body: InstanceBody::Manual { members },
                ..
            } => {
                self.scan_instance_members(&members, dep_idx);
            }
            Expr::Class {
                name,
                params,
                members,
                ..
            } if dep_idx.is_none() => {
                self.scan_class(name, &params, &members)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn scan_foreign_with_attrs(
        &mut self,
        abi: Symbol,
        decls: &[ForeignDecl],
        attrs: &[Attr],
    ) -> EmitResult {
        let abi_str = self.interner.resolve(abi).trim_matches('"').to_owned();
        let link_attrs = extract_link_attrs(attrs, self.interner);
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
                attrs: binding_attrs,
                name,
                params: _,
                constraints: _,
                ext_name,
                ty: decl_ty,
                span,
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
                self.resolve_fn_type(def_id, any_type_id, unit_type_id);

            let ext_sym = match *ext_name {
                Some(s) => {
                    let trimmed = self.interner.resolve(s).trim_matches('"').to_owned();
                    self.interner.intern(&trimmed)
                }
                None => *name,
            };
            if abi_str != "C" {
                return Err(EmitError::UnsupportedFeature {
                    desc: format!("unknown calling convention \"{abi_str}\"; use #[link(name := \"...\")]  to specify a library").into(),
                });
            }
            let library = link_attrs.name.map(|s| {
                let trimmed = self.interner.resolve(s).trim_matches('"').to_owned();
                self.interner.intern(&trimmed)
            });
            let link_kind_tag = link_attrs.kind.map_or(0x00u8, |s| {
                match self.interner.resolve(s).trim_matches('"') {
                    "static" => 0x01,
                    "framework" => 0x02,
                    _ => 0x00, // dynamic
                }
            });
            let abi_tag = match abi_str.as_str() {
                "stdcall" => 0x01,
                "system" => 0x02,
                _ => 0x00, // "C" and unknown ABIs both map to 0x00 (default C ABI)
            };

            let attr_variadic = binding_attrs
                .iter()
                .any(|a| self.interner.resolve(a.name) == "variadic");
            let type_variadic = matches!(
                self.ast.exprs[*decl_ty],
                Expr::FnType { variadic: true, .. }
            );
            let is_variadic = attr_variadic || type_variadic;

            let idx = u32::try_from(self.foreign_fns.len())
                .map_err(|_| EmitError::overflow("foreign fn index overflow"))?;
            self.foreign_fns.push(ForeignFn {
                musi_name: *name,
                ext_name: ext_sym,
                library,
                abi: abi_tag,
                link_kind_tag,
                signature_type_id: 0,
                ffi_flags: 0,
                param_type_ids,
                ret_type_id,
                variadic: is_variadic,
            });
            let _ = self.foreign_map.insert(def_id, idx);
        }
        Ok(())
    }

    fn resolve_fn_type(
        &mut self,
        def_id: DefId,
        param_fallback: u32,
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
                    .unwrap_or(param_fallback)
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

    fn scan_effect(&mut self, effect_name: Symbol, ops: &[EffectOp]) -> EmitResult {
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

            if let Some(did) = op_def_id {
                let _ = self.op_id_map.insert(did, op_id);
            }

            op_defs.push(EffectOpDef {
                signature: op_id,
                name: op.name,
                fatal: op.fatal,
            });
        }

        self.effects.push(EffectDef {
            name: effect_name,
            type_param_names: vec![],
            ops: op_defs,
            laws: vec![],
        });
        let _ = self.effect_id_map.insert(effect_def_id, effect_id);
        Ok(())
    }

    /// Register a class declaration from the AST into `self.classes`.
    ///
    /// Skips duplicate registrations (idempotent on class name).
    fn scan_class(
        &mut self,
        class_name: Symbol,
        type_params: &[TyParam],
        members: &[ClassMember],
    ) -> EmitResult {
        // Idempotent: skip if already registered.
        if self.class_id_map.contains_key(&class_name) {
            return Ok(());
        }

        let class_idx = u16::try_from(self.classes.len())
            .map_err(|_| EmitError::overflow("too many class declarations"))?;

        let mut method_defs: Vec<ClassMethodDef> = vec![];
        for member in members {
            let ClassMember::Fn { sig, default, .. } = member else {
                continue;
            };
            // Use 0 as signature_type_id; full type lowering requires the
            // sema types arena which can't be borrowed mutably alongside tp.
            // The type ref is informational metadata - the VM uses fn_map refs
            // for actual dispatch.
            let _ = self
                .sema
                .defs
                .iter()
                .find(|d| d.kind == DefKind::Fn && d.name == sig.name && d.span == sig.span)
                .map(|d| d.id);
            method_defs.push(ClassMethodDef {
                name: sig.name,
                signature_type_id: 0,
                has_default: default.is_some(),
            });
        }

        // Laws from ClassMember::Law entries.
        let laws: Vec<(Symbol, u16)> = members
            .iter()
            .filter_map(|m| {
                if let ClassMember::Law { name, .. } = m {
                    Some((*name, 0u16))
                } else {
                    None
                }
            })
            .collect();

        let type_param_syms: Vec<Symbol> = type_params.iter().map(|p| p.name).collect();

        self.classes.push(ClassDef {
            name: class_name,
            type_params: type_param_syms,
            methods: method_defs,
            laws,
            superclasses: vec![],
        });
        let _ = self.class_id_map.insert(class_name, class_idx);
        Ok(())
    }

    /// Populate `self.class_instances` from `sema.instances`.
    ///
    /// Must be called after `scan_top_level` (and dep modules) so that all
    /// class declarations have been registered in `class_id_map`.
    fn collect_class_instances(&mut self) {
        let instances = self.sema.instances.clone();
        for inst in &instances {
            // Resolve class_ref: find the class name from its DefId, then look up in class_id_map.
            let class_name = self
                .sema
                .defs
                .iter()
                .find(|d| d.id == inst.class && d.kind == DefKind::Class)
                .map(|d| d.name);
            let Some(class_name) = class_name else {
                // Class not declared in this module's CLSS table - skip.
                continue;
            };
            let Some(&class_ref) = self.class_id_map.get(&class_name) else {
                // Class declaration wasn't scanned (e.g., imported from another module).
                continue;
            };

            // type_ref: lower the instance target type.
            let type_ref = self
                .tp
                .lower_sema_type(
                    inst.target,
                    &self.sema.types,
                    &self.sema.unify,
                    &self.sema.well_known,
                )
                .ok()
                .and_then(|id| u16::try_from(id).ok())
                .unwrap_or(0xFFFF);

            // Determine instance kind:
            // Manual instances have all their member DefIds registered in fn_map
            // by scan_instance_members. Via/derives instances have empty members
            // or members not in fn_map.
            let (kind, method_refs, delegate_type_ref) = if inst.members.is_empty() {
                // No members: treat as derives (0x02).
                (0x02u8, vec![], None)
            } else {
                let all_in_fn_map = inst
                    .members
                    .iter()
                    .all(|(_, did)| self.fn_map.contains_key(did));
                if all_in_fn_map {
                    // Manual instance: emit one method_ref per member.
                    let refs: Vec<u16> = inst
                        .members
                        .iter()
                        .filter_map(|(_, did)| {
                            self.fn_map
                                .get(did)
                                .and_then(|&fn_id| u16::try_from(fn_id).ok())
                        })
                        .collect();
                    (0x00u8, refs, None)
                } else {
                    // Via delegation: members were copied from the delegate instance.
                    // No separate delegate_type_ref is available from InstanceInfo.
                    (0x01u8, vec![], None)
                }
            };

            self.class_instances.push(ClassInstanceDef {
                class_ref,
                type_ref,
                kind,
                method_refs,
                delegate_type_ref,
            });
        }
    }

    fn scan_binding(&mut self, fields: &LetFields) {
        let Some(value_idx) = fields.value else {
            return;
        };

        let Some((fn_params, fn_body)) = extract_fn(value_idx, self.ast) else {
            return;
        };

        let fn_id = self.alloc_fn_id();

        let binding_span = pat_span(fields.pat, self.ast);
        let binding_def_id = if let Some(&did) = self.pat_defs().get(&binding_span) {
            let _ = self.fn_map.insert(did, fn_id);
            Some(did)
        } else {
            None
        };

        let fn_name = pat_name_str(fields.pat, self.ast, self.interner);
        self.fn_entries.push(FnEntry {
            fn_id,
            def_id: binding_def_id,
            name: fn_name,
            params: fn_params,
            body: fn_body,
            dep_idx: None,
        });
    }

    /// Register instance method bodies as top-level functions so `emit_functions`
    /// can compile them. Laws are skipped - they are not emitted.
    fn scan_instance_members(&mut self, members: &[ClassMember], dep_idx: Option<usize>) {
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
                dep_idx,
            });
        }
    }

    fn scan_dep_modules(&mut self) -> EmitResult {
        for dep_idx in 0..self.dep_contexts.len() {
            let stmts = self.dep_contexts[dep_idx].stmts.clone();
            let saved_ast = self.ast;
            self.ast = self.dep_contexts[dep_idx].ast;
            self.active_dep = Some(dep_idx);

            for stmt in &stmts {
                self.scan_dep_stmt(stmt.expr, dep_idx)?;
            }

            self.ast = saved_ast;
            self.active_dep = None;
        }
        Ok(())
    }

    fn scan_dep_stmt(&mut self, expr_idx: ExprIdx, dep_idx: usize) -> EmitResult {
        let expr = self.ast.exprs[expr_idx].clone();
        match expr {
            Expr::Annotated { attrs, inner, .. } => {
                self.scan_annotated(&attrs, inner, Some(dep_idx))?;
            }
            Expr::Let { fields, .. } => {
                self.scan_dep_binding(&fields, dep_idx)?;
            }
            Expr::Foreign { abi, decls, .. } => {
                self.scan_foreign_with_attrs(abi, &decls, &[])?;
            }
            Expr::Instance {
                body: InstanceBody::Manual { members },
                ..
            } => {
                self.scan_instance_members(&members, Some(dep_idx));
            }
            // Top-level side-effect statements (e.g., `parse_value_ref <- parse_value`)
            // must execute when the module is loaded.
            Expr::BinOp {
                op: BinOp::Assign, ..
            } => {
                self.dep_side_effects.push((expr_idx, dep_idx));
            }
            _ => {}
        }
        Ok(())
    }

    fn scan_dep_binding(&mut self, fields: &LetFields, dep_idx: usize) -> EmitResult {
        let Some(value_idx) = fields.value else {
            return Ok(());
        };

        // Mutable bindings are always globals, even if the initial value is a
        // function literal. They can be reassigned at runtime via `<-`, so they
        // need LD_GLB/ST_GLB instead of a static fn_map entry.
        if fields.kind == BindKind::Mut {
            let binding_span = pat_span(fields.pat, self.ast);
            if let Some(&did) = self.pat_defs().get(&binding_span) {
                let slot = self.register_global(did, fields.pat, fields.kind)?;
                self.dep_global_inits.push((value_idx, dep_idx, did, slot));
            }
            return Ok(());
        }

        let Some((fn_params, fn_body)) = extract_fn(value_idx, self.ast) else {
            // Non-function binding: register as a global so dep functions can
            // reference it at runtime via LD_GLB.
            // Skip imports and record defs - they don't produce runtime values
            // and are handled via sub-module record construction instead.
            if matches!(
                self.ast.exprs[value_idx],
                Expr::Import { .. } | Expr::RecordDef { .. }
            ) {
                return Ok(());
            }
            let binding_span = pat_span(fields.pat, self.ast);
            if let Some(&did) = self.pat_defs().get(&binding_span) {
                let slot = self.register_global(did, fields.pat, fields.kind)?;
                self.dep_global_inits.push((value_idx, dep_idx, did, slot));
            }
            return Ok(());
        };

        let fn_id = self.alloc_fn_id();

        let binding_span = pat_span(fields.pat, self.ast);
        let binding_def_id = if let Some(&did) = self.pat_defs().get(&binding_span) {
            let _ = self.fn_map.insert(did, fn_id);
            Some(did)
        } else {
            None
        };

        let fn_name = pat_name_str(fields.pat, self.ast, self.interner);
        self.fn_entries.push(FnEntry {
            fn_id,
            def_id: binding_def_id,
            name: fn_name,
            params: fn_params,
            body: fn_body,
            dep_idx: Some(dep_idx),
        });
        Ok(())
    }

    fn register_global(&mut self, did: DefId, pat: PatIdx, kind: BindKind) -> EmitResult<u32> {
        let slot = self.next_global;
        self.next_global += 1;
        let _ = self.global_map.insert(did, slot);

        // Track per-dep-module field ownership so that expression emission can
        // lower cross-module global loads to `ld.loc M` + `rec.get N`.
        if let Some(dep_idx) = self.active_dep {
            let fields = self.dep_global_fields.entry(dep_idx).or_default();
            let field_index = u32::try_from(fields.len()).unwrap_or(u32::MAX);
            fields.push((did, field_index));
        }

        let binding_name = pat_name_str(pat, self.ast, self.interner);
        let name_stridx = self.string_table.intern_str(&binding_name)?;
        let is_exported = self
            .sema
            .defs
            .iter()
            .find(|d| d.id == did)
            .is_some_and(|d| d.exported);
        let mut flags = 0u8;
        if is_exported {
            flags |= global_table::FLAG_EXPORTED;
        }
        if kind == BindKind::Mut {
            flags |= global_table::FLAG_MUTABLE;
        }
        flags |= global_table::FLAG_HAS_INIT;
        let _ = self.global_table.add(GlobalEntry {
            name_stridx,
            type_ref: 0,
            flags,
            initializer: None,
        })?;
        Ok(slot)
    }

    fn emit_functions(&mut self) -> EmitResult<Vec<FnBytecode>> {
        let entries = mem::take(&mut self.fn_entries);
        let mut results = Vec::with_capacity(entries.len());
        for entry in entries {
            let bc = self.emit_one_function(&entry)?;
            results.push(bc);
        }
        // Script mode: emit a synthetic entry function from top-level statements.
        if let Some(entry_id) = self.entry_fn_id
            && !results.iter().any(|f| f.fn_id == entry_id)
        {
            let bc = self.emit_script_entry(entry_id)?;
            results.push(bc);
        }
        let mut nested = mem::take(&mut self.nested_fns);
        results.append(&mut nested);
        results.sort_by_key(|b| b.fn_id);
        Ok(results)
    }

    /// Emit a synthetic entry function that executes all top-level statements
    /// top-to-bottom. The last statement's value is the return value.
    fn emit_script_entry(&mut self, fn_id: u32) -> EmitResult<FnBytecode> {
        let stmts: Vec<ExprIdx> = self.stmts.iter().map(|s| s.expr).collect();
        let mut fc = FnCtx::new(0);

        // Emit global initializers from dependency modules first.
        let inits = mem::take(&mut self.dep_global_inits);
        for &(value_idx, dep_idx, _def_id, slot) in &inits {
            let saved_ast = self.ast;
            let saved_dep = self.active_dep;
            self.ast = self.dep_contexts[dep_idx].ast;
            self.active_dep = Some(dep_idx);
            let produced = expr::emit_expr(self, &mut fc, value_idx)?;
            self.ast = saved_ast;
            self.active_dep = saved_dep;
            if produced {
                fc.fe.emit_st_glb(slot);
            }
        }

        // Per-dep module records: pack each dep's globals into a TUP_NEW and store
        // in a local slot so imports lower to `ld.loc M` + `rec.get N`.
        let dep_field_snapshot: Vec<(usize, Vec<(DefId, u32)>)> = self
            .dep_global_fields
            .iter()
            .map(|(&dep_idx, fields)| (dep_idx, fields.clone()))
            .collect();
        for (dep_idx, fields) in dep_field_snapshot {
            if fields.is_empty() {
                continue;
            }
            let mut ordered = fields;
            ordered.sort_by_key(|&(_, fi)| fi);
            for (did, _) in &ordered {
                fc.fe.emit_ld_glb(self.global_map[did]);
            }
            let field_count = u32::try_from(ordered.len())
                .map_err(|_| EmitError::overflow("dep module record fields"))?;
            let stack_pop = i32::try_from(ordered.len())
                .map_err(|_| EmitError::overflow("dep module record fields"))?;
            fc.fe.emit_mk_prd(field_count, stack_pop)?;
            let module_slot = fc.fe.alloc_local();
            fc.fe.emit_st_loc(module_slot);
            let _ = self.dep_module_slots.insert(dep_idx, module_slot);
        }

        // Emit top-level side-effect statements from dep modules
        // (e.g., mutable global reassignment like `parse_value_ref <- parse_value`).
        let side_effects = mem::take(&mut self.dep_side_effects);
        for &(expr_idx, dep_idx) in &side_effects {
            let saved_ast = self.ast;
            let saved_dep = self.active_dep;
            self.ast = self.dep_contexts[dep_idx].ast;
            self.active_dep = Some(dep_idx);
            let produced = expr::emit_expr(self, &mut fc, expr_idx)?;
            self.ast = saved_ast;
            self.active_dep = saved_dep;
            if produced {
                fc.fe.emit_pop();
            }
        }

        let last_idx = stmts.len().saturating_sub(1);
        let mut last_produced = false;
        for (i, &expr_idx) in stmts.iter().enumerate() {
            let is_last = i == last_idx;
            let produced = expr::emit_expr_tail(self, &mut fc, expr_idx, is_last)?;
            if produced && !is_last {
                fc.fe.emit_pop();
            }
            if is_last {
                last_produced = produced;
            }
        }

        if last_produced {
            fc.fe.emit_ret();
        } else {
            fc.fe.emit_ret_u();
        }

        fc.fe.resolve_fixups("__main__")?;

        let unit_type_id = self
            .tp
            .lower_well_known_def(self.sema.well_known.unit, &self.sema.well_known)
            .unwrap_or(0);

        Ok(FnBytecode {
            fn_id,
            name_stridx: 0,
            type_id: unit_type_id,
            local_count: fc.fe.local_count,
            param_count: 0,
            max_stack: fc.fe.max_stack,
            upvalue_count: 0,
            dict_param_count: 0,
            exported: false,
            effectful: false,
            source_map: fc.fe.source_map,
            local_names: vec![],
            code: fc.fe.code,
            handlers: fc.fe.handlers,
            safepoints: fc.fe.safepoints,
            effect_refs: vec![],
        })
    }

    fn resolve_fn_type_id(&mut self, def_id: Option<DefId>) -> EmitResult<u32> {
        if let Some(did) = def_id
            && let Some(def_info) = self.sema.defs.iter().find(|d| d.id == did)
            && let Some(ty_idx) = def_info.ty_info.ty
        {
            // polymorphic / unresolved - fall through to unit on error
            if let Ok(id) = self.tp.lower_sema_type(
                ty_idx,
                &self.sema.types,
                &self.sema.unify,
                &self.sema.well_known,
            ) {
                return Ok(id);
            }
        }
        self.tp
            .lower_well_known_def(self.sema.well_known.unit, &self.sema.well_known)
            .ok_or_else(|| EmitError::unresolvable("Unit type"))
    }

    fn emit_one_function(&mut self, entry: &FnEntry) -> EmitResult<FnBytecode> {
        let saved = entry.dep_idx.map(|dep_idx| {
            let saved_ast = self.ast;
            self.ast = self.dep_contexts[dep_idx].ast;
            self.active_dep = Some(dep_idx);
            saved_ast
        });

        let mut fc = self.build_fn_ctx(entry)?;
        let dict_count = usize::from(fc.fe.param_count) - entry.params.len();

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
        let (exported, effectful, effect_refs) = self.resolve_fn_def_flags(entry.def_id);
        let dict_param_count = u8::try_from(dict_count)
            .map_err(|_| EmitError::overflow("dict_param_count overflow"))?;
        let local_names = self.build_local_names(&fc);

        if let Some(saved_ast) = saved {
            self.ast = saved_ast;
            self.active_dep = None;
        }

        Ok(FnBytecode {
            fn_id: entry.fn_id,
            name_stridx: 0,
            type_id,
            local_count: fc.fe.local_count,
            param_count: fc.fe.param_count,
            max_stack: fc.fe.max_stack,
            upvalue_count: 0,
            dict_param_count,
            exported,
            effectful,
            source_map: fc.fe.source_map,
            local_names,
            code: fc.fe.code,
            handlers: fc.fe.handlers,
            safepoints: fc.fe.safepoints,
            effect_refs,
        })
    }

    /// Build and populate a `FnCtx` for `entry`: allocates param/dict slots.
    fn build_fn_ctx(&self, entry: &FnEntry) -> EmitResult<FnCtx> {
        let constraints = entry
            .def_id
            .and_then(|did| self.active_fn_constraints().get(&did));
        let dict_count = constraints.map_or(0, Vec::len);
        let total_params = entry.params.len() + dict_count;
        let param_count =
            u16::try_from(total_params).map_err(|_| EmitError::overflow("too many params"))?;

        let mut fc = FnCtx::new(param_count);

        // Slots 0..dict_count are implicit dictionary parameters.
        if let Some(constraints) = constraints {
            for (i, ob) in constraints.iter().enumerate() {
                let slot =
                    u32::try_from(i).map_err(|_| EmitError::overflow("dict slot overflow"))?;
                let _ = fc.dict_slots.insert(ob.class, slot);
            }
        }

        // Slots dict_count..total are explicit parameters (shifted by dict_count).
        for (i, param) in entry.params.iter().enumerate() {
            let slot = u32::try_from(i + dict_count)
                .map_err(|_| EmitError::overflow("param index overflow"))?;
            if let Some(&did) = self.pat_defs().get(&param.span) {
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

        Ok(fc)
    }

    /// Resolve exported/effectful flags and effect refs for a definition.
    fn resolve_fn_def_flags(&self, def_id: Option<DefId>) -> (bool, bool, Vec<u16>) {
        let Some(did) = def_id else {
            return (false, false, vec![]);
        };
        let def_info = self.sema.defs.iter().find(|d| d.id == did);
        let is_exported = def_info.is_some_and(|d| d.exported);
        let resolved_ty = def_info
            .and_then(|d| d.ty_info.ty)
            .map(|ty_idx| self.sema.unify.resolve(ty_idx, &self.sema.types));
        let is_effectful = resolved_ty.is_some_and(|resolved| {
            matches!(&self.sema.types[resolved], Type::Fn { effects, .. } if !effects.is_pure())
        });
        let refs: Vec<u16> = resolved_ty
            .and_then(|resolved| {
                if let Type::Fn { effects, .. } = &self.sema.types[resolved] {
                    Some(
                        effects
                            .effects
                            .iter()
                            .filter_map(|entry| {
                                self.effect_id_map
                                    .get(&entry.def)
                                    .map(|&idx| u16::from(idx))
                            })
                            .collect(),
                    )
                } else {
                    None
                }
            })
            .unwrap_or_default();
        (is_exported, is_effectful, refs)
    }

    /// Build the `local_names` debug table from the completed `FnCtx`.
    ///
    /// Uses conservative full-function scope (`scope_start=0`, `scope_end=code_len`)
    /// since fine-grained scope tracking is not performed during emission.
    fn build_local_names(&mut self, fc: &FnCtx) -> Vec<(u16, u32, u32)> {
        let code_len = u32::try_from(fc.fe.code.len()).unwrap_or(u32::MAX);
        let local_count_usize = usize::from(fc.fe.local_count);
        let mut local_names: Vec<(u16, u32, u32)> = vec![(0u16, 0u32, 0u32); local_count_usize];
        for (&def_id, &slot) in &fc.local_map {
            let Some(slot_idx) = usize::try_from(slot)
                .ok()
                .filter(|&i| i < local_count_usize)
            else {
                continue;
            };
            let name_sym = self
                .sema
                .defs
                .iter()
                .find(|d| d.id == def_id)
                .map(|d| d.name);
            let name_stridx = name_sym
                .and_then(|sym| self.string_table.intern(sym, self.interner).ok())
                .unwrap_or(0);
            local_names[slot_idx] = (name_stridx, 0, code_len);
        }
        local_names
    }
}

/// Serialize the function pool section into `buf` (BE encoding).
pub fn write_function_pool(buf: &mut Vec<u8>, functions: &[FnBytecode]) -> EmitResult {
    let count =
        u16::try_from(functions.len()).map_err(|_| EmitError::overflow("too many functions"))?;
    buf.extend_from_slice(&count.to_be_bytes());
    for fn_bc in functions {
        // u16 name_stridx
        buf.extend_from_slice(&fn_bc.name_stridx.to_be_bytes());
        // u16 sig_type
        buf.extend_from_slice(
            &u16::try_from(fn_bc.type_id)
                .unwrap_or(u16::MAX)
                .to_be_bytes(),
        );
        // u8 flags: bit0=exported, bit2=effectful
        let mut flags: u8 = 0;
        if fn_bc.exported {
            flags |= 0x01;
        }
        if fn_bc.effectful {
            flags |= 0x04;
        }
        buf.push(flags);
        // u16 param_count, u16 local_count, u16 max_stack, u16 upvalue_count
        buf.extend_from_slice(&fn_bc.param_count.to_be_bytes());
        buf.extend_from_slice(&fn_bc.local_count.to_be_bytes());
        buf.extend_from_slice(&fn_bc.max_stack.to_be_bytes());
        buf.extend_from_slice(&fn_bc.upvalue_count.to_be_bytes());
        // u8 dict_param_count
        buf.push(fn_bc.dict_param_count);
        // u32 bytecode length + bytes
        let code_len = u32::try_from(fn_bc.code.len()).map_err(|_| EmitError::FunctionTooLarge)?;
        buf.extend_from_slice(&code_len.to_be_bytes());
        buf.extend_from_slice(&fn_bc.code);
        let handler_count = u16::try_from(fn_bc.handlers.len())
            .map_err(|_| EmitError::overflow("too many handler entries"))?;
        buf.extend_from_slice(&handler_count.to_be_bytes());
        for h in &fn_bc.handlers {
            buf.push(h.effect_id);
            buf.extend_from_slice(&h.handler_fn_id.to_be_bytes());
        }
        // Safepoint map: u16 count, each: u32 offset + u16 stack_depth + bitmap(stack) + bitmap(locals).
        // Bitmaps are conservative (all bits set - every slot potentially a ref).
        let sp_count = u16::try_from(fn_bc.safepoints.len())
            .map_err(|_| EmitError::overflow("safepoint count"))?;
        buf.extend_from_slice(&sp_count.to_be_bytes());
        for sp in &fn_bc.safepoints {
            buf.extend_from_slice(&sp.offset.to_be_bytes());
            buf.extend_from_slice(&sp.stack_depth.to_be_bytes());
            let stack_bitmap_bytes = usize::from(sp.stack_depth).div_ceil(8);
            buf.extend(repeat_n(0xFF_u8, stack_bitmap_bytes));
            let local_bitmap_bytes = usize::from(fn_bc.local_count).div_ceil(8);
            buf.extend(repeat_n(0xFF_u8, local_bitmap_bytes));
        }
        // Effect set: u16 count, each: u16 effect ref.
        let esc = u16::try_from(fn_bc.effect_refs.len())
            .map_err(|_| EmitError::overflow("effect set count"))?;
        buf.extend_from_slice(&esc.to_be_bytes());
        for &eref in &fn_bc.effect_refs {
            buf.extend_from_slice(&eref.to_be_bytes());
        }
    }
    Ok(())
}

/// Recursively unwrap Annotated/Paren to find an inner Fn.
fn extract_fn(expr_idx: ExprIdx, ast: &AstArenas) -> Option<(Vec<Param>, ExprIdx)> {
    match &ast.exprs[expr_idx] {
        Expr::Fn { params, body, .. } => Some((params.clone(), *body)),
        Expr::Annotated { inner, .. } | Expr::Paren { inner, .. } => extract_fn(*inner, ast),
        _ => None,
    }
}

struct LinkAttrs {
    name: Option<Symbol>,
    kind: Option<Symbol>,
}

fn extract_link_attrs(attrs: &[Attr], interner: &Interner) -> LinkAttrs {
    for attr in attrs {
        if interner.resolve(attr.name) != "link" {
            continue;
        }
        match &attr.value {
            Some(AttrValue::Lit {
                lit: Lit::Str { value, .. },
                ..
            }) => {
                return LinkAttrs {
                    name: Some(*value),
                    kind: None,
                };
            }
            Some(AttrValue::Tuple { lits, .. }) => {
                let name = lits.first().and_then(|l| match l {
                    Lit::Str { value, .. } => Some(*value),
                    _ => None,
                });
                let kind = lits.get(1).and_then(|l| match l {
                    Lit::Str { value, .. } => Some(*value),
                    _ => None,
                });
                return LinkAttrs { name, kind };
            }
            Some(AttrValue::Named { fields, .. }) => {
                let mut name = None;
                let mut kind = None;
                for field in fields {
                    match interner.resolve(field.name) {
                        "name" => {
                            if let Lit::Str { value, .. } = &field.value {
                                name = Some(*value);
                            }
                        }
                        "kind" => {
                            if let Lit::Str { value, .. } = &field.value {
                                kind = Some(*value);
                            }
                        }
                        _ => {}
                    }
                }
                return LinkAttrs { name, kind };
            }
            _ => {}
        }
    }
    LinkAttrs {
        name: None,
        kind: None,
    }
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
