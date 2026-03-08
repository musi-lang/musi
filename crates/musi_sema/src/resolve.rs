//! Name resolution: two-pass over the AST to bind identifiers to definitions.
//!
//! **Pass 1** (`collect_top_level`): registers all module-level `fn`, `record`,
//! and `choice` declaration names in the root scope so that forward references
//! to functions and types work within the module.
//!
//! **Pass 2** (`resolve_items`): walks every expression recursively, resolving
//! each identifier via the scope chain and recording `Idx<Expr> → DefId`.
//! Inner scopes (function bodies, blocks) are pushed/popped on the fly.

use std::collections::HashMap;
use std::hash::BuildHasher;

use musi_ast::{
    ArrayItem, AstArenas, BindKind, ChoiceVariant, Cond, ElifBranch, Expr, FieldInit, ImportClause,
    MatchArm, ParsedModule, Pat, PatField, PatSuffix, PostfixOp, VariantPayload,
};
use musi_shared::{DiagnosticBag, FileId, Idx, Interner, Slice, Span, Symbol};

use crate::ModuleExports;
use crate::def::{DefId, DefInfo, DefKind, TypeFlavor};
use crate::scope::{ScopeId, ScopeTree};
use crate::types::Type;

/// The result of the name-resolution pass.
pub struct ResolveResult {
    /// All definitions encountered (index = `DefId.0`).
    pub defs: Vec<DefInfo>,
    /// Maps each `Ident` expression node to the `DefId` it refers to.
    pub expr_defs: HashMap<Idx<Expr>, DefId>,
    /// Maps each *binding site* span to its `DefId`.
    ///
    /// Keys are the [`Span`]s of pattern identifiers, function parameter
    /// names, and variant declarations.
    pub pat_defs: HashMap<Span, DefId>,
    /// The fully-built scope tree.
    pub scopes: ScopeTree,
    /// The root (module-level) scope.
    pub root: ScopeId,
    /// Maps each named function's `DefId` to its ordered list of parameter names.
    /// Used to generate call-site parameter name inlay hints.
    pub fn_params: HashMap<DefId, Vec<Symbol>>,
    /// Call sites: each entry is `(fn_def_id, arg_expr_indices)`.
    /// Enables generating Scala-style `param_name:` inlay hints at call sites.
    pub call_sites: Vec<(DefId, Vec<Idx<Expr>>)>,
    /// Maps each type-annotation span to the `DefId` of the referenced type.
    pub ty_refs: HashMap<Span, DefId>,
}

struct Resolver<'a> {
    interner: &'a Interner,
    diags: &'a mut DiagnosticBag,
    file_id: FileId,
    scopes: ScopeTree,
    defs: Vec<DefInfo>,
    next_id: u32,
    expr_defs: HashMap<Idx<Expr>, DefId>,
    pat_defs: HashMap<Span, DefId>,
    /// Exports keyed by alias symbol for `import * as Name` imports.
    namespace_exports: HashMap<Symbol, HashMap<String, Type>>,
    /// Maps named function `DefIds` to their ordered param names.
    fn_params: HashMap<DefId, Vec<Symbol>>,
    /// Call sites for parameter-name inlay hints.
    call_sites: Vec<(DefId, Vec<Idx<Expr>>)>,
    /// Maps each type-annotation span to the `DefId` of the referenced type.
    ty_refs: HashMap<Span, DefId>,
}

impl<'a> Resolver<'a> {
    fn new(interner: &'a Interner, diags: &'a mut DiagnosticBag, file_id: FileId) -> Self {
        Self {
            interner,
            diags,
            file_id,
            scopes: ScopeTree::new(),
            defs: Vec::new(),
            next_id: 0,
            expr_defs: HashMap::new(),
            pat_defs: HashMap::new(),
            namespace_exports: HashMap::new(),
            fn_params: HashMap::new(),
            call_sites: Vec::new(),
            ty_refs: HashMap::new(),
        }
    }

    fn alloc_def(&mut self, name: Symbol, kind: DefKind, span: Span) -> DefId {
        let id = DefId(self.next_id);
        self.next_id += 1;
        self.defs.push(DefInfo {
            id,
            name,
            kind,
            span,
            ty: None,
            scheme_vars: Vec::new(),
            use_count: 0,
            is_extrin_param: false,
            is_var: false,
            type_flavor: None,
            parent_type: None,
        });
        id
    }

    fn define_in_scope(&mut self, scope: ScopeId, name: Symbol, def_id: DefId, dup_span: Span) {
        let prev = self.scopes.define(scope, name, def_id);
        if let Some(_prev_id) = prev {
            let name_str = self.interner.resolve(name);
            let _d = self.diags.error(
                format!("duplicate definition of `{name_str}`"),
                dup_span,
                self.file_id,
            );
        }
    }

    /// Registers all module-level `fn`, `record`, `choice`, and `class` names
    /// into `root_scope`, enabling forward references.
    fn collect_top_level(&mut self, module: &ParsedModule, root_scope: ScopeId) {
        for &item_idx in module.ctx.expr_lists.get_slice(module.items) {
            let expr = module.ctx.exprs.get(item_idx);
            match expr {
                Expr::FnDef { name, span, .. } => {
                    self.alloc_and_define(*name, DefKind::Fn, *span, root_scope);
                }
                Expr::Record {
                    name: Some(name),
                    span,
                    ..
                } => {
                    let def_id = self.alloc_def(*name, DefKind::Type, *span);
                    self.defs.last_mut().unwrap().type_flavor = Some(TypeFlavor::Record);
                    self.define_in_scope(root_scope, *name, def_id, *span);
                }
                Expr::Choice {
                    name: Some(name),
                    span,
                    ..
                } => {
                    let def_id = self.alloc_def(*name, DefKind::Type, *span);
                    self.defs.last_mut().unwrap().type_flavor = Some(TypeFlavor::Choice);
                    self.define_in_scope(root_scope, *name, def_id, *span);
                }
                Expr::ClassDef { name, span, .. } => {
                    self.alloc_and_define(*name, DefKind::Class, *span, root_scope);
                }
                _ => {}
            }
        }
    }

    fn collect_imports<S: BuildHasher>(
        &mut self,
        module: &ParsedModule,
        root_scope: ScopeId,
        imports: &HashMap<String, ModuleExports, S>,
        interner: &Interner,
    ) {
        for &item_idx in module.ctx.expr_lists.get_slice(module.items) {
            let Expr::Import { items, path, span } = module.ctx.exprs.get(item_idx) else {
                continue;
            };
            let raw_path = interner.resolve(*path);
            let path_str = raw_path.trim_matches('"').to_owned();
            let Some(module_exports) = imports.get(&path_str) else {
                continue;
            };
            match items {
                ImportClause::GlobAs(alias) => {
                    let def_id = DefId(self.next_id);
                    self.next_id += 1;
                    self.defs.push(DefInfo {
                        id: def_id,
                        name: *alias,
                        kind: DefKind::Namespace,
                        span: *span,
                        ty: None,
                        scheme_vars: Vec::new(),
                        use_count: 0,
                        is_extrin_param: false,
                        is_var: false,
                        type_flavor: None,
                        parent_type: None,
                    });
                    let prev = self.scopes.define(root_scope, *alias, def_id);
                    let _ = prev;
                    let _prev = self
                        .namespace_exports
                        .insert(*alias, module_exports.names.clone());
                }

                ImportClause::Items(import_items) => {
                    for import_item in import_items {
                        let name_str = interner.resolve(import_item.name);
                        let _exported_name = import_item.alias.map_or_else(
                            || name_str.to_owned(),
                            |a| interner.resolve(a).to_owned(),
                        );
                        let Some(ty) = module_exports.names.get(name_str) else {
                            continue;
                        };
                        let def_id = DefId(self.next_id);
                        self.next_id += 1;
                        let mut info = DefInfo {
                            id: def_id,
                            name: import_item.name,
                            kind: DefKind::Const,
                            span: import_item.span,
                            ty: Some(ty.clone()),
                            scheme_vars: Vec::new(),
                            use_count: 0,
                            is_extrin_param: false,
                            is_var: false,
                            type_flavor: None,
                            parent_type: None,
                        };
                        let bind_sym = import_item.alias.unwrap_or(import_item.name);
                        info.name = bind_sym;
                        self.defs.push(info);
                        let _prev = self.pat_defs.insert(import_item.span, def_id);
                        let prev = self.scopes.define(root_scope, bind_sym, def_id);
                        let _ = prev; // silently allow shadowing imports
                    }
                }

                ImportClause::Glob => {}
            }
            let _ = span;
        }
    }

    fn resolve_items(&mut self, module: &ParsedModule, root_scope: ScopeId) {
        for &item_idx in module.ctx.expr_lists.get_slice(module.items) {
            self.resolve_expr(item_idx, &module.ctx, root_scope);
        }
    }

    /// Recursively resolves an expression, recording `Ident → DefId` mappings.
    ///
    /// For binding constructs (`Bind`, `FnDef`, etc.) this also registers the
    /// newly introduced names into the appropriate scope.
    fn resolve_expr(&mut self, idx: Idx<Expr>, ctx: &AstArenas, scope: ScopeId) {
        let expr = ctx.exprs.get(idx);
        match expr {
            Expr::Ident { name, span } => self.resolve_ident(idx, *name, *span, scope),

            Expr::Lit { .. }
            | Expr::Unit { .. }
            | Expr::Error { .. }
            | Expr::Import { .. }
            | Expr::Export { .. }
            | Expr::Record { .. } => {}

            Expr::Paren { inner, .. } => self.resolve_expr(*inner, ctx, scope),

            Expr::Tuple { elements, .. } => self.resolve_tuple_expr(*elements, ctx, scope),

            Expr::Block { stmts, tail, .. } => self.resolve_block_expr(*stmts, *tail, ctx, scope),

            Expr::Array { items, .. } => self.resolve_array_expr(items, ctx, scope),

            Expr::AnonRec { fields, .. } => self.resolve_anonrec_expr(fields, ctx, scope),

            Expr::If {
                cond,
                then_body,
                elif_chains,
                else_body,
                ..
            } => self.resolve_if_expr(
                cond,
                *then_body,
                elif_chains,
                else_body.as_ref().copied(),
                ctx,
                scope,
            ),

            Expr::Match {
                scrutinee, arms, ..
            } => self.resolve_match_expr(*scrutinee, arms, ctx, scope),

            Expr::While {
                cond, guard, body, ..
            } => self.resolve_while_expr(cond, guard.as_ref(), *body, ctx, scope),

            Expr::Loop {
                body, post_cond, ..
            } => self.resolve_loop_expr(*body, post_cond.as_deref(), ctx, scope),

            Expr::For {
                pat,
                iter,
                guard,
                body,
                ..
            } => self.resolve_for_expr(pat, *iter, guard.as_ref().copied(), *body, ctx, scope),

            Expr::Defer { body, .. } => {
                self.resolve_expr(*body, ctx, scope);
            }

            Expr::Return { value, .. } | Expr::Break { value, .. } => {
                if let Some(&v) = value.as_ref() {
                    self.resolve_expr(v, ctx, scope);
                }
            }

            Expr::Cycle { guard, .. } => {
                if let Some(&g) = guard.as_ref() {
                    self.resolve_expr(g, ctx, scope);
                }
            }

            Expr::Using { init, body, .. } => {
                self.resolve_expr(*init, ctx, scope);
                self.resolve_expr(*body, ctx, scope);
            }

            Expr::DotPrefix { args, .. } => {
                for &arg in ctx.expr_lists.get_slice(*args) {
                    self.resolve_expr(arg, ctx, scope);
                }
            }

            other => self.resolve_expr_decl(other, ctx, scope),
        }
    }

    fn resolve_tuple_expr(&mut self, elements: Slice<Idx<Expr>>, ctx: &AstArenas, scope: ScopeId) {
        for &e in ctx.expr_lists.get_slice(elements) {
            self.resolve_expr(e, ctx, scope);
        }
    }

    fn resolve_array_expr(&mut self, items: &[ArrayItem], ctx: &AstArenas, scope: ScopeId) {
        for &item in items {
            self.resolve_expr(
                match item {
                    ArrayItem::Single(i) | ArrayItem::Spread(i) => i,
                },
                ctx,
                scope,
            );
        }
    }

    fn resolve_anonrec_expr(&mut self, fields: &[FieldInit], ctx: &AstArenas, scope: ScopeId) {
        for field in fields {
            self.resolve_field_init(field, ctx, scope);
        }
    }

    fn resolve_match_expr(
        &mut self,
        scrutinee: Idx<Expr>,
        arms: &[MatchArm],
        ctx: &AstArenas,
        scope: ScopeId,
    ) {
        self.resolve_expr(scrutinee, ctx, scope);
        for arm in arms {
            self.resolve_match_arm(arm, ctx, scope);
        }
    }

    fn resolve_while_expr(
        &mut self,
        cond: &Cond,
        guard: Option<&Idx<Expr>>,
        body: Idx<Expr>,
        ctx: &AstArenas,
        scope: ScopeId,
    ) {
        self.resolve_cond(cond, ctx, scope);
        if let Some(&g) = guard {
            self.resolve_expr(g, ctx, scope);
        }
        self.resolve_expr(body, ctx, scope);
    }

    fn resolve_loop_expr(
        &mut self,
        body: Idx<Expr>,
        post_cond: Option<&Cond>,
        ctx: &AstArenas,
        scope: ScopeId,
    ) {
        self.resolve_expr(body, ctx, scope);
        if let Some(pc) = post_cond {
            self.resolve_cond(pc, ctx, scope);
        }
    }

    fn register_params(&mut self, params: &[musi_ast::Param], scope: ScopeId, is_extrin: bool) {
        for param in params {
            let def_id = self.alloc_def(param.name, DefKind::Param, param.span);
            if let Some(d) = self.defs.last_mut() {
                if is_extrin {
                    d.is_extrin_param = true;
                }
                d.is_var = param.mutable;
            }
            self.define_in_scope(scope, param.name, def_id, param.span);
            let _prev = self.pat_defs.insert(param.span, def_id);
            if let Some(ref ty) = param.ty {
                self.resolve_ty(ty, scope);
            }
        }
    }

    fn resolve_ty(&mut self, ty: &musi_ast::Ty, scope: ScopeId) {
        match ty {
            musi_ast::Ty::Named { name, args, span } => {
                if let Some(def_id) = self.scopes.lookup(scope, *name) {
                    let _ = self.ty_refs.insert(*span, def_id);
                }
                for arg in args {
                    self.resolve_ty(arg, scope);
                }
            }
            musi_ast::Ty::Arrow { params, ret, .. } => {
                for p in params {
                    self.resolve_ty(p, scope);
                }
                self.resolve_ty(ret, scope);
            }
            musi_ast::Ty::Option { inner, .. } => self.resolve_ty(inner, scope),
            musi_ast::Ty::Prod { elements, .. } => {
                for e in elements {
                    self.resolve_ty(e, scope);
                }
            }
            musi_ast::Ty::Arr { element, .. } => self.resolve_ty(element, scope),
            musi_ast::Ty::Var { .. } | musi_ast::Ty::Error { .. } => {}
        }
    }

    fn alloc_and_define(&mut self, name: Symbol, kind: DefKind, span: Span, scope: ScopeId) {
        let def_id = self.alloc_def(name, kind, span);
        self.define_in_scope(scope, name, def_id, span);
    }

    fn define_pat_name(&mut self, name: Symbol, kind: BindKind, span: Span, scope: ScopeId) {
        let def_kind = if kind == BindKind::Var {
            DefKind::Var
        } else {
            DefKind::Const
        };
        let def_id = self.alloc_def(name, def_kind, span);
        self.define_in_scope(scope, name, def_id, span);
        let _prev = self.pat_defs.insert(span, def_id);
    }

    fn resolve_ident(&mut self, idx: Idx<Expr>, name: Symbol, span: Span, scope: ScopeId) {
        if let Some(def_id) = self.scopes.lookup(scope, name) {
            let _prev = self.expr_defs.insert(idx, def_id);
            let def_idx = usize::try_from(def_id.0).expect("def index in range");
            self.defs[def_idx].use_count += 1;
        } else {
            let name_str = self.interner.resolve(name);
            let visible = self.scopes.visible_names(scope);
            let suggestion = best_suggestion(name_str, &visible, self.interner);
            let msg = match suggestion {
                Some(s) => {
                    let s_str = self.interner.resolve(s);
                    format!("undefined name `{name_str}`; did you mean `{s_str}`?")
                }
                None => format!("undefined name `{name_str}`"),
            };
            let _d = self.diags.error(msg, span, self.file_id);
        }
    }

    fn resolve_if_expr(
        &mut self,
        cond: &Cond,
        then_body: Idx<Expr>,
        elif_chains: &[ElifBranch],
        else_body: Option<Idx<Expr>>,
        ctx: &AstArenas,
        scope: ScopeId,
    ) {
        self.resolve_cond(cond, ctx, scope);
        self.resolve_expr(then_body, ctx, scope);
        for chain in elif_chains {
            self.resolve_elif(chain, ctx, scope);
        }
        if let Some(eb) = else_body {
            self.resolve_expr(eb, ctx, scope);
        }
    }

    fn resolve_for_expr(
        &mut self,
        pat: &Pat,
        iter: Idx<Expr>,
        guard: Option<Idx<Expr>>,
        body: Idx<Expr>,
        ctx: &AstArenas,
        scope: ScopeId,
    ) {
        self.resolve_expr(iter, ctx, scope);
        let for_scope = self.scopes.push_child(scope);
        self.collect_pat_defs(pat, BindKind::Const, for_scope);
        self.resolve_pat(pat, ctx, for_scope);
        if let Some(g) = guard {
            self.resolve_expr(g, ctx, for_scope);
        }
        self.resolve_expr(body, ctx, for_scope);
    }

    fn resolve_expr_decl(&mut self, expr: &Expr, ctx: &AstArenas, scope: ScopeId) {
        match expr {
            Expr::Choice { name, variants, .. } => {
                let parent_sym = *name;
                for variant in variants {
                    self.register_variant(variant, scope, parent_sym);
                }
            }

            Expr::FnDef {
                name,
                params,
                ret_ty,
                body,
                ..
            } => {
                let fn_scope = self.scopes.push_child(scope);
                // extrin fn has no body — suppress "unused param" warnings for its params
                self.register_params(params, fn_scope, body.is_none());
                if let Some(rt) = ret_ty {
                    self.resolve_ty(rt, fn_scope);
                }
                // Record param names for call-site inlay hints.
                if let Some(fn_def_id) = self.scopes.lookup(scope, *name) {
                    let param_names: Vec<Symbol> = params.iter().map(|p| p.name).collect();
                    let _prev = self.fn_params.insert(fn_def_id, param_names);
                }
                if let Some(&body_idx) = body.as_ref() {
                    self.resolve_expr(body_idx, ctx, fn_scope);
                }
            }

            Expr::Lambda {
                params,
                ret_ty,
                body,
                ..
            } => {
                let lam_scope = self.scopes.push_child(scope);
                self.register_params(params, lam_scope, false);
                if let Some(rt) = ret_ty {
                    self.resolve_ty(rt, lam_scope);
                }
                self.resolve_expr(*body, ctx, lam_scope);
            }

            Expr::Bind {
                kind, pat, init, ..
            } => {
                let kind = *kind;
                if let Some(&init_idx) = init.as_ref() {
                    self.resolve_expr(init_idx, ctx, scope);
                }
                self.collect_pat_defs(pat, kind, scope);
                self.resolve_pat(pat, ctx, scope);
            }

            Expr::Prefix { operand, .. } => self.resolve_expr(*operand, ctx, scope),

            Expr::Binary { lhs, rhs, .. } => {
                self.resolve_expr(*lhs, ctx, scope);
                self.resolve_expr(*rhs, ctx, scope);
            }

            Expr::Assign { target, value, .. } => {
                self.resolve_expr(*target, ctx, scope);
                self.resolve_expr(*value, ctx, scope);
            }

            Expr::Postfix { base, op, .. } => {
                self.resolve_expr(*base, ctx, scope);
                self.resolve_postfix(*base, op, ctx, scope);
            }

            Expr::ClassDef { members, .. } => {
                for member in members {
                    match member {
                        musi_ast::ClassMember::Method(idx) => {
                            self.resolve_expr(*idx, ctx, scope);
                        }
                        musi_ast::ClassMember::Law { params, body, .. } => {
                            let law_scope = self.scopes.push_child(scope);
                            self.register_params(params, law_scope, false);
                            self.resolve_expr(*body, ctx, law_scope);
                        }
                    }
                }
            }

            Expr::GivenDef { members, .. } => {
                let given_scope = self.scopes.push_child(scope);
                for member in members {
                    match member {
                        musi_ast::ClassMember::Method(idx) => {
                            self.resolve_expr(*idx, ctx, given_scope);
                        }
                        musi_ast::ClassMember::Law { params, body, .. } => {
                            let law_scope = self.scopes.push_child(given_scope);
                            self.register_params(params, law_scope, false);
                            self.resolve_expr(*body, ctx, law_scope);
                        }
                    }
                }
            }

            _ => {}
        }
    }

    /// Resolves a statement that appears directly inside a block, with special
    /// handling for declarations that extend the block's scope.
    fn resolve_postfix(
        &mut self,
        base: Idx<Expr>,
        op: &PostfixOp,
        ctx: &AstArenas,
        scope: ScopeId,
    ) {
        match op {
            PostfixOp::Call { args, .. } => {
                let arg_slice = ctx.expr_lists.get_slice(*args);
                for &arg in arg_slice {
                    self.resolve_expr(arg, ctx, scope);
                }
                if let Some(&fn_def_id) = self.expr_defs.get(&base)
                    && self.fn_params.contains_key(&fn_def_id)
                {
                    let arg_idxs = arg_slice.to_vec();
                    self.call_sites.push((fn_def_id, arg_idxs));
                }
            }
            PostfixOp::Index { args, .. } => {
                for &arg in ctx.expr_lists.get_slice(*args) {
                    self.resolve_expr(arg, ctx, scope);
                }
            }
            PostfixOp::RecDot { fields, .. } => {
                for field in fields {
                    self.resolve_field_init(field, ctx, scope);
                }
            }
            PostfixOp::Field {
                name: field_sym,
                span: field_span,
            } => {
                // Validate field access on namespace aliases.
                if let Expr::Ident { name: base_sym, .. } = ctx.exprs.get(base)
                    && let Some(def_id) = self.expr_defs.get(&base)
                {
                    let def_idx = usize::try_from(def_id.0).expect("def index fits usize");
                    if self.defs[def_idx].kind == DefKind::Namespace {
                        let field_str = self.interner.resolve(*field_sym);
                        if let Some(exports) = self.namespace_exports.get(base_sym)
                            && !exports.contains_key(field_str)
                        {
                            let ns_str = self.interner.resolve(*base_sym);
                            let _d = self.diags.error(
                                format!("no export `{field_str}` in namespace `{ns_str}`"),
                                *field_span,
                                self.file_id,
                            );
                        }
                    }
                }
            }
            PostfixOp::OptField { .. } | PostfixOp::As { .. } => {}
        }
    }

    fn resolve_block_expr(
        &mut self,
        stmts: Slice<Idx<Expr>>,
        tail: Option<Idx<Expr>>,
        ctx: &AstArenas,
        scope: ScopeId,
    ) {
        let block_scope = self.scopes.push_child(scope);
        for &stmt in ctx.expr_lists.get_slice(stmts) {
            self.resolve_block_stmt(stmt, ctx, block_scope);
        }
        if let Some(t) = tail {
            self.resolve_expr(t, ctx, block_scope);
        }
    }

    fn resolve_block_stmt(&mut self, stmt: Idx<Expr>, ctx: &AstArenas, block_scope: ScopeId) {
        let expr = ctx.exprs.get(stmt);
        match expr {
            Expr::FnDef { name, span, .. } => {
                // Register the local function name before resolving the body
                // so it is at least visible to code after it in the block.
                self.alloc_and_define(*name, DefKind::Fn, *span, block_scope);
                self.resolve_expr(stmt, ctx, block_scope);
            }
            Expr::Choice {
                name: Some(name),
                span,
                ..
            } => {
                let def_id = self.alloc_def(*name, DefKind::Type, *span);
                self.defs.last_mut().unwrap().type_flavor = Some(TypeFlavor::Choice);
                self.define_in_scope(block_scope, *name, def_id, *span);
                self.resolve_expr(stmt, ctx, block_scope);
            }
            Expr::Record {
                name: Some(name),
                span,
                ..
            } => {
                let def_id = self.alloc_def(*name, DefKind::Type, *span);
                self.defs.last_mut().unwrap().type_flavor = Some(TypeFlavor::Record);
                self.define_in_scope(block_scope, *name, def_id, *span);
                // No sub-expressions to resolve.
            }
            Expr::ClassDef { name, span, .. } => {
                self.alloc_and_define(*name, DefKind::Class, *span, block_scope);
                self.resolve_expr(stmt, ctx, block_scope);
            }
            _ => self.resolve_expr(stmt, ctx, block_scope),
        }
    }

    fn resolve_cond(&mut self, cond: &Cond, ctx: &AstArenas, scope: ScopeId) {
        match cond {
            Cond::Expr(e) => self.resolve_expr(*e, ctx, scope),
            Cond::Case { pat, init, .. } => {
                self.resolve_expr(*init, ctx, scope);
                self.collect_pat_defs(pat, BindKind::Const, scope);
                self.resolve_pat(pat, ctx, scope);
            }
        }
    }

    fn resolve_elif(&mut self, chain: &ElifBranch, ctx: &AstArenas, scope: ScopeId) {
        self.resolve_cond(&chain.cond, ctx, scope);
        if let Some(&g) = chain.guard.as_ref() {
            self.resolve_expr(g, ctx, scope);
        }
        self.resolve_expr(chain.body, ctx, scope);
    }

    fn resolve_match_arm(&mut self, arm: &MatchArm, ctx: &AstArenas, scope: ScopeId) {
        let arm_scope = self.scopes.push_child(scope);
        self.collect_pat_defs(&arm.pat, BindKind::Const, arm_scope);
        self.resolve_pat(&arm.pat, ctx, arm_scope);
        if let Some(&g) = arm.guard.as_ref() {
            self.resolve_expr(g, ctx, arm_scope);
        }
        self.resolve_expr(arm.body, ctx, arm_scope);
    }

    fn resolve_pat(&mut self, pat: &Pat, ctx: &AstArenas, scope: ScopeId) {
        match pat {
            Pat::Ident {
                suffix: Some(PatSuffix::Positional { args, .. }),
                ..
            }
            | Pat::DotPrefix { args, .. } => {
                for a in args {
                    self.resolve_pat(a, ctx, scope);
                }
            }
            Pat::Ident {
                suffix: Some(PatSuffix::Named { fields, .. }),
                ..
            }
            | Pat::AnonRec { fields, .. } => {
                for f in fields {
                    self.resolve_pat_field(f, ctx, scope);
                }
            }
            Pat::Prod { elements, .. } | Pat::Arr { elements, .. } => {
                for elem in elements {
                    self.resolve_pat(elem, ctx, scope);
                }
            }
            Pat::Or { alternatives, .. } => {
                for alt in alternatives {
                    self.resolve_pat(alt, ctx, scope);
                }
            }
            Pat::Ident { suffix: None, .. }
            | Pat::Lit { .. }
            | Pat::Wild { .. }
            | Pat::Error { .. } => {}
        }
    }

    fn resolve_pat_field(&mut self, field: &PatField, ctx: &AstArenas, scope: ScopeId) {
        if let Some(ref sub) = field.pat {
            self.resolve_pat(sub, ctx, scope);
        }
    }

    fn resolve_field_init(&mut self, field: &FieldInit, ctx: &AstArenas, scope: ScopeId) {
        match field {
            FieldInit::Named { value, .. } => self.resolve_expr(*value, ctx, scope),
            FieldInit::Spread { expr: e, .. } => self.resolve_expr(*e, ctx, scope),
        }
    }

    /// Registers all names introduced by a pattern into `scope`.
    fn collect_pat_defs(&mut self, pat: &Pat, kind: BindKind, scope: ScopeId) {
        match pat {
            Pat::Ident {
                name, span, is_mut, ..
            } => {
                // Each identifier in a pattern can opt in to `var` via `is_mut`,
                // overriding the enclosing `kind` (e.g. the for-loop default Const).
                let effective_kind = if *is_mut { BindKind::Var } else { kind };
                self.define_pat_name(*name, effective_kind, *span, scope);
            }
            Pat::Prod { elements, .. } | Pat::Arr { elements, .. } => {
                for elem in elements {
                    self.collect_pat_defs(elem, kind, scope);
                }
            }
            Pat::AnonRec { fields, .. } => {
                for field in fields {
                    if let Some(ref sub) = field.pat {
                        self.collect_pat_defs(sub, kind, scope);
                    } else {
                        // Shorthand `{ x }` -- bind `x`.
                        self.define_pat_name(field.name, kind, field.span, scope);
                    }
                }
            }
            Pat::Or { alternatives, .. } => {
                // All alternatives must bind the same names; for now, use the first.
                if let Some(first) = alternatives.first() {
                    self.collect_pat_defs(first, kind, scope);
                }
            }
            Pat::Lit { .. } | Pat::Wild { .. } | Pat::Error { .. } => {}
            Pat::DotPrefix { args, .. } => {
                for a in args {
                    self.collect_pat_defs(a, kind, scope);
                }
            }
        }
    }

    fn register_variant(
        &mut self,
        variant: &ChoiceVariant,
        scope: ScopeId,
        parent: Option<Symbol>,
    ) {
        let def_id = self.alloc_def(variant.name, DefKind::Variant, variant.span);
        if let Some(d) = self.defs.last_mut() {
            d.parent_type = parent;
        }
        self.define_in_scope(scope, variant.name, def_id, variant.span);
        let _prev = self.pat_defs.insert(variant.span, def_id);

        match &variant.payload {
            Some(VariantPayload::Positional(types)) => {
                for ty in types {
                    self.resolve_ty(ty, scope);
                }
            }
            Some(VariantPayload::Named(fields)) => {
                for field in fields {
                    let _fid = self.alloc_def(field.name, DefKind::Const, field.span);
                    if let Some(ref ty) = field.ty {
                        self.resolve_ty(ty, scope);
                    }
                }
            }
            None | Some(VariantPayload::Discriminant(_)) => {}
        }
    }
}

/// Runs name resolution on `module` and returns the [`ResolveResult`].
///
/// # Panics
///
/// Panics if the internal definition list is unexpectedly empty after
/// allocating a prelude-injected definition (indicates a logic error).
///
/// Errors and warnings are pushed into `diags`.
pub fn resolve<S: BuildHasher>(
    module: &ParsedModule,
    interner: &Interner,
    file_id: FileId,
    diags: &mut DiagnosticBag,
    imports: &HashMap<String, ModuleExports, S>,
) -> ResolveResult {
    let mut resolver = Resolver::new(interner, diags, file_id);
    let root = resolver.scopes.push_root();

    // Auto-inject prelude exports — they are available without an explicit import.
    if let Some(prelude) = imports.get("<prelude>") {
        for (name_str, ty) in &prelude.names {
            if let Some(sym) = interner.get(name_str) {
                let def_id = resolver.alloc_def(sym, DefKind::Fn, Span::default());
                resolver.defs.last_mut().unwrap().ty = Some(ty.clone());
                let prev = resolver.scopes.define(root, sym, def_id);
                let _ = prev;
            }
        }
    }

    resolver.collect_top_level(module, root);
    resolver.collect_imports(module, root, imports, interner);
    resolver.resolve_items(module, root);

    ResolveResult {
        defs: resolver.defs,
        expr_defs: resolver.expr_defs,
        pat_defs: resolver.pat_defs,
        ty_refs: resolver.ty_refs,
        scopes: resolver.scopes,
        root,
        fn_params: resolver.fn_params,
        call_sites: resolver.call_sites,
    }
}

/// Returns the best name suggestion for `query` among `candidates`, or `None`
/// if all candidates exceed the threshold distance (`query.len() / 3 + 1`).
fn best_suggestion(query: &str, candidates: &[Symbol], interner: &Interner) -> Option<Symbol> {
    let threshold = query.len() / 3 + 1;
    let mut best_dist = usize::MAX;
    let mut best: Option<Symbol> = None;

    for &sym in candidates {
        let cand = interner.resolve(sym);
        let d = edit_distance(query, cand);
        if d < best_dist {
            best_dist = d;
            best = Some(sym);
        }
    }

    if best_dist <= threshold { best } else { None }
}

/// Computes the Levenshtein edit distance between two strings.
fn edit_distance(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let m = a_chars.len();
    let n = b_chars.len();

    if m == 0 {
        return n;
    }
    if n == 0 {
        return m;
    }

    let mut row: Vec<usize> = (0..=n).collect();

    for i in 1..=m {
        let mut prev = row[0];
        row[0] = i;
        for j in 1..=n {
            let old = row[j];
            row[j] = if a_chars[i - 1] == b_chars[j - 1] {
                prev
            } else {
                1 + prev.min(row[j]).min(row[j - 1])
            };
            prev = old;
        }
    }

    row[n]
}
