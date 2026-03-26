use std::collections::HashMap;
use std::path::PathBuf;

use music_arena::Arena;
use music_ast::common::{MemberDecl, ModifierSet, Param};
use music_ast::expr::{
    CaseArm, CompClause, DataBody, ExprKind, FStrPart, ImportKind, InstanceBody, LetBinding,
    PwGuard, QuoteKind, RecordField, SpliceKind,
};
use music_ast::pat::PatKind;
use music_ast::ty::TyKind;
use music_ast::{ExprId, ExprList, ParamList, PatId, TyId};
use music_builtins::prelude::PRELUDE_CLASSES;
use music_builtins::types::BuiltinType;
use music_db::Db;
use music_shared::{Ident, Span, Symbol, SymbolList};

use crate::def::{DefId, DefInfo, DefKind, Visibility};
use crate::errors::{ResolveError, ResolveErrorKind};
use crate::graph::{ModuleExports, ModuleGraph};
use crate::loader::{ModuleLoader, ResolvedImport};
use crate::scope::{ScopeArena, ScopeId, ScopeKind};

/// Stores all resolution results: definitions, scope chains, and
/// per-node resolution mappings.
pub struct ResolutionMap {
    pub defs: Arena<DefInfo>,
    pub expr_res: HashMap<ExprId, DefId>,
    pub ty_res: HashMap<TyId, DefId>,
    pub pat_variant_res: HashMap<PatId, DefId>,
    pub scopes: ScopeArena,
    pub captures: HashMap<ExprId, SymbolList>,
}

impl ResolutionMap {
    #[must_use]
    pub fn new() -> Self {
        Self {
            defs: Arena::new(),
            expr_res: HashMap::new(),
            ty_res: HashMap::new(),
            pat_variant_res: HashMap::new(),
            scopes: ScopeArena::new(),
            captures: HashMap::new(),
        }
    }
}

impl Default for ResolutionMap {
    fn default() -> Self {
        Self::new()
    }
}

/// Wraps `Db` with resolution state. Owns the `Db` during resolution
/// and returns it via `finish()`.
pub struct ResolveDb {
    pub db: Db,
    pub resolution: ResolutionMap,
    pub errors: Vec<ResolveError>,
    module_scope: ScopeId,
    loader: ModuleLoader,
    graph: ModuleGraph,
    current_file: PathBuf,
    current_lambda: Option<ExprId>,
}

impl ResolveDb {
    #[must_use]
    pub fn new(db: Db, root: PathBuf) -> Self {
        let mut resolution = ResolutionMap::new();
        let module_scope = resolution.scopes.push(ScopeKind::Module, None);
        let loader = ModuleLoader::new(root);
        Self {
            db,
            resolution,
            errors: Vec::new(),
            module_scope,
            loader,
            graph: ModuleGraph::new(),
            current_file: PathBuf::new(),
            current_lambda: None,
        }
    }

    /// Create a resolver with a shared module graph (for multi-module resolution).
    #[must_use]
    pub fn with_graph(
        db: Db,
        loader: ModuleLoader,
        graph: ModuleGraph,
        current_file: PathBuf,
    ) -> Self {
        let mut resolution = ResolutionMap::new();
        let module_scope = resolution.scopes.push(ScopeKind::Module, None);
        Self {
            db,
            resolution,
            errors: Vec::new(),
            module_scope,
            loader,
            graph,
            current_file,
            current_lambda: None,
        }
    }

    /// Set the file path of the module currently being resolved.
    pub fn set_current_file(&mut self, path: PathBuf) {
        self.current_file = path;
    }

    /// Access the module loader (e.g. to configure imports).
    #[must_use]
    pub const fn loader_mut(&mut self) -> &mut ModuleLoader {
        &mut self.loader
    }

    /// Take ownership of the module graph out of the resolver.
    #[must_use]
    pub fn take_graph(self) -> ModuleGraph {
        self.graph
    }

    /// Populate the module scope with builtin types and prelude classes.
    pub fn seed_builtins(&mut self) {
        let scope = self.module_scope;
        for builtin in BuiltinType::ALL {
            let name = self.db.interner.intern(builtin.name());
            let _ = self.define_and_bind(
                name,
                Span::DUMMY,
                DefKind::Builtin(*builtin),
                Visibility::Exported,
                scope,
            );
        }

        for class in PRELUDE_CLASSES {
            let name = self.db.interner.intern(class.name);
            let _ = self.define_and_bind(
                name,
                Span::DUMMY,
                DefKind::TypeClass,
                Visibility::Exported,
                scope,
            );

            for method in class.methods {
                let op_name = self.db.interner.intern(method.op_name);
                let _ = self.define_and_bind(
                    op_name,
                    Span::DUMMY,
                    DefKind::Method,
                    Visibility::Exported,
                    scope,
                );
            }
        }
    }

    /// Resolve all top-level expressions in the module.
    pub fn resolve_module(&mut self) {
        let root = self.db.ast.root.clone();
        for &expr_id in &root {
            self.resolve_top_level(expr_id);
        }
    }

    /// Consume the resolver, returning the database, resolution map, and errors.
    #[must_use]
    pub fn finish(self) -> (Db, ResolutionMap, Vec<ResolveError>) {
        (self.db, self.resolution, self.errors)
    }

    /// Consume the resolver, also returning the module graph.
    #[must_use]
    pub fn finish_with_graph(self) -> (Db, ResolutionMap, Vec<ResolveError>, ModuleGraph) {
        (self.db, self.resolution, self.errors, self.graph)
    }

    fn define_and_bind(
        &mut self,
        name: Symbol,
        span: Span,
        kind: DefKind,
        vis: Visibility,
        scope: ScopeId,
    ) -> DefId {
        let def_id = self.resolution.defs.alloc(DefInfo {
            name,
            span,
            kind,
            vis,
            scope,
        });
        let _ = self.resolution.scopes.get_mut(scope).bind(name, def_id);
        def_id
    }

    fn define_value(&mut self, ident: &Ident, scope: ScopeId) -> DefId {
        self.define_and_bind(
            ident.name,
            ident.span,
            DefKind::Value,
            Visibility::Private,
            scope,
        )
    }

    fn bind_params(&mut self, params: &[Param], scope: ScopeId) {
        for param in params {
            let _ = self.define_value(&param.name, scope);
        }
    }

    fn resolve_top_level(&mut self, expr_id: ExprId) {
        let kind = self.db.ast.exprs.get(expr_id).kind.clone();
        match kind {
            ExprKind::Let(binding) => self.resolve_let(&binding, self.module_scope),
            ExprKind::Import { path, kind } => {
                self.resolve_import(expr_id, path, &kind);
            }
            _ => self.resolve_expr(expr_id, self.module_scope),
        }
    }

    fn resolve_import(&mut self, expr_id: ExprId, path: Symbol, kind: &ImportKind) {
        let path_str = self.db.interner.resolve(path);
        let current_file = self.current_file.clone();
        let resolved = self.loader.resolve(path_str, &current_file);

        match resolved {
            None => {
                let span = self.db.ast.exprs.get(expr_id).span;
                self.errors.push(ResolveError {
                    kind: ResolveErrorKind::ImportNotFound(path),
                    span,
                });
            }
            Some(ResolvedImport::Builtin) => {
                // musi: prefix -- builtins already seeded in scope.
                // For qualified imports, bind the alias name.
                if let ImportKind::Qualified(name) = kind {
                    let _def = self.define_and_bind(
                        name.name,
                        name.span,
                        DefKind::Import,
                        Visibility::Private,
                        self.module_scope,
                    );
                }
            }
            Some(ResolvedImport::ReservedRegistry) => {
                let span = self.db.ast.exprs.get(expr_id).span;
                self.errors.push(ResolveError {
                    kind: ResolveErrorKind::RegistryNotAvailable(path),
                    span,
                });
            }
            Some(ResolvedImport::File(file_path) | ResolvedImport::Git(file_path)) => {
                let mod_id = self.graph.add_module(file_path);

                if self.graph.is_loading(mod_id) {
                    let span = self.db.ast.exprs.get(expr_id).span;
                    self.errors.push(ResolveError {
                        kind: ResolveErrorKind::CyclicImport(path),
                        span,
                    });
                    return;
                }

                if let Some(exports) = self.graph.get_exports(mod_id) {
                    let exports_clone = exports.clone();
                    self.add_imports_to_scope(&exports_clone, kind);
                    return;
                }

                // Module not yet loaded. In single-module mode, mark as loaded
                // with empty exports. The resolve_project() driver handles
                // recursive loading for multi-module builds.
                self.graph.mark_loaded(mod_id, ModuleExports::new());
            }
        }
    }

    fn add_imports_to_scope(&mut self, exports: &ModuleExports, kind: &ImportKind) {
        let scope = self.module_scope;
        match kind {
            ImportKind::Qualified(name) => {
                // Bind the module name as an Import def; individual member
                // access is deferred to type-checking.
                let _def = self.define_and_bind(
                    name.name,
                    name.span,
                    DefKind::Import,
                    Visibility::Private,
                    scope,
                );
            }
            ImportKind::Wildcard => {
                for (&name_sym, &def_id) in &exports.exports {
                    let _prev = self.resolution.scopes.get_mut(scope).bind(name_sym, def_id);
                }
            }
            ImportKind::Selective(_namespace, names) => {
                for name in names {
                    if let Some(&def_id) = exports.exports.get(&name.name) {
                        let _prev = self
                            .resolution
                            .scopes
                            .get_mut(scope)
                            .bind(name.name, def_id);
                    }
                }
            }
        }
    }

    fn resolve_let(&mut self, binding: &LetBinding, scope: ScopeId) {
        let pat_node = self.db.ast.pats.get(binding.pat);
        if let PatKind::Bind(ident) = &pat_node.kind {
            let vis = visibility_from_modifiers(&binding.modifiers);
            let kind = if binding.sig.is_some() {
                DefKind::Function
            } else {
                DefKind::Value
            };
            // Bind before resolving value so self-recursion works
            let _ = self.define_and_bind(ident.name, ident.span, kind, vis, scope);
        } else {
            // Non-bind patterns (destructuring) -- resolve the pattern into scope
            self.resolve_pat(binding.pat, scope);
        }

        // Resolve type annotations in the signature
        if let Some(sig) = &binding.sig {
            for param in &sig.params {
                if let Some(ty) = param.ty {
                    self.resolve_ty(ty, scope);
                }
            }
            if let Some(ret_ty) = sig.ret_ty {
                self.resolve_ty(ret_ty, scope);
            }
        }

        if let Some(value) = binding.value {
            if let Some(sig) = &binding.sig {
                let fn_scope = self
                    .resolution
                    .scopes
                    .push(ScopeKind::Function, Some(scope));
                self.bind_params(&sig.params, fn_scope);
                self.resolve_expr(value, fn_scope);
            } else {
                self.resolve_expr(value, scope);
            }
        }
    }

    fn resolve_expr(&mut self, expr_id: ExprId, scope: ScopeId) {
        let kind = self.db.ast.exprs.get(expr_id).kind.clone();
        match kind {
            ExprKind::Var(ident) => match self.resolution.scopes.resolve(scope, ident.name) {
                Some(def_id) => {
                    let _prev = self.resolution.expr_res.insert(expr_id, def_id);
                    let def_scope = self.resolution.defs.get(def_id).scope;
                    if self
                        .resolution
                        .scopes
                        .crosses_lambda_boundary(def_scope, scope)
                    {
                        if let Some(lambda_id) = self.current_lambda {
                            let captures = self.resolution.captures.entry(lambda_id).or_default();
                            if !captures.contains(&ident.name) {
                                captures.push(ident.name);
                            }
                        }
                    }
                }
                None => {
                    self.errors.push(ResolveError {
                        kind: ResolveErrorKind::UndefinedName(ident.name),
                        span: ident.span,
                    });
                }
            },
            ExprKind::Lit(_) | ExprKind::Import { .. } | ExprKind::ForeignImport(_) => {}
            ExprKind::TupleLit(elems)
            | ExprKind::ArrayLit(elems)
            | ExprKind::VariantLit(_, elems) => {
                for e in elems {
                    self.resolve_expr(e, scope);
                }
            }
            ExprKind::Let(binding) => self.resolve_let(&binding, scope),
            ExprKind::Seq(stmts) => self.resolve_seq(stmts, scope),
            ExprKind::Lambda {
                params,
                body,
                ret_ty,
            } => {
                self.resolve_lambda(expr_id, params, ret_ty, body, scope);
            }
            ExprKind::Case(ref data) => self.resolve_case(data.scrutinee, &data.arms, scope),
            ExprKind::Comprehension(ref data) => {
                self.resolve_comprehension(data.expr, &data.clauses, scope);
            }
            ExprKind::BinOp(_, lhs, rhs) | ExprKind::Assign(lhs, rhs) => {
                self.resolve_expr(lhs, scope);
                self.resolve_expr(rhs, scope);
            }
            ExprKind::UnaryOp(_, operand)
            | ExprKind::Need(operand)
            | ExprKind::Postfix { expr: operand, .. }
            | ExprKind::Access { expr: operand, .. } => {
                self.resolve_expr(operand, scope);
            }
            ExprKind::App(callee, args) => {
                self.resolve_expr(callee, scope);
                for arg in args {
                    self.resolve_expr(arg, scope);
                }
            }
            ExprKind::Return(opt) | ExprKind::Resume(opt) => {
                if let Some(e) = opt {
                    self.resolve_expr(e, scope);
                }
            }
            ExprKind::Index { expr, indices, .. } => {
                self.resolve_expr(expr, scope);
                for i in indices {
                    self.resolve_expr(i, scope);
                }
            }
            ExprKind::TypeOp { expr, ty, .. } => {
                self.resolve_expr(expr, scope);
                self.resolve_ty(ty, scope);
            }
            ExprKind::Branch {
                cond,
                then_br,
                else_br,
            } => {
                self.resolve_expr(cond, scope);
                self.resolve_expr(then_br, scope);
                self.resolve_expr(else_br, scope);
            }
            _ => self.resolve_expr_compound(kind, scope),
        }
    }

    fn resolve_expr_compound(&mut self, kind: ExprKind, scope: ScopeId) {
        match kind {
            ExprKind::Piecewise(arms) => {
                for arm in arms {
                    self.resolve_expr(arm.value, scope);
                    if let PwGuard::Expr(e) = arm.guard {
                        self.resolve_expr(e, scope);
                    }
                }
            }
            ExprKind::MatrixLit(rows) => {
                for row in rows {
                    for e in row {
                        self.resolve_expr(e, scope);
                    }
                }
            }
            ExprKind::RecordLit(fields) => self.resolve_record_fields(&fields, scope),
            ExprKind::RecordUpdate { base, fields } => {
                self.resolve_expr(base, scope);
                self.resolve_record_fields(&fields, scope);
            }
            ExprKind::FStrLit(parts) => {
                for part in parts {
                    if let FStrPart::Expr(e) = part {
                        self.resolve_expr(e, scope);
                    }
                }
            }
            ExprKind::Handle(ref data) => {
                for handler in &data.handlers {
                    if let Some(handler_body) = handler.body {
                        self.resolve_expr(handler_body, scope);
                    }
                }
                self.resolve_expr(data.body, scope);
            }
            ExprKind::Quote(qk) => match qk {
                QuoteKind::Expr(e) => self.resolve_expr(e, scope),
                QuoteKind::Block(es) => {
                    for e in es {
                        self.resolve_expr(e, scope);
                    }
                }
            },
            ExprKind::Splice(sk) => match sk {
                SpliceKind::Expr(e) => self.resolve_expr(e, scope),
                SpliceKind::Array(es) => {
                    for e in es {
                        self.resolve_expr(e, scope);
                    }
                }
                SpliceKind::Ident(_) => {}
            },
            ExprKind::DataDef(ref body) => match body.as_ref() {
                DataBody::Product(fields) => {
                    for field in fields {
                        self.resolve_ty(field.ty, scope);
                        if let Some(default) = field.default {
                            self.resolve_expr(default, scope);
                        }
                    }
                }
                DataBody::Sum(variants) => {
                    for variant in variants {
                        if let Some(payload) = variant.payload {
                            self.resolve_ty(payload, scope);
                        }
                        if let Some(default) = variant.default {
                            self.resolve_expr(default, scope);
                        }
                    }
                }
            },
            ExprKind::EffectDef(ref members) => {
                self.resolve_member_bodies(members, scope);
            }
            ExprKind::ClassDef(ref data) => {
                self.resolve_member_bodies(&data.members, scope);
            }
            ExprKind::InstanceDef(inst) => {
                if let InstanceBody::Methods(ref members) = inst.body {
                    self.resolve_member_bodies(members, scope);
                }
            }
            _ => {}
        }
    }

    fn resolve_seq(&mut self, stmts: ExprList, scope: ScopeId) {
        let seq_scope = self.resolution.scopes.push(ScopeKind::Block, Some(scope));
        for stmt in stmts {
            let stmt_kind = self.db.ast.exprs.get(stmt).kind.clone();
            if let ExprKind::Let(binding) = stmt_kind {
                self.resolve_let(&binding, seq_scope);
            } else {
                self.resolve_expr(stmt, seq_scope);
            }
        }
    }

    fn resolve_lambda(
        &mut self,
        expr_id: ExprId,
        params: ParamList,
        ret_ty: Option<TyId>,
        body: ExprId,
        scope: ScopeId,
    ) {
        let lambda_scope = self.resolution.scopes.push(ScopeKind::Lambda, Some(scope));
        self.bind_params(&params, lambda_scope);
        for param in params {
            if let Some(ty) = param.ty {
                self.resolve_ty(ty, scope);
            }
        }
        if let Some(rt) = ret_ty {
            self.resolve_ty(rt, scope);
        }
        let saved_lambda = self.current_lambda;
        self.current_lambda = Some(expr_id);
        self.resolve_expr(body, lambda_scope);
        self.current_lambda = saved_lambda;
    }

    fn resolve_case(&mut self, scrutinee: ExprId, arms: &[CaseArm], scope: ScopeId) {
        self.resolve_expr(scrutinee, scope);
        for arm in arms {
            let arm_scope = self.resolution.scopes.push(ScopeKind::CaseArm, Some(scope));
            self.resolve_pat(arm.pat, arm_scope);
            if let Some(guard) = arm.guard {
                self.resolve_expr(guard, arm_scope);
            }
            self.resolve_expr(arm.body, arm_scope);
        }
    }

    fn resolve_comprehension(&mut self, expr: ExprId, clauses: &[CompClause], scope: ScopeId) {
        let comp_scope = self
            .resolution
            .scopes
            .push(ScopeKind::Comprehension, Some(scope));
        for clause in clauses {
            match *clause {
                CompClause::Generator { pat, iter } => {
                    self.resolve_expr(iter, comp_scope);
                    self.resolve_pat(pat, comp_scope);
                }
                CompClause::Filter(e) => {
                    self.resolve_expr(e, comp_scope);
                }
            }
        }
        self.resolve_expr(expr, comp_scope);
    }

    fn resolve_record_fields(&mut self, fields: &[RecordField], scope: ScopeId) {
        for field in fields {
            match field {
                RecordField::Named { value, .. } => {
                    if let Some(v) = value {
                        self.resolve_expr(*v, scope);
                    }
                }
                RecordField::Spread(e) => {
                    self.resolve_expr(*e, scope);
                }
            }
        }
    }

    fn resolve_member_bodies(&mut self, members: &[MemberDecl], scope: ScopeId) {
        for member in members {
            match member {
                MemberDecl::Fn(decl) => {
                    if let Some(body) = decl.body {
                        self.resolve_expr(body, scope);
                    }
                }
                MemberDecl::Law(law) => {
                    self.resolve_expr(law.body, scope);
                }
            }
        }
    }

    fn resolve_pat(&mut self, pat_id: PatId, scope: ScopeId) {
        let kind = self.db.ast.pats.get(pat_id).kind.clone();
        match kind {
            PatKind::Bind(ident) => {
                let _ = self.define_value(&ident, scope);
            }
            PatKind::As { name, pat } => {
                let _ = self.define_value(&name, scope);
                self.resolve_pat(pat, scope);
            }
            PatKind::Tuple(pats) | PatKind::Array(pats) | PatKind::Or(pats) => {
                for p in pats {
                    self.resolve_pat(p, scope);
                }
            }
            PatKind::Variant { tag, fields } => {
                if let Some(def_id) = self.resolution.scopes.resolve(scope, tag.name) {
                    let _prev = self.resolution.pat_variant_res.insert(pat_id, def_id);
                }
                for p in fields {
                    self.resolve_pat(p, scope);
                }
            }
            PatKind::Record(fields) => {
                for f in fields {
                    if let Some(p) = f.pat {
                        self.resolve_pat(p, scope);
                    } else {
                        let _ = self.define_value(&f.name, scope);
                    }
                }
            }
            PatKind::Wildcard | PatKind::Lit(_) => {}
        }
    }

    fn resolve_ty(&mut self, ty_id: TyId, scope: ScopeId) {
        let kind = self.db.ast.types.get(ty_id).kind.clone();
        match kind {
            TyKind::Named { name, args } => {
                match self.resolution.scopes.resolve(scope, name.name) {
                    Some(def_id) => {
                        let _prev = self.resolution.ty_res.insert(ty_id, def_id);
                    }
                    None => {
                        self.errors.push(ResolveError {
                            kind: ResolveErrorKind::UndefinedType(name.name),
                            span: name.span,
                        });
                    }
                }
                for arg in args {
                    self.resolve_ty(arg, scope);
                }
            }
            TyKind::Arrow { from, to } | TyKind::EffectArrow { from, to } => {
                self.resolve_ty(from, scope);
                self.resolve_ty(to, scope);
            }
            TyKind::Sum(tys) | TyKind::Product(tys) | TyKind::Tuple(tys) => {
                for t in tys {
                    self.resolve_ty(t, scope);
                }
            }
            TyKind::Mut(inner) | TyKind::Option(inner) => {
                self.resolve_ty(inner, scope);
            }
            TyKind::Array { elem, .. } => {
                self.resolve_ty(elem, scope);
            }
            TyKind::Pi {
                name,
                param_ty,
                ret_ty,
            } => {
                self.resolve_ty(param_ty, scope);
                let pi_scope = self
                    .resolution
                    .scopes
                    .push(ScopeKind::TypeParams, Some(scope));
                let _ = self.define_and_bind(
                    name.name,
                    name.span,
                    DefKind::TypeParam,
                    Visibility::Private,
                    pi_scope,
                );
                self.resolve_ty(ret_ty, pi_scope);
            }
        }
    }
}

const fn visibility_from_modifiers(m: &ModifierSet) -> Visibility {
    if m.opaque {
        Visibility::Opaque
    } else if m.exported {
        Visibility::Exported
    } else {
        Visibility::Private
    }
}
