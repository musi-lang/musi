use std::path::Path;

use crate::errors::ResolveErrorKind;
use crate::graph::ModuleExports;
use crate::loader::ResolvedImport;

use super::*;

impl ResolveDb {
    pub(super) fn define_and_bind(
        &mut self,
        name: Symbol,
        span: Span,
        kind: DefKind,
        vis: Visibility,
        scope: ScopeId,
    ) -> DefId {
        self.define_and_bind_with_module(name, span, kind, vis, scope, self.current_module_name())
    }

    pub(super) fn define_and_bind_with_module(
        &mut self,
        name: Symbol,
        span: Span,
        kind: DefKind,
        vis: Visibility,
        scope: ScopeId,
        module_name: Option<String>,
    ) -> DefId {
        let def_id = self.resolution.defs.alloc(DefInfo {
            name,
            span,
            kind,
            vis,
            scope,
            module_name,
        });
        let _ = self.resolution.scopes.get_mut(scope).bind(name, def_id);
        def_id
    }

    pub(super) fn current_module_name(&self) -> Option<String> {
        if self.current_file.as_os_str().is_empty() {
            None
        } else {
            Some(infer_module_name(&self.current_file))
        }
    }

    pub(super) fn define_value(&mut self, ident: &Ident, scope: ScopeId) -> DefId {
        self.define_and_bind(
            ident.name,
            ident.span,
            DefKind::Value,
            Visibility::Private,
            scope,
        )
    }

    pub(super) fn bind_params(&mut self, params: &[Param], scope: ScopeId) {
        for param in params {
            let _ = self.define_value(&param.name, scope);
        }
    }

    pub(super) fn resolve_top_level(&mut self, expr_id: ExprId) {
        let kind = self.db.ast.exprs.get(expr_id).kind.clone();
        match kind {
            ExprKind::Let(binding) => self.resolve_let(&binding, self.module_scope),
            ExprKind::Import { path, kind } => {
                self.resolve_import(expr_id, path, &kind);
            }
            _ => self.resolve_expr(expr_id, self.module_scope),
        }
    }

    pub(super) fn resolve_import(&mut self, expr_id: ExprId, path: Symbol, kind: &ImportKind) {
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
            Some(ResolvedImport::ReservedRegistry) => {
                let span = self.db.ast.exprs.get(expr_id).span;
                self.errors.push(ResolveError {
                    kind: ResolveErrorKind::RegistryNotAvailable(path),
                    span,
                });
            }
            Some(ResolvedImport::File(file_path) | ResolvedImport::Git(file_path)) => {
                let existing = self.graph.lookup(&file_path);
                let mod_id = existing.unwrap_or_else(|| self.graph.add_module(file_path.clone()));

                if existing.is_some() && self.graph.is_loading(mod_id) {
                    let span = self.db.ast.exprs.get(expr_id).span;
                    self.errors.push(ResolveError {
                        kind: ResolveErrorKind::CyclicImport(path),
                        span,
                    });
                    return;
                }

                if let Some(exports) = self.graph.get_exports(mod_id) {
                    let exports_clone = exports.clone();
                    self.add_imports_to_scope(&exports_clone, kind, &file_path);
                    return;
                }

                self.graph.mark_loaded(mod_id, ModuleExports::new());
            }
        }
    }

    pub(super) fn add_imports_to_scope(
        &mut self,
        exports: &ModuleExports,
        kind: &ImportKind,
        path: &Path,
    ) {
        let scope = self.module_scope;
        match kind {
            ImportKind::Qualified(name) => {
                let _ = self.define_and_bind(
                    name.name,
                    name.span,
                    DefKind::Import,
                    Visibility::Private,
                    scope,
                );
            }
            ImportKind::Wildcard => {
                for imported in exports.exports.values() {
                    let name_sym = self.db.interner.intern(&imported.name);
                    let imported_module_name = imported
                        .module_name
                        .clone()
                        .or_else(|| Some(infer_module_name(path)));
                    let _ = self.define_and_bind_with_module(
                        name_sym,
                        Span::DUMMY,
                        imported.kind,
                        Visibility::Private,
                        scope,
                        imported_module_name.clone(),
                    );
                    if imported.kind == DefKind::Effect {
                        let _ = self
                            .resolution
                            .imported_effect_modules
                            .insert(name_sym, imported_module_name.unwrap_or_default());
                    }
                }
            }
            ImportKind::Selective(_namespace, names) => {
                for name in names {
                    let selected = self.db.interner.resolve(name.name);
                    if let Some(imported) = exports.exports.get(selected) {
                        let imported_module_name = imported
                            .module_name
                            .clone()
                            .or_else(|| Some(infer_module_name(path)));
                        let _ = self.define_and_bind_with_module(
                            name.name,
                            Span::DUMMY,
                            imported.kind,
                            Visibility::Private,
                            scope,
                            imported_module_name.clone(),
                        );
                        if imported.kind == DefKind::Effect {
                            let _ = self
                                .resolution
                                .imported_effect_modules
                                .insert(name.name, imported_module_name.unwrap_or_default());
                        }
                    }
                }
            }
        }
    }

    pub(super) fn resolve_let(&mut self, binding: &LetBinding, scope: ScopeId) {
        let pat_node = self.db.ast.pats.get(binding.pat);
        if let PatKind::Bind(ident) = &pat_node.kind {
            let vis = visibility_from_modifiers(&binding.modifiers);
            let kind = binding
                .value
                .map(|value_id| match &self.db.ast.exprs.get(value_id).kind {
                    ExprKind::EffectDef(_) => DefKind::Effect,
                    ExprKind::ClassDef(_) => DefKind::TypeClass,
                    ExprKind::DataDef(_) => DefKind::Type,
                    _ if binding.sig.is_some() => DefKind::Function,
                    _ => DefKind::Value,
                })
                .unwrap_or_else(|| {
                    if binding.sig.is_some() {
                        DefKind::Function
                    } else {
                        DefKind::Value
                    }
                });
            let _ = self.define_and_bind(ident.name, ident.span, kind, vis, scope);
        } else {
            self.resolve_pat(binding.pat, scope);
        }

        if let Some(sig) = &binding.sig {
            let sig_scope = if sig.ty_params.is_empty() {
                scope
            } else {
                let type_scope = self
                    .resolution
                    .scopes
                    .push(ScopeKind::TypeParams, Some(scope));
                for ty_param in &sig.ty_params {
                    let _ = self.define_and_bind(
                        ty_param.name,
                        ty_param.span,
                        DefKind::TypeParam,
                        Visibility::Private,
                        type_scope,
                    );
                }
                type_scope
            };
            for param in &sig.params {
                if let Some(ty) = param.ty {
                    self.resolve_ty(ty, sig_scope);
                }
            }
            if let Some(ret_ty) = sig.ret_ty {
                self.resolve_ty(ret_ty, sig_scope);
            }
            for constraint in &sig.constraints {
                match constraint {
                    Constraint::Implements { class, .. }
                    | Constraint::Subtype { bound: class, .. } => {
                        for arg in &class.args {
                            self.resolve_ty(*arg, sig_scope);
                        }
                        if self
                            .resolution
                            .scopes
                            .resolve(sig_scope, class.name.name)
                            .is_none()
                        {
                            self.errors.push(ResolveError {
                                kind: ResolveErrorKind::UndefinedType(class.name.name),
                                span: class.name.span,
                            });
                        }
                    }
                }
            }
            if let Some(value) = binding.value {
                let fn_scope = self
                    .resolution
                    .scopes
                    .push(ScopeKind::Function, Some(sig_scope));
                self.bind_params(&sig.params, fn_scope);
                self.resolve_expr(value, fn_scope);
            }
        } else if let Some(value) = binding.value {
            self.resolve_expr(value, scope);
        }
    }
}

pub(super) fn infer_module_name(path: &Path) -> String {
    if path
        .components()
        .rev()
        .take(3)
        .any(|component| component.as_os_str() == "modules")
    {
        if let Some(stem) = path.file_stem().and_then(|stem| stem.to_str()) {
            return format!("musi:{stem}");
        }
    }
    path.to_string_lossy().into_owned()
}

pub(super) const fn visibility_from_modifiers(m: &ModifierSet) -> Visibility {
    if m.opaque {
        Visibility::Opaque
    } else if m.exported {
        Visibility::Exported
    } else {
        Visibility::Private
    }
}
