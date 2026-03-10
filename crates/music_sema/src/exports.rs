//! Module export collection.
//!
//! After semantic analysis, this module collects the exported bindings of a
//! module into a flat list. This is used by the multi-file pipeline to build
//! record types for cross-module imports.

use std::collections::HashMap;
use std::hash::BuildHasher;

use music_ast::decl::ForeignDecl;
use music_ast::pat::Pat;
use music_ast::{AstArenas, Expr, ExprIdx, ParsedModule, PatIdx};
use music_shared::{Arena, Span, Symbol};

use crate::def::{DefId, DefInfo};
use crate::types::{RecordField, Type, TypeIdx};

/// A single exported binding.
pub struct ExportBinding {
    pub name: Symbol,
    pub ty: TypeIdx,
    pub def_id: DefId,
}

/// All exports from a module.
pub struct ModuleExports {
    pub bindings: Vec<ExportBinding>,
}

/// Collects the exported bindings from a parsed and type-checked module.
///
/// Walks top-level `Expr::Binding { exported: true, .. }` nodes and looks up
/// their types from the definition table.
#[must_use]
pub fn collect_exports<S: BuildHasher>(
    parsed: &ParsedModule,
    defs: &[DefInfo],
    pat_defs: &HashMap<Span, DefId, S>,
) -> ModuleExports {
    let mut bindings = Vec::new();

    for stmt in &parsed.stmts {
        collect_exports_from_expr(stmt.expr, &parsed.arenas, defs, pat_defs, &mut bindings);
    }

    ModuleExports { bindings }
}

fn collect_exports_from_expr<S: BuildHasher>(
    expr_idx: ExprIdx,
    arenas: &AstArenas,
    defs: &[DefInfo],
    pat_defs: &HashMap<Span, DefId, S>,
    out: &mut Vec<ExportBinding>,
) {
    match &arenas.exprs[expr_idx] {
        Expr::Binding {
            exported: true,
            fields,
            ..
        } => {
            collect_pat_exports(fields.pat, arenas, defs, pat_defs, out);
        }
        Expr::Foreign {
            exported: true,
            decls,
            ..
        } => {
            for decl in decls {
                if let ForeignDecl::Fn { name, span, .. } = decl
                    && let Some(&def_id) = pat_defs.get(span)
                {
                    let idx = usize::try_from(def_id.0).expect("def id fits in usize");
                    if let Some(def) = defs.get(idx)
                        && let Some(ty) = def.ty_info.ty
                    {
                        out.push(ExportBinding {
                            name: *name,
                            ty,
                            def_id,
                        });
                    }
                }
            }
        }
        Expr::Annotated { inner, .. } => {
            collect_exports_from_expr(*inner, arenas, defs, pat_defs, out);
        }
        _ => {}
    }
}

fn collect_pat_exports<S: BuildHasher>(
    pat_idx: PatIdx,
    arenas: &AstArenas,
    defs: &[DefInfo],
    pat_defs: &HashMap<Span, DefId, S>,
    out: &mut Vec<ExportBinding>,
) {
    match &arenas.pats[pat_idx] {
        Pat::Bind { span, .. } => {
            if let Some(&def_id) = pat_defs.get(span) {
                let idx = usize::try_from(def_id.0).expect("def id fits in usize");
                if let Some(def) = defs.get(idx)
                    && let Some(ty) = def.ty_info.ty
                {
                    out.push(ExportBinding {
                        name: def.name,
                        ty,
                        def_id,
                    });
                }
            }
        }
        Pat::Tuple { elems, .. } => {
            for &elem in elems {
                collect_pat_exports(elem, arenas, defs, pat_defs, out);
            }
        }
        _ => {}
    }
}

/// Builds a record type from a module's exports, allocating into the given arena.
#[must_use]
pub fn exports_to_record_type(exports: &ModuleExports, types: &mut Arena<Type>) -> TypeIdx {
    let fields: Vec<RecordField> = exports
        .bindings
        .iter()
        .map(|b| RecordField {
            name: b.name,
            ty: b.ty,
        })
        .collect();
    types.alloc(Type::Record {
        fields,
        open: false,
    })
}
