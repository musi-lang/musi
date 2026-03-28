mod data;
mod decls;
mod effects;
mod expr;
mod instances;
mod patterns;
mod ty_lower;

use std::collections::{HashMap, HashSet};

use music_ast::ExprId;
use music_ast::expr::{BinOp, UnaryOp};
use music_db::Db;
use music_il::opcode::Opcode;
use music_resolve::ResolutionMap;
use music_resolve::def::{DefId, DefKind, Visibility};
use music_shared::{Ident, Literal, Span, Symbol};

use crate::env::TypeEnv;
use crate::errors::{SemaError, SemaErrorKind};
use crate::types::SemaTypeId;

/// Tag index and arity for a variant within its parent sum type.
#[derive(Debug, Clone, Copy)]
struct VariantDefInfo {
    tag_index: u16,
    arity: u8,
}

type VariantRegistry = HashMap<Symbol, VariantDefInfo>;

/// Lightweight extraction of common `ExprKind` variants.
enum QuickExpr {
    Var(ExprId, Ident),
    Lit(Literal),
    BinOp(BinOp, ExprId, ExprId),
    UnaryOp(UnaryOp, ExprId),
    Assign(ExprId, ExprId),
    Branch(ExprId, ExprId, ExprId),
    Return(Option<ExprId>),
    Perform(ExprId),
    Passthrough(ExprId),
    FStr,
    ClassDef,
    Other,
}

/// Wraps `Db` with type-checking state. Owns the `Db` during semantic
/// analysis and returns it via `finish()`.
pub struct SemaDb {
    pub db: Db,
    pub resolution: ResolutionMap,
    pub env: TypeEnv,
    pub errors: Vec<SemaError>,
    depth: u32,
    in_quote: bool,
    mutable_defs: HashSet<DefId>,
    used_defs: HashSet<DefId>,
    current_handler_ret: Option<SemaTypeId>,
    def_constraints: HashMap<DefId, Vec<(Symbol, Symbol)>>,
    intrinsic_methods: HashMap<Symbol, Opcode>,
    intrinsic_variants: HashMap<Symbol, Opcode>,
    variant_registry: VariantRegistry,
    name_to_def: HashMap<Symbol, DefId>,
    let_types: HashMap<Symbol, SemaTypeId>,
}

impl SemaDb {
    #[must_use]
    pub fn new(db: Db, resolution: ResolutionMap) -> Self {
        let hint = db.ast.exprs.len();
        let mut env = TypeEnv::with_capacity(hint);
        env.seed_builtins();
        let name_to_def: HashMap<Symbol, DefId> = resolution
            .defs
            .iter()
            .map(|(id, info)| (info.name, id))
            .collect();
        Self {
            db,
            resolution,
            env,
            errors: Vec::new(),
            depth: 0,
            in_quote: false,
            mutable_defs: HashSet::new(),
            used_defs: HashSet::new(),
            current_handler_ret: None,
            def_constraints: HashMap::new(),
            intrinsic_methods: HashMap::new(),
            intrinsic_variants: HashMap::new(),
            variant_registry: VariantRegistry::new(),
            name_to_def,
            let_types: HashMap::new(),
        }
    }

    pub fn check_module(&mut self) {
        self.seed_prelude_intrinsics();
        let root = self.db.ast.root.clone();
        for &expr_id in &root {
            let _ = self.synth(expr_id);
        }
        self.check_unused();
    }

    fn check_unused(&mut self) {
        let defs: Vec<_> = self
            .resolution
            .defs
            .iter()
            .map(|(id, info)| (id, info.name, info.kind, info.vis, info.span))
            .collect();
        for (def_id, name, kind, vis, span) in defs {
            if self.used_defs.contains(&def_id) {
                continue;
            }
            let name_str = self.db.interner.resolve(name);
            if name_str.starts_with('_') {
                continue;
            }
            if span == Span::DUMMY {
                continue;
            }
            if matches!(vis, Visibility::Exported | Visibility::Opaque) {
                continue;
            }
            if matches!(kind, DefKind::Value | DefKind::Function) {
                self.errors.push(SemaError {
                    kind: SemaErrorKind::UnusedBinding { name },
                    span,
                    context: None,
                });
            }
        }
    }

    #[must_use]
    pub fn finish(self) -> (Db, ResolutionMap, TypeEnv, Vec<SemaError>) {
        (self.db, self.resolution, self.env, self.errors)
    }
}
