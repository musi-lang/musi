use std::collections::{HashMap, HashSet};
use std::mem;

use music_ast::common::{MemberDecl, MemberName, Param};
use music_ast::expr::{
    BinOp, CaseArm, CompClause, ExprKind, FStrPart, FieldTarget, HandlerClause, ImportKind,
    InstanceBody, InstanceDef, LetBinding, PostfixOp, QuoteKind, RecordField, SpliceKind,
    TypeOpKind, UnaryOp,
};
use music_ast::pat::{PatKind, RecordPatField};
use music_ast::ty::TyKind;
use music_ast::{AttrId, ExprId, ExprList, PatId, TyId};
use music_hir::attrs::{AttrSpec, attr_expr_string, attr_path_matches, bind_attr};
use music_hir::TypedModule;
use music_il::format::{
    self, ClassDescriptor, ClassInstance, ClassMethod, EffectDescriptor, EffectOpDescriptor,
    FfiType, ForeignAbi, ForeignDescriptor, TypeDescriptor, TypeKind,
};
use music_il::instruction::{Instruction, Operand};
use music_il::opcode::Opcode;
use music_owned::types::BuiltinType;
use music_resolve::def::{DefKind, Visibility};
use music_sema::env::{DispatchInfo, TypeKey};
use music_sema::types::{NominalKey, SemaTypeId, Ty};
use music_shared::{Ident, Literal, Symbol, SymbolList};

use crate::error::EmitError;
use crate::pool::{ConstantEntry, ConstantPool};

type EmitResult<T = ()> = Result<T, EmitError>;

const LINK_ATTR: AttrSpec = AttrSpec {
    path: &["link"],
    params: &["name", "symbol"],
    required: &[],
};

/// A compiled method ready for serialisation.
///
/// `name` is `None` for the implicit module-level main method.
pub struct MethodEntry {
    pub name: Option<Symbol>,
    pub source_name: Option<String>,
    pub instructions: Vec<Instruction>,
    pub locals_count: u16,
    pub absolute_global_loads: Vec<usize>,
}

/// A module-level global binding.
pub struct GlobalEntry {
    pub name: Symbol,
    pub source_name: String,
    pub exported: bool,
    pub opaque: bool,
}

/// The fully-emitted module, ready for `.seam` serialisation.
pub struct SeamModule {
    pub constants: ConstantPool,
    pub methods: Vec<MethodEntry>,
    pub globals: Vec<GlobalEntry>,
    pub types: Vec<TypeDescriptor>,
    pub effects: Vec<EffectDescriptor>,
    pub classes: Vec<ClassDescriptor>,
    pub foreigns: Vec<ForeignDescriptor>,
}

/// Lowers typed frontend expressions to SEAM bytecode instructions.
struct Emitter<'module> {
    typed_module: &'module TypedModule,
    pool: ConstantPool,
    methods: Vec<MethodEntry>,
    globals: Vec<GlobalEntry>,
    types: Vec<TypeDescriptor>,
    type_ids: HashMap<String, u16>,
    foreigns: Vec<ForeignDescriptor>,
    /// Maps binding names to their foreign descriptor index for `FfiCall` emission.
    foreign_globals: HashMap<Symbol, u16>,
    /// Maps imported binding names to merged global indices from dependency modules.
    imported_globals: HashMap<String, u16>,
    /// Maps import path symbols to the exported globals of that module.
    /// Populated by `emit_project` for multi-module compilation.
    module_exports: HashMap<Symbol, Vec<ImportedGlobal>>,
    current_instructions: Vec<Instruction>,
    current_absolute_global_loads: Vec<usize>,
    current_locals: SymbolList,
    current_upvalues: SymbolList,
    /// Locals that are heap-boxed cells (captured mutable variables).
    cell_locals: HashSet<Symbol>,
    next_anon: u32,
    next_type_id: u16,
}

/// Entry point: lower a typed IR bundle into a [`SeamModule`].
///
/// # Errors
///
/// Returns [`EmitError::Unimplemented`] when the bundle contains a language
/// feature that has no codegen path yet (e.g. `via` derived instances,
/// dynamic dispatch, splice-by-name, or named field assignment).
pub fn emit(typed_module: &TypedModule) -> EmitResult<SeamModule> {
    emit_with_context(typed_module, HashMap::new())
}

/// Emit a module with import context from a multi-module project.
///
/// `module_exports` maps import path symbols (as interned in this module)
/// to the global indices of the imported module's exports in the combined
/// output. Used by `emit_project` to wire up qualified imports.
///
/// # Errors
///
/// Returns [`EmitError`] on emission failure.
#[expect(clippy::implicit_hasher, reason = "always called with default HashMap")]
pub fn emit_with_context(
    typed_module: &TypedModule,
    module_exports: HashMap<Symbol, Vec<ImportedGlobal>>,
) -> EmitResult<SeamModule> {
    let mut emitter = Emitter {
        typed_module,
        pool: ConstantPool::new(),
        methods: Vec::new(),
        globals: Vec::new(),
        types: builtin_type_descriptors(),
        type_ids: builtin_type_ids(),
        foreigns: Vec::new(),
        foreign_globals: HashMap::new(),
        imported_globals: HashMap::new(),
        module_exports,
        current_instructions: Vec::new(),
        current_absolute_global_loads: Vec::new(),
        current_locals: Vec::new(),
        current_upvalues: Vec::new(),
        cell_locals: HashSet::new(),
        next_anon: u32::MAX - 1,
        next_type_id: format::FIRST_EMITTED_TYPE_ID,
    };
    emitter.emit_module()?;
    let effects = emitter.collect_effects();
    let classes = emitter.collect_classes();
    Ok(SeamModule {
        constants: emitter.pool,
        methods: emitter.methods,
        globals: emitter.globals,
        types: emitter.types,
        effects,
        classes,
        foreigns: emitter.foreigns,
    })
}

fn builtin_type_descriptors() -> Vec<TypeDescriptor> {
    BuiltinType::ALL
        .iter()
        .map(|bt| TypeDescriptor {
            id: bt.type_id(),
            key: bt.name().to_owned(),
            kind: TypeKind::Builtin,
            member_count: 0,
        })
        .collect()
}

fn builtin_type_ids() -> HashMap<String, u16> {
    BuiltinType::ALL
        .iter()
        .map(|bt| (bt.name().to_owned(), bt.type_id()))
        .collect()
}

#[derive(Debug, Clone)]
pub struct ImportedGlobal {
    pub name: String,
    pub index: u16,
}

const fn instruction_byte_size(instr: &Instruction) -> usize {
    1 + match &instr.operand {
        Operand::None => 0,
        Operand::U8(_) => 1,
        Operand::I16(_) | Operand::U16(_) => 2,
        Operand::Wide(_, _) => 3,
        Operand::TypeLen(_, _) => 4,
        Operand::TypeTagged(_, _, _) => 5,
        Operand::Effect(_, _) => 4,
        Operand::IndexedJump(_, _) => 4,
        Operand::EffectJump(_, _, _) => 6,
        Operand::Table(offsets) => 2 + offsets.len() * 2,
    }
}

const fn member_name_symbol(name: &MemberName) -> Symbol {
    match name {
        MemberName::Ident(ident) | MemberName::Op(ident, _) => ident.name,
    }
}

#[expect(
    clippy::unwrap_in_result,
    reason = "capacity overflow `expect`s guard structural invariants (e.g. >65535 locals) — \
              these are compiler ICEs, not recoverable errors that belong in EmitError"
)]
impl Emitter<'_> {
    fn collect_effects(&self) -> Vec<EffectDescriptor> {
        self.typed_module
            .type_env
            .effect_order
            .iter()
            .filter_map(|effect_name| {
                self.typed_module
                    .type_env
                    .effect_defs
                    .get(effect_name)
                    .map(|effect| EffectDescriptor {
                        id: effect.id,
                        module_name: effect.module_name.clone(),
                        name: self
                            .typed_module
                            .db
                            .interner
                            .resolve(effect.name)
                            .to_owned(),
                        operations: effect
                            .operations
                            .iter()
                            .map(|op| EffectOpDescriptor {
                                id: op.id,
                                name: self.typed_module.db.interner.resolve(op.name).to_owned(),
                            })
                            .collect(),
                    })
            })
            .collect()
    }

    fn collect_classes(&mut self) -> Vec<ClassDescriptor> {
        let mut classes = Vec::new();
        let root = self.typed_module.db.ast.root.clone();
        for expr_id in root {
            let ExprKind::Let(binding) = &self.typed_module.db.ast.exprs.get(expr_id).kind else {
                continue;
            };
            let Some(value) = binding.value else {
                continue;
            };
            let ExprKind::ClassDef(data) = &self.typed_module.db.ast.exprs.get(value).kind else {
                continue;
            };
            let PatKind::Bind(ident) = &self.typed_module.db.ast.pats.get(binding.pat).kind else {
                continue;
            };
            let Some(class_id) = self.typed_module.class_id(ident.name) else {
                continue;
            };
            let class_name_idx = self
                .pool
                .add(ConstantEntry::Str(
                    self.typed_module.db.interner.resolve(ident.name).to_owned(),
                ))
                .into();
            let class_methods: Vec<Symbol> = data
                .members
                .iter()
                .filter_map(|member| match member {
                    MemberDecl::Fn(decl) => Some(member_name_symbol(&decl.name)),
                    MemberDecl::Law(_) => None,
                })
                .collect();
            let method_names = class_methods
                .iter()
                .map(|name| {
                    self.pool
                        .add(ConstantEntry::Str(
                            self.typed_module.db.interner.resolve(*name).to_owned(),
                        ))
                        .into()
                })
                .collect::<Vec<_>>();
            let method_count =
                u16::try_from(class_methods.len()).expect("too many class methods (>65535)");
            classes.push(ClassDescriptor {
                id: class_id,
                name_idx: class_name_idx,
                method_count,
                method_names,
                instances: self.collect_class_instances(ident.name, &class_methods),
            });
        }
        classes.sort_by_key(|class| class.id);
        classes
    }

    fn collect_class_instances(
        &mut self,
        class_name: Symbol,
        class_methods: &[Symbol],
    ) -> Vec<ClassInstance> {
        let mut instances = Vec::new();
        let root = self.typed_module.db.ast.root.clone();
        for expr_id in root {
            let ExprKind::InstanceDef(inst) = &self.typed_module.db.ast.exprs.get(expr_id).kind
            else {
                continue;
            };
            if inst.ty.name.name != class_name {
                continue;
            }
            let Some(type_key) = self
                .typed_module
                .type_env
                .instance_keys
                .get(&expr_id)
                .cloned()
            else {
                continue;
            };
            let type_id = self.register_type_key(&type_key);
            let methods = class_methods
                .iter()
                .map(|name| ClassMethod {
                    name_idx: self
                        .pool
                        .add(ConstantEntry::Str(
                            self.typed_module.db.interner.resolve(*name).to_owned(),
                        ))
                        .into(),
                    method_idx: self.method_index_by_name(*name).unwrap_or(u16::MAX),
                })
                .collect();
            instances.push(ClassInstance { type_id, methods });
        }
        instances.sort_by_key(|instance| instance.type_id);
        instances
    }

    fn method_index_by_name(&self, name: Symbol) -> Option<u16> {
        self.methods
            .iter()
            .position(|method| method.name == Some(name))
            .and_then(|index| u16::try_from(index).ok())
    }

    fn expr_runtime_type_id(&mut self, expr_id: ExprId, fallback: u16) -> u16 {
        let Some(expr_ty) = self.typed_module.expr_type(expr_id) else {
            return fallback;
        };
        let Some(type_key) = self.typed_module.type_env.type_key(expr_ty) else {
            return fallback;
        };
        self.register_type_key(&type_key)
    }

    fn register_type_key(&mut self, type_key: &TypeKey) -> u16 {
        let key = self.type_key_string(type_key);
        if let Some(&type_id) = self.type_ids.get(&key) {
            return type_id;
        }
        let type_id = self.next_type_id;
        self.next_type_id = self
            .next_type_id
            .checked_add(1)
            .expect("runtime type table overflow");
        let (kind, member_count) = self.type_descriptor_shape(type_key);
        self.types.push(TypeDescriptor {
            id: type_id,
            key: key.clone(),
            kind,
            member_count,
        });
        let _ = self.type_ids.insert(key, type_id);
        type_id
    }

    fn type_descriptor_shape(&self, type_key: &TypeKey) -> (TypeKind, u16) {
        match type_key {
            TypeKey::Builtin(_) => (TypeKind::Builtin, 0),
            TypeKey::Record(fields) => (
                TypeKind::Record,
                u16::try_from(fields.len()).expect("record member count overflow"),
            ),
            TypeKey::Choice(variants) => (
                TypeKind::Choice,
                u16::try_from(variants.len()).expect("choice member count overflow"),
            ),
            _ => (TypeKind::Record, 0),
        }
    }

    fn type_key_string(&self, type_key: &TypeKey) -> String {
        match type_key {
            TypeKey::Builtin(bt) => bt.name().to_owned(),
            TypeKey::Named(NominalKey { module_name, name }) => module_name.as_ref().map_or_else(
                || self.typed_module.db.interner.resolve(*name).to_owned(),
                |module_name| {
                    format!(
                        "{module_name}::{}",
                        self.typed_module.db.interner.resolve(*name)
                    )
                },
            ),
            TypeKey::Param(name) | TypeKey::Class(name) | TypeKey::Effect(name) => {
                self.typed_module.db.interner.resolve(*name).to_owned()
            }
            TypeKey::Record(fields) => format!(
                "{{{}}}",
                fields
                    .iter()
                    .map(|(name, ty)| format!(
                        "{}:{}",
                        self.typed_module.db.interner.resolve(*name),
                        self.type_key_string(ty)
                    ))
                    .collect::<Vec<_>>()
                    .join(";")
            ),
            TypeKey::Choice(variants) => format!(
                "choice({})",
                variants
                    .iter()
                    .map(|(name, payload)| match payload {
                        Some(ty) => format!(
                            "{}:{}",
                            self.typed_module.db.interner.resolve(*name),
                            self.type_key_string(ty)
                        ),
                        None => self.typed_module.db.interner.resolve(*name).to_owned(),
                    })
                    .collect::<Vec<_>>()
                    .join("|")
            ),
            TypeKey::Arrow { param, ret } => {
                format!(
                    "({})->{}",
                    self.type_key_string(param),
                    self.type_key_string(ret)
                )
            }
            TypeKey::EffectArrow {
                param,
                ret,
                effects,
            } => format!(
                "({})~>{{{}}}{}",
                self.type_key_string(param),
                effects
                    .iter()
                    .map(|effect| self.type_key_string(effect))
                    .collect::<Vec<_>>()
                    .join(","),
                self.type_key_string(ret)
            ),
            TypeKey::Tuple(items) => format!(
                "({})",
                items
                    .iter()
                    .map(|item| self.type_key_string(item))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            TypeKey::Array(item) => format!("[{}]", self.type_key_string(item)),
            TypeKey::List(item) => format!("List<{}>", self.type_key_string(item)),
            TypeKey::Union(items) => items
                .iter()
                .map(|item| self.type_key_string(item))
                .collect::<Vec<_>>()
                .join("+"),
            TypeKey::Mut(inner) => format!("mut {}", self.type_key_string(inner)),
            TypeKey::App(base, args) => format!(
                "{}[{}]",
                self.type_key_string(base),
                args.iter()
                    .map(|arg| self.type_key_string(arg))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            TypeKey::Any => "Any".to_owned(),
            TypeKey::Unknown => "Unknown".to_owned(),
            TypeKey::Empty => "Empty".to_owned(),
            TypeKey::Unit => "Unit".to_owned(),
            TypeKey::EffectOp { effect, op, ret } => format!(
                "{}.{}:{}",
                self.typed_module.db.interner.resolve(*effect),
                self.typed_module.db.interner.resolve(*op),
                self.type_key_string(ret)
            ),
        }
    }

    fn ast_type_key(&self, ty_id: TyId) -> Option<TypeKey> {
        match &self.typed_module.db.ast.types.get(ty_id).kind {
            TyKind::Named { name, args } => {
                let def_kind = self
                    .typed_module
                    .resolution
                    .ty_res
                    .get(&ty_id)
                    .map(|def_id| self.typed_module.resolution.defs.get(*def_id).kind);
                match def_kind {
                    Some(DefKind::Builtin(bt)) => Some(TypeKey::Builtin(bt)),
                    Some(DefKind::TypeParam) => Some(TypeKey::Param(name.name)),
                    Some(DefKind::Effect) => Some(TypeKey::Effect(name.name)),
                    _ => {
                        let base = TypeKey::Named(NominalKey {
                            module_name: self.typed_module.resolution.ty_res.get(&ty_id).and_then(
                                |def_id| {
                                    self.typed_module
                                        .resolution
                                        .defs
                                        .get(*def_id)
                                        .module_name
                                        .clone()
                                },
                            ),
                            name: name.name,
                        });
                        if args.is_empty() {
                            Some(base)
                        } else {
                            Some(TypeKey::App(
                                Box::new(base),
                                args.iter()
                                    .map(|arg| self.ast_type_key(*arg))
                                    .collect::<Option<Vec<_>>>()?,
                            ))
                        }
                    }
                }
            }
            TyKind::Arrow { from, to } => Some(TypeKey::Arrow {
                param: Box::new(self.ast_type_key(*from)?),
                ret: Box::new(self.ast_type_key(*to)?),
            }),
            TyKind::EffectArrow { from, to } => Some(TypeKey::EffectArrow {
                param: Box::new(self.ast_type_key(*from)?),
                ret: Box::new(self.ast_type_key(*to)?),
                effects: Vec::new(),
            }),
            TyKind::Sum(items) => Some(TypeKey::Union(
                items
                    .iter()
                    .map(|item| self.ast_type_key(*item))
                    .collect::<Option<Vec<_>>>()?,
            )),
            TyKind::Product(items) | TyKind::Tuple(items) => Some(TypeKey::Tuple(
                items
                    .iter()
                    .map(|item| self.ast_type_key(*item))
                    .collect::<Option<Vec<_>>>()?,
            )),
            TyKind::Mut(inner) => Some(TypeKey::Mut(Box::new(self.ast_type_key(*inner)?))),
            TyKind::Option(inner) => Some(TypeKey::Union(vec![
                self.ast_type_key(*inner)?,
                TypeKey::Unit,
            ])),
            TyKind::Pi { ret_ty, .. } => self.ast_type_key(*ret_ty),
            TyKind::Array { elem, .. } => Some(TypeKey::Array(Box::new(self.ast_type_key(*elem)?))),
        }
    }

    fn emit_runtime_type_id(&mut self, type_id: u16) {
        let const_idx = self.pool.add(ConstantEntry::Int(i64::from(type_id)));
        self.push(Instruction::with_u16(Opcode::LdConst, const_idx));
    }

    fn emit_module(&mut self) -> EmitResult {
        let root = self.typed_module.db.ast.root.clone();
        for expr_id in root {
            self.emit_top_level(expr_id)?;
        }
        self.finish_main();
        Ok(())
    }

    fn emit_top_level(&mut self, expr_id: ExprId) -> EmitResult {
        let kind = self.typed_module.db.ast.exprs.get(expr_id).kind.clone();
        if let ExprKind::Let(binding) = kind {
            self.emit_top_let(&binding)?;
        } else {
            self.emit_expr(expr_id)?;
        }
        Ok(())
    }

    fn emit_top_let(&mut self, binding: &LetBinding) -> EmitResult {
        let pat_node = self.typed_module.db.ast.pats.get(binding.pat);
        let PatKind::Bind(ident) = &pat_node.kind else {
            return Ok(());
        };
        let name = ident.name;

        if binding.modifiers.foreign {
            self.emit_foreign_let(name, binding);
            return Ok(());
        }

        if let Some(ref sig) = binding.sig {
            if let Some(body) = binding.value {
                let global_idx = self.alloc_top_level_global(name, resolve_visibility(binding));
                if sig.has_param_list {
                    let method_idx = self.compile_function_method(Some(name), &sig.params, body)?;
                    self.push(Instruction::with_wide(Opcode::ClsNew, method_idx, 0));
                    self.push(Instruction::with_u16(Opcode::StGlob, global_idx));
                } else {
                    self.emit_expr(body)?;
                    self.push(Instruction::with_u16(Opcode::StGlob, global_idx));
                }
            }
        } else if let Some(value) = binding.value {
            let global_idx = self.alloc_top_level_global(name, resolve_visibility(binding));
            self.emit_expr(value)?;
            self.push(Instruction::with_u16(Opcode::StGlob, global_idx));
        }
        Ok(())
    }

    fn alloc_top_level_global(&mut self, name: Symbol, vis: Visibility) -> u16 {
        let global_idx = u16::try_from(self.globals.len()).expect("too many globals (>65535)");
        self.globals.push(GlobalEntry {
            name,
            source_name: self.typed_module.db.interner.resolve(name).to_owned(),
            exported: vis == Visibility::Exported || vis == Visibility::Opaque,
            opaque: vis == Visibility::Opaque,
        });
        global_idx
    }

    fn compile_function_method(
        &mut self,
        name: Option<Symbol>,
        params: &[Param],
        body: ExprId,
    ) -> EmitResult<u16> {
        let saved_instructions = mem::take(&mut self.current_instructions);
        let saved_absolute_global_loads = mem::take(&mut self.current_absolute_global_loads);
        let saved_locals = mem::take(&mut self.current_locals);
        let saved_cells = mem::take(&mut self.cell_locals);

        for param in params {
            let _slot = self.local_slot(param.name.name);
        }

        self.emit_expr(body)?;
        self.push(Instruction::simple(Opcode::Ret));

        let locals_count =
            u16::try_from(self.current_locals.len()).expect("too many locals (>65535)");
        let method_idx = u16::try_from(self.methods.len()).expect("too many methods (>65535)");
        self.methods.push(MethodEntry {
            name,
            source_name: name.map(|sym| self.typed_module.db.interner.resolve(sym).to_owned()),
            instructions: mem::replace(&mut self.current_instructions, saved_instructions),
            locals_count,
            absolute_global_loads: mem::replace(
                &mut self.current_absolute_global_loads,
                saved_absolute_global_loads,
            ),
        });
        self.current_locals = saved_locals;
        self.cell_locals = saved_cells;
        Ok(method_idx)
    }

    fn emit_function(&mut self, name: Symbol, params: &[Param], body: ExprId) -> EmitResult {
        let _ = self.compile_function_method(Some(name), params, body)?;
        Ok(())
    }

    fn finish_main(&mut self) {
        if !self.current_instructions.is_empty() {
            self.push(Instruction::simple(Opcode::Halt));
            let locals_count =
                u16::try_from(self.current_locals.len()).expect("too many locals (>65535)");
            self.methods.push(MethodEntry {
                name: None,
                source_name: None,
                instructions: mem::take(&mut self.current_instructions),
                locals_count,
                absolute_global_loads: mem::take(&mut self.current_absolute_global_loads),
            });
        }
    }

    #[expect(
        clippy::panic,
        reason = "Piecewise must be lowered to Branch before emission — reaching this arm is a compiler ICE"
    )]
    fn emit_expr(&mut self, expr_id: ExprId) -> EmitResult {
        let kind = self.typed_module.db.ast.exprs.get(expr_id).kind.clone();
        match kind {
            ExprKind::Lit(ref lit) => self.emit_literal(lit),
            ExprKind::Var(ref ident) => self.emit_var(ident),
            ExprKind::App(callee, ref args) => self.emit_app(callee, args)?,
            ExprKind::BinOp(op, lhs, rhs) => self.emit_binop(op, lhs, rhs, expr_id)?,
            ExprKind::UnaryOp(op, operand) => self.emit_unary(op, operand, expr_id)?,
            ExprKind::Branch {
                cond,
                then_br,
                else_br,
            } => self.emit_branch(cond, then_br, else_br)?,
            ExprKind::Let(ref binding) => self.emit_let(binding)?,
            ExprKind::Lambda {
                ref params, body, ..
            } => self.emit_lambda(expr_id, params, body)?,
            ExprKind::Seq(ref stmts) => self.emit_seq(stmts)?,
            ExprKind::Case(ref data) => self.emit_case(data.scrutinee, &data.arms)?,
            ExprKind::RecordLit(ref fields) => self.emit_record_lit(expr_id, fields)?,
            ExprKind::VariantLit(ref tag, ref args) => self.emit_variant_lit(expr_id, tag, args)?,
            ExprKind::Access { expr, field, .. } => self.emit_access(expr, &field)?,
            ExprKind::Return(opt) => self.emit_return(opt)?,
            ExprKind::TupleLit(ref elems) => self.emit_tuple_lit(expr_id, elems)?,
            ExprKind::ArrayLit(ref elems) => self.emit_array_lit(expr_id, elems)?,
            ExprKind::Assign(target, value) => self.emit_assign(target, value)?,
            ExprKind::Index {
                expr, ref indices, ..
            } => self.emit_index(expr, indices)?,
            ExprKind::FStrLit(ref parts) => self.emit_fstr(parts)?,
            ExprKind::Perform(operand) => self.emit_perform(expr_id, operand)?,
            ExprKind::Handle(ref data) => self.emit_handle(expr_id, &data.clauses, data.body)?,
            ExprKind::Resume(opt) => self.emit_resume(opt)?,
            ExprKind::MatrixLit(ref rows) => self.emit_matrix_lit(expr_id, rows)?,
            ExprKind::RecordUpdate { base, ref fields } => self.emit_record_update(base, fields)?,
            ExprKind::Postfix { expr, op } => self.emit_postfix(expr, op)?,
            ExprKind::TypeOp { expr, ty, kind } => self.emit_type_op(expr, ty, kind)?,
            ExprKind::InstanceDef(ref inst) => self.emit_instance_def(expr_id, inst)?,
            ExprKind::ForeignImport(sym) => self.emit_foreign_import(sym),
            ExprKind::Comprehension(ref data) => {
                self.emit_comprehension(data.expr, &data.clauses)?;
            }
            ExprKind::Quote(ref qk) => self.emit_quote(qk)?,
            ExprKind::Splice(ref sk) => self.emit_splice(sk)?,
            ExprKind::Import { path, ref kind } => self.emit_import(path, kind),
            ExprKind::DataDef(_) | ExprKind::EffectDef(_) | ExprKind::ClassDef(_) => {
                self.push(Instruction::simple(Opcode::LdUnit));
            }
            ExprKind::Piecewise(_) => {
                panic!("Piecewise should be lowered to Branch before emission")
            }
        }
        Ok(())
    }

    fn emit_literal(&mut self, lit: &Literal) {
        match *lit {
            Literal::Int(0) => self.push(Instruction::simple(Opcode::LdNil)),
            Literal::Int(1) => self.push(Instruction::simple(Opcode::LdOne)),
            Literal::Int(n) => {
                if let Ok(small) = i16::try_from(n) {
                    self.push(Instruction::with_i16(Opcode::LdSmi, small));
                } else {
                    let idx = self.pool.add(ConstantEntry::Int(n));
                    self.push(Instruction::with_u16(Opcode::LdConst, idx));
                }
            }
            Literal::Float(f) => {
                let idx = self.pool.add(ConstantEntry::Float(f.to_bits()));
                self.push(Instruction::with_u16(Opcode::LdConst, idx));
            }
            Literal::Str(ref s) => {
                let idx = self.pool.add(ConstantEntry::Str(s.clone()));
                self.push(Instruction::with_u16(Opcode::LdConst, idx));
            }
            Literal::Rune(c) => {
                let scalar = u32::from(c);
                if let Ok(small) = i16::try_from(scalar) {
                    self.push(Instruction::with_i16(Opcode::LdSmi, small));
                } else {
                    let idx = self.pool.add(ConstantEntry::Int(i64::from(scalar)));
                    self.push(Instruction::with_u16(Opcode::LdConst, idx));
                }
            }
        }
    }

    #[expect(
        clippy::panic,
        reason = "unresolved variable after sema is a compiler ICE — sema guarantees all variables resolve"
    )]
    fn emit_var(&mut self, ident: &Ident) {
        if let Some(slot) = self.find_local(ident.name) {
            self.emit_ld_loc(slot);
            if self.cell_locals.contains(&ident.name) {
                self.push(Instruction::with_u8(Opcode::ArrGetI, 0));
            }
        } else if let Some(slot) = self.find_upvalue(ident.name) {
            self.push(Instruction::with_u16(Opcode::LdUpv, u16::from(slot)));
            // Upvalues pointing to cells also need dereferencing
            if self.cell_locals.contains(&ident.name) {
                self.push(Instruction::with_u8(Opcode::ArrGetI, 0));
            }
        } else if let Some(idx) = self.find_global(ident.name) {
            self.push(Instruction::with_u16(Opcode::LdGlob, idx));
        } else if let Some(idx) = self
            .imported_globals
            .get(self.typed_module.db.interner.resolve(ident.name))
        {
            self.push_absolute_global_load(*idx);
        } else {
            panic!(
                "unresolved variable: '{}'",
                self.typed_module.db.interner.resolve(ident.name)
            )
        }
    }

    fn emit_app(&mut self, callee: ExprId, args: &ExprList) -> EmitResult {
        if let Some(DispatchInfo::Static { opcode }) = self.typed_module.dispatch(callee) {
            for &arg in args {
                self.emit_expr(arg)?;
            }
            self.push(Instruction::simple(*opcode));
            return Ok(());
        }

        if let Some(foreign_idx) = self.resolve_foreign_callee(callee) {
            for &arg in args {
                self.emit_expr(arg)?;
            }
            let pin_count = self.emit_ffi_pins(foreign_idx);
            self.push(Instruction::with_u16(Opcode::FfiCall, foreign_idx));
            for _ in 0..pin_count {
                self.push(Instruction::simple(Opcode::GcUnpin));
            }
            return Ok(());
        }

        self.emit_expr(callee)?;
        for &arg in args {
            self.emit_expr(arg)?;
        }
        let arity = u8::try_from(args.len()).expect("too many arguments (>255)");
        self.push(Instruction::with_u8(Opcode::Call, arity));
        Ok(())
    }

    fn emit_binop(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId, expr_id: ExprId) -> EmitResult {
        if matches!(op, BinOp::Range | BinOp::RangeExcl) {
            return self.emit_range(op, lhs, rhs);
        }
        if op == BinOp::NilCoalesce {
            return self.emit_nil_coalesce(lhs, rhs);
        }

        if let Some(dispatch) = self.typed_module.dispatch(expr_id) {
            match dispatch {
                DispatchInfo::Static { opcode } => {
                    self.emit_expr(lhs)?;
                    self.emit_expr(rhs)?;
                    self.push(Instruction::simple(*opcode));
                    return Ok(());
                }
                DispatchInfo::Dictionary { class, method_idx } => {
                    let method_u8 =
                        u8::try_from(*method_idx).expect("method index overflow (>255)");
                    let class_id = self.typed_module.class_id(*class).unwrap_or(0);
                    let lhs_type_id = self.expr_runtime_type_id(lhs, format::BUILTIN_TYPE_ANY);
                    self.emit_expr(lhs)?;
                    self.emit_runtime_type_id(lhs_type_id);
                    self.push(Instruction::with_u16(Opcode::TyclDict, class_id));
                    self.push(Instruction::with_u8(Opcode::TyclCall, method_u8));
                    self.push(Instruction::simple(Opcode::Swap));
                    self.emit_expr(rhs)?;
                    self.push(Instruction::with_u8(Opcode::Call, 2));
                    return Ok(());
                }
                DispatchInfo::Dynamic => {
                    self.emit_expr(lhs)?;
                    self.emit_expr(rhs)?;
                    return self.emit_dynamic_binop(op);
                }
            }
        }

        self.emit_expr(lhs)?;
        self.emit_expr(rhs)?;
        self.push(Instruction::simple(binop_to_opcode(op)));
        Ok(())
    }

    /// Emits a runtime type-switch for binary operators on `Any`-typed values.
    ///
    /// Stack state on entry: `[..., lhs, rhs]`.
    /// Emits: save rhs, check lhs tag, branch to int or float path, rejoin.
    fn emit_dynamic_binop(&mut self, op: BinOp) -> EmitResult {
        let Some((int_opcode, float_opcode)) = dynamic_binop_opcodes(op) else {
            return Err(EmitError::Unimplemented(
                "dynamic dispatch for this operator",
            ));
        };

        // Stack: [lhs, rhs]
        self.push(Instruction::simple(Opcode::Swap)); // [rhs, lhs]
        self.push(Instruction::simple(Opcode::Dup)); // [rhs, lhs, lhs]
        self.push(Instruction::simple(Opcode::TyTag)); // [rhs, lhs, tag]
        self.push(Instruction::with_i16(
            Opcode::LdSmi,
            i16::from(format::NAN_BOX_SMI),
        )); // [rhs, lhs, tag, 1]
        self.push(Instruction::simple(Opcode::CmpEq)); // [rhs, lhs, is_int]
        let int_path_jump = self.placeholder_jump(Opcode::BrTrue);

        // Float path (fallthrough)
        self.push(Instruction::simple(Opcode::Swap)); // [lhs, rhs]
        self.push(Instruction::simple(float_opcode));
        let end_jump = self.placeholder_jump(Opcode::BrJmp);

        // Int path
        self.patch_jump(int_path_jump);
        self.push(Instruction::simple(Opcode::Swap)); // [lhs, rhs]
        self.push(Instruction::simple(int_opcode));

        self.patch_jump(end_jump);
        Ok(())
    }

    fn emit_unary(&mut self, op: UnaryOp, operand: ExprId, expr_id: ExprId) -> EmitResult {
        self.emit_expr(operand)?;
        match op {
            UnaryOp::Neg => {
                if let Some(DispatchInfo::Static { opcode }) = self.typed_module.dispatch(expr_id) {
                    self.push(Instruction::simple(*opcode));
                    return Ok(());
                }
                self.push(Instruction::simple(Opcode::INeg));
            }
            UnaryOp::Not => self.push(Instruction::simple(Opcode::Not)),
            UnaryOp::Mut | UnaryOp::Spread => {}
        }
        Ok(())
    }

    fn emit_branch(&mut self, cond: ExprId, then_br: ExprId, else_br: ExprId) -> EmitResult {
        self.emit_expr(cond)?;
        let false_jump = self.placeholder_jump(Opcode::BrFalse);
        self.emit_expr(then_br)?;
        let end_jump = self.placeholder_jump(Opcode::BrJmp);
        self.patch_jump(false_jump);
        self.emit_expr(else_br)?;
        self.patch_jump(end_jump);
        Ok(())
    }

    fn placeholder_jump(&mut self, opcode: Opcode) -> usize {
        let pos = self.current_instructions.len();
        self.push(Instruction::with_i16(opcode, 0));
        pos
    }

    fn patch_jump(&mut self, placeholder_pos: usize) {
        let current = self.current_instructions.len();
        let byte_offset: usize = self.current_instructions[placeholder_pos + 1..current]
            .iter()
            .map(instruction_byte_size)
            .sum();
        let offset = i16::try_from(byte_offset).expect("jump too far");
        let old_opcode = self.current_instructions[placeholder_pos].opcode;
        self.current_instructions[placeholder_pos] = Instruction::with_i16(old_opcode, offset);
    }

    fn emit_let(&mut self, binding: &LetBinding) -> EmitResult {
        let pat_node = self.typed_module.db.ast.pats.get(binding.pat);
        if let PatKind::Bind(ident) = &pat_node.kind {
            if let Some(value) = binding.value {
                let is_captured_mut = binding.modifiers.mutable
                    && self
                        .typed_module
                        .type_env
                        .captured_mutable_names
                        .contains(&ident.name);

                if is_captured_mut {
                    // Allocate a heap cell: ArrNew(1) then store value at index 0
                    self.push_arr_new(format::INTERNAL_TYPE_CELL, 1);
                    self.emit_expr(value)?;
                    self.push(Instruction::with_u8(Opcode::ArrSetI, 0));
                    let slot = self.local_slot(ident.name);
                    self.emit_st_loc(slot);
                    let _inserted = self.cell_locals.insert(ident.name);
                } else if let Some(ref sig) = binding.sig {
                    if sig.has_param_list {
                        let method_idx =
                            self.compile_function_method(Some(ident.name), &sig.params, value)?;
                        self.push(Instruction::with_wide(Opcode::ClsNew, method_idx, 0));
                        let slot = self.local_slot(ident.name);
                        self.emit_st_loc(slot);
                    } else {
                        self.emit_expr(value)?;
                        let slot = self.local_slot(ident.name);
                        self.emit_st_loc(slot);
                    }
                } else {
                    self.emit_expr(value)?;
                    let slot = self.local_slot(ident.name);
                    self.emit_st_loc(slot);
                }
            }
        }
        self.push(Instruction::simple(Opcode::LdUnit));
        Ok(())
    }

    fn emit_lambda(&mut self, expr_id: ExprId, params: &[Param], body: ExprId) -> EmitResult {
        let captured = self
            .typed_module
            .resolution
            .captures
            .get(&expr_id)
            .cloned()
            .unwrap_or_default();

        // Before entering the lambda body, push captured values onto the stack
        // in the *outer* frame so ClsNew can close over them.
        for &cap in &captured {
            if let Some(slot) = self.find_local(cap) {
                self.emit_ld_loc(slot);
            } else if let Some(slot) = self.find_upvalue(cap) {
                self.push(Instruction::with_u16(Opcode::LdUpv, u16::from(slot)));
            } else if let Some(idx) = self.find_global(cap) {
                self.push(Instruction::with_u16(Opcode::LdGlob, idx));
            } else if let Some(idx) = self
                .imported_globals
                .get(self.typed_module.db.interner.resolve(cap))
            {
                self.push_absolute_global_load(*idx);
            }
        }

        let saved_instructions = mem::take(&mut self.current_instructions);
        let saved_absolute_global_loads = mem::take(&mut self.current_absolute_global_loads);
        let saved_locals = mem::take(&mut self.current_locals);
        let saved_upvalues = mem::take(&mut self.current_upvalues);
        let saved_cells = mem::take(&mut self.cell_locals);

        self.current_upvalues.clone_from(&captured);
        // Propagate cell status for captured mutable variables into the lambda
        for &cap in &captured {
            if saved_cells.contains(&cap) {
                let _inserted = self.cell_locals.insert(cap);
            }
        }

        for param in params {
            let _slot = self.local_slot(param.name.name);
        }

        self.emit_expr(body)?;
        self.push(Instruction::simple(Opcode::Ret));

        let locals_count =
            u16::try_from(self.current_locals.len()).expect("too many locals (>65535)");
        let method_idx = u16::try_from(self.methods.len()).expect("too many methods (>65535)");
        let upval_count = u8::try_from(captured.len()).expect("too many upvalues (>255)");
        let anon_name = self.alloc_anon_symbol();

        self.methods.push(MethodEntry {
            name: Some(anon_name),
            source_name: None,
            instructions: mem::replace(&mut self.current_instructions, saved_instructions),
            locals_count,
            absolute_global_loads: mem::replace(
                &mut self.current_absolute_global_loads,
                saved_absolute_global_loads,
            ),
        });
        self.current_locals = saved_locals;
        self.current_upvalues = saved_upvalues;
        self.cell_locals = saved_cells;

        self.push(Instruction::with_wide(
            Opcode::ClsNew,
            method_idx,
            upval_count,
        ));
        Ok(())
    }

    fn emit_seq(&mut self, stmts: &ExprList) -> EmitResult {
        if stmts.is_empty() {
            self.push(Instruction::simple(Opcode::LdUnit));
            return Ok(());
        }
        for (i, &stmt) in stmts.iter().enumerate() {
            self.emit_expr(stmt)?;
            if i + 1 < stmts.len() {
                self.push(Instruction::simple(Opcode::Pop));
            }
        }
        Ok(())
    }

    fn emit_case(&mut self, scrutinee: ExprId, arms: &[CaseArm]) -> EmitResult {
        self.emit_expr(scrutinee)?;

        if arms.is_empty() {
            return Ok(());
        }

        // Try BrTbl for dense variant dispatch: all arms must be variant
        // patterns with known tag indices and no guards.
        if self.try_emit_brtbl_case(arms)?.is_some() {
            return Ok(());
        }

        let mut end_jumps = Vec::new();

        for (i, arm) in arms.iter().enumerate() {
            let is_last = i + 1 == arms.len();
            let pat_node = self.typed_module.db.ast.pats.get(arm.pat);

            match &pat_node.kind {
                PatKind::Variant { tag, fields } => {
                    let tag = *tag;
                    let fields = fields.clone();
                    self.emit_match_variant(&tag, &fields, arm, is_last, &mut end_jumps)?;
                }
                PatKind::Bind(ident) => {
                    let slot = self.local_slot(ident.name);
                    self.emit_st_loc(slot);
                    self.emit_guard_and_body(arm, is_last, &mut end_jumps)?;
                }
                PatKind::Lit(literal) => {
                    let literal = literal.clone();
                    self.emit_match_lit(&literal, arm, is_last, &mut end_jumps)?;
                }
                PatKind::Tuple(pats) | PatKind::Array(pats) => {
                    let pats = pats.clone();
                    self.emit_match_tuple_or_array(&pats, arm, is_last, &mut end_jumps)?;
                }
                PatKind::Record(fields) => {
                    let fields = fields.clone();
                    self.emit_match_record(&fields, arm, is_last, &mut end_jumps)?;
                }
                PatKind::Or(pats) => {
                    let pats = pats.clone();
                    self.emit_match_or(&pats, arm, is_last, &mut end_jumps)?;
                }
                PatKind::As { name, pat } => {
                    let name = *name;
                    let inner_pat = *pat;
                    self.emit_match_as(name, inner_pat, arm, is_last, &mut end_jumps)?;
                }
                PatKind::Wildcard => {
                    self.push(Instruction::simple(Opcode::Pop));
                    self.emit_guard_and_body(arm, is_last, &mut end_jumps)?;
                }
            }
        }

        for end_jump in end_jumps {
            self.patch_jump(end_jump);
        }
        Ok(())
    }

    /// Variant branch-table dispatch is disabled until user-defined variants
    /// have a real numeric tag ABI in the VM. Current runtime matching uses
    /// string tags, so `BrTbl` would misdispatch.
    fn try_emit_brtbl_case(&mut self, arms: &[CaseArm]) -> EmitResult<Option<()>> {
        let _ = arms;
        Ok(None)
    }

    fn emit_guard_and_body(
        &mut self,
        arm: &CaseArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) -> EmitResult {
        let guard_jump = if let Some(guard) = arm.guard {
            self.emit_expr(guard)?;
            if is_last {
                None
            } else {
                Some(self.placeholder_jump(Opcode::BrFalse))
            }
        } else {
            None
        };

        self.emit_expr(arm.body)?;

        if !is_last {
            end_jumps.push(self.placeholder_jump(Opcode::BrJmp));
        }
        if let Some(gj) = guard_jump {
            self.patch_jump(gj);
        }
        Ok(())
    }

    fn emit_match_lit(
        &mut self,
        literal: &Literal,
        arm: &CaseArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) -> EmitResult {
        self.push(Instruction::simple(Opcode::Dup));
        self.emit_literal(literal);
        self.push(Instruction::simple(Opcode::CmpEq));

        let next_arm = if is_last {
            None
        } else {
            Some(self.placeholder_jump(Opcode::BrFalse))
        };

        self.push(Instruction::simple(Opcode::Pop));
        self.emit_guard_and_body(arm, is_last, end_jumps)?;

        if let Some(next) = next_arm {
            self.patch_jump(next);
        }
        Ok(())
    }

    fn emit_match_tuple_or_array(
        &mut self,
        pats: &[PatId],
        arm: &CaseArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) -> EmitResult {
        for (i, &sub_pat) in pats.iter().enumerate() {
            let sub_pat_node = self.typed_module.db.ast.pats.get(sub_pat);
            if let PatKind::Bind(ident) = &sub_pat_node.kind {
                self.push(Instruction::simple(Opcode::Dup));
                let idx = u8::try_from(i).expect("too many tuple/array elements (>255)");
                self.push(Instruction::with_u8(Opcode::ArrGetI, idx));
                let slot = self.local_slot(ident.name);
                self.emit_st_loc(slot);
            }
        }

        self.push(Instruction::simple(Opcode::Pop));
        self.emit_guard_and_body(arm, is_last, end_jumps)?;
        Ok(())
    }

    fn emit_match_record(
        &mut self,
        fields: &[RecordPatField],
        arm: &CaseArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) -> EmitResult {
        for (i, field) in fields.iter().enumerate() {
            let bind_ident = if let Some(pat_id) = field.pat {
                let pat_node = self.typed_module.db.ast.pats.get(pat_id);
                if let PatKind::Bind(ident) = &pat_node.kind {
                    Some(*ident)
                } else {
                    None
                }
            } else {
                Some(field.name)
            };

            if let Some(ident) = bind_ident {
                self.push(Instruction::simple(Opcode::Dup));
                let idx = u8::try_from(i).expect("too many record fields (>255)");
                self.push(Instruction::with_u8(Opcode::ArrGetI, idx));
                let slot = self.local_slot(ident.name);
                self.emit_st_loc(slot);
            }
        }

        self.push(Instruction::simple(Opcode::Pop));
        self.emit_guard_and_body(arm, is_last, end_jumps)?;
        Ok(())
    }

    fn emit_match_variant(
        &mut self,
        tag: &Ident,
        fields: &[PatId],
        arm: &CaseArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) -> EmitResult {
        self.push(Instruction::simple(Opcode::Dup));
        if let Some(opcode) = self.builtin_bool_variant_opcode(tag.name) {
            self.push(Instruction::simple(opcode));
        } else {
            let const_idx = self.pool.add(ConstantEntry::Str(
                self.typed_module.db.interner.resolve(tag.name).into(),
            ));
            self.push(Instruction::simple(Opcode::ArrTag));
            self.push(Instruction::with_u16(Opcode::LdConst, const_idx));
        }
        self.push(Instruction::simple(Opcode::CmpEq));

        let next_arm = if is_last {
            None
        } else {
            Some(self.placeholder_jump(Opcode::BrFalse))
        };

        for (fi, &field_pat) in fields.iter().enumerate() {
            let field_pat_node = self.typed_module.db.ast.pats.get(field_pat);
            if let PatKind::Bind(field_ident) = &field_pat_node.kind {
                self.push(Instruction::simple(Opcode::Dup));
                let field_u8 = u8::try_from(fi).expect("too many fields (>255)");
                self.push(Instruction::with_u8(Opcode::ArrGetI, field_u8));
                let slot = self.local_slot(field_ident.name);
                self.emit_st_loc(slot);
            }
        }

        self.push(Instruction::simple(Opcode::Pop));
        self.emit_guard_and_body(arm, is_last, end_jumps)?;

        if let Some(next) = next_arm {
            self.patch_jump(next);
        }
        Ok(())
    }

    fn emit_record_lit(&mut self, expr_id: ExprId, fields: &[RecordField]) -> EmitResult {
        let field_count = u16::try_from(fields.len()).expect("too many fields (>65535)");
        let type_id = self.expr_runtime_type_id(expr_id, format::BUILTIN_TYPE_ANY);
        self.push_arr_new(type_id, field_count);

        for (i, field) in fields.iter().enumerate() {
            match field {
                RecordField::Named { value, .. } => {
                    if let Some(val_id) = value {
                        let idx = u8::try_from(i).expect("too many fields (>255)");
                        self.emit_expr(*val_id)?;
                        self.push(Instruction::with_u8(Opcode::ArrSetI, idx));
                    }
                }
                RecordField::Spread(expr_id) => {
                    self.emit_expr(*expr_id)?;
                    self.push(Instruction::simple(Opcode::ArrCaten));
                }
            }
        }
        Ok(())
    }

    fn emit_variant_lit(&mut self, expr_id: ExprId, tag: &Ident, args: &ExprList) -> EmitResult {
        if let Some(DispatchInfo::Static { opcode }) = self.typed_module.dispatch(expr_id) {
            self.push(Instruction::simple(*opcode));
            return Ok(());
        }

        let tag_idx = self.pool.add(ConstantEntry::Str(
            self.typed_module.db.interner.resolve(tag.name).to_owned(),
        ));
        let tag_byte = u8::try_from(tag_idx).expect("variant tag constant index overflow (>255)");

        let len = u16::try_from(args.len()).expect("too many variant args (>65535)");
        let type_id = self.expr_runtime_type_id(expr_id, format::BUILTIN_TYPE_ANY);
        self.push_arr_new_tagged(type_id, tag_byte, len);

        for (i, &arg) in args.iter().enumerate() {
            self.emit_expr(arg)?;
            let idx = u8::try_from(i).expect("too many variant args (>255)");
            self.push(Instruction::with_u8(Opcode::ArrSetI, idx));
        }
        Ok(())
    }

    fn emit_access(&mut self, expr: ExprId, field: &FieldTarget) -> EmitResult {
        self.emit_expr(expr)?;
        match *field {
            FieldTarget::Index(idx) => {
                let field_u8 = u8::try_from(idx).expect("field index overflow (>255)");
                self.push(Instruction::with_u8(Opcode::ArrGetI, field_u8));
            }
            FieldTarget::Name(ref ident) => {
                if let Some(idx) = self.resolve_field_index(expr, ident.name) {
                    self.push(Instruction::with_u8(Opcode::ArrGetI, idx));
                } else if self.is_len_access(expr, ident.name) {
                    self.push(Instruction::simple(Opcode::ArrLen));
                } else {
                    return Err(EmitError::Unimplemented("named field access on non-record"));
                }
            }
        }
        Ok(())
    }

    fn emit_return(&mut self, value: Option<ExprId>) -> EmitResult {
        if let Some(expr_id) = value {
            self.emit_expr(expr_id)?;
        } else {
            self.push(Instruction::simple(Opcode::LdUnit));
        }
        self.push(Instruction::simple(Opcode::Ret));
        Ok(())
    }

    fn emit_tuple_lit(&mut self, expr_id: ExprId, elems: &ExprList) -> EmitResult {
        let len = u16::try_from(elems.len()).expect("too many tuple elements (>65535)");
        let type_id = self.expr_runtime_type_id(expr_id, format::BUILTIN_TYPE_ANY);
        self.push_arr_new(type_id, len);
        for (i, &elem) in elems.iter().enumerate() {
            self.emit_expr(elem)?;
            let idx = u8::try_from(i).expect("too many tuple elements (>255)");
            self.push(Instruction::with_u8(Opcode::ArrSetI, idx));
        }
        Ok(())
    }

    fn emit_array_lit(&mut self, expr_id: ExprId, elems: &ExprList) -> EmitResult {
        let type_id = self.expr_runtime_type_id(expr_id, BuiltinType::Array.type_id());
        if elems.iter().any(|&elem| self.is_spread_expr(elem)) {
            self.push_arr_new(type_id, 0);
            for &elem in elems {
                if let Some(inner) = self.spread_expr_operand(elem) {
                    self.emit_expr(inner)?;
                } else {
                    self.push_arr_new(type_id, 1);
                    self.emit_expr(elem)?;
                    self.push(Instruction::with_u8(Opcode::ArrSetI, 0));
                }
                self.push(Instruction::simple(Opcode::ArrCaten));
            }
            return Ok(());
        }

        let len = u16::try_from(elems.len()).expect("too many array elements (>65535)");
        self.push_arr_new(type_id, len);
        for (i, &elem) in elems.iter().enumerate() {
            self.emit_expr(elem)?;
            let idx = u8::try_from(i).expect("too many array elements (>255)");
            self.push(Instruction::with_u8(Opcode::ArrSetI, idx));
        }
        Ok(())
    }

    fn emit_index(&mut self, expr: ExprId, indices: &ExprList) -> EmitResult {
        self.emit_expr(expr)?;
        for &index in indices {
            self.emit_expr(index)?;
            self.push(Instruction::simple(Opcode::ArrGet));
        }
        Ok(())
    }

    fn emit_fstr(&mut self, parts: &[FStrPart]) -> EmitResult {
        if parts.is_empty() {
            let idx = self.pool.add(ConstantEntry::Str(String::new()));
            self.push(Instruction::with_u16(Opcode::LdConst, idx));
            return Ok(());
        }

        if parts.len() == 1 {
            match &parts[0] {
                FStrPart::Lit(s) => {
                    let idx = self.pool.add(ConstantEntry::Str(s.clone()));
                    self.push(Instruction::with_u16(Opcode::LdConst, idx));
                }
                FStrPart::Expr(expr_id) => self.emit_expr(*expr_id)?,
            }
            return Ok(());
        }

        for (i, part) in parts.iter().enumerate() {
            match part {
                FStrPart::Lit(s) => {
                    let idx = self.pool.add(ConstantEntry::Str(s.clone()));
                    self.push(Instruction::with_u16(Opcode::LdConst, idx));
                }
                FStrPart::Expr(expr_id) => self.emit_expr(*expr_id)?,
            }
            if i > 0 {
                self.push(Instruction::simple(Opcode::ArrCaten));
            }
        }
        Ok(())
    }

    fn emit_range(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId) -> EmitResult {
        let tag_str = if op == BinOp::RangeExcl {
            "RangeExcl"
        } else {
            "Range"
        };
        let tag_idx = self.pool.add(ConstantEntry::Str(tag_str.into()));
        let tag_byte = u8::try_from(tag_idx & 0xFF).expect("tag index overflow");
        self.push_arr_new_tagged(format::BUILTIN_TYPE_ANY, tag_byte, 2);

        self.emit_expr(lhs)?;
        self.push(Instruction::with_u8(Opcode::ArrSetI, 0));

        self.emit_expr(rhs)?;
        self.push(Instruction::with_u8(Opcode::ArrSetI, 1));
        Ok(())
    }

    fn emit_perform(&mut self, expr_id: ExprId, operand: ExprId) -> EmitResult {
        match self.typed_module.db.ast.exprs.get(operand).kind.clone() {
            ExprKind::App(_, args) => {
                if args.is_empty() {
                    self.push(Instruction::simple(Opcode::LdUnit));
                } else if args.len() == 1 {
                    self.emit_expr(args[0])?;
                } else {
                    self.push_arr_new(
                        BuiltinType::Array.type_id(),
                        u16::try_from(args.len()).expect("too many effect arguments (>65535)"),
                    );
                    for (i, arg) in args.iter().enumerate() {
                        self.emit_expr(*arg)?;
                        self.push(Instruction::with_u8(
                            Opcode::ArrSetI,
                            u8::try_from(i).expect("too many effect arguments (>255)"),
                        ));
                    }
                }
            }
            _ => self.emit_expr(operand)?,
        }
        let effect_use = self
            .typed_module
            .type_env
            .perform_effects
            .get(&expr_id)
            .copied()
            .expect("missing effect metadata for perform expression");
        self.push(Instruction::with_effect(
            Opcode::EffInvk,
            effect_use.effect_id,
            effect_use.op_id,
        ));
        Ok(())
    }

    fn emit_handle(
        &mut self,
        expr_id: ExprId,
        clauses: &[HandlerClause],
        body: ExprId,
    ) -> EmitResult {
        let effect_id = self
            .typed_module
            .type_env
            .handle_effects
            .get(&expr_id)
            .copied()
            .expect("missing effect metadata for handle expression");

        for clause in clauses {
            let HandlerClause::Op {
                name,
                args,
                cont,
                body: handler_body,
            } = clause
            else {
                continue;
            };
            let op_name = name.name;
            let op_id = self
                .typed_module
                .type_env
                .effect_by_id(effect_id)
                .and_then(|effect| effect.operations.iter().find(|op| op.name == op_name))
                .map(|op| op.id)
                .expect("missing effect operation metadata for handler");
            let pos = self.current_instructions.len();
            self.push(Instruction::with_effect_jump(
                Opcode::EffHdlPush,
                effect_id,
                op_id,
                0,
            ));
            self.emit_handler_clause_prologue(effect_id, op_name, args, *cont);
            self.emit_expr(*handler_body)?;
            let current = self.current_instructions.len();
            let byte_offset: usize = self.current_instructions[pos + 1..current]
                .iter()
                .map(instruction_byte_size)
                .sum();
            let offset = i16::try_from(byte_offset).expect("jump too far");
            self.current_instructions[pos] =
                Instruction::with_effect_jump(Opcode::EffHdlPush, effect_id, op_id, offset);
        }

        let return_clause = clauses.iter().find_map(|clause| match clause {
            HandlerClause::Return { binder, body } => Some((*binder, *body)),
            HandlerClause::Op { .. } => None,
        });
        self.emit_expr(body)?;
        if let Some((binder, return_body)) = return_clause {
            let slot = self.local_slot(binder.name);
            self.emit_st_loc(slot);
            self.emit_expr(return_body)?;
        }
        for clause in clauses {
            if matches!(clause, HandlerClause::Op { .. }) {
                self.push(Instruction::simple(Opcode::EffHdlPop));
            }
        }
        Ok(())
    }

    fn emit_handler_clause_prologue(
        &mut self,
        effect_id: u16,
        op_name: Symbol,
        args: &[Ident],
        cont: Ident,
    ) {
        let cont_slot = self.local_slot(cont.name);
        self.emit_st_loc(cont_slot);
        let Some(op_info) = self
            .typed_module
            .type_env
            .effect_by_id(effect_id)
            .and_then(|effect| effect.operations.iter().find(|op| op.name == op_name))
        else {
            return;
        };
        match self.effect_payload_arity(op_info.param_ty) {
            0 => self.push(Instruction::simple(Opcode::Pop)),
            1 => {
                if let Some(arg) = args.first() {
                    let arg_slot = self.local_slot(arg.name);
                    self.emit_st_loc(arg_slot);
                } else {
                    self.push(Instruction::simple(Opcode::Pop));
                }
            }
            _ => {
                let payload_sym = self.alloc_anon_symbol();
                let payload_slot = self.local_slot(payload_sym);
                self.emit_st_loc(payload_slot);
                for (idx, arg) in args.iter().enumerate() {
                    self.emit_ld_loc(payload_slot);
                    self.push(Instruction::with_u8(
                        Opcode::ArrGetI,
                        u8::try_from(idx).expect("too many handler payload arguments (>255)"),
                    ));
                    let arg_slot = self.local_slot(arg.name);
                    self.emit_st_loc(arg_slot);
                }
            }
        }
    }

    fn effect_payload_arity(&self, param_ty: Option<SemaTypeId>) -> usize {
        let Some(param_ty) = param_ty else {
            return 0;
        };
        let resolved = self.typed_module.type_env.resolve_var(param_ty);
        match self.typed_module.type_env.types.get(resolved) {
            Ty::Unit => 0,
            Ty::Tuple(elems) => elems.len(),
            _ => 1,
        }
    }

    fn emit_resume(&mut self, value: Option<ExprId>) -> EmitResult {
        if let Some(expr_id) = value {
            self.emit_expr(expr_id)?;
            self.push(Instruction::with_u8(Opcode::EffCont, 1));
        } else {
            self.push(Instruction::with_u8(Opcode::EffCont, 0));
        }
        Ok(())
    }

    fn emit_matrix_lit(&mut self, expr_id: ExprId, rows: &[ExprList]) -> EmitResult {
        let row_count = u16::try_from(rows.len()).expect("too many matrix rows (>65535)");
        let type_id = self.expr_runtime_type_id(expr_id, BuiltinType::Array.type_id());
        self.push_arr_new(type_id, row_count);

        for (ri, row) in rows.iter().enumerate() {
            let col_count = u16::try_from(row.len()).expect("too many matrix columns (>65535)");
            self.push_arr_new(BuiltinType::Array.type_id(), col_count);

            for (ci, &elem) in row.iter().enumerate() {
                self.emit_expr(elem)?;
                let col_idx = u8::try_from(ci).expect("too many matrix columns (>255)");
                self.push(Instruction::with_u8(Opcode::ArrSetI, col_idx));
            }

            let row_idx = u8::try_from(ri).expect("too many matrix rows (>255)");
            self.push(Instruction::with_u8(Opcode::ArrSetI, row_idx));
        }
        Ok(())
    }

    fn emit_record_update(&mut self, base: ExprId, fields: &[RecordField]) -> EmitResult {
        self.emit_expr(base)?;
        self.push(Instruction::simple(Opcode::ArrCopy));

        for (i, field) in fields.iter().enumerate() {
            match field {
                RecordField::Named { value, .. } => {
                    if let Some(val_id) = value {
                        let idx = u8::try_from(i).expect("too many fields (>255)");
                        self.emit_expr(*val_id)?;
                        self.push(Instruction::with_u8(Opcode::ArrSetI, idx));
                    }
                }
                RecordField::Spread(expr_id) => {
                    self.emit_expr(*expr_id)?;
                    self.push(Instruction::simple(Opcode::ArrCaten));
                }
            }
        }
        Ok(())
    }

    fn emit_postfix(&mut self, expr: ExprId, op: PostfixOp) -> EmitResult {
        self.emit_expr(expr)?;
        self.push(Instruction::simple(Opcode::Dup));
        self.push(Instruction::simple(Opcode::ArrTag));

        let none_idx = self.pool.add(ConstantEntry::Str("None".into()));
        self.push(Instruction::with_u16(Opcode::LdConst, none_idx));
        self.push(Instruction::simple(Opcode::CmpEq));

        let skip_jump = self.placeholder_jump(Opcode::BrFalse);

        match op {
            PostfixOp::Force => self.push(Instruction::simple(Opcode::Panic)),
            PostfixOp::Propagate => self.push(Instruction::simple(Opcode::Ret)),
        }

        self.patch_jump(skip_jump);
        self.push(Instruction::with_u8(Opcode::ArrGetI, 0));
        Ok(())
    }

    fn emit_type_op(&mut self, expr: ExprId, ty_id: TyId, kind: TypeOpKind) -> EmitResult {
        self.emit_expr(expr)?;
        let type_id = self.type_to_table_id(ty_id);
        match kind {
            TypeOpKind::Test(opt_ident) => {
                self.push(Instruction::with_u16(Opcode::TyChk, type_id));
                if let Some(ident) = opt_ident {
                    self.push(Instruction::simple(Opcode::Dup));
                    let slot = self.local_slot(ident.name);
                    self.emit_st_loc(slot);
                }
            }
            TypeOpKind::Cast => {
                self.push(Instruction::with_u16(Opcode::TyCast, type_id));
            }
        }
        Ok(())
    }

    fn emit_nil_coalesce(&mut self, lhs: ExprId, rhs: ExprId) -> EmitResult {
        self.emit_expr(lhs)?;
        self.push(Instruction::simple(Opcode::Dup));
        self.push(Instruction::simple(Opcode::ArrTag));

        let none_idx = self.pool.add(ConstantEntry::Str("None".into()));
        self.push(Instruction::with_u16(Opcode::LdConst, none_idx));
        self.push(Instruction::simple(Opcode::CmpEq));

        let end_jump = self.placeholder_jump(Opcode::BrFalse);

        self.push(Instruction::simple(Opcode::Pop));
        self.emit_expr(rhs)?;

        self.patch_jump(end_jump);
        Ok(())
    }

    fn emit_instance_def(&mut self, _expr_id: ExprId, inst: &InstanceDef) -> EmitResult {
        match &inst.body {
            InstanceBody::Methods(members) => {
                for member in members {
                    if let MemberDecl::Fn(decl) = member {
                        if let Some(body) = decl.body {
                            let name = match decl.name {
                                MemberName::Ident(ident) | MemberName::Op(ident, _) => ident.name,
                            };
                            let params = decl.params.as_deref().unwrap_or(&[]);
                            self.emit_function(name, params, body)?;
                        }
                    }
                }
            }
            InstanceBody::Via(_) => {
                // Via-derived instances delegate to the target type's methods.
                // Sema registers the instance; actual forwarding stubs are
                // generated when class descriptors are built. No bytecode
                // needed at the instance definition site.
            }
        }
        Ok(())
    }

    fn emit_foreign_import(&mut self, sym: Symbol) {
        let name = self.typed_module.db.interner.resolve(sym);
        let foreign_idx =
            u16::try_from(self.foreigns.len()).expect("too many foreign imports (>65535)");
        self.foreigns.push(ForeignDescriptor {
            name_idx: self.pool.add(ConstantEntry::Str(name.into())).into(),
            symbol_idx: u32::MAX,
            lib_idx: u32::MAX,
            abi: format::ForeignAbi::Default,
            arity: 0,
            exported: false,
            param_types: Vec::new(),
            return_type: format::FfiType::Void,
        });
        self.push(Instruction::with_u16(Opcode::FfiCall, foreign_idx));
    }

    /// Emit code for an import expression.
    ///
    /// - `Wildcard`: register imported globals under their local names
    /// - `Qualified(alias)`: construct a record of the module's exported globals
    /// - `Selective(alias, names)`: same as qualified but only selected names
    fn emit_import(&mut self, path: Symbol, kind: &ImportKind) {
        match kind {
            ImportKind::Wildcard => {
                if let Some(imports) = self.module_exports.get(&path) {
                    for imported in imports {
                        let _prev = self
                            .imported_globals
                            .insert(imported.name.clone(), imported.index);
                    }
                }
            }
            ImportKind::Qualified(alias) => {
                let global_indices = self.module_exports.get(&path).cloned().unwrap_or_default();
                self.emit_import_record(alias.name, &global_indices);
            }
            ImportKind::Selective(alias, names) => {
                let selected = self
                    .module_exports
                    .get(&path)
                    .map(|imports| {
                        imports
                            .iter()
                            .filter(|imported| {
                                names.iter().any(|name| {
                                    self.typed_module.db.interner.resolve(name.name)
                                        == imported.name
                                })
                            })
                            .cloned()
                            .collect::<Vec<_>>()
                    })
                    .unwrap_or_default();
                self.emit_import_record(alias.name, &selected);
            }
        }
    }

    /// Emit bytecode to construct a record (array) of imported globals and
    /// store it in the alias's local slot.
    fn emit_import_record(&mut self, alias: Symbol, globals: &[ImportedGlobal]) {
        if globals.is_empty() {
            // No exports resolved -- emit an empty record.
            self.push_arr_new(format::BUILTIN_TYPE_ANY, 0);
        } else {
            let field_count =
                u16::try_from(globals.len()).expect("too many import exports (>65535)");
            self.push_arr_new(format::BUILTIN_TYPE_ANY, field_count);
            for (i, imported) in globals.iter().enumerate() {
                self.push_absolute_global_load(imported.index);
                let field_idx = u8::try_from(i).expect("too many import fields (>255)");
                self.push(Instruction::with_u8(Opcode::ArrSetI, field_idx));
            }
        }
        let slot = self.local_slot(alias);
        self.emit_st_loc(slot);
    }

    fn alloc_anon_slot(&mut self) -> u16 {
        let sym = self.alloc_anon_symbol();
        let slot = u16::try_from(self.current_locals.len()).expect("too many locals (>65535)");
        self.current_locals.push(sym);
        slot
    }

    fn alloc_anon_symbol(&mut self) -> Symbol {
        let sym = Symbol::synthetic(self.next_anon);
        self.next_anon = self.next_anon.wrapping_sub(1);
        sym
    }

    fn emit_comprehension(&mut self, body: ExprId, clauses: &[CompClause]) -> EmitResult {
        self.push_arr_new(BuiltinType::Array.type_id(), 0);

        for (clause_idx, clause) in clauses.iter().enumerate() {
            match clause {
                CompClause::Generator { pat, iter } => {
                    let iter_expr = *iter;
                    let pat_id = *pat;

                    self.emit_expr(iter_expr)?;
                    let iter_slot = self.alloc_anon_slot();
                    self.emit_st_loc(iter_slot);

                    self.push(Instruction::simple(Opcode::LdNil));
                    let counter_slot = self.alloc_anon_slot();
                    self.emit_st_loc(counter_slot);

                    let loop_start = self.current_instructions.len();

                    self.emit_ld_loc(counter_slot);
                    self.emit_ld_loc(iter_slot);
                    self.push(Instruction::simple(Opcode::ArrLen));
                    self.push(Instruction::simple(Opcode::CmpGeq));
                    let end_jump = self.placeholder_jump(Opcode::BrTrue);

                    self.emit_ld_loc(iter_slot);
                    self.emit_ld_loc(counter_slot);
                    self.push(Instruction::simple(Opcode::ArrGet));

                    let pat_node = self.typed_module.db.ast.pats.get(pat_id);
                    if let PatKind::Bind(ident) = &pat_node.kind {
                        let slot = self.local_slot(ident.name);
                        self.emit_st_loc(slot);
                    } else {
                        self.push(Instruction::simple(Opcode::Pop));
                    }

                    let mut filter_jumps = Vec::new();
                    for subsequent in &clauses[clause_idx + 1..] {
                        match subsequent {
                            CompClause::Filter(guard) => {
                                self.emit_expr(*guard)?;
                                filter_jumps.push(self.placeholder_jump(Opcode::BrFalse));
                            }
                            CompClause::Generator { .. } => break,
                        }
                    }

                    self.emit_expr(body)?;
                    self.push(Instruction::simple(Opcode::ArrCaten));

                    for fj in filter_jumps {
                        self.patch_jump(fj);
                    }

                    self.emit_ld_loc(counter_slot);
                    self.push(Instruction::simple(Opcode::LdOne));
                    self.push(Instruction::simple(Opcode::IAdd));
                    self.emit_st_loc(counter_slot);

                    let brback_pos = self.current_instructions.len();
                    self.push(Instruction::with_i16(Opcode::BrBack, 0));
                    let back_bytes: usize = self.current_instructions[loop_start..=brback_pos]
                        .iter()
                        .map(instruction_byte_size)
                        .sum();
                    let back_i16 = -i16::try_from(back_bytes).expect("backward jump too far");
                    self.current_instructions[brback_pos] =
                        Instruction::with_i16(Opcode::BrBack, back_i16);

                    self.patch_jump(end_jump);
                }
                CompClause::Filter(_) => {
                    // Handled inline within the Generator arm
                }
            }
        }
        Ok(())
    }

    fn emit_match_or(
        &mut self,
        pats: &[PatId],
        arm: &CaseArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) -> EmitResult {
        let mut body_jumps = Vec::new();

        for (si, &sub_pat) in pats.iter().enumerate() {
            let sub_pat_node = self.typed_module.db.ast.pats.get(sub_pat);
            let is_last_sub = si + 1 == pats.len();

            match &sub_pat_node.kind {
                PatKind::Lit(literal) => {
                    let literal = literal.clone();
                    self.push(Instruction::simple(Opcode::Dup));
                    self.emit_literal(&literal);
                    self.push(Instruction::simple(Opcode::CmpEq));
                    if is_last_sub {
                        if is_last {
                            self.push(Instruction::simple(Opcode::Pop));
                            for bj in &body_jumps {
                                self.patch_jump(*bj);
                            }
                            self.emit_guard_and_body(arm, is_last, end_jumps)?;
                        } else {
                            let next_arm_jump = self.placeholder_jump(Opcode::BrFalse);
                            self.push(Instruction::simple(Opcode::Pop));
                            for bj in &body_jumps {
                                self.patch_jump(*bj);
                            }
                            self.emit_guard_and_body(arm, is_last, end_jumps)?;
                            self.patch_jump(next_arm_jump);
                        }
                    } else {
                        body_jumps.push(self.placeholder_jump(Opcode::BrTrue));
                    }
                }
                PatKind::Variant { tag, .. } => {
                    let tag = *tag;
                    self.push(Instruction::simple(Opcode::Dup));
                    self.push(Instruction::simple(Opcode::ArrTag));
                    let tag_idx = self.pool.add(ConstantEntry::Str(
                        self.typed_module.db.interner.resolve(tag.name).into(),
                    ));
                    self.push(Instruction::with_u16(Opcode::LdConst, tag_idx));
                    self.push(Instruction::simple(Opcode::CmpEq));
                    if is_last_sub {
                        if is_last {
                            self.push(Instruction::simple(Opcode::Pop));
                            for bj in &body_jumps {
                                self.patch_jump(*bj);
                            }
                            self.emit_guard_and_body(arm, is_last, end_jumps)?;
                        } else {
                            let next_arm_jump = self.placeholder_jump(Opcode::BrFalse);
                            self.push(Instruction::simple(Opcode::Pop));
                            for bj in &body_jumps {
                                self.patch_jump(*bj);
                            }
                            self.emit_guard_and_body(arm, is_last, end_jumps)?;
                            self.patch_jump(next_arm_jump);
                        }
                    } else {
                        body_jumps.push(self.placeholder_jump(Opcode::BrTrue));
                    }
                }
                _ => {
                    if is_last_sub {
                        self.push(Instruction::simple(Opcode::Pop));
                        for bj in &body_jumps {
                            self.patch_jump(*bj);
                        }
                        self.emit_guard_and_body(arm, is_last, end_jumps)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn emit_match_as(
        &mut self,
        name: Ident,
        inner_pat: PatId,
        arm: &CaseArm,
        is_last: bool,
        end_jumps: &mut Vec<usize>,
    ) -> EmitResult {
        self.push(Instruction::simple(Opcode::Dup));
        let name_slot = self.local_slot(name.name);
        self.emit_st_loc(name_slot);

        let inner_node = self.typed_module.db.ast.pats.get(inner_pat);
        match &inner_node.kind {
            PatKind::Variant { tag, fields } => {
                let tag = *tag;
                let fields = fields.clone();
                self.emit_match_variant(&tag, &fields, arm, is_last, end_jumps)?;
            }
            PatKind::Lit(literal) => {
                let literal = literal.clone();
                self.emit_match_lit(&literal, arm, is_last, end_jumps)?;
            }
            PatKind::Bind(ident) => {
                let slot = self.local_slot(ident.name);
                self.emit_st_loc(slot);
                self.emit_guard_and_body(arm, is_last, end_jumps)?;
            }
            _ => {
                self.push(Instruction::simple(Opcode::Pop));
                self.emit_guard_and_body(arm, is_last, end_jumps)?;
            }
        }
        Ok(())
    }

    fn emit_assign(&mut self, target: ExprId, value: ExprId) -> EmitResult {
        let target_kind = self.typed_module.db.ast.exprs.get(target).kind.clone();
        match target_kind {
            ExprKind::Var(ident) => {
                if self.cell_locals.contains(&ident.name) {
                    if let Some(slot) = self.find_local(ident.name) {
                        self.emit_ld_loc(slot);
                    } else if let Some(slot) = self.find_upvalue(ident.name) {
                        self.push(Instruction::with_u16(Opcode::LdUpv, u16::from(slot)));
                    }
                    self.emit_expr(value)?;
                    self.push(Instruction::with_u8(Opcode::ArrSetI, 0));
                } else {
                    self.emit_expr(value)?;
                    if let Some(slot) = self.find_local(ident.name) {
                        self.emit_st_loc(slot);
                    } else if let Some(slot) = self.find_upvalue(ident.name) {
                        self.push(Instruction::with_u16(Opcode::StUpv, u16::from(slot)));
                    } else if let Some(idx) = self.find_global(ident.name) {
                        self.push(Instruction::with_u16(Opcode::StGlob, idx));
                    }
                }
            }
            ExprKind::Index {
                expr, ref indices, ..
            } => {
                // arr.[i] <- val  →  [arr, idx, val] ArrSet
                // For chained indices like arr.[i].[j] <- val, navigate with ArrGet
                // to the penultimate array, then ArrSet the last index.
                let indices = indices.clone();
                if let Some((last_idx, leading)) = indices.split_last() {
                    // Navigate to the containing array.
                    self.emit_expr(expr)?;
                    for &idx in leading {
                        self.emit_expr(idx)?;
                        self.push(Instruction::simple(Opcode::ArrGet));
                    }
                    self.emit_expr(*last_idx)?;
                    self.emit_expr(value)?;
                    self.push(Instruction::simple(Opcode::ArrSet));
                }
            }
            ExprKind::Access { expr, field, .. } => {
                // rec.field <- val  →  [rec, val] ArrSeti(field_idx)
                self.emit_expr(expr)?;
                self.emit_expr(value)?;
                match field {
                    FieldTarget::Index(idx) => {
                        let field_u8 = u8::try_from(idx).expect("field index overflow (>255)");
                        self.push(Instruction::with_u8(Opcode::ArrSetI, field_u8));
                    }
                    FieldTarget::Name(ref ident) => {
                        if let Some(idx) = self.resolve_field_index(expr, ident.name) {
                            self.push(Instruction::with_u8(Opcode::ArrSetI, idx));
                        } else {
                            return Err(EmitError::Unimplemented(
                                "named field assignment on non-record",
                            ));
                        }
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn emit_foreign_let(&mut self, name: Symbol, binding: &LetBinding) {
        let name_str = self.typed_module.db.interner.resolve(name);
        let name_idx = u32::from(self.pool.add(ConstantEntry::Str(name_str.into())));
        let (lib_idx, symbol_idx) = self.extract_link_attr(&binding.attrs);
        let symbol_idx = symbol_idx.unwrap_or(name_idx);

        let abi = match binding.modifiers.foreign_abi {
            Some(sym) => {
                let abi_str = self.typed_module.db.interner.resolve(sym);
                match abi_str {
                    "cdecl" | "C" => ForeignAbi::Cdecl,
                    "stdcall" => ForeignAbi::Stdcall,
                    "fastcall" => ForeignAbi::Fastcall,
                    _ => ForeignAbi::Default,
                }
            }
            None => ForeignAbi::Default,
        };

        let (arity, param_types, return_type) = binding.sig.as_ref().map_or_else(
            || (0, Vec::new(), FfiType::Void),
            |sig| {
                let arity = u8::try_from(sig.params.len()).expect("too many foreign params (>255)");
                let pts: Vec<FfiType> = sig
                    .params
                    .iter()
                    .map(|p| self.param_to_ffi_type(p))
                    .collect();
                let ret = sig.ret_ty.map_or(FfiType::Void, |t| self.ty_to_ffi_type(t));
                (arity, pts, ret)
            },
        );

        let foreign_idx =
            u16::try_from(self.foreigns.len()).expect("too many foreign descriptors (>65535)");
        self.foreigns.push(ForeignDescriptor {
            name_idx,
            symbol_idx,
            lib_idx: lib_idx.unwrap_or(u32::MAX),
            abi,
            arity,
            exported: binding.modifiers.exported,
            param_types,
            return_type,
        });
        let _prev = self.foreign_globals.insert(name, foreign_idx);
    }

    fn resolve_foreign_callee(&self, callee: ExprId) -> Option<u16> {
        let kind = &self.typed_module.db.ast.exprs.get(callee).kind;
        if let ExprKind::Var(ident) = kind {
            return self.foreign_globals.get(&ident.name).copied();
        }
        None
    }

    fn ty_to_ffi_type(&self, ty_id: TyId) -> FfiType {
        let ty_kind = &self.typed_module.db.ast.types.get(ty_id).kind;
        if let TyKind::Named { name, .. } = ty_kind {
            let resolved = self.typed_module.db.interner.resolve(name.name);
            match resolved {
                "Int" | "Int8" | "Int16" | "Int32" | "Int64" | "Nat" | "Nat8" | "Nat16"
                | "Nat32" | "Nat64" | "Rune" => FfiType::Int,
                "Float" | "Float32" | "Float64" => FfiType::Float,
                "Bool" => FfiType::Bool,
                "Unit" => FfiType::Void,
                "String" | "CString" => FfiType::Str,
                // CPtr and unknown types default to Ptr
                _ => FfiType::Ptr,
            }
        } else {
            FfiType::Ptr
        }
    }

    fn param_to_ffi_type(&self, param: &Param) -> FfiType {
        param
            .ty
            .map_or(FfiType::Ptr, |ty_id| self.ty_to_ffi_type(ty_id))
    }

    fn extract_link_attr(&mut self, attrs: &[AttrId]) -> (Option<u32>, Option<u32>) {
        for &attr_id in attrs {
            let attr = self.typed_module.db.ast.attrs.get(attr_id);
            if !attr_path_matches(&self.typed_module.db, &attr.kind, LINK_ATTR.path) {
                continue;
            }
            let Ok(bound) = bind_attr(&self.typed_module.db, &attr.kind, &LINK_ATTR) else {
                continue;
            };
            let lib_idx = bound.get("name").and_then(|expr_id| {
                attr_expr_string(&self.typed_module.db, expr_id)
                    .map(|value| u32::from(self.pool.add(ConstantEntry::Str(value))))
            });
            let symbol_idx = bound.get("symbol").and_then(|expr_id| {
                attr_expr_string(&self.typed_module.db, expr_id)
                    .map(|value| u32::from(self.pool.add(ConstantEntry::Str(value))))
            });
            if lib_idx.is_some() || symbol_idx.is_some() {
                return (lib_idx, symbol_idx);
            }
        }
        (None, None)
    }

    fn push(&mut self, instruction: Instruction) {
        self.current_instructions.push(instruction);
    }

    fn push_absolute_global_load(&mut self, index: u16) {
        let position = self.current_instructions.len();
        self.current_absolute_global_loads.push(position);
        self.push(Instruction::with_u16(Opcode::LdGlob, index));
    }

    fn local_slot(&mut self, name: Symbol) -> u16 {
        if let Some(slot) = self.find_local(name) {
            return slot;
        }
        let slot = u16::try_from(self.current_locals.len()).expect("too many locals (>65535)");
        self.current_locals.push(name);
        slot
    }

    fn find_local(&self, name: Symbol) -> Option<u16> {
        self.current_locals
            .iter()
            .position(|&s| s == name)
            .and_then(|p| u16::try_from(p).ok())
    }

    fn emit_ld_loc(&mut self, slot: u16) {
        if let Ok(s) = u8::try_from(slot) {
            self.push(Instruction::with_u8(Opcode::LdLoc, s));
        } else {
            self.push(Instruction::with_u16(Opcode::LdLocW, slot));
        }
    }

    fn emit_st_loc(&mut self, slot: u16) {
        if let Ok(s) = u8::try_from(slot) {
            self.push(Instruction::with_u8(Opcode::StLoc, s));
        } else {
            self.push(Instruction::with_u16(Opcode::StLocW, slot));
        }
    }

    fn is_spread_expr(&self, expr_id: ExprId) -> bool {
        matches!(
            self.typed_module.db.ast.exprs.get(expr_id).kind,
            ExprKind::UnaryOp(UnaryOp::Spread, _)
        )
    }

    fn spread_expr_operand(&self, expr_id: ExprId) -> Option<ExprId> {
        match self.typed_module.db.ast.exprs.get(expr_id).kind {
            ExprKind::UnaryOp(UnaryOp::Spread, inner) => Some(inner),
            _ => None,
        }
    }

    fn is_len_access(&self, expr_id: ExprId, field: Symbol) -> bool {
        if self.typed_module.db.interner.resolve(field) != "len" {
            return false;
        }

        let Some(expr_ty) = self.typed_module.expr_type(expr_id) else {
            return true;
        };
        matches!(
            self.typed_module
                .type_env
                .types
                .get(self.typed_module.type_env.resolve_var(expr_ty)),
            Ty::Array(_) | Ty::Builtin(BuiltinType::String) | Ty::Any | Ty::Unknown | Ty::Var(_)
        )
    }

    fn builtin_bool_variant_opcode(&self, tag: Symbol) -> Option<Opcode> {
        match self.typed_module.db.interner.resolve(tag) {
            "True" => Some(Opcode::LdTru),
            "False" => Some(Opcode::LdFls),
            _ => None,
        }
    }

    /// Emits `GcPin` for each pointer-type argument before an FFI call.
    ///
    /// Returns the number of pins emitted so the caller can emit matching unpins.
    fn emit_ffi_pins(&mut self, foreign_idx: u16) -> usize {
        let idx = usize::from(foreign_idx);
        if idx >= self.foreigns.len() {
            return 0;
        }
        let param_types = self.foreigns[idx].param_types.clone();
        let mut pin_count = 0usize;
        for pt in &param_types {
            if matches!(pt, FfiType::Str | FfiType::Ptr) {
                self.push(Instruction::simple(Opcode::GcPin));
                pin_count += 1;
            }
        }
        pin_count
    }

    fn push_arr_new(&mut self, type_id: u16, len: u16) {
        self.push(Instruction::with_type_len(Opcode::ArrNew, type_id, len));
    }

    fn push_arr_new_tagged(&mut self, type_id: u16, tag: u8, len: u16) {
        self.push(Instruction::with_type_tagged(
            Opcode::ArrNewT,
            type_id,
            tag,
            len,
        ));
    }

    fn type_to_table_id(&mut self, ty_id: TyId) -> u16 {
        let Some(type_key) = self.ast_type_key(ty_id) else {
            return format::BUILTIN_TYPE_ANY;
        };
        self.register_type_key(&type_key)
    }

    fn resolve_field_index(&self, base_expr: ExprId, field_name: Symbol) -> Option<u8> {
        let ty_id = self.typed_module.expr_type(base_expr)?;
        let resolved = self.typed_module.type_env.resolve_var(ty_id);
        let ty = self.typed_module.type_env.types.get(resolved);
        if let Ty::Record { fields } = ty {
            for (i, &(name, _)) in fields.iter().enumerate() {
                if name == field_name {
                    return u8::try_from(i).ok();
                }
            }
        }
        None
    }

    fn find_upvalue(&self, name: Symbol) -> Option<u8> {
        self.current_upvalues
            .iter()
            .position(|&s| s == name)
            .and_then(|p| u8::try_from(p).ok())
    }

    fn find_global(&self, name: Symbol) -> Option<u16> {
        self.globals
            .iter()
            .position(|g| g.name == name)
            .and_then(|p| u16::try_from(p).ok())
    }

    fn emit_quote(&mut self, qk: &QuoteKind) -> EmitResult {
        match qk {
            QuoteKind::Expr(e) => self.emit_expr(*e)?,
            QuoteKind::Block(stmts) => self.emit_seq(stmts)?,
        }
        Ok(())
    }

    fn emit_splice(&mut self, sk: &SpliceKind) -> EmitResult {
        match sk {
            SpliceKind::Ident(_) => return Err(EmitError::Unimplemented("splice-by-name")),
            SpliceKind::Expr(e) => self.emit_expr(*e)?,
            SpliceKind::Array(es) => {
                let len = u16::try_from(es.len()).expect("too many splice elements (>65535)");
                self.push_arr_new(BuiltinType::Array.type_id(), len);
                for (i, &e) in es.iter().enumerate() {
                    self.emit_expr(e)?;
                    let idx = u8::try_from(i).expect("too many splice elements (>255)");
                    self.push(Instruction::with_u8(Opcode::ArrSetI, idx));
                }
            }
        }
        Ok(())
    }
}

const fn resolve_visibility(binding: &LetBinding) -> Visibility {
    if binding.modifiers.opaque {
        Visibility::Opaque
    } else if binding.modifiers.exported {
        Visibility::Exported
    } else {
        Visibility::Private
    }
}

/// Maps a binary operator to `(int_opcode, float_opcode)` for dynamic dispatch.
///
/// Returns `None` for operators that have no float variant (logic, cons, etc.).
const fn dynamic_binop_opcodes(op: BinOp) -> Option<(Opcode, Opcode)> {
    match op {
        BinOp::Add => Some((Opcode::IAdd, Opcode::FAdd)),
        BinOp::Sub => Some((Opcode::ISub, Opcode::FSub)),
        BinOp::Shl => Some((Opcode::Shl, Opcode::Shl)),
        BinOp::Shr => Some((Opcode::Shr, Opcode::Shr)),
        BinOp::Mul => Some((Opcode::IMul, Opcode::FMul)),
        BinOp::Div => Some((Opcode::IDiv, Opcode::FDiv)),
        BinOp::Rem => Some((Opcode::IRem, Opcode::IRem)),
        BinOp::Eq => Some((Opcode::CmpEq, Opcode::CmpEq)),
        BinOp::NotEq => Some((Opcode::CmpNeq, Opcode::CmpNeq)),
        BinOp::Lt => Some((Opcode::CmpLt, Opcode::CmpLt)),
        BinOp::Gt => Some((Opcode::CmpGt, Opcode::CmpGt)),
        BinOp::LtEq => Some((Opcode::CmpLeq, Opcode::CmpLeq)),
        BinOp::GtEq => Some((Opcode::CmpGeq, Opcode::CmpGeq)),
        BinOp::And
        | BinOp::Or
        | BinOp::Xor
        | BinOp::Cons
        | BinOp::NilCoalesce
        | BinOp::PipeRight
        | BinOp::Range
        | BinOp::RangeExcl => None,
    }
}

#[expect(
    clippy::panic,
    reason = "NilCoalesce/PipeRight/Range/RangeExcl are intercepted by callers — reaching this arm is a compiler ICE"
)]
fn binop_to_opcode(op: BinOp) -> Opcode {
    match op {
        BinOp::Add => Opcode::IAdd,
        BinOp::Sub => Opcode::ISub,
        BinOp::Shl => Opcode::Shl,
        BinOp::Shr => Opcode::Shr,
        BinOp::Mul => Opcode::IMul,
        BinOp::Div => Opcode::IDiv,
        BinOp::Rem => Opcode::IRem,
        BinOp::Eq => Opcode::CmpEq,
        BinOp::NotEq => Opcode::CmpNeq,
        BinOp::Lt => Opcode::CmpLt,
        BinOp::Gt => Opcode::CmpGt,
        BinOp::LtEq => Opcode::CmpLeq,
        BinOp::GtEq => Opcode::CmpGeq,
        BinOp::And => Opcode::And,
        BinOp::Or => Opcode::Or,
        BinOp::Xor => Opcode::Xor,
        BinOp::Cons => Opcode::ArrCaten,
        BinOp::NilCoalesce | BinOp::PipeRight | BinOp::Range | BinOp::RangeExcl => {
            panic!("intercepted before reaching binop_to_opcode")
        }
    }
}
