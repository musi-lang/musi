use super::*;

#[expect(
    clippy::unwrap_in_result,
    reason = "capacity overflow `expect`s guard structural invariants (e.g. >65535 locals) — \
              these are compiler ICEs, not recoverable errors that belong in EmitError"
)]
impl Emitter<'_> {
    pub(super) fn collect_effects(&self) -> Vec<EffectDescriptor> {
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

    pub(super) fn collect_classes(&mut self) -> Vec<ClassDescriptor> {
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

    pub(super) fn collect_class_instances(
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

    pub(super) fn method_index_by_name(&self, name: Symbol) -> Option<u16> {
        self.methods
            .iter()
            .position(|method| method.name == Some(name))
            .and_then(|index| u16::try_from(index).ok())
    }

    pub(super) fn expr_runtime_type_id(&mut self, expr_id: ExprId, fallback: u16) -> u16 {
        let Some(expr_ty) = self.typed_module.expr_type(expr_id) else {
            return fallback;
        };
        let Some(type_key) = self.typed_module.type_env.type_key(expr_ty) else {
            return fallback;
        };
        self.register_type_key(&type_key)
    }

    pub(super) fn register_type_key(&mut self, type_key: &TypeKey) -> u16 {
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

    pub(super) fn type_descriptor_shape(&self, type_key: &TypeKey) -> (TypeKind, u16) {
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

    pub(super) fn type_key_string(&self, type_key: &TypeKey) -> String {
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

    pub(super) fn ast_type_key(&self, ty_id: TyId) -> Option<TypeKey> {
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

    pub(super) fn emit_runtime_type_id(&mut self, type_id: u16) {
        let const_idx = self.pool.add(ConstantEntry::Int(i64::from(type_id)));
        self.push(Instruction::with_u16(Opcode::LdConst, const_idx));
    }

    pub(super) fn type_to_table_id(&mut self, ty_id: TyId) -> u16 {
        let Some(type_key) = self.ast_type_key(ty_id) else {
            return format::BUILTIN_TYPE_ANY;
        };
        self.register_type_key(&type_key)
    }
}
