use std::collections::HashMap;
use std::mem;
use std::path::Path;

use music_basic::{SourceId, SourceMap, Span, path as import_path, string_lit};
use music_hir::{
    HirArg, HirArrayItem, HirBinaryOp, HirChainKind, HirExprId, HirExprKind, HirLit, HirLitKind,
    HirMemberKey, HirParam, HirPatKind, HirPrefixOp, HirRecordItem, HirStore, HirStringLit,
};
use music_il::{
    ConstantEntry, ConstantPool, Instruction, MethodEntry, MethodName, Opcode, Operand,
    TypeDescriptor,
};
use music_ir::{IrExprTy, IrModuleInfo, IrScalarTy, IrTypeRef};
use music_names::{Ident, Interner, NameBindingId, NameResolution, NameSite, Symbol, SymbolSlice};

use crate::errors::{EmitError, EmitErrorKind, EmitResult};

use super::number::{parse_float, parse_int};
use super::types::{
    builtin_type_id_for_ref, ensure_choice_type, ensure_record_type, ensure_tuple_type,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RecordFieldValue {
    Expr(HirExprId),
    Shorthand(Ident),
}

pub(super) struct MethodEmitter<'a> {
    interner: &'a Interner,
    sources: &'a SourceMap,
    source_id: SourceId,
    store: &'a HirStore,
    names: &'a NameResolution,
    ir: &'a IrModuleInfo,
    binding_by_site: HashMap<NameSite, NameBindingId>,
    globals_by_binding: &'a HashMap<NameBindingId, u16>,
    import_globals_by_binding: &'a HashMap<NameBindingId, u16>,
    module_export_globals: &'a HashMap<(String, Symbol), u16>,
    constants: &'a mut ConstantPool,
    methods: &'a mut Vec<MethodEntry>,
    types: &'a mut Vec<TypeDescriptor>,

    pub(super) instructions: Vec<Instruction>,
    locals: HashMap<NameBindingId, u16>,
    next_local_slot: u16,
}

impl<'a> MethodEmitter<'a> {
    pub(super) fn new(
        interner: &'a Interner,
        sources: &'a SourceMap,
        source_id: SourceId,
        store: &'a HirStore,
        names: &'a NameResolution,
        ir: &'a IrModuleInfo,
        globals_by_binding: &'a HashMap<NameBindingId, u16>,
        import_globals_by_binding: &'a HashMap<NameBindingId, u16>,
        module_export_globals: &'a HashMap<(String, Symbol), u16>,
        constants: &'a mut ConstantPool,
        methods: &'a mut Vec<MethodEntry>,
        types: &'a mut Vec<TypeDescriptor>,
    ) -> Self {
        let mut binding_by_site = HashMap::with_capacity(names.bindings.len());
        for (id, binding) in &names.bindings {
            let _prev = binding_by_site.insert(binding.site, id);
        }
        Self {
            interner,
            sources,
            source_id,
            store,
            names,
            ir,
            binding_by_site,
            globals_by_binding,
            import_globals_by_binding,
            module_export_globals,
            constants,
            methods,
            types,
            instructions: vec![],
            locals: HashMap::new(),
            next_local_slot: 0,
        }
    }

    pub(super) fn locals_count(&self) -> usize {
        usize::from(self.next_local_slot)
    }

    pub(super) fn finish_method(&mut self, name: MethodName, locals_count: u16) -> usize {
        let index = self.methods.len();
        self.methods.push(MethodEntry {
            name,
            instructions: mem::take(&mut self.instructions),
            locals_count,
            absolute_global_loads: vec![],
        });
        index
    }

    pub(super) fn bind_params(&mut self, params: &[HirParam]) {
        for p in params.iter() {
            let site = NameSite::new(self.source_id, p.name.span);
            let binding = self.binding_by_site.get(&site).copied();
            let Some(binding) = binding else {
                continue;
            };
            let slot = self.alloc_local_slot();
            let _prev = self.locals.insert(binding, slot);
        }
    }

    pub(super) fn emit_expr(&mut self, expr_id: HirExprId) -> EmitResult<()> {
        let expr = self.store.exprs.get(expr_id).clone();
        match expr.kind {
            HirExprKind::Lit { lit } => {
                self.emit_lit(lit)?;
            }
            HirExprKind::Name { ident } => {
                self.emit_name(expr.origin.span, ident)?;
            }
            HirExprKind::Record { items } => {
                self.emit_record(expr_id, items.as_ref())?;
            }
            HirExprKind::Variant { name, payload } => {
                self.emit_variant(expr_id, name, payload)?;
            }
            HirExprKind::Tuple { items } => {
                self.emit_tuple(items.as_ref())?;
            }
            HirExprKind::Array { items } => {
                self.emit_array(expr_id, items.as_ref())?;
            }
            HirExprKind::Sequence { exprs, yields_unit } => {
                for id in exprs.iter().copied() {
                    self.emit_expr(id)?;
                    self.instructions.push(Instruction::basic(Opcode::Pop));
                }
                if yields_unit {
                    self.instructions.push(Instruction::basic(Opcode::LdUnit));
                }
            }
            HirExprKind::Binary { op, left, right } => {
                if op == HirBinaryOp::Assign {
                    self.emit_assign(left, right)?;
                } else {
                    self.emit_expr(left)?;
                    self.emit_expr(right)?;
                    self.emit_binop(expr_id, op, left, right)?;
                }
            }
            HirExprKind::Call { callee, args } => {
                self.emit_expr(callee)?;
                let mut arity = 0u8;
                for arg in args.iter() {
                    match arg {
                        HirArg::Expr(id) => {
                            self.emit_expr(*id)?;
                            arity = arity.saturating_add(1);
                        }
                        HirArg::Spread { .. } => {
                            return Err(EmitError {
                                kind: EmitErrorKind::UnsupportedExpr,
                            });
                        }
                    }
                }
                self.instructions
                    .push(Instruction::with_u8(Opcode::Call, arity));
            }
            HirExprKind::Lambda { params, body, .. } => {
                let method_idx = self.emit_lambda_method(params.as_ref(), body)?;
                self.instructions.push(Instruction::with_wide(
                    Opcode::ClsNew,
                    u16::try_from(method_idx).unwrap_or(u16::MAX),
                    0,
                ));
            }
            HirExprKind::Let {
                has_params,
                params,
                pat,
                value,
                ..
            } => {
                // Expression `let` is compiled as local store after evaluating rhs.
                let pat = self.store.pats.get(pat).clone();
                let HirPatKind::Bind { name, .. } = pat.kind else {
                    return Err(EmitError {
                        kind: EmitErrorKind::UnsupportedPat,
                    });
                };

                let binding = self
                    .binding_by_site
                    .get(&NameSite::new(self.source_id, name.span))
                    .copied();

                let Some(binding) = binding else {
                    self.instructions.push(Instruction::basic(Opcode::LdUnit));
                    return Ok(());
                };

                if has_params {
                    let Some(body) = value else {
                        self.instructions.push(Instruction::basic(Opcode::LdUnit));
                        return Ok(());
                    };
                    let method_idx = self.emit_lambda_method(params.as_ref(), body)?;
                    self.instructions.push(Instruction::with_wide(
                        Opcode::ClsNew,
                        u16::try_from(method_idx).unwrap_or(u16::MAX),
                        0,
                    ));
                } else if let Some(value) = value {
                    self.emit_expr(value)?;
                } else {
                    self.instructions.push(Instruction::basic(Opcode::LdUnit));
                }

                let slot = self.alloc_local_slot();
                let _prev = self.locals.insert(binding, slot);
                self.emit_st_loc(slot);
                self.instructions.push(Instruction::basic(Opcode::LdUnit));
            }
            HirExprKind::Import { path, .. } => {
                self.emit_import(expr_id, path)?;
            }
            HirExprKind::Member { base, chain, key } => {
                self.emit_member(expr_id, base, chain, key)?;
            }
            HirExprKind::Index { base, indices } => {
                self.emit_index(expr_id, base, indices.as_ref())?;
            }
            HirExprKind::Prefix { op, expr } => {
                self.emit_prefix(expr_id, op, expr)?;
            }
            HirExprKind::RecordUpdate { base, items } => {
                self.emit_record_update(expr_id, base, items.as_ref())?;
            }
            HirExprKind::TypeTest { .. }
            | HirExprKind::TypeCast { .. }
            | HirExprKind::Case { .. }
            | HirExprKind::Perform { .. }
            | HirExprKind::Handle { .. }
            | HirExprKind::Resume { .. }
            | HirExprKind::ForeignBlock { .. }
            | HirExprKind::Data { .. }
            | HirExprKind::Effect { .. }
            | HirExprKind::Class { .. }
            | HirExprKind::Instance { .. }
            | HirExprKind::Quote { .. }
            | HirExprKind::Splice { .. }
            | HirExprKind::Error => {
                return Err(EmitError {
                    kind: EmitErrorKind::UnsupportedExpr,
                });
            }
        }
        Ok(())
    }

    fn emit_lambda_method(&mut self, params: &[HirParam], body: HirExprId) -> EmitResult<usize> {
        let mut sub = MethodEmitter::new(
            self.interner,
            self.sources,
            self.source_id,
            self.store,
            self.names,
            self.ir,
            self.globals_by_binding,
            self.import_globals_by_binding,
            self.module_export_globals,
            self.constants,
            self.methods,
            self.types,
        );
        sub.bind_params(params);
        sub.emit_expr(body)?;
        sub.instructions.push(Instruction::basic(Opcode::Ret));

        let locals_count = u16::try_from(sub.locals_count()).unwrap_or(u16::MAX);
        Ok(sub.finish_method(MethodName::Anonymous, locals_count))
    }

    fn alloc_local_slot(&mut self) -> u16 {
        let slot = self.next_local_slot;
        self.next_local_slot = self.next_local_slot.saturating_add(1);
        slot
    }

    fn emit_lit(&mut self, lit: HirLit) -> EmitResult {
        match lit.kind {
            HirLitKind::Int { span, .. } => {
                let text = self.slice(span);
                let value = parse_int(text).unwrap_or(0);
                if let Ok(small) = i16::try_from(value) {
                    self.instructions
                        .push(Instruction::with_i16(Opcode::LdSmi, small));
                } else {
                    let idx = self.constants.add(ConstantEntry::Int(value));
                    self.instructions
                        .push(Instruction::with_u16(Opcode::LdConst, idx));
                }
            }
            HirLitKind::Float { span, .. } => {
                let text = self.slice(span);
                let value = parse_float(text).unwrap_or(0.0);
                let idx = self.constants.add(ConstantEntry::Float(value.to_bits()));
                self.instructions
                    .push(Instruction::with_u16(Opcode::LdConst, idx));
            }
            HirLitKind::Rune { span, .. } => {
                let text = self.slice(span);
                let decoded = string_lit::decode(text);
                let ch = decoded.chars().next().unwrap_or('\0');
                let value = i64::from(u32::from(ch));
                let idx = self.constants.add(ConstantEntry::Int(value));
                self.instructions
                    .push(Instruction::with_u16(Opcode::LdConst, idx));
            }
            HirLitKind::String(s) => {
                let text = self.slice(s.span);
                let value = string_lit::decode(text);
                let idx = self.constants.add(ConstantEntry::Str(value));
                self.instructions
                    .push(Instruction::with_u16(Opcode::LdConst, idx));
            }
            HirLitKind::FString { .. } => {
                return Err(EmitError {
                    kind: EmitErrorKind::UnsupportedExpr,
                });
            }
        }
        Ok(())
    }

    fn emit_name(&mut self, span: Span, ident: Ident) -> EmitResult<()> {
        let site = NameSite::new(self.source_id, span);
        let Some(binding) = self.names.refs.get(&site).copied() else {
            self.instructions.push(Instruction::basic(Opcode::LdUnit));
            return Ok(());
        };

        if let Some(slot) = self.locals.get(&binding).copied() {
            self.emit_ld_loc(slot);
            return Ok(());
        }

        if self.import_globals_by_binding.contains_key(&binding) {
            let idx = self
                .import_globals_by_binding
                .get(&binding)
                .copied()
                .unwrap_or(u16::MAX);
            if idx == u16::MAX {
                return Err(EmitError {
                    kind: EmitErrorKind::ImportTargetMissing,
                });
            }
            self.instructions
                .push(Instruction::with_u16(Opcode::LdGlob, idx));
            return Ok(());
        }

        let idx = self
            .globals_by_binding
            .get(&binding)
            .copied()
            .unwrap_or(u16::MAX);
        if idx == u16::MAX {
            self.instructions.push(Instruction::basic(Opcode::LdUnit));
            return Ok(());
        }
        self.instructions
            .push(Instruction::with_u16(Opcode::LdGlob, idx));
        let _ = ident;
        Ok(())
    }

    fn emit_tuple(&mut self, items: &[HirExprId]) -> EmitResult<()> {
        // Encode tuples as record aggregates with tag=0 and N fields.
        self.emit_tag(0)?;
        for id in items.iter().copied() {
            self.emit_expr(id)?;
        }
        let type_id = ensure_tuple_type(self.types, items.len());
        self.instructions.push(Instruction {
            opcode: Opcode::DataNew,
            operand: Operand::TypeLen(type_id, u16::try_from(items.len()).unwrap_or(0)),
        });
        Ok(())
    }

    fn emit_record(&mut self, expr_id: HirExprId, items: &[HirRecordItem]) -> EmitResult<()> {
        let (layout_fields, type_id) = self.record_layout_and_type(expr_id)?;

        let mut values = HashMap::<Symbol, RecordFieldValue>::new();
        for item in items {
            match item {
                HirRecordItem::Field { name, value, .. } => {
                    let entry = match value {
                        Some(expr) => RecordFieldValue::Expr(*expr),
                        None => RecordFieldValue::Shorthand(*name),
                    };
                    let _prev = values.insert(name.name, entry);
                }
                HirRecordItem::Spread { .. } => {
                    return Err(EmitError {
                        kind: EmitErrorKind::UnsupportedExpr,
                    });
                }
            }
        }

        self.emit_tag(0)?;
        for field in layout_fields.iter().copied() {
            let Some(value) = values.get(&field).copied() else {
                return Err(EmitError {
                    kind: EmitErrorKind::UnsupportedExpr,
                });
            };
            match value {
                RecordFieldValue::Expr(id) => self.emit_expr(id)?,
                RecordFieldValue::Shorthand(ident) => self.emit_name(ident.span, ident)?,
            }
        }
        self.instructions.push(Instruction::with_type_len(
            Opcode::DataNew,
            type_id,
            u16::try_from(layout_fields.len()).unwrap_or(0),
        ));
        Ok(())
    }

    fn emit_import(&mut self, expr_id: HirExprId, path: HirStringLit) -> EmitResult<()> {
        let (layout_fields, type_id) = self.record_layout_and_type(expr_id)?;

        let raw = string_lit::decode(self.slice(path.span));
        let import_path = if raw.starts_with('@') {
            raw
        } else if raw.starts_with('.') || Path::new(raw.as_str()).is_absolute() {
            let from_path = self
                .sources
                .get(self.source_id)
                .map(|s| s.path())
                .unwrap_or_else(|| Path::new(""));
            import_path::resolve_import_path(from_path, raw.as_str())
                .to_string_lossy()
                .into_owned()
        } else {
            raw
        };

        self.emit_tag(0)?;
        for field in layout_fields.iter().copied() {
            let idx = self
                .module_export_globals
                .get(&(import_path.clone(), field))
                .copied()
                .ok_or(EmitError {
                    kind: EmitErrorKind::ImportTargetMissing,
                })?;
            self.instructions
                .push(Instruction::with_u16(Opcode::LdGlob, idx));
        }
        self.instructions.push(Instruction::with_type_len(
            Opcode::DataNew,
            type_id,
            u16::try_from(layout_fields.len()).unwrap_or(0),
        ));
        Ok(())
    }

    fn emit_record_update(
        &mut self,
        expr_id: HirExprId,
        base: HirExprId,
        items: &[HirRecordItem],
    ) -> EmitResult {
        let (layout_fields, type_id) = self.record_layout_and_type(expr_id)?;

        let mut updates = HashMap::<Symbol, RecordFieldValue>::new();
        for item in items {
            match item {
                HirRecordItem::Field { name, value, .. } => {
                    let entry = match value {
                        Some(expr) => RecordFieldValue::Expr(*expr),
                        None => RecordFieldValue::Shorthand(*name),
                    };
                    let _prev = updates.insert(name.name, entry);
                }
                HirRecordItem::Spread { .. } => {
                    return Err(EmitError {
                        kind: EmitErrorKind::UnsupportedExpr,
                    });
                }
            }
        }

        // Evaluate base once; use a temp local for repeated field loads.
        self.emit_expr(base)?;
        let base_slot = self.alloc_local_slot();
        self.emit_st_loc(base_slot);

        self.emit_tag(0)?;
        for (i, field) in layout_fields.iter().copied().enumerate() {
            if let Some(value) = updates.get(&field).copied() {
                match value {
                    RecordFieldValue::Expr(id) => self.emit_expr(id)?,
                    RecordFieldValue::Shorthand(ident) => self.emit_name(ident.span, ident)?,
                }
                continue;
            }

            self.emit_ld_loc(base_slot);
            let idx = u16::try_from(i).unwrap_or(u16::MAX);
            self.instructions
                .push(Instruction::with_u16(Opcode::DataGet, idx));
        }

        self.instructions.push(Instruction::with_type_len(
            Opcode::DataNew,
            type_id,
            u16::try_from(layout_fields.len()).unwrap_or(0),
        ));
        Ok(())
    }

    fn emit_variant(
        &mut self,
        expr_id: HirExprId,
        name: Ident,
        payload: Option<HirExprId>,
    ) -> EmitResult<()> {
        let ty = self.ir_expr_ty(expr_id);
        let IrExprTy::Named(owner) = ty else {
            return Err(EmitError {
                kind: EmitErrorKind::UnsupportedExpr,
            });
        };
        let Some(layout) = self.ir.data_layouts.get(&owner) else {
            return Err(EmitError {
                kind: EmitErrorKind::UnsupportedExpr,
            });
        };
        let Some(tag) = layout.choice_variant_tag(name.name) else {
            return Err(EmitError {
                kind: EmitErrorKind::UnsupportedExpr,
            });
        };
        let Some(variants) = layout.choice_variants.as_ref() else {
            return Err(EmitError {
                kind: EmitErrorKind::UnsupportedExpr,
            });
        };

        let key = format!("choice({})", self.interner.resolve(owner));
        let type_id = ensure_choice_type(self.types, key, variants.len());

        self.emit_tag(tag)?;
        let mut field_count = 0u16;
        if let Some(payload) = payload {
            self.emit_expr(payload)?;
            field_count = 1;
        }
        self.instructions.push(Instruction::with_type_len(
            Opcode::DataNew,
            type_id,
            field_count,
        ));
        Ok(())
    }

    fn emit_array(&mut self, expr_id: HirExprId, items: &[HirArrayItem]) -> EmitResult {
        let elem = match self.ir_expr_ty(expr_id) {
            IrExprTy::Array { elem } => elem,
            _ => IrTypeRef::Unknown,
        };
        let elem_type_id = self.type_id_for_ref(elem);

        let len = u16::try_from(items.len()).unwrap_or(u16::MAX);
        self.instructions.push(Instruction::with_type_len(
            Opcode::SeqNew,
            elem_type_id,
            len,
        ));

        for (i, item) in items.iter().enumerate() {
            let HirArrayItem::Expr(value) = item else {
                return Err(EmitError {
                    kind: EmitErrorKind::UnsupportedExpr,
                });
            };
            self.instructions.push(Instruction::basic(Opcode::Dup));
            self.emit_small_int(i as i64);
            self.emit_expr(*value)?;
            self.instructions.push(Instruction::basic(Opcode::SeqSet));
            self.instructions.push(Instruction::basic(Opcode::Pop));
        }

        Ok(())
    }

    fn emit_member(
        &mut self,
        _expr_id: HirExprId,
        base: HirExprId,
        chain: HirChainKind,
        key: HirMemberKey,
    ) -> EmitResult<()> {
        if chain != HirChainKind::Normal {
            return Err(EmitError {
                kind: EmitErrorKind::UnsupportedExpr,
            });
        }

        let idx = self.member_index(base, key)?;
        self.emit_expr(base)?;
        self.instructions
            .push(Instruction::with_u16(Opcode::DataGet, idx));
        Ok(())
    }

    fn emit_index(
        &mut self,
        _expr_id: HirExprId,
        base: HirExprId,
        indices: &[HirExprId],
    ) -> EmitResult<()> {
        let Some(&index) = indices.first() else {
            return Err(EmitError {
                kind: EmitErrorKind::UnsupportedExpr,
            });
        };
        if indices.len() != 1 {
            return Err(EmitError {
                kind: EmitErrorKind::UnsupportedExpr,
            });
        }
        self.emit_expr(base)?;
        self.emit_expr(index)?;
        self.instructions.push(Instruction::basic(Opcode::SeqGet));
        Ok(())
    }

    fn emit_assign(&mut self, left: HirExprId, right: HirExprId) -> EmitResult<()> {
        let left_expr = self.store.exprs.get(left).clone();
        match left_expr.kind {
            HirExprKind::Name { ident } => {
                self.emit_expr(right)?;
                self.emit_store_name(ident.span)?;
                self.instructions.push(Instruction::basic(Opcode::LdUnit));
                Ok(())
            }
            HirExprKind::Member { base, chain, key } => {
                if chain != HirChainKind::Normal {
                    return Err(EmitError {
                        kind: EmitErrorKind::UnsupportedExpr,
                    });
                }
                let idx = self.member_index(base, key)?;
                self.emit_expr(base)?;
                self.emit_expr(right)?;
                self.instructions
                    .push(Instruction::with_u16(Opcode::DataSet, idx));
                self.instructions.push(Instruction::basic(Opcode::LdUnit));
                Ok(())
            }
            HirExprKind::Index { base, indices } => {
                if indices.len() != 1 {
                    return Err(EmitError {
                        kind: EmitErrorKind::UnsupportedExpr,
                    });
                }
                let index = indices[0];
                self.emit_expr(base)?;
                self.emit_expr(index)?;
                self.emit_expr(right)?;
                self.instructions.push(Instruction::basic(Opcode::SeqSet));
                self.instructions.push(Instruction::basic(Opcode::LdUnit));
                Ok(())
            }
            _ => Err(EmitError {
                kind: EmitErrorKind::UnsupportedExpr,
            }),
        }
    }

    fn emit_prefix(
        &mut self,
        expr_id: HirExprId,
        op: HirPrefixOp,
        expr: HirExprId,
    ) -> EmitResult<()> {
        match op {
            HirPrefixOp::Mut => {
                self.emit_expr(expr)?;
                Ok(())
            }
            HirPrefixOp::Not => {
                self.emit_expr(expr)?;
                self.instructions.push(Instruction::basic(Opcode::Not));
                Ok(())
            }
            HirPrefixOp::Negate => {
                self.emit_expr(expr)?;
                let opcode = if self.is_float_expr(expr_id) || self.is_float_expr(expr) {
                    Opcode::FNeg
                } else {
                    Opcode::INeg
                };
                self.instructions.push(Instruction::basic(opcode));
                Ok(())
            }
        }
    }

    fn emit_store_name(&mut self, span: Span) -> EmitResult<()> {
        let site = NameSite::new(self.source_id, span);
        let Some(binding) = self.names.refs.get(&site).copied() else {
            self.instructions.push(Instruction::basic(Opcode::Pop));
            return Ok(());
        };

        if let Some(slot) = self.locals.get(&binding).copied() {
            self.emit_st_loc(slot);
            return Ok(());
        }

        if self.import_globals_by_binding.contains_key(&binding) {
            return Err(EmitError {
                kind: EmitErrorKind::UnsupportedExpr,
            });
        }

        let idx = self
            .globals_by_binding
            .get(&binding)
            .copied()
            .unwrap_or(u16::MAX);
        if idx == u16::MAX {
            self.instructions.push(Instruction::basic(Opcode::Pop));
            return Ok(());
        }
        self.instructions
            .push(Instruction::with_u16(Opcode::StGlob, idx));
        Ok(())
    }

    fn member_index(&self, base: HirExprId, key: HirMemberKey) -> EmitResult<u16> {
        match key {
            HirMemberKey::IntLit { span, .. } => {
                let text = self.slice(span);
                let value = parse_int(text).unwrap_or(0);
                u16::try_from(value).map_err(|_| EmitError {
                    kind: EmitErrorKind::UnsupportedExpr,
                })
            }
            HirMemberKey::Name(ident) => {
                let base_ty = self.ir_expr_ty(base);
                match base_ty {
                    IrExprTy::Named(owner) => {
                        let Some(layout) = self.ir.data_layouts.get(&owner) else {
                            return Err(EmitError {
                                kind: EmitErrorKind::UnsupportedExpr,
                            });
                        };
                        layout.record_field_index(ident.name).ok_or(EmitError {
                            kind: EmitErrorKind::UnsupportedExpr,
                        })
                    }
                    IrExprTy::Record { fields } => fields
                        .iter()
                        .position(|&sym| sym == ident.name)
                        .and_then(|i| u16::try_from(i).ok())
                        .ok_or(EmitError {
                            kind: EmitErrorKind::UnsupportedExpr,
                        }),
                    _ => Err(EmitError {
                        kind: EmitErrorKind::UnsupportedExpr,
                    }),
                }
            }
        }
    }

    fn record_layout_and_type(&mut self, expr_id: HirExprId) -> EmitResult<(SymbolSlice, u16)> {
        match self.ir_expr_ty(expr_id) {
            IrExprTy::Named(owner) => {
                let Some(layout) = self.ir.data_layouts.get(&owner) else {
                    return Err(EmitError {
                        kind: EmitErrorKind::UnsupportedExpr,
                    });
                };
                let Some(fields) = layout.record_fields.as_ref() else {
                    return Err(EmitError {
                        kind: EmitErrorKind::UnsupportedExpr,
                    });
                };
                let key = format!("record({})", self.interner.resolve(owner));
                let type_id = ensure_record_type(self.types, key, fields.len());
                Ok((fields.clone(), type_id))
            }
            IrExprTy::Record { fields } => {
                let key = format!(
                    "record({})",
                    fields
                        .iter()
                        .map(|&sym| self.interner.resolve(sym))
                        .collect::<Vec<_>>()
                        .join(",")
                );
                let type_id = ensure_record_type(self.types, key, fields.len());
                Ok((fields.clone(), type_id))
            }
            _ => Err(EmitError {
                kind: EmitErrorKind::UnsupportedExpr,
            }),
        }
    }

    fn type_id_for_ref(&mut self, ty: IrTypeRef) -> u16 {
        match ty {
            IrTypeRef::Named(sym) => self.ensure_named_type_id(sym),
            other => builtin_type_id_for_ref(other),
        }
    }

    fn ensure_named_type_id(&mut self, sym: Symbol) -> u16 {
        if let Some(layout) = self.ir.data_layouts.get(&sym) {
            if let Some(fields) = layout.record_fields.as_ref() {
                let key = format!("record({})", self.interner.resolve(sym));
                return ensure_record_type(self.types, key, fields.len());
            }
            if let Some(variants) = layout.choice_variants.as_ref() {
                let key = format!("choice({})", self.interner.resolve(sym));
                return ensure_choice_type(self.types, key, variants.len());
            }
        }
        builtin_type_id_for_ref(IrTypeRef::Unknown)
    }

    fn emit_small_int(&mut self, value: i64) {
        if let Ok(small) = i16::try_from(value) {
            self.instructions
                .push(Instruction::with_i16(Opcode::LdSmi, small));
        } else {
            let idx = self.constants.add(ConstantEntry::Int(value));
            self.instructions
                .push(Instruction::with_u16(Opcode::LdConst, idx));
        }
    }

    fn ir_expr_ty(&self, expr_id: HirExprId) -> IrExprTy {
        let idx = usize::try_from(expr_id.raw()).unwrap_or(usize::MAX);
        self.ir
            .expr_tys
            .get(idx)
            .cloned()
            .unwrap_or(IrExprTy::Unknown)
    }

    fn emit_binop(
        &mut self,
        expr_id: HirExprId,
        op: HirBinaryOp,
        left: HirExprId,
        right: HirExprId,
    ) -> EmitResult<()> {
        let opcode = match op {
            HirBinaryOp::Add => {
                self.emit_numeric_binop(expr_id, left, right, Opcode::IAdd, Opcode::FAdd)
            }
            HirBinaryOp::Sub => {
                self.emit_numeric_binop(expr_id, left, right, Opcode::ISub, Opcode::FSub)
            }
            HirBinaryOp::Mul => {
                self.emit_numeric_binop(expr_id, left, right, Opcode::IMul, Opcode::FMul)
            }
            HirBinaryOp::Div => {
                self.emit_numeric_binop(expr_id, left, right, Opcode::IDiv, Opcode::FDiv)
            }
            HirBinaryOp::Mod => Opcode::IRem,
            HirBinaryOp::Eq => Opcode::CmpEq,
            HirBinaryOp::NotEq => Opcode::CmpNeq,
            HirBinaryOp::Lt => Opcode::CmpLt,
            HirBinaryOp::Gt => Opcode::CmpGt,
            HirBinaryOp::LtEq => Opcode::CmpLeq,
            HirBinaryOp::GtEq => Opcode::CmpGeq,
            HirBinaryOp::And => Opcode::And,
            HirBinaryOp::Or => Opcode::Or,
            HirBinaryOp::Xor => Opcode::Xor,
            HirBinaryOp::Shl => Opcode::Shl,
            HirBinaryOp::Shr => Opcode::Shr,
            HirBinaryOp::Pipe | HirBinaryOp::Assign | HirBinaryOp::Symbolic(_) => {
                return Err(EmitError {
                    kind: EmitErrorKind::UnsupportedExpr,
                });
            }
        };
        self.instructions.push(Instruction::basic(opcode));
        Ok(())
    }

    fn emit_numeric_binop(
        &self,
        expr_id: HirExprId,
        left: HirExprId,
        right: HirExprId,
        int_opcode: Opcode,
        float_opcode: Opcode,
    ) -> Opcode {
        if self.is_float_expr(expr_id) || self.is_float_expr(left) || self.is_float_expr(right) {
            float_opcode
        } else {
            int_opcode
        }
    }

    fn is_float_expr(&self, expr_id: HirExprId) -> bool {
        let idx = usize::try_from(expr_id.raw()).unwrap_or(usize::MAX);
        let Some(ty) = self.ir.expr_tys.get(idx) else {
            return false;
        };
        matches!(ty, IrExprTy::Scalar(IrScalarTy::Float))
    }

    fn emit_tag(&mut self, tag: u16) -> EmitResult<()> {
        let idx = self.constants.add(ConstantEntry::Tag(tag));
        self.instructions
            .push(Instruction::with_u16(Opcode::LdConst, idx));
        Ok(())
    }

    fn emit_ld_loc(&mut self, slot: u16) {
        if slot <= u16::from(u8::MAX) {
            self.instructions
                .push(Instruction::with_u8(Opcode::LdLoc, slot as u8));
        } else {
            self.instructions
                .push(Instruction::with_u16(Opcode::LdLocW, slot));
        }
    }

    fn emit_st_loc(&mut self, slot: u16) {
        if slot <= u16::from(u8::MAX) {
            self.instructions
                .push(Instruction::with_u8(Opcode::StLoc, slot as u8));
        } else {
            self.instructions
                .push(Instruction::with_u16(Opcode::StLocW, slot));
        }
    }

    fn slice(&self, span: Span) -> &str {
        let Some(source) = self.sources.get(self.source_id) else {
            return "";
        };
        let start = usize::try_from(span.start).unwrap_or(0);
        let end = usize::try_from(span.end).unwrap_or(start);
        source.text().get(start..end).unwrap_or("")
    }
}
