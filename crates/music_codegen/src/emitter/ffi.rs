use super::*;

#[expect(
    clippy::unwrap_in_result,
    reason = "capacity overflow `expect`s guard structural invariants (e.g. >65535 locals) — \
              these are compiler ICEs, not recoverable errors that belong in EmitError"
)]
impl Emitter<'_> {
    pub(super) fn emit_foreign_import(&mut self, sym: Symbol) {
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

    pub(super) fn emit_foreign_let(&mut self, name: Symbol, binding: &LetBinding) {
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

    pub(super) fn resolve_foreign_callee(&self, callee: ExprId) -> Option<u16> {
        let kind = &self.typed_module.db.ast.exprs.get(callee).kind;
        if let ExprKind::Var(ident) = kind {
            return self.foreign_globals.get(&ident.name).copied();
        }
        None
    }

    pub(super) fn ty_to_ffi_type(&self, ty_id: TyId) -> FfiType {
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
                _ => FfiType::Ptr,
            }
        } else {
            FfiType::Ptr
        }
    }

    pub(super) fn param_to_ffi_type(&self, param: &Param) -> FfiType {
        param
            .ty
            .map_or(FfiType::Ptr, |ty_id| self.ty_to_ffi_type(ty_id))
    }

    pub(super) fn extract_link_attr(&mut self, attrs: &[AttrId]) -> (Option<u32>, Option<u32>) {
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

    pub(super) fn emit_ffi_pins(&mut self, foreign_idx: u16) -> usize {
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
}
