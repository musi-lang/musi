use super::*;

#[expect(
    clippy::unwrap_in_result,
    reason = "capacity overflow `expect`s guard structural invariants (e.g. >65535 locals) — \
              these are compiler ICEs, not recoverable errors that belong in EmitError"
)]
impl Emitter<'_> {
    pub(super) fn emit_import(&mut self, path: Symbol, kind: &ImportKind) {
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

    pub(super) fn emit_import_record(&mut self, alias: Symbol, globals: &[ImportedGlobal]) {
        if globals.is_empty() {
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
}
