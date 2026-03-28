use std::mem;

use super::*;

#[expect(
    clippy::unwrap_in_result,
    reason = "capacity overflow `expect`s guard structural invariants (e.g. >65535 locals) — \
              these are compiler ICEs, not recoverable errors that belong in EmitError"
)]
impl Emitter<'_> {
    pub(super) fn emit_module(&mut self) -> EmitResult {
        let root = self.typed_module.db.ast.root.clone();
        for expr_id in root {
            self.emit_top_level(expr_id)?;
        }
        self.finish_main();
        Ok(())
    }

    pub(super) fn emit_top_level(&mut self, expr_id: ExprId) -> EmitResult {
        let kind = self.typed_module.db.ast.exprs.get(expr_id).kind.clone();
        if let ExprKind::Let(binding) = kind {
            self.emit_top_let(&binding)?;
        } else {
            self.emit_expr(expr_id)?;
        }
        Ok(())
    }

    pub(super) fn emit_top_let(&mut self, binding: &LetBinding) -> EmitResult {
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

    pub(super) fn alloc_top_level_global(&mut self, name: Symbol, vis: Visibility) -> u16 {
        let global_idx = u16::try_from(self.globals.len()).expect("too many globals (>65535)");
        self.globals.push(GlobalEntry {
            name,
            source_name: self.typed_module.db.interner.resolve(name).to_owned(),
            exported: vis == Visibility::Exported || vis == Visibility::Opaque,
            opaque: vis == Visibility::Opaque,
        });
        global_idx
    }

    pub(super) fn compile_function_method(
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

    pub(super) fn emit_function(
        &mut self,
        name: Symbol,
        params: &[Param],
        body: ExprId,
    ) -> EmitResult {
        let _ = self.compile_function_method(Some(name), params, body)?;
        Ok(())
    }

    pub(super) fn finish_main(&mut self) {
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
}
