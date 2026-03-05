//! AST → bytecode emitter (Phase 6).
//!
//! [`emit`] compiles a prelude [`ParsedModule`] and a user [`ParsedModule`]
//! into a [`Module`] that the Musi VM can execute directly.

#![allow(clippy::too_many_arguments)]
#![allow(clippy::too_many_lines)]

use std::collections::HashMap;
use std::fmt;

use musi_parse::ast::{
    AttrArg, BinOp, Cond, ElifChain, Expr, LitValue, Modifier, Pat, PostfixOp, PrefixOp,
    ParsedModule,
};
use musi_shared::{Arena, Idx, Interner};

use crate::{ConstEntry, FunctionEntry, Module, Opcode, SymbolEntry, SymbolFlags};

// ── Error ─────────────────────────────────────────────────────────────────────

/// Errors that can occur during bytecode generation.
#[derive(Debug)]
pub enum CodegenError {
    /// More than `u16::MAX` functions were defined.
    TooManyFunctions,
    /// More than `u16::MAX` constants were added to the const pool.
    TooManyConstants,
    /// More than `u16::MAX` symbols were added to the symbol table.
    TooManySymbols,
    /// A function has more than `u8::MAX` parameters.
    ParameterCountOverflow,
    /// A call-expression refers to a function name that is not defined.
    UnknownFunction(Box<str>),
    /// An expression kind that is not supported was encountered.
    UnsupportedExpr,
    /// A variable name was referenced but is not in scope.
    UndefinedVariable(Box<str>),
    /// Too many local variables (limit `u16::MAX`).
    TooManyLocals,
    /// A jump offset overflowed the `i32` range.
    JumpOffsetOverflow,
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TooManyFunctions => write!(f, "too many functions (limit u16::MAX)"),
            Self::TooManyConstants => write!(f, "too many constants (limit u16::MAX)"),
            Self::TooManySymbols => write!(f, "too many symbols (limit u16::MAX)"),
            Self::ParameterCountOverflow => {
                write!(f, "function has more than u8::MAX parameters")
            }
            Self::UnknownFunction(name) => write!(f, "unknown function '{name}'"),
            Self::UnsupportedExpr => {
                write!(f, "expression kind is not supported")
            }
            Self::UndefinedVariable(name) => write!(f, "undefined variable '{name}'"),
            Self::TooManyLocals => write!(f, "too many local variables (limit u16::MAX)"),
            Self::JumpOffsetOverflow => write!(f, "jump offset overflowed i32 range"),
        }
    }
}

// ── Intrinsic table ───────────────────────────────────────────────────────────

/// Returns the intrinsic id for a well-known native function name, or `None`.
fn intrinsic_id_for(name: &str) -> Option<u16> {
    match name {
        "writeln" => Some(1),
        "write" => Some(2),
        "int_to_string" => Some(3),
        _ => None,
    }
}

// ── Loop context ──────────────────────────────────────────────────────────────

/// State needed while emitting a loop body to support `break` and `cycle`.
struct LoopCtx {
    /// Byte position of the first instruction of the loop (for `cycle` / back-edges).
    start_pos: usize,
    /// Byte positions of `Br` placeholders emitted by `break` statements.
    /// These are patched to the instruction after the loop once the loop ends.
    break_fixups: Vec<usize>,
}

// ── Internal emitter state ────────────────────────────────────────────────────

/// Accumulates raw bytecode for a single function body.
///
/// Writes directly to a `Vec<u8>` so that jump offsets can be patched in place.
struct FnEmitter {
    /// Raw encoded bytecode for this function.
    code: Vec<u8>,
    /// Stack of lexical scopes: each scope maps a variable name to a local slot.
    scopes: Vec<HashMap<String, u16>>,
    /// The index of the next local-variable slot to allocate.
    next_slot: u16,
    /// Stack of loop contexts (for `break` / `cycle` support).
    loop_stack: Vec<LoopCtx>,
}

impl FnEmitter {
    const BR: u8 = 0x60;
    const BR_TRUE: u8 = 0x61;
    const BR_FALSE: u8 = 0x62;

    const fn new() -> Self {
        Self {
            code: Vec::new(),
            scopes: Vec::new(),
            next_slot: 0,
            loop_stack: Vec::new(),
        }
    }

    // ── Scope management ──────────────────────────────────────────────────────

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        let _discarded = self.scopes.pop();
    }

    /// Allocates a new local slot for `name` in the innermost scope.
    fn define_local(&mut self, name: &str) -> Result<u16, CodegenError> {
        let slot = self.next_slot;
        self.next_slot = self
            .next_slot
            .checked_add(1)
            .ok_or(CodegenError::TooManyLocals)?;
        let scope = self
            .scopes
            .last_mut()
            .expect("define_local called with no active scope");
        let _prev = scope.insert(name.to_owned(), slot);
        Ok(slot)
    }

    /// Looks up a variable name, searching from the innermost scope outward.
    fn lookup_local(&self, name: &str) -> Option<u16> {
        for scope in self.scopes.iter().rev() {
            if let Some(&slot) = scope.get(name) {
                return Some(slot);
            }
        }
        None
    }

    // ── Low-level emission ────────────────────────────────────────────────────

    /// Encodes `op` directly into the code buffer.
    fn push(&mut self, op: &Opcode) {
        op.encode_into(&mut self.code);
    }

    /// Current byte length of the code buffer.
    const fn len(&self) -> usize {
        self.code.len()
    }

    // ── Jump patching ─────────────────────────────────────────────────────────

    /// Emits a 5-byte branch instruction with a placeholder offset of `i32::MAX`.
    ///
    /// Returns the byte index of the instruction's first byte so the caller can
    /// patch it later with [`patch_jump_to_here`].
    fn emit_jump_placeholder(&mut self, opcode_tag: u8) -> usize {
        let pos = self.code.len();
        self.code.push(opcode_tag);
        self.code.extend_from_slice(&i32::MAX.to_le_bytes());
        pos
    }

    /// Patches the `i32` offset in the jump instruction at `fixup_pos` so that
    /// it jumps to the current end of the code buffer.
    ///
    /// The offset is relative to the byte *after* the 5-byte instruction, i.e.:
    ///   `offset = current_len - (fixup_pos + 5)`
    fn patch_jump_to_here(&mut self, fixup_pos: usize) -> Result<(), CodegenError> {
        let after_instr = fixup_pos
            .checked_add(5)
            .expect("fixup_pos + 5 fits usize");
        let target = self.code.len();
        let target_isize =
            isize::try_from(target).map_err(|_| CodegenError::JumpOffsetOverflow)?;
        let after_isize =
            isize::try_from(after_instr).map_err(|_| CodegenError::JumpOffsetOverflow)?;
        let offset_isize = target_isize.wrapping_sub(after_isize);
        let offset =
            i32::try_from(offset_isize).map_err(|_| CodegenError::JumpOffsetOverflow)?;
        self.code[fixup_pos + 1..fixup_pos + 5].copy_from_slice(&offset.to_le_bytes());
        Ok(())
    }

    /// Emits a `Br` instruction with an offset pointing back to `target_pos`.
    ///
    /// The offset is relative to the byte *after* the 5-byte instruction.
    fn emit_br_back(&mut self, target_pos: usize) -> Result<(), CodegenError> {
        // The Br instruction will be at the current position; it is 5 bytes long.
        let after_instr = self
            .code
            .len()
            .checked_add(5)
            .expect("code len + 5 fits usize");
        let target_isize =
            isize::try_from(target_pos).map_err(|_| CodegenError::JumpOffsetOverflow)?;
        let after_isize =
            isize::try_from(after_instr).map_err(|_| CodegenError::JumpOffsetOverflow)?;
        let offset_isize = target_isize.wrapping_sub(after_isize);
        let offset =
            i32::try_from(offset_isize).map_err(|_| CodegenError::JumpOffsetOverflow)?;
        self.push(&Opcode::Br(offset));
        Ok(())
    }

    // ── Loop context ──────────────────────────────────────────────────────────

    fn push_loop(&mut self, start_pos: usize) {
        self.loop_stack.push(LoopCtx {
            start_pos,
            break_fixups: Vec::new(),
        });
    }

    /// Removes the innermost loop context and returns it.
    fn pop_loop(&mut self) -> LoopCtx {
        self.loop_stack
            .pop()
            .expect("pop_loop called with no active loop")
    }

    /// Returns the start position of the innermost loop (for `cycle`).
    fn current_loop_start(&self) -> Option<usize> {
        self.loop_stack.last().map(|ctx| ctx.start_pos)
    }

    /// Records a `break` fixup position in the innermost loop context.
    fn push_break_fixup(&mut self, fixup_pos: usize) {
        let ctx = self
            .loop_stack
            .last_mut()
            .expect("push_break_fixup called outside loop");
        ctx.break_fixups.push(fixup_pos);
    }

    /// Patches all `break` fixups in `ctx` to jump to the current position.
    fn patch_break_fixups(&mut self, ctx: LoopCtx) -> Result<(), CodegenError> {
        for pos in ctx.break_fixups {
            self.patch_jump_to_here(pos)?;
        }
        Ok(())
    }

    /// Begins a new loop: records the current code position and pushes a loop
    /// context.  Returns the start position (for back-edges).
    fn start_loop(&mut self) -> usize {
        let start_pos = self.len();
        self.push_loop(start_pos);
        start_pos
    }

    /// Ends the current loop: pops the loop context, patches all break fixups,
    /// and pushes `Unit` (the result of a loop expression).
    fn close_loop(&mut self) -> Result<(), CodegenError> {
        let loop_ctx = self.pop_loop();
        self.patch_break_fixups(loop_ctx)?;
        self.push(&Opcode::LdImmUnit);
        Ok(())
    }

    // ── Finalisation ──────────────────────────────────────────────────────────

    /// Returns the total local count (used to populate `FunctionEntry::local_count`).
    const fn local_count(&self) -> u16 {
        self.next_slot
    }

    /// Appends all buffered code bytes into `buf`.
    fn flush_into(self, buf: &mut Vec<u8>) {
        buf.extend_from_slice(&self.code);
    }

    /// Byte length of the buffered code.
    const fn byte_len(&self) -> usize {
        self.code.len()
    }
}

// ── Registration context ──────────────────────────────────────────────────────

/// Bundles the mutable module and fn-map together to keep argument counts low.
struct RegCtx<'a> {
    module: &'a mut Module,
    fn_map: &'a mut HashMap<String, u16>,
}

// ── Public API ────────────────────────────────────────────────────────────────

/// Compiles a prelude module and a user module into a [`Module`] ready for the
/// Musi VM.
///
/// The prelude is processed first so that its native-function declarations are
/// visible when compiling the user code.  The returned module's last
/// `function_table` entry is always the synthetic `main` function that
/// sequentially executes the user-module's top-level statement expressions.
///
/// # Errors
///
/// Returns a [`CodegenError`] if any limit is exceeded or an unsupported
/// expression is encountered in statement position.
pub fn emit(
    prelude: &ParsedModule,
    user: &ParsedModule,
    interner: &Interner,
) -> Result<Module, CodegenError> {
    let mut module = Module::new();
    let mut fn_map: HashMap<String, u16> = HashMap::new();
    let mut reg = RegCtx {
        module: &mut module,
        fn_map: &mut fn_map,
    };

    // ── Pass 1: register all FnDef declarations ───────────────────────────────
    // Prelude first so prelude natives are visible to user code.
    for item_idx in &prelude.items {
        register_fn_def(*item_idx, &prelude.ctx.exprs, interner, &mut reg)?;
    }
    for item_idx in &user.items {
        register_fn_def(*item_idx, &user.ctx.exprs, interner, &mut reg)?;
    }

    // ── Pass 2: emit main body from user items ────────────────────────────────
    let mut main_emitter = FnEmitter::new();
    // Top-level statements live in an implicit scope.
    main_emitter.push_scope();

    for item_idx in &user.items {
        let expr = user.ctx.exprs.get(*item_idx);
        match expr {
            // Skip declarations — they were handled in Pass 1.
            Expr::FnDef { .. }
            | Expr::Lambda { .. }
            | Expr::Record { .. }
            | Expr::Choice { .. } => {}
            _ => {
                emit_expr(
                    expr,
                    &user.ctx.exprs,
                    interner,
                    reg.module,
                    reg.fn_map,
                    &mut main_emitter,
                )?;
                main_emitter.push(&Opcode::Drop);
            }
        }
    }

    main_emitter.pop_scope();
    main_emitter.push(&Opcode::Halt);

    // ── Finalize main ─────────────────────────────────────────────────────────
    let local_count = main_emitter.local_count();
    let code_offset = u32::try_from(module.code.len())
        .map_err(|_| CodegenError::TooManyConstants)?;
    let code_byte_len = main_emitter.byte_len();
    main_emitter.flush_into(&mut module.code);
    let code_length =
        u32::try_from(code_byte_len).map_err(|_| CodegenError::TooManyConstants)?;

    let main_sym_idx = push_symbol(
        &mut module,
        SymbolEntry {
            name: "main".into(),
            flags: SymbolFlags::new(0),
            intrinsic_id: 0xFFFF,
        },
    )?;

    // Index not needed beyond this point; bind to _ to satisfy unused_results.
    let _main_fn_idx = push_function(
        &mut module,
        FunctionEntry {
            symbol_idx: main_sym_idx,
            param_count: 0,
            local_count,
            code_offset,
            code_length,
        },
    )?;

    Ok(module)
}

// ── Pass 1 helpers ────────────────────────────────────────────────────────────

/// Attempts to register a single top-level item as a function definition.
/// Non-`FnDef` items are silently skipped.
fn register_fn_def(
    item_idx: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    ctx: &mut RegCtx<'_>,
) -> Result<(), CodegenError> {
    let Expr::FnDef {
        attrs,
        modifiers,
        name,
        params,
        body,
        ..
    } = exprs.get(item_idx)
    else {
        return Ok(());
    };

    let fn_name = interner.resolve(*name).to_owned();

    let is_native = body.is_none()
        || modifiers
            .iter()
            .any(|m| matches!(m, Modifier::Native(_)));
    let flags_raw: u8 = if is_native { SymbolFlags::NATIVE } else { 0 };

    let intrinsic_id: u16 = if is_native {
        let found = attrs.iter().find_map(|attr| {
            if interner.resolve(attr.name) != "intrinsic" {
                return None;
            }
            attr.args.first().and_then(|arg| match arg {
                AttrArg::Named { name: arg_name, .. } => {
                    intrinsic_id_for(interner.resolve(*arg_name))
                }
                AttrArg::Lit(LitValue::Str(sym), _) => {
                    intrinsic_id_for(interner.resolve(*sym))
                }
                AttrArg::Lit(
                    LitValue::Int(_)
                    | LitValue::Float(_)
                    | LitValue::Char(_)
                    | LitValue::Bool(_),
                    _,
                ) => None,
            })
        });
        found.or_else(|| intrinsic_id_for(&fn_name)).unwrap_or(0xFFFF)
    } else {
        0xFFFF
    };

    let sym_idx = push_symbol(
        ctx.module,
        SymbolEntry {
            name: fn_name.clone().into_boxed_str(),
            flags: SymbolFlags::new(flags_raw),
            intrinsic_id,
        },
    )?;

    let param_count =
        u8::try_from(params.len()).map_err(|_| CodegenError::ParameterCountOverflow)?;

    let fn_idx = push_function(
        ctx.module,
        FunctionEntry {
            symbol_idx: sym_idx,
            param_count,
            local_count: 0,
            code_offset: 0,
            code_length: 0,
        },
    )?;

    let _prev = ctx.fn_map.insert(fn_name, fn_idx);
    Ok(())
}

// ── Pass 2 helpers ────────────────────────────────────────────────────────────

/// Emits bytecode for a single expression, leaving one value on the stack.
fn emit_expr(
    expr: &Expr,
    exprs: &Arena<Expr>,
    interner: &Interner,
    module: &mut Module,
    fn_map: &HashMap<String, u16>,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match expr {
        // ── Atoms ─────────────────────────────────────────────────────────────
        Expr::Lit { value, .. } => emit_lit(value, interner, module, out),

        Expr::Unit { .. } => {
            out.push(&Opcode::LdImmUnit);
            Ok(())
        }

        Expr::Paren { inner, .. } => {
            let inner_expr = exprs.get(*inner);
            emit_expr(inner_expr, exprs, interner, module, fn_map, out)
        }

        Expr::Ident { name, .. } => {
            let name_str = interner.resolve(*name);
            let slot = out
                .lookup_local(name_str)
                .ok_or_else(|| CodegenError::UndefinedVariable(name_str.into()))?;
            out.push(&Opcode::LdLoc(slot));
            Ok(())
        }

        // ── Variable binding ──────────────────────────────────────────────────
        Expr::Bind { pat, init, .. } => {
            let Pat::Ident { name, .. } = pat else {
                return Err(CodegenError::UnsupportedExpr);
            };
            let name_str = interner.resolve(*name);
            // Allocate the slot before emitting the init so that any later
            // shadowing works correctly.
            let slot = out.define_local(name_str)?;
            emit_optional_expr(*init, exprs, interner, module, fn_map, out)?;
            out.push(&Opcode::StLoc(slot));
            // A binding expression produces Unit.
            out.push(&Opcode::LdImmUnit);
            Ok(())
        }

        // ── Assignment ────────────────────────────────────────────────────────
        Expr::Assign { target, value, .. } => {
            let target_expr = exprs.get(*target);
            let Expr::Ident { name, .. } = target_expr else {
                return Err(CodegenError::UnsupportedExpr);
            };
            let name_str = interner.resolve(*name);
            let slot = out
                .lookup_local(name_str)
                .ok_or_else(|| CodegenError::UndefinedVariable(name_str.into()))?;
            let val_expr = exprs.get(*value);
            emit_expr(val_expr, exprs, interner, module, fn_map, out)?;
            out.push(&Opcode::StLoc(slot));
            out.push(&Opcode::LdImmUnit);
            Ok(())
        }

        // ── Prefix operators ──────────────────────────────────────────────────
        Expr::Prefix { op, operand, .. } => {
            let opcode = match op {
                PrefixOp::Neg => Opcode::NegI64,
                PrefixOp::Not => Opcode::Not,
                PrefixOp::BitNot => Opcode::BitNot,
                PrefixOp::Deref | PrefixOp::AddrOf => {
                    return Err(CodegenError::UnsupportedExpr);
                }
            };
            let operand_expr = exprs.get(*operand);
            emit_expr(operand_expr, exprs, interner, module, fn_map, out)?;
            out.push(&opcode);
            Ok(())
        }

        // ── Binary operators ──────────────────────────────────────────────────
        Expr::Binary { op, lhs, rhs, .. } => {
            emit_binary(*op, *lhs, *rhs, exprs, interner, module, fn_map, out)
        }

        // ── Block ─────────────────────────────────────────────────────────────
        Expr::Block { stmts, tail, .. } => {
            out.push_scope();
            for stmt_idx in stmts {
                let stmt_expr = exprs.get(*stmt_idx);
                emit_expr(stmt_expr, exprs, interner, module, fn_map, out)?;
                out.push(&Opcode::Drop);
            }
            emit_optional_expr(*tail, exprs, interner, module, fn_map, out)?;
            out.pop_scope();
            Ok(())
        }

        // ── Postfix (function call) ────────────────────────────────────────────
        Expr::Postfix {
            base,
            op: PostfixOp::Call { args, .. },
            ..
        } => {
            let callee_expr = exprs.get(*base);
            let Expr::Ident { name, .. } = callee_expr else {
                return Err(CodegenError::UnsupportedExpr);
            };
            let callee_str = interner.resolve(*name);
            let fn_idx = fn_map
                .get(callee_str)
                .copied()
                .ok_or_else(|| CodegenError::UnknownFunction(callee_str.into()))?;

            for arg_idx in args {
                let arg_expr = exprs.get(*arg_idx);
                emit_expr(arg_expr, exprs, interner, module, fn_map, out)?;
            }

            out.push(&Opcode::Call(fn_idx));
            Ok(())
        }

        // ── Control flow — if/elif/else ────────────────────────────────────────
        Expr::If {
            cond,
            then_body,
            elif_chains,
            else_body,
            ..
        } => emit_if(
            cond,
            *then_body,
            elif_chains,
            *else_body,
            exprs,
            interner,
            module,
            fn_map,
            out,
        ),

        // ── Control flow — while ───────────────────────────────────────────────
        Expr::While { cond, guard, body, .. } => {
            if guard.is_some() {
                return Err(CodegenError::UnsupportedExpr);
            }
            emit_while(cond, *body, exprs, interner, module, fn_map, out)
        }

        // ── Control flow — loop ────────────────────────────────────────────────
        Expr::Loop { body, post_cond, .. } => {
            if post_cond.is_some() {
                return Err(CodegenError::UnsupportedExpr);
            }
            emit_loop(*body, exprs, interner, module, fn_map, out)
        }

        // ── Control flow — for ─────────────────────────────────────────────────
        Expr::For { pat, iter, guard, body, .. } => {
            if guard.is_some() {
                return Err(CodegenError::UnsupportedExpr);
            }
            emit_for(pat, *iter, *body, exprs, interner, module, fn_map, out)
        }

        // ── Control flow — break ───────────────────────────────────────────────
        Expr::Break { label, value, .. } => {
            if label.is_some() {
                return Err(CodegenError::UnsupportedExpr);
            }
            emit_optional_expr(*value, exprs, interner, module, fn_map, out)?;
            let fixup = out.emit_jump_placeholder(FnEmitter::BR);
            out.push_break_fixup(fixup);
            Ok(())
        }

        // ── Control flow — cycle ───────────────────────────────────────────────
        Expr::Cycle { label, guard, .. } => {
            if label.is_some() || guard.is_some() {
                return Err(CodegenError::UnsupportedExpr);
            }
            let start_pos = out
                .current_loop_start()
                .ok_or(CodegenError::UnsupportedExpr)?;
            out.emit_br_back(start_pos)?;
            Ok(())
        }

        // ── Control flow — return ──────────────────────────────────────────────
        Expr::Return { value, .. } => {
            emit_optional_expr(*value, exprs, interner, module, fn_map, out)?;
            out.push(&Opcode::Ret);
            Ok(())
        }

        // ── Unsupported ───────────────────────────────────────────────────────
        Expr::Tuple { .. }
        | Expr::Array { .. }
        | Expr::AnonRec { .. }
        | Expr::Match { .. }
        | Expr::Label { .. }
        | Expr::Defer { .. }
        | Expr::Import { .. }
        | Expr::Record { .. }
        | Expr::Choice { .. }
        | Expr::FnDef { .. }
        | Expr::Lambda { .. }
        | Expr::Postfix { .. }
        | Expr::Error { .. } => Err(CodegenError::UnsupportedExpr),
    }
}

/// Emits an optional expression: if `Some`, emits the expression; if `None`,
/// pushes `Unit`.
fn emit_optional_expr(
    opt: Option<Idx<Expr>>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    module: &mut Module,
    fn_map: &HashMap<String, u16>,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match opt {
        Some(idx) => {
            let e = exprs.get(idx);
            emit_expr(e, exprs, interner, module, fn_map, out)
        }
        None => {
            out.push(&Opcode::LdImmUnit);
            Ok(())
        }
    }
}

// ── Binary operator emission ──────────────────────────────────────────────────

fn emit_short_circuit_binop(
    lhs: Idx<Expr>,
    rhs: Idx<Expr>,
    branch_tag: u8,
    exprs: &Arena<Expr>,
    interner: &Interner,
    module: &mut Module,
    fn_map: &HashMap<String, u16>,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let lhs_expr = exprs.get(lhs);
    emit_expr(lhs_expr, exprs, interner, module, fn_map, out)?;
    let fixup = out.emit_jump_placeholder(branch_tag);
    let rhs_expr = exprs.get(rhs);
    emit_expr(rhs_expr, exprs, interner, module, fn_map, out)?;
    out.patch_jump_to_here(fixup)
}

fn emit_binary(
    op: BinOp,
    lhs: Idx<Expr>,
    rhs: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    module: &mut Module,
    fn_map: &HashMap<String, u16>,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    // Short-circuit operators require special control-flow treatment.
    match op {
        BinOp::And => return emit_short_circuit_binop(lhs, rhs, FnEmitter::BR_FALSE, exprs, interner, module, fn_map, out),
        BinOp::Or  => return emit_short_circuit_binop(lhs, rhs, FnEmitter::BR_TRUE, exprs, interner, module, fn_map, out),
        BinOp::Add
        | BinOp::Sub
        | BinOp::Mul
        | BinOp::Div
        | BinOp::Rem
        | BinOp::BitOr
        | BinOp::BitXor
        | BinOp::BitAnd
        | BinOp::Shl
        | BinOp::Shr
        | BinOp::Xor
        | BinOp::Eq
        | BinOp::NotEq
        | BinOp::Lt
        | BinOp::Gt
        | BinOp::LtEq
        | BinOp::GtEq
        | BinOp::In
        | BinOp::Range
        | BinOp::RangeExcl
        | BinOp::Cons => {}
    }

    // For all remaining ops: emit lhs, rhs, then the opcode.
    let lhs_expr = exprs.get(lhs);
    emit_expr(lhs_expr, exprs, interner, module, fn_map, out)?;
    let rhs_expr = exprs.get(rhs);
    emit_expr(rhs_expr, exprs, interner, module, fn_map, out)?;

    let opcode = match op {
        BinOp::Add => Opcode::AddI64,
        BinOp::Sub => Opcode::SubI64,
        BinOp::Mul => Opcode::MulI64,
        BinOp::Div => Opcode::DivI64,
        BinOp::Rem => Opcode::RemI64,
        BinOp::BitOr => Opcode::BitOr,
        BinOp::BitXor | BinOp::Xor => Opcode::BitXor,
        BinOp::BitAnd => Opcode::BitAnd,
        BinOp::Shl => Opcode::Shl,
        BinOp::Shr => Opcode::Shr,
        BinOp::Eq => Opcode::EqI64,
        BinOp::NotEq => Opcode::NeqI64,
        BinOp::Lt => Opcode::LtI64,
        BinOp::Gt => Opcode::GtI64,
        BinOp::LtEq => Opcode::LeqI64,
        BinOp::GtEq => Opcode::GeqI64,
        BinOp::In
        | BinOp::Range
        | BinOp::RangeExcl
        | BinOp::Cons
        | BinOp::And
        | BinOp::Or => {
            return Err(CodegenError::UnsupportedExpr);
        }
    };
    out.push(&opcode);
    Ok(())
}

// ── If / elif / else emission ─────────────────────────────────────────────────

fn emit_if(
    cond: &Cond,
    then_body: Idx<Expr>,
    elif_chains: &[ElifChain],
    else_body: Option<Idx<Expr>>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    module: &mut Module,
    fn_map: &HashMap<String, u16>,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    // Emit the condition.
    emit_cond(cond, exprs, interner, module, fn_map, out)?;

    let else_fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);

    // Then body.
    let then_expr = exprs.get(then_body);
    emit_expr(then_expr, exprs, interner, module, fn_map, out)?;

    let first_end_fixup = out.emit_jump_placeholder(FnEmitter::BR);

    // Patch the brfalse to here (start of elif/else).
    out.patch_jump_to_here(else_fixup)?;

    // Collect all Br-to-end fixups so they can be patched together.
    let mut end_fixups: Vec<usize> = vec![first_end_fixup];

    // Emit elif chains.
    for chain in elif_chains {
        if chain.guard.is_some() {
            return Err(CodegenError::UnsupportedExpr);
        }
        emit_cond(&chain.cond, exprs, interner, module, fn_map, out)?;
        let next_fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);
        let body_expr = exprs.get(chain.body);
        emit_expr(body_expr, exprs, interner, module, fn_map, out)?;
        let end_fixup = out.emit_jump_placeholder(FnEmitter::BR);
        end_fixups.push(end_fixup);
        // Patch the brfalse to here (start of next elif/else).
        out.patch_jump_to_here(next_fixup)?;
    }

    emit_optional_expr(else_body, exprs, interner, module, fn_map, out)?;

    // Patch all unconditional jumps to the end (current position).
    for fixup in end_fixups {
        out.patch_jump_to_here(fixup)?;
    }

    Ok(())
}

// ── Condition emission ────────────────────────────────────────────────────────

fn emit_cond(
    cond: &Cond,
    exprs: &Arena<Expr>,
    interner: &Interner,
    module: &mut Module,
    fn_map: &HashMap<String, u16>,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match cond {
        Cond::Expr(idx) => {
            let e = exprs.get(*idx);
            emit_expr(e, exprs, interner, module, fn_map, out)
        }
        Cond::Case { .. } => Err(CodegenError::UnsupportedExpr),
    }
}

// ── While emission ────────────────────────────────────────────────────────────

fn emit_while(
    cond: &Cond,
    body: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    module: &mut Module,
    fn_map: &HashMap<String, u16>,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let start_pos = out.start_loop();

    emit_cond(cond, exprs, interner, module, fn_map, out)?;

    let end_fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);

    let body_expr = exprs.get(body);
    emit_expr(body_expr, exprs, interner, module, fn_map, out)?;
    out.push(&Opcode::Drop); // body value is discarded

    out.emit_br_back(start_pos)?;
    out.patch_jump_to_here(end_fixup)?;
    out.close_loop()
}

// ── Loop emission ─────────────────────────────────────────────────────────────

fn emit_loop(
    body: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    module: &mut Module,
    fn_map: &HashMap<String, u16>,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    let start_pos = out.start_loop();

    let body_expr = exprs.get(body);
    emit_expr(body_expr, exprs, interner, module, fn_map, out)?;
    out.push(&Opcode::Drop); // body value is discarded

    out.emit_br_back(start_pos)?;
    out.close_loop()
}

// ── For emission ──────────────────────────────────────────────────────────────

fn emit_for(
    pat: &Pat,
    iter: Idx<Expr>,
    body: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    module: &mut Module,
    fn_map: &HashMap<String, u16>,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    // Only simple identifier patterns are supported.
    let Pat::Ident { name: pat_name, suffix: None, .. } = pat else {
        return Err(CodegenError::UnsupportedExpr);
    };

    // Only range iterators are supported: `a..b` or `a..<b`.
    let iter_expr = exprs.get(iter);
    let Expr::Binary { op, lhs, rhs, .. } = iter_expr else {
        return Err(CodegenError::UnsupportedExpr);
    };
    if !matches!(op, BinOp::Range | BinOp::RangeExcl) {
        return Err(CodegenError::UnsupportedExpr);
    }
    // Both `a..b` and `a..<b` produce `a <= x < b` semantics; use LtI64.
    let use_lt = true;

    // Emit a new scope for the loop variables.
    out.push_scope();

    // Allocate counter and limit slots.
    let pat_name_str = interner.resolve(*pat_name);
    let counter_slot = out.define_local(pat_name_str)?;
    let limit_slot = out.define_local("$limit")?;

    // Initialise counter = lhs, limit = rhs.
    let lhs_expr = exprs.get(*lhs);
    emit_expr(lhs_expr, exprs, interner, module, fn_map, out)?;
    out.push(&Opcode::StLoc(counter_slot));

    let rhs_expr = exprs.get(*rhs);
    emit_expr(rhs_expr, exprs, interner, module, fn_map, out)?;
    out.push(&Opcode::StLoc(limit_slot));

    let start_pos = out.start_loop();

    // Condition: counter < limit (or <= for inclusive range, though grammar
    // currently only has exclusive upper-bound ranges).
    out.push(&Opcode::LdLoc(counter_slot));
    out.push(&Opcode::LdLoc(limit_slot));
    if use_lt {
        out.push(&Opcode::LtI64);
    } else {
        out.push(&Opcode::LeqI64);
    }
    let end_fixup = out.emit_jump_placeholder(FnEmitter::BR_FALSE);

    // Body.
    let body_expr = exprs.get(body);
    emit_expr(body_expr, exprs, interner, module, fn_map, out)?;
    out.push(&Opcode::Drop);

    // Increment: counter = counter + 1.
    out.push(&Opcode::LdLoc(counter_slot));
    out.push(&Opcode::LdImmI64(1));
    out.push(&Opcode::AddI64);
    out.push(&Opcode::StLoc(counter_slot));

    // Back-edge.
    out.emit_br_back(start_pos)?;

    out.patch_jump_to_here(end_fixup)?;
    out.close_loop()?;

    out.pop_scope();
    Ok(())
}

/// Emits a literal value onto the stack.
fn emit_lit(
    value: &LitValue,
    interner: &Interner,
    module: &mut Module,
    out: &mut FnEmitter,
) -> Result<(), CodegenError> {
    match value {
        LitValue::Str(sym) => {
            let s: Box<str> = interner.resolve(*sym).into();
            let const_idx = push_const(module, ConstEntry::String(s))?;
            out.push(&Opcode::LdConst(const_idx));
            Ok(())
        }
        LitValue::Int(v) => {
            out.push(&Opcode::LdImmI64(*v));
            Ok(())
        }
        LitValue::Float(v) => {
            out.push(&Opcode::LdImmF64(*v));
            Ok(())
        }
        LitValue::Bool(v) => {
            out.push(&Opcode::LdImmBool(*v));
            Ok(())
        }
        LitValue::Char(c) => {
            // Encode char as i64 Unicode scalar value.
            let code_point = i64::from(u32::from(*c));
            out.push(&Opcode::LdImmI64(code_point));
            Ok(())
        }
    }
}

// ── Module mutation helpers ───────────────────────────────────────────────────

/// Pushes a [`SymbolEntry`] and returns its index, or errors if the table is
/// full.
fn push_symbol(module: &mut Module, entry: SymbolEntry) -> Result<u16, CodegenError> {
    let idx = u16::try_from(module.symbol_table.len())
        .map_err(|_| CodegenError::TooManySymbols)?;
    module.symbol_table.push(entry);
    Ok(idx)
}

/// Pushes a [`FunctionEntry`] and returns its index, or errors if the table is
/// full.
fn push_function(module: &mut Module, entry: FunctionEntry) -> Result<u16, CodegenError> {
    let idx = u16::try_from(module.function_table.len())
        .map_err(|_| CodegenError::TooManyFunctions)?;
    module.function_table.push(entry);
    Ok(idx)
}

/// Pushes a [`ConstEntry`] and returns its index, or errors if the pool is
/// full.
fn push_const(module: &mut Module, entry: ConstEntry) -> Result<u16, CodegenError> {
    let idx = u16::try_from(module.const_pool.len())
        .map_err(|_| CodegenError::TooManyConstants)?;
    module.const_pool.push(entry);
    Ok(idx)
}
